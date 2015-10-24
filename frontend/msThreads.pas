unit msThreads;

interface

uses
  Classes, SysUtils, StrUtils, shlObj, Dialogs, ComCtrls,
  // mte units
  mteHelpers, mteLogger, mteTracker,
  // mp units
  msFrontend,
  // xedit units
  wbBSA, wbInterface, wbImplementation;


type
  // THREADS AND CALLBACKS
  TCallback = procedure of object;
  TStatusCallback = procedure(s: string) of object;
  TInitThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TConnectThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TLoaderThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TPatchThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TSaveThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TTreeThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  InitCallback, LoaderCallback, ErrorCheckCallback, ErrorFixCallback,
  PatchCallback, SaveCallback, UpdateCallback, ConnectCallback, TreeCallback: TCallback;
  StatusCallback: TStatusCallback;

implementation


{******************************************************************************}
{ THREAD METHODS
  These are threads that the program will run for various large jobs which need
  to be decoupled from general program operation and the GUI.

  List of methods:
  - TInitThread.Execute
  - LoaderProgress
  - TLoaderThread.Execute
  - TErrorCheckThread.Execute
  - TMergeThread.Execute
  - TSaveThread.Execute
}
{******************************************************************************}

{ TConnectThread }
procedure TConnectThread.Execute;
begin
  ConnectToServer;
  if Assigned(ConnectCallback) then
    Synchronize(nil, ConnectCallback);
end;

{ TInitThread }
procedure TInitThread.Execute;
var
  wbPluginsFileName: string;
  sl: TStringList;
  i: integer;
  plugin: TPlugin;
  aFile: IwbFile;
begin
  try
    // INITIALIZE VARIABLES
    ProgramVersion := GetVersionMem;
    TempPath := ProgramPath + 'temp\';
    LogPath := ProgramPath + 'logs\';
    ProfilePath := ProgramPath + 'profiles\' + CurrentProfile.name + '\';
    ForceDirectories(TempPath);
    ForceDirectories(LogPath);
    ForceDirectories(ProfilePath);
    PatchesList := TList.Create;
    PluginsList := TList.Create;
    bLoaderDone := false;
    LastStatusTime := 0;
    Status := TmsStatus.Create;
    LoadChangelog;

    // SET GAME VARS
    SetGame(CurrentProfile.gameMode);
    wbVWDInTemporary := wbGameMode in [gmTES5, gmFO3, gmFNV];
    Logger.Write('GENERAL', 'Game', 'Using '+wbGameName);
    Logger.Write('GENERAL', 'Path', 'Using '+wbDataPath);

    // INITIALIZE SETTINGS FOR GAME
    LoadSettings;
    LoadLanguage;
    if settings.usingMO then
      ModOrganizerInit;

    // INITIALIZE TES5EDIT API
    wbDisplayLoadOrderFormID := True;
    wbSortSubRecords := True;
    wbDisplayShorterNames := True;
    wbHideUnused := True;
    wbFlagsAsArray := True;
    wbRequireLoadOrder := True;
    wbLanguage := settings.language;
    wbEditAllowed := True;
    handler := wbCreateContainerHandler;
    handler._AddRef;
    LoadExcludedGroups;

    // IF AUTOMATIC UPDATING IS ENABLED, CHECK FOR UPDATE
    InitializeClient;
    {if settings.updateDictionary or settings.updateProgram then try
      Tracker.Write('Checking for updates');
      ConnectToServer;
      if TCPClient.Connected then begin
        UpdateCallback;
        if bInstallUpdate then begin
          InitCallback;
          exit;
        end;
      end;
    except
      on x: Exception do
        Logger.Write('CLIENT', 'Update', 'Failed to get automatic update '+x.Message);
    end;  }

    // INITIALIZE DICTIONARY
    dictionaryFilename := wbAppName+'Dictionary.txt';
    Logger.Write('GENERAL', 'Dictionary', 'Using '+dictionaryFilename);
    LoadDictionary;

    // INITIALIZE TES5EDIT DEFINITIONS
    Logger.Write('GENERAL', 'Definitions', 'Using '+wbAppName+'Edit Definitions');
    LoadDefinitions;

    // LOAD PATCHES
    Tracker.Write('Loading settings');
    LoadPatches;

    // PREPARE TO LOAD PLUGINS
    if settings.usingMO then
      wbPluginsFileName := settings.MOPath + 'profiles\'+ActiveModProfile+'\plugins.txt'
    else
      wbPluginsFileName := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA) + wbGameName + '\Plugins.txt';
    Logger.Write('GENERAL', 'Load Order', 'Using '+wbPluginsFileName);
    sl := TStringList.Create;
    sl.LoadFromFile(wbPluginsFileName);
    RemoveCommentsAndEmpty(sl);
    RemoveMissingFiles(sl);
    // if GameMode is not Skyrim sort by date modified
    // else add Update.esm and Skyrim.esm to load order
    if wbGameMode <> gmTES5 then begin
      GetPluginDates(sl);
      sl.CustomSort(PluginListCompare);
    end
    else begin
      if sl.IndexOf('Update.esm') = -1 then
        sl.Insert(0, 'Update.esm');
      if sl.IndexOf('Skyrim.esm') = -1 then
        sl.Insert(0, 'Skyrim.esm');
    end;

    // PRINT LOAD ORDER TO LOG
    for i := 0 to Pred(sl.Count) do
      Logger.Write('LOAD', 'Order', '['+IntToHex(i, 2)+'] '+sl[i]);

    // LOAD PLUGINS
    for i := 0 to Pred(sl.Count) do begin
      Tracker.Write('Loading '+sl[i]);
      try
        plugin := TPlugin.Create;
        plugin.filename := sl[i];
        plugin._File := wbFile(wbDataPath + sl[i], i);
        plugin._File._AddRef;
        plugin.GetData;
        PluginsList.Add(Pointer(plugin));
      except
        on x: Exception do begin
          Logger.Write('ERROR', 'Load', 'Exception loading '+sl[i]);
          Logger.Write('ERROR', 'Load', x.Message);
          bLoadException := true;
        end;
      end;

      // load hardcoded dat
      if i = 0 then try
        if not FileExists(wbProgramPath + wbGameName + wbHardcodedDat) then
          raise Exception.Create('Hardcoded dat file missing!');
        aFile := wbFile(wbProgramPath + wbGameName + wbHardcodedDat, 0);
        aFile._AddRef;
      except
        on x: Exception do begin
          Logger.Write('ERROR', 'Load', 'Exception loading '+wbGameName+wbHardcodedDat);
          Logger.Write('ERROR', 'Load', 'Please download and install this dat file!');
          raise x;
        end;
      end;
    end;

    // ASSIGN PATCHES TO PLUGINS
    AssignPatchesToPlugins;

    // LOAD PLUGIN INFORMATION
    Tracker.Write('Loading plugin information');
    LoadPluginInfo;

    // CLEAN UP
    sl.Free;
  except
    on x: Exception do begin
      if Assigned(sl) then
        sl.Free;
      bInitException := true;
      Logger.Write('ERROR', 'Load', x.Message);
    end;
  end;

  if Assigned(InitCallback) then
    Synchronize(nil, InitCallback);
end;

{ TLoaderThread }
procedure LoaderProgress(const s: string);
begin
  if s <> '' then
    Logger.Write('LOAD', 'Background', s);
  if bForceTerminate then
    Abort;
end;

procedure TLoaderThread.Execute;
var
  i: Integer;
  f: IwbFile;
  plugin: TPlugin;
begin
  StatusCallback(Format('%s (%d/%d)',
    [GetString('mpMain_LoaderInProgress'), 1, PluginsList.Count]));
  try
    for i := 0 to Pred(PluginsList.Count) do begin
      StatusCallback(Format('%s (%d/%d)',
        [GetString('mpMain_LoaderInProgress'), i + 1, PluginsList.Count]));
      plugin := TPlugin(PluginsList[i]);
      f := plugin._File;
      if SameText(plugin.filename, wbGameName + '.esm') then
        continue;
      LoaderProgress('[' + plugin.filename + '] Building reference info.');
      f.BuildRef;
      if bForceTerminate then begin
        LoaderProgress('Aborted.');
        break;
      end;
    end;
  except
    on E: Exception do begin
      LoaderProgress('Fatal: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := true;
      bInitException := true;
    end;
  end;
  bLoaderDone := true;
  LoaderProgress('finished');
  StatusCallback(GetString('mpMain_LoaderFinished'));
  if Assigned(LoaderCallback) then
    Synchronize(nil, LoaderCallback);
end;

{ TMergeThread }
procedure TPatchThread.Execute;
var
  i: integer;
  patch: TPatch;
begin
  // build merges
  for i := 0 to Pred(patchesToBuild.Count) do begin
    if Tracker.Cancel then break;
    patch := TPatch(patchesToBuild[i]);
    StatusCallback(Format('%s "%s" (%d/%d)',
      [GetString('mpProg_Merging'), patch.name, i + 1, patchesToBuild.Count]));
    try
      if (patch.status in RebuildStatuses) then
        //RebuildPatch(patch)
      else
        //BuildMerge(patch);
    except
      on x : Exception do begin
        patch.status := psFailed;
        Tracker.Write('Exception: '+x.Message);
      end;
    end;
    Tracker.Write(' '#13#10);
    Tracker.SetProgress(IntegerListSum(timeCosts, i));
    if Tracker.Cancel then Tracker.Write('Merging canceled.');
  end;

  // say thread is done if it wasn't cancelled
  if not Tracker.Cancel then
    Tracker.Write('All done!');

  // clean up, fire callback
  StatusCallback(GetString('mpProg_DoneBuilding'));
  Tracker.Cancel := false;
  if Assigned(PatchCallback) then
    Synchronize(nil, PatchCallback);
end;

{ TSaveThread }
procedure TSaveThread.Execute;
begin
  // send statistics, then disconnect from the server
  if not settings.dontSendStatistics then
    SendStatistics;
  TCPClient.Disconnect;

  // save ESPs only if it's safe to do so
  if not bInitException then begin
    // Save plugin errors
    SavePluginInfo;
    Tracker.SetProgress(PluginsList.Count + 1);
    Tracker.Write(' ');
    // save merges
    SavePatches;
    // rename saved plugins
    if bLoaderDone then RenameSavedPlugins;
  end;

  // save statistics and settings
  SaveStatistics;
  SaveSettings;

  // delete temppath
  if not settings.preserveTempPath then
    DeleteTempPath;

  Tracker.Cancel := false;
  if Assigned(SaveCallback) then
    Synchronize(nil, SaveCallback);
end;

function GetChild(node: TTreeNode; text: string): TTreeNode;
var
  child: TTreeNode;
begin
  Result := nil;
  child := node.getFirstChild;
  while Assigned(child) do begin
    if child.Text = text then begin
      Result := child;
      break;
    end;
    child := child.getNextSibling;
  end;
end;

procedure BuildTreeElements(node: TTreeNode; e: IwbElement);
var
  container: IwbContainerElementRef;
  element: IwbElement;
  child: TTreeNode;
  i: Integer;
begin
  // treat element as container so we can access its children
  if not Supports(e, IwbContainerElementRef, container) then
    exit;

  // loop through children
  for i := 0 to Pred(container.ElementCount) do begin
    if Tracker.Cancel then break;
    element := container.Elements[i];
    child := GetChild(node, element.name);
    // create child if it doesn't exist yet
    if not Assigned(child) then begin
      child := TreeView.Items.AddChild(node, element.Name);
      child.Data := Pointer(TElementData.Create(0, false, false, false));
    end;
    BuildTreeElements(child, element);
  end;
end;

procedure BuildTreeRecords(node: TTreeNode; e: IwbElement;
  overridesOnly: boolean);
var
  container: IwbContainerElementRef;
  element: IwbElement;
  group: IwbGroupRecord;
  mainRecord: IwbMainRecord;
  child: TTreeNode;
  i: Integer;
begin
  // treat element as container so we can access its children
  if not Supports(e, IwbContainerElementRef, container) then
    exit;

  // loop through children
  for i := 0 to Pred(container.ElementCount) do begin
    if Tracker.Cancel then break;
    element := container.Elements[i];
    if Supports(element, IwbGroupRecord, group) then begin
      if MatchStr(group.GetShortName, excludedGroups) then
        continue;
      child := GetChild(node, group.GetShortName);
      Tracker.Write(StringOfChar(' ', (node.Level + 1) * 2) + 'Processing group '+group.GetShortName);
      // create child if it doesn't exist yet
      if not Assigned(child) then begin
        child := TreeView.Items.AddChild(node, group.GetShortName);
        child.Data := Pointer(TElementData.Create(0, false, false, false));
      end;
      BuildTreeRecords(child, element, overridesOnly);
    end
    {else if Supports(element, IwbMainRecord, mainRecord) then begin
      if mainRecord.signature = 'TES4' then
        continue;
      Tracker.UpdateProgress(1);
      if not overridesOnly then
        BuildTreeElements(node, IwbElement(mainRecord))
      else if IsOverride(mainRecord) then
        BuildTreeElements(node, IwbElement(mainRecord));
    end};
  end;
end;

procedure TTreeThread.Execute;
var
  i: Integer;
  rootNode: TTreeNode;
  plugin: TPlugin;
begin
  // execute thread
  rootNode := TreeView.Items.Add(nil, 'Records');
  for i := 0 to Pred(pluginsToHandle.Count) do begin
    if Tracker.Cancel then break;
    plugin := TPlugin(pluginsToHandle[i]);
    Tracker.Write('Building tree for '+plugin.filename);
    BuildTreeRecords(rootNode, plugin._File, bOverridesOnly);
    Tracker.Write(' ');
    Tracker.SetProgress(IntegerListSum(timeCosts, i));
    if Tracker.Cancel then Tracker.Write('Tree building canceled.');
  end;

  // say thread is done if it wasn't cancelled
  if not Tracker.Cancel then
    Tracker.Write('All done!');

  // clean up, fire callback
  Tracker.Cancel := false;
  if Assigned(TreeCallback) then
    Synchronize(nil, TreeCallback);
end;

end.
