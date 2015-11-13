unit msThreads;

interface

uses
  Classes, SysUtils, StrUtils, shlObj, Dialogs, ComCtrls,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteTracker,
  // ms units
  msFrontend, msSmash,
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

var
  InitCallback, LoaderCallback, ErrorCheckCallback, ErrorFixCallback,
  PatchCallback, SaveCallback, UpdateCallback, ConnectCallback: TCallback;
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
  FreeOnTerminate := true;
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
  FreeOnTerminate := true;
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
    wbLoaderDone := false;
    LastStatusTime := 0;
    Status := TmsStatus.Create;

    // SET GAME VARS
    SetGame(CurrentProfile.gameMode);
    wbVWDInTemporary := wbGameMode in [gmTES5, gmFO3, gmFNV];
    Logger.Write('GENERAL', 'Game', 'Using '+wbGameName);
    Logger.Write('GENERAL', 'Path', 'Using '+wbDataPath);

    // INITIALIZE SETTINGS FOR GAME
    LoadSettings;
    LoadStatistics;
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
    wbContainerHandler := wbCreateContainerHandler;
    LoadExcludedGroups;
    wbContainerHandler.AddFolder(wbDataPath);

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

    // LOAD SMASH SETTINGS
    Tracker.Write('Loading smash settings');
    LoadSmashSettings;

    // LOAD PATCHES
    Tracker.Write('Loading patches');
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
        // load plugin and add to pluginslist
        plugin := TPlugin.Create;
        plugin.filename := sl[i];
        plugin._File := wbFile(wbDataPath + sl[i], i);
        plugin._File._AddRef;
        plugin.GetData;
        PluginsList.Add(Pointer(plugin));
        LoadBSA(sl[i]);
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

  // increment times run and call the callback
  Inc(sessionStatistics.timesRun);
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
  FreeOnTerminate := true;
  StatusCallback(Format('%s (%d/%d)',
    [GetString('msMain_LoaderInProgress'), 1, PluginsList.Count]));
  try
    for i := 0 to Pred(PluginsList.Count) do begin
      StatusCallback(Format('%s (%d/%d)',
        [GetString('msMain_LoaderInProgress'), i + 1, PluginsList.Count]));
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
  LoaderProgress('finished');
  StatusCallback(GetString('msMain_LoaderFinished'));
  if Assigned(LoaderCallback) then
    Synchronize(nil, LoaderCallback);
end;

{ TMergeThread }
procedure TPatchThread.Execute;
var
  i: integer;
  patch: TPatch;
begin
  FreeOnTerminate := true;
  // build merges
  for i := 0 to Pred(patchesToBuild.Count) do begin
    if Tracker.Cancel then break;
    patch := TPatch(patchesToBuild[i]);
    StatusCallback(Format('%s "%s" (%d/%d)',
      [GetString('msProg_Smashing'), patch.name, i + 1, patchesToBuild.Count]));
    try
      if (patch.status in RebuildStatuses) then
        RebuildPatch(patch)
      else
        BuildPatch(patch);
    except
      on x : Exception do begin
        patch.status := psFailed;
        Tracker.Write('Exception: '+x.Message);
      end;
    end;
    Tracker.Write(' '#13#10);
    Tracker.SetProgress(IntegerListSum(timeCosts, i));
    if Tracker.Cancel then Tracker.Write('Smashing canceled.');
  end;

  // say thread is done if it wasn't cancelled
  if not Tracker.Cancel then
    Tracker.Write('All done!');

  // clean up, fire callback
  StatusCallback(GetString('msProg_DoneBuilding'));
  Tracker.Cancel := false;
  if Assigned(PatchCallback) then
    Synchronize(nil, PatchCallback);
end;

{ TSaveThread }
procedure TSaveThread.Execute;
begin
  FreeOnTerminate := true;
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
    // save patches
    SavePatches;
    // rename saved plugins
    RenameSavedPlugins;
  end;

  // save statistics and settings
  SaveStatistics;
  SaveSettings;

  // delete temppath
  if not settings.preserveTempPath then
    DeleteTempPath;

  // print final messages
  Tracker.Write(' ');
  Tracker.Write('All done');

  // unbind events
  Logger.OnLogEvent := nil;
  Tracker.OnLogEvent := nil;
  Tracker.OnStatusEvent := nil;

  if Assigned(SaveCallback) then
    Synchronize(nil, SaveCallback);
end;

end.
