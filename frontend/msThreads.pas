unit msThreads;

interface

uses
  Classes, SysUtils, StrUtils, shlObj, Dialogs, ComCtrls,
  Windows, MMSystem,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteLogging, mteTracker, mteBase,
  // ms units
  msCore, msConfiguration, msLoader, msSmash,
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
  PatchCallback, SaveCallback: TCallback;
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
  - TPatchThread.Execute
  - TSaveThread.Execute
}
{******************************************************************************}

{ TInitThread }
procedure TInitThread.Execute;
var
  i: integer;
  plugin: TPlugin;
  aFile: IwbFile;
begin
  try
    // PRINT LOAD ORDER TO LOG
    for i := 0 to Pred(slPlugins.Count) do
      Logger.Write('LOAD', 'Order', '['+IntToHex(i, 2)+'] '+slPlugins[i]);

    // LOAD RESOURCES
    Tracker.Write('Loading Resources');
    wbContainerHandler.AddFolder(wbDataPath);
    LoadBSAs;

    // LOAD PLUGINS
    for i := 0 to Pred(slPlugins.Count) do begin
      Tracker.Write('Loading '+slPlugins[i]);
      try
        plugin := TPlugin.Create;
        plugin.filename := slPlugins[i];
        plugin._File := wbFile(wbDataPath + slPlugins[i], i, '', False, False);
        plugin._File._AddRef;
        plugin.GetMsData;
        PluginsList.Add(Pointer(plugin));
      except
        on x: Exception do begin
          Logger.Write('ERROR', 'Load', 'Exception loading '+slPlugins[i]);
          Logger.Write('ERROR', 'Load', x.Message);
          ProgramStatus.bLoadException := true;
        end;
      end;

      // load hardcoded dat
      if i = 0 then try
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

    // LOAD PLUGIN INFORMATION
    Tracker.Write('Loading plugin information');
    TPatchHelpers.AssignPatchesToPlugins;
    LoadPluginInfo;
    LoadSettingTags;

    // CLEAN UP
    slPlugins.Free;
  except
    on x: Exception do begin
      if Assigned(slPlugins) then
        slPlugins.Free;
      ProgramStatus.bInitException := true;
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
  if ProgramStatus.bForceTerminate then
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
    [GetLanguageString('msMain_LoaderInProgress'), 1, PluginsList.Count]));
  try
    for i := 0 to Pred(PluginsList.Count) do begin
      StatusCallback(Format('%s (%d/%d)',
        [GetLanguageString('msMain_LoaderInProgress'), i + 1, PluginsList.Count]));
      plugin := TPlugin(PluginsList[i]);
      f := plugin._File;
      if SameText(plugin.filename, wbGameName + '.esm') then
        continue;
      LoaderProgress('[' + plugin.filename + '] Building reference info.');
      f.BuildRef;
      if ProgramStatus.bForceTerminate then begin
        LoaderProgress('Aborted.');
        break;
      end;
    end;
  except
    on E: Exception do begin
      LoaderProgress('Fatal: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := true;
      ProgramStatus.bInitException := true;
    end;
  end;
  LoaderProgress('finished');
  StatusCallback(GetLanguageString('msMain_LoaderFinished'));
  if Assigned(LoaderCallback) then
    Synchronize(nil, LoaderCallback);
end;

{ TPatchThread }
procedure PlaySmashSound;
var
  HResource: TResourceHandle;
  HResData: THandle;
  PWav: Pointer;
begin
  HResource := FindResource(HInstance, PChar('SMASH'), 'WAV');
  if HResource = 0 then exit;
  HResData := LoadResource(HInstance, HResource);
  if HResData = 0 then exit;
  PWav := LockResource(HResData);
  if not Assigned(PWav) then exit;
  sndPlaySound(nil, SND_NODEFAULT);
  sndPlaySound(PWav, SND_ASYNC or SND_MEMORY);
end;

procedure TPatchThread.Execute;
var
  i: integer;
  patch: TPatch;
begin
  FreeOnTerminate := true;
  // build patches
  for i := 0 to Pred(patchesToBuild.Count) do begin
    if Tracker.Cancel then break;
    patch := TPatch(patchesToBuild[i]);
    StatusCallback(Format('%s "%s" (%d/%d)',
      [GetLanguageString('msProg_Smashing'), patch.name, i + 1, patchesToBuild.Count]));
    try
      if (patch.status in RebuildStatuses) then
        RebuildPatch(patch)
      else
        BuildPatch(patch);
    except
      on x : Exception do begin
        patch.status := psFailed;
        Tracker.Write('Exception: ' + x.Message);
      end;
    end;

    Tracker.Write(' '#13#10);
    Tracker.SetProgress(IntegerListSum(timeCosts, i));

    if Tracker.Cancel then
      Tracker.Write('Smashing canceled.');
  end;

  // say thread is done if it wasn't cancelled
  if not Tracker.Cancel then begin
    Tracker.Write('All done!');
    try
      if settings.smashSound then PlaySmashSound;
    except
      on x: Exception do
        Tracker.Write('Failed to play Smash sound.');
    end;
  end;

  // clean up, fire callback
  StatusCallback(GetLanguageString('msProg_DoneBuilding'));
  Tracker.Cancel := false;
  if Assigned(PatchCallback) then
    Synchronize(nil, PatchCallback);
end;

{ TSaveThread }
procedure TSaveThread.Execute;
begin
  FreeOnTerminate := true;

  // save ESPs only if it's safe to do so
  if not ProgramStatus.bInitException then begin
    // Save plugin errors
    try
      SavePluginInfo;
    except
      on x: Exception do
        Tracker.Write('Exception saving plugin errors '+x.Message);
    end;
    Tracker.SetProgress(PluginsList.Count + 1);

    // save patches
    SavePatches;

    // force close files
    wbFileForceClosed;

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
