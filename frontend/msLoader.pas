unit msLoader;

interface

uses
  Classes,
  // mte units
  mteHelpers, mteBase,
  // mp units
  msConfiguration;

  { Initialization Methods }
  function InitBase: boolean;
  function GamePathValid(path: string; id: integer): boolean;
  procedure SetGame(id: integer);
  function GetGameID(name: string): integer;
  function GetGamePath(mode: TGameMode): string;
  procedure LoadDefinitions;
  procedure LoadBSAs;
  { Mod Organizer methods }
  procedure ModOrganizerInit;
  function GetActiveProfile: string;
  procedure GetActiveMods(var modlist: TStringList; profileName: string);
  { Load order functions }
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure RemoveSmashedPatches(var sl: TStringList);
  procedure FixLoadOrder(var sl: TStringList; const filename: String; var index: Integer);
  procedure AddBaseMasters(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  procedure LoadPluginsList(const sLoadPath: String; var sl: TStringList; noDelete: Boolean = False);
  procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
  procedure PrepareLoadOrder(var slLoadOrder, slPlugins: TStringList);

var
  slPlugins: TStringList;
  UpdateCallback: TCallback;

implementation

uses
  SysUtils, StrUtils, IniFiles, ShlObj, Controls,
  // mte units
  mteTracker, mteLogger, mteLogging, mtePluginSelectionForm,
  // mp units
  msCore,
  // mp forms
  // xEdit units
  wbHelpers, wbInterface, wbImplementation, wbBSA,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5, wbDefinitionsFO4;


{******************************************************************************}
{ Initialization Methods
  Methods that are used for initialization.

  List of methods:
  - GamePathValid
  - SetGame
  - GetGameID
  - GetGamePath
  - LoadDataPath
  - LoadDefinitions
  - InitPapyrus
}
{******************************************************************************}

function InitBase: boolean;
var
  slLoadOrder: TStringList;
  psForm: TPluginSelectionForm;
begin
  Result := false;

  // INITIALIZE VARIABLES
  LogPath := PathList.Values['ProgramPath'] + 'logs\';
  PathList.Values['TempPath'] := PathList.Values['ProgramPath'] + 'temp\';
  PathList.Values['ProfilePath'] := PathList.Values['ProgramPath'] +
    'profiles\'+ CurrentProfile.name + '\';
  ForceDirectories(PathList.Values['TempPath']);
  ForceDirectories(LogPath);
  ForceDirectories(PathList.Values['ProfilePath']);

  // SET GAME VARS
  SetGame(CurrentProfile.gameMode);
  wbVWDInTemporary := wbGameMode in [gmSSE, gmTES5, gmFO3, gmFNV];
  wbVWDAsQuestChildren := wbGameMode = gmFO4;
  wbArchiveExtension := IfThen(wbGameMode = gmFO4, '.ba2', '.bsa');
  wbLoadBSAs := wbGameMode in [gmFO4, gmSSE, gmTES5, gmTES4];
  Logger.Write('GENERAL', 'Game', 'Using '+wbGameName);
  Logger.Write('GENERAL', 'Path', 'Using '+wbDataPath);
  Logger.Write('GENERAL', 'GameIni', 'Using '+wbTheGameIniFileName);

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
  wbHideIgnored := False;
  wbFlagsAsArray := True;
  wbRequireLoadOrder := True;
  wbLanguage := settings.language;
  wbEditAllowed := True;
  wbContainerHandler := wbCreateContainerHandler;
  wbContainerHandler._AddRef;

  // INITIALIZE DICTIONARY
  Logger.Write('GENERAL', 'Dictionary', 'Using '+wbAppName+'Dictionary.txt');
  //LoadDictionary;

  // INITIALIZE TES5EDIT DEFINITIONS
  Logger.Write('GENERAL', 'Definitions', 'Using '+wbAppName+'Edit Definitions');
  LoadDefinitions;

  // LOAD SMASH SETTINGS
  Tracker.Write('Loading smash settings');
  LoadSmashSettings;

  // LOAD PATCHES
  Tracker.Write('Loading patches');
  LoadPatches;

  // PREPARE LOAD ORDER
  slLoadOrder := TStringList.Create;
  slPlugins := TStringList.Create;
  PrepareLoadOrder(slLoadOrder, slPlugins);

  // DISPLAY PLUGIN SELECTION FORM
  THeaderHelpers.LoadPluginHeaders(slLoadOrder);
  psForm := TPluginSelectionForm.Create(nil);
  psForm.slCheckedPlugins := slPlugins;
  psForm.slAllPlugins := slLoadOrder;
  psForm.sColumns := 'Plugin,Merge';
  psForm.GetPluginInfo := TPatchHelpers.GetPatchForPlugin;
  psForm.GetPluginMasters := THeaderHelpers.GetPluginMasters;
  psForm.GetPluginDependencies := THeaderHelpers.GetPluginDependencies;
  if psForm.ShowModal = mrCancel then
    exit;
  slPlugins.Text := psForm.slCheckedPlugins.Text;
  psForm.Free;
  wbFileForceClosed;
  FreeList(HeaderList);

  // ALL DONE
  Result := true;
end;

{ Check if game paths are valid }
function GamePathValid(path: string; id: integer): boolean;
begin
  Result := FileExists(path + GameArray[id].exeName)
    and DirectoryExists(path + 'Data');
end;

{ Sets the game mode in the TES5Edit API }
procedure SetGame(id: integer);
var
  sMyDocumentsPath: string;
  sIniPath: string;
begin
  ProgramStatus.GameMode := GameArray[id];
  wbGameName := ProgramStatus.GameMode.gameName;
  wbGameName2 := ProgramStatus.GameMode.regName;
  wbGameMode := ProgramStatus.GameMode.gameMode;
  wbAppName := ProgramStatus.GameMode.appName;
  wbDataPath := CurrentProfile.gamePath + 'Data\';

  // set general paths
  PathList.Values['DataPath'] := wbDataPath;
  PathList.Values['GamePath'] := UpDirectory(wbDataPath);

  // find game ini inside the user's documents folder.
  sMyDocumentsPath := GetCSIDLShellFolder(CSIDL_PERSONAL);
  if sMyDocumentsPath <> '' then begin
    sIniPath := sMyDocumentsPath + 'My Games\' + wbGameName2 + '\';
    if wbGameMode in [gmFO3, gmFNV] then
      wbTheGameIniFileName := sIniPath + 'Fallout.ini'
    else
      wbTheGameIniFileName := sIniPath + wbGameName + '.ini';
  end;
end;

{ Get the game ID associated with a game long name }
function GetGameID(name: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Low(GameArray) to High(GameArray) do
    if GameArray[i].longName = name then begin
      Result := i;
      exit;
    end;
end;

{ Gets the path of a game from registry key or app path }
function GetGamePath(mode: TGameMode): string;
const
  sBethRegKey     = '\SOFTWARE\Bethesda Softworks\';
  sBethRegKey64   = '\SOFTWARE\Wow6432Node\Bethesda Softworks\';
  sSteamRegKey    = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+
    'Steam App ';
  sSteamRegKey64  = '\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\'+
    'Uninstall\Steam App ';
var
  i: Integer;
  regName: string;
  keys, appIDs: TStringList;
begin
  Result := '';

  // initialize variables
  regName := mode.regName;
  keys := TStringList.Create;
  appIDs := TStringList.Create;
  appIDs.CommaText := mode.appIDs;

  // add keys to check
  keys.Add(sBethRegKey + regName + '\Installed Path');
  keys.Add(sBethRegKey64 + regName + '\Installed Path');
  for i := 0 to Pred(appIDs.Count) do begin
    keys.Add(sSteamRegKey + appIDs[i] + '\InstallLocation');
    keys.Add(sSteamRegKey64 + appIDs[i] + '\InstallLocation');
  end;

  // try to find path from registry
  Result := TryRegistryKeys(keys);

  // free memory
  keys.Free;
  appIDs.Free;

  // set result
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ Loads definitions based on wbGameMode }
procedure LoadDefinitions;
begin
  case wbGameMode of
    gmTES5: DefineTES5;
    gmFNV: DefineFNV;
    gmTES4: DefineTES4;
    gmFO3: DefineFO3;
    gmFO4: DefineFO4;
    gmSSE: DefineTES5;
  end;
end;

procedure LoadBSAFile(sFileName: String);
var
  sFileExt: String;
begin
  sFileExt := ExtractFileExt(sFileName);
  Logger.Write('LOAD', 'Resources', 'Loading resources from ' + sFileName);
  if sFileExt = '.bsa' then
    wbContainerHandler.AddBSA(wbDataPath + sFileName)
  else if sFileExt = '.ba2' then
    wbContainerHandler.AddBA2(wbDataPath + sFileName);
end;

{ Loads all of the BSAs specified in the game ini and by plugins }
procedure LoadBSAs;
var
  slBSAFileNames: TStringList;
  slErrors: TStringList;
  i: Integer;
  modIndex: Integer;
  plugin: TPlugin;
  bIsTES5: Boolean;
begin
  slBSAFileNames := TStringList.Create;
  try
    slErrors:= TStringList.Create;
    try
      FindBSAs(wbTheGameIniFileName, wbDataPath, slBSAFileNames, slErrors);
      for i := 0 to slBSAFileNames.Count - 1 do
        LoadBSAFile(slBSAFileNames[i]);
      for i := 0 to slErrors.Count - 1 do
        Logger.Write('ERROR', 'Load', slErrors[i] + ' was not found');

      for modIndex := 0 to PluginsList.Count - 1 do begin
        slBSAFileNames.Clear;
        slErrors.Clear;
        plugin := TPlugin(PluginsList[modIndex]);
        bIsTES5 := wbGameMode in [gmTES5, gmSSE];

        HasBSAs(ChangeFileExt(plugin.filename, ''), wbDataPath, bIsTES5,
          bIsTES5, slBSAFileNames, slErrors);
        for i := 0 to slBSAFileNames.Count - 1 do
          LoadBSAFile(slBSAFileNames[i]);
        for i := 0 to slErrors.Count - 1 do
          Logger.Write('ERROR', 'Load', slErrors[i] + ' was not found');
      end;
    finally
      slErrors.Free;
    end;
  finally
    slBSAFileNames.Free;
  end;
end;

{******************************************************************************}
{ Mod Organizer methods
  Set of methods that allow interaction Mod Organizer settings.

  List of methods:
  - ModOrganizerInit
  - GetActiveProfile
  - GetActiveMods
  - GetModContainingFile
}
{******************************************************************************}

procedure ModOrganizerInit;
begin
  ActiveMods := TStringList.Create;
  ActiveModProfile := GetActiveProfile;
  GetActiveMods(ActiveMods, ActiveModProfile);
end;

function GetMODataPath: string;
var
  sAppData: String;
begin
  sAppData := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA);
  if settings.InstanceName <> '' then
    Result := Format('%sModOrganizer\%s\', [sAppData, settings.InstanceName])
  else
    Result := settings.ManagerPath;
end;

function GetActiveProfile: string;
var
  ini : TMemIniFile;
  fname : string;
begin
  // exit if not using MO
  Result := '';
  if (not settings.usingMO) or (Trim(settings.ManagerPath) = '') then
    exit;

  // load ini file
  fname := GetMODataPath + 'ModOrganizer.ini';
  if(not FileExists(fname)) then begin
    Logger.Write('GENERAL', 'ModOrganizer', 'Mod Organizer ini file ' + fname + ' does not exist');
    exit;
  end;
  ini := TMemIniFile.Create(fname);

  // get selected_profile
  Result := ini.ReadString( 'General', 'selected_profile', '');
  ini.Free;
end;

procedure GetActiveMods(var modlist: TStringList; profileName: string);
var
  modlistFilePath: string;
  s: string;
  i: integer;
begin
  // exit if not using MO
  if (not settings.usingMO) or (Trim(settings.ManagerPath) = '') then
    exit;

  // prepare to load modlist
  modlistFilePath := settings.ManagerPath + 'profiles/' + profileName + '/modlist.txt';
  modlist.Clear;

  // exit if modlist file doesn't exist
  if not (FileExists(modlistFilePath)) then begin
    Tracker.Write('Cannot find modlist ' + modListFilePath);
    Logger.Write('GENERAL', 'ModOrganizer', 'Cannot find modlist ' + modListFilePath);
    exit;
  end;

  // load modlist
  modlist.LoadFromFile(modlistFilePath);
  for i := Pred(modlist.Count) downto 0 do begin
    s := modList[i];
    // if line starts with '+', then it's an active mod
    if (Pos('+', s) = 1) then
      modlist[i] := Copy(s, 2, Length(s) - 1)
    // else it's a comment or inactive mod, so delete it
    else
      modlist.Delete(i);
  end;
end;


{******************************************************************************}
{ Load order functions
  Set of functions for building a working load order.

  List of functions:
  - RemoveCommentsAndEmpty
  - RemoveMissingFiles
  - AddMissingFiles
  - PluginListCompare
{******************************************************************************}

{ Remove comments and empty lines from a stringlist }
procedure RemoveCommentsAndEmpty(var sl: TStringList);
var
  i, j, k: integer;
  s: string;
begin
  for i := Pred(sl.Count) downto 0 do begin
    s := Trim(sl.Strings[i]);
    j := Pos('#', s);
    k := Pos('*', s);
    if j > 0 then
      System.Delete(s, j, High(Integer));
    if s = '' then
      sl.Delete(i);
    if k = 1 then
      sl[i] := Copy(s, 2, Length(s));
  end;
end;

{ Remove nonexistent files from stringlist }
procedure RemoveMissingFiles(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if not FileExists(wbDataPath + sl.Strings[i]) then
      sl.Delete(i);
end;

{ Remove smashed patch plugins from stringlist }
procedure RemoveSmashedPatches(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if Assigned(TPatchHelpers.PatchByFilename(PatchesList, sl[i])) then
      sl.Delete(i);
end;

{ Forces a plugin to load at a specific position }
procedure FixLoadOrder(var sl: TStringList; const filename: String; var index: Integer);
var
  oldIndex: Integer;
begin
  oldIndex := sl.IndexOf(filename);
  if (oldIndex > -1) then begin
    if oldIndex <> index then begin
      sl.Delete(oldIndex);
      sl.Insert(index, filename);
    end;
  end
  else if FileExists(wbDataPath + filename) then
    sl.Insert(index, filename)
  else
    exit;
  Inc(index);
end;

procedure AddBaseMasters(var sl: TStringList);
var
  index: Integer;
begin
  index := 0;
  if (wbGameMode = gmTES5) then begin
    FixLoadOrder(sl, 'Skyrim.esm', index);
    FixLoadOrder(sl, 'Update.esm', index);
  end
  else if (wbGameMode = gmSSE) then begin
    FixLoadOrder(sl, 'Skyrim.esm', index);
    FixLoadOrder(sl, 'Update.esm', index);
    FixLoadOrder(sl, 'Dawnguard.esm', index);
    FixLoadOrder(sl, 'HearthFires.esm', index);
    FixLoadOrder(sl, 'Dragonborn.esm', index);
  end
  else if (wbGameMode = gmFO4) then begin
    FixLoadOrder(sl, 'Fallout4.esm', index);
    FixLoadOrder(sl, 'DLCRobot.esm', index);
    FixLoadOrder(sl, 'DLCworkshop01.esm', index);
    FixLoadOrder(sl, 'DLCCoast.esm', index);
    FixLoadOrder(sl, 'DLCworkshop02.esm', index);
    FixLoadOrder(sl, 'DLCworkshop03.esm', index);
    FixLoadOrder(sl, 'DLCNukaWorld.esm', index);
    FixLoadOrder(sl, 'DLCUltraHighResolution.esm', index);
  end;
end;

{ Add missing *.esp and *.esm files to list }
procedure AddMissingFiles(var sl: TStringList);
var
  F: TSearchRec;
  i, j: integer;
  slNew: TStringList;
begin
  slNew := TStringList.Create;
  try
    // search for missing plugins and masters
    if FindFirst(wbDataPath + '*.*', faAnyFile, F) = 0 then try
      repeat
        if not (IsFileESM(F.Name) or IsFileESP(F.Name)) then
          continue;
        if sl.IndexOf(F.Name) = -1 then
          slNew.AddObject(F.Name, TObject(FileAge(wbDataPath + F.Name)));
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;

    // sort the list
    slNew.CustomSort(PluginListCompare);

    // The for loop won't initialize j if sl.count = 0, we must force it
    // to -1 so inserting will happen at index 0
    if sl.Count = 0 then
      j := -1
    else
      // find position of last master
      for j := Pred(sl.Count) downto 0 do
        if IsFileESM(sl[j]) then
          Break;

    // add esm masters after the last master, add esp plugins at the end
    Inc(j);
    for i := 0 to Pred(slNew.Count) do begin
      if IsFileESM(slNew[i]) then begin
        sl.InsertObject(j, slNew[i], slNew.Objects[i]);
        Inc(j);
      end else
        sl.AddObject(slNew[i], slNew.Objects[i]);
    end;
  finally
    slNew.Free;
  end;
end;

{ Get date modified for plugins in load order and store in stringlist objects }
procedure GetPluginDates(var sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to Pred(sl.Count) do
    sl.Objects[i] := TObject(FileAge(wbDataPath + sl[i]));
end;

{ Compare function for sorting load order by date modified/esms }
function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  IsESM1, IsESM2: Boolean;
  FileAge1,FileAge2: Integer;
  FileDateTime1, FileDateTime2: TDateTime;
begin
  IsESM1 := IsFileESM(List[Index1]);
  IsESM2 := IsFileESM(List[Index2]);

  if IsESM1 = IsESM2 then begin
    FileAge1 := Integer(List.Objects[Index1]);
    FileAge2 := Integer(List.Objects[Index2]);

    if FileAge1 < FileAge2 then
      Result := -1
    else if FileAge1 > FileAge2 then
      Result := 1
    else begin
      if not SameText(List[Index1], List[Index1])
      and FileAge(List[Index1], FileDateTime1) and FileAge(List[Index2], FileDateTime2) then begin
        if FileDateTime1 < FileDateTime2 then
          Result := -1
        else if FileDateTime1 > FileDateTime2 then
          Result := 1
        else
          Result := 0;
      end else
        Result := 0;
    end;

  end else if IsESM1 then
    Result := -1
  else
    Result := 1;
end;

procedure ProcessAsterisks(var sl: TStringList; index: Integer; noDelete: Boolean);
var
  s: String;
begin
  s := sl[index];
  if s[1] <> '*' then begin
    if not noDelete then sl.Delete(index);
  end
  else
    sl[index] := Copy(s, 2, Length(s));
end;

procedure ProcessPluginsFormat(var sl: TStringList; noDelete: Boolean);
var
  i: Integer;
begin
  for i := Pred(sl.Count) downto 0 do
    ProcessAsterisks(sl, i, noDelete);
end;

procedure LoadPluginsList(const sLoadPath: String; var sl: TStringList; noDelete: Boolean = False);
var
  sPath: String;
begin
  sPath := sLoadPath + 'plugins.txt';
  if FileExists(sPath) then begin
    sl.LoadFromFile(sPath);
    if (wbGameMode = gmSSE) or (wbGameMode = gmFO4) then
      ProcessPluginsFormat(sl, noDelete);
  end
  else
    AddMissingFiles(sl);

  // remove comments and missing files
  AddBaseMasters(sl);
  RemoveCommentsAndEmpty(sl);
  RemoveMissingFiles(sl);
  RemoveSmashedPatches(sl);
end;

procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
var
  sPath: String;
begin
  sPath := sLoadPath + 'loadorder.txt';
  if (wbGameMode <> gmSSE) and (wbGameMode <> gmFO4)
  and FileExists(sPath) then
    slLoadOrder.LoadFromFile(sPath)
  else
    slLoadOrder.AddStrings(slPlugins);

  // remove comments and add/remove files
  AddBaseMasters(slLoadOrder);
  RemoveCommentsAndEmpty(slLoadOrder);
  RemoveMissingFiles(slLoadOrder);
  AddMissingFiles(slLoadOrder);
  RemoveSmashedPatches(slLoadOrder);
end;

procedure PrepareLoadOrder(var slLoadOrder, slPlugins: TStringList);
var
  sLoadPath: String;
begin
  sLoadPath := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA) + wbGameName2 +'\';
  LoadPluginsList(sLoadPath, slPlugins, True);
  LoadLoadOrder(sLoadPath, slLoadOrder, slPlugins);

  // if GameMode is not SkyrimSE or Fallout 4 and we don't
  // have a loadorder.txt, sort by date modified
  if (wbGameMode <> gmSSE) and (wbGameMode <> gmFO4)
  and not FileExists(sLoadPath + 'loadorder.txt') then begin
    GetPluginDates(slLoadOrder);
    slLoadOrder.CustomSort(PluginListCompare);
  end;
end;

{ Log Initialization }
procedure InitLog;
begin
  // INITIALIZE GROUP FILTERS
  GroupFilters.Add(TFilter.Create('GENERAL', true));
  GroupFilters.Add(TFilter.Create('LOAD', true));
  GroupFilters.Add(TFilter.Create('CLIENT', true));
  GroupFilters.Add(TFilter.Create('MERGE', true));
  GroupFilters.Add(TFilter.Create('PLUGIN', true));
  GroupFilters.Add(TFilter.Create('ERROR', true));
  // INITIALIZE LABEL FILTERS
  LabelFilters.Add(TFilter.Create('GENERAL', 'Game', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Status', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Path', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Definitions', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Dictionary', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Load Order', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Log', true));
  LabelFilters.Add(TFilter.Create('LOAD', 'Order', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Plugins', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Background', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Connect', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Login', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Response', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Update', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Report', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Status', false));
  LabelFilters.Add(TFilter.Create('MERGE', 'Create', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Edit', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Check', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Clean', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Delete', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Build', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Check', true));
end;

end.
