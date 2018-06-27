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
  function GetLanguageFileSuffix: String;
  function GetGamePath(mode: TGameMode): string;
  procedure LoadDefinitions;
  procedure LoadBSAs;
  { Load order functions }
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure RemoveSmashedPatches(var sl: TStringList);
  procedure FixLoadOrder(var sl: TStringList; const filename: String; var index: Integer);
  procedure AddBaseMasters(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  procedure LoadPluginsList(const sLoadPath: String; var sl: TStringList; noDelete: Boolean = False);
  procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder: TStringList);
  procedure PrepareLoadOrder(var slLoadOrder, slPlugins: TStringList);

var
  slPlugins, slLanguageMap: TStringList;
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

  // INITIALIZE XEDIT
  wbStringEncoding := seUTF8;
  wbDisplayLoadOrderFormID := True;
  wbSortSubRecords := True;
  wbDisplayShorterNames := True;
  wbHideUnused := True;
  wbHideIgnored := False;
  wbFlagsAsArray := True;
  wbRequireLoadOrder := True;
  wbLanguage := GetLanguageFileSuffix;
  wbEditAllowed := True;
  wbContainerHandler := wbCreateContainerHandler;
  wbContainerHandler._AddRef;

  // INITIALIZE DEFINITIONS
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
  psForm.sColumns := 'Plugin,Patch';
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

{ Gets language file suffix }
function GetLanguageFileSuffix: String;
begin
  Result := settings.language;
  if (wbGameMode = gmFO4) and (slLanguageMap.IndexOfName(Result) > -1) then
    Result := slLanguageMap.Values[Result];
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

{ Remove ESLs from stringlist }
procedure RemoveESLs(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if StrEndsWith(sl[i], '.esl') then
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
  FixLoadOrder(sl, wbGameName + '.esm', index);
  if (wbGameMode = gmTES5) then
    FixLoadOrder(sl, 'Update.esm', index)
  else if (wbGameMode = gmSSE) then begin
    FixLoadOrder(sl, 'Update.esm', index);
    FixLoadOrder(sl, 'Dawnguard.esm', index);
    FixLoadOrder(sl, 'HearthFires.esm', index);
    FixLoadOrder(sl, 'Dragonborn.esm', index);
  end
  else if (wbGameMode = gmFO4) then begin
    FixLoadOrder(sl, 'DLCRobot.esm', index);
    FixLoadOrder(sl, 'DLCworkshop01.esm', index);
    FixLoadOrder(sl, 'DLCCoast.esm', index);
    FixLoadOrder(sl, 'DLCworkshop02.esm', index);
    FixLoadOrder(sl, 'DLCworkshop03.esm', index);
    FixLoadOrder(sl, 'DLCNukaWorld.esm', index);
    FixLoadOrder(sl, 'DLCUltraHighResolution.esm', index);
  end;
end;

function GetPluginDate(const aFileName: string): Cardinal;
const
  DateOmitYears = 60;
  DatePrecision = 100000;
var
  F: TSearchRec;
begin
  // Try to fit a meaningful modified date of a file into 32 bits integer value
  // For relative load order sorting only
  // Oblivion GOG version has dates from 1969 year and FileAge() doesn't support them
  if FindFirst(aFileName, faAnyFile, F) = 0 then begin
    Result := Round((F.TimeStamp - 364 * DateOmitYears) * DatePrecision);
    FindClose(F);
  end else
    Result := 0;
end;

{ Compare function for sorting load order by date modified/esms }
function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  IsESM1, IsESM2: Boolean;
  FileSK1, FileSK2: Integer;
begin
  IsESM1 := IsFileESM(List[Index1]);
  IsESM2 := IsFileESM(List[Index2]);

  if IsESM1 = IsESM2 then begin
    FileSK1 := Cardinal(List.Objects[Index1]);
    FileSK2 := Cardinal(List.Objects[Index2]);

    if FileSK1 < FileSK2 then
      Result := -1
    else if FileSK1 > FileSK2 then
      Result := 1
    else
      Result := 0;

  end else if IsESM1 then
    Result := -1
  else
    Result := 1;
end;

{ Add missing *.esp and *.esm files to list }
procedure AddMissingFiles(var sl: TStringList);
var
  F: TSearchRec;
  i, j: integer;
  slNew: TStringList;
  fileSortKey: Cardinal;
begin
  slNew := TStringList.Create;
  try
    // search for missing plugins and masters
    if FindFirst(wbDataPath + '*.*', faAnyFile, F) = 0 then try
      repeat
        if not (IsFileESM(F.Name) or IsFileESP(F.Name)) then
          continue;
        if sl.IndexOf(F.Name) = -1 then begin
          fileSortKey := GetPluginDate(wbDataPath + F.Name);
          slNew.AddObject(F.Name, TObject(fileSortKey));
        end;
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
  if noDelete then AddMissingFiles(sl);
  RemoveESLs(sl);
  RemoveSmashedPatches(sl);
end;

procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder: TStringList);
var
  sPath: String;
begin
  sPath := sLoadPath + 'loadorder.txt';
  if (wbGameMode <> gmSSE) and (wbGameMode <> gmFO4)
  and FileExists(sPath) then begin
    slLoadOrder.LoadFromFile(sPath);

    // remove comments and add/remove files
    AddBaseMasters(slLoadOrder);
    RemoveCommentsAndEmpty(slLoadOrder);
    RemoveMissingFiles(slLoadOrder);
    RemoveESLs(slLoadOrder);
    AddMissingFiles(slLoadOrder);
    RemoveSmashedPatches(slLoadOrder);
  end
  else
    LoadPluginsList(sLoadPath, slLoadOrder, True);
end;

procedure PrepareLoadOrder(var slLoadOrder, slPlugins: TStringList);
var
  sLoadPath: String;
begin
  sLoadPath := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA) + wbGameName2 +'\';
  LoadPluginsList(sLoadPath, slPlugins);
  LoadLoadOrder(sLoadPath, slLoadOrder);
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
  LabelFilters.Add(TFilter.Create('PATCH', 'Status', false));
  LabelFilters.Add(TFilter.Create('PATCH', 'Create', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Edit', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Check', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Clean', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Delete', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Build', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Check', true));
end;

initialization
  slLanguageMap := TStringList.Create;
  slLanguageMap.Text :=
    'English=en'#13 +
    'French=fr'#13 +
    'German=de'#13 +
    'Italian=it'#13 +
    'Spanish=es'#13 +
    'Russian=ru'#13 +
    'Polish=pl'#13 +
    'Japanese=ja'#13 +
    'Portugese=pt'#13 +
    'Chinese=zh';

finalization
  slLanguageMap.Free;

end.
