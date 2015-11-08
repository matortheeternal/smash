unit msFrontend;

interface

uses
  Windows, SysUtils, Classes, IniFiles, Dialogs, Registry, Graphics, ShlObj,
  Forms, ShellAPI, ComCtrls,
  // indy components
  IdTCPClient, IdStack, IdGlobal,
  // superobject json library
  superobject,
  // abbrevia components
  AbZBrows, AbUnZper, AbArcTyp, AbMeter, AbBrowse, AbBase,
  // mte components
  CRC32, mteLogger, mteTracker, mteHelpers, mteProgressForm,
  RttiIni, RttiJson, RttiTranslation,
  // xedit components
  wbHelpers, wbInterface, wbImplementation,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5;

type
  // LOGGING
  TFilter = class(TObject)
  public
    group: string;
    &label: string;
    enabled: boolean;
    constructor Create(group: string; enabled: boolean); Overload;
    constructor Create(group, &label: string; enabled: boolean); Overload;
  end;
  TLogMessage = class (TObject)
  public
    time: string;
    appTime: string;
    group: string;
    &label: string;
    text: string;
    constructor Create(time, appTime, group, &label, text: string); Overload;
  end;
  // SERVER/CLIENT
  TmsMessage = class(TObject)
  public
    id: integer;
    username: string;
    auth: string;
    data: string;
    constructor Create(id: integer; username, auth, data: string); Overload;
  end;
  TmsStatus = class(TObject)
  public
    programVersion: string;
    tes5Hash: string;
    tes4Hash: string;
    fnvHash: string;
    fo3Hash: string;
    constructor Create; Overload;
  end;
  // GENERAL
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    appName: string;
    exeName: string;
    appIDs: string;
    bsaOptMode: string;
  end;
  TElementData = class(TObject)
    priority: byte;
    process: boolean;
    preserveDeletions: boolean;
    singleEntity: boolean;
    linkTo: string;
    linkFrom: string;
    constructor Create(priority: byte; process: boolean;
      preserveDeletions: boolean; singleEntity: boolean;
      linkTo, linkFrom: string); overload;
  end;
  TSmashSetting = class(TObject)
  public
    name: string;
    hash: string;
    description: string;
    records: string;
    tree: ISuperObject;
    color: Int64;
    constructor Create;
    destructor Destroy; override;
    constructor Clone(s: TSmashSetting);
    function GetRecordDef(sig: string): ISuperObject;
    procedure LoadDump(dump: ISuperObject);
    function Dump: ISuperObject;
    procedure Save;
    procedure Rename(newName: string);
  end;
  TReport = class(TObject)
  public
    game: string;
    username: string;
    filename: string;
    hash: string;
    recordCount: integer;
    rating: integer;
    patchVersion: string;
    notes: string;
    dateSubmitted: TDateTime;
    procedure SetNotes(notes: string);
    function GetNotes: string;
    procedure Save(const filename: string);
  end;
  // PLUGINS AND MERGES
  TPatchStatusID = ( psUnknown, psNoPlugins, psDirInvalid, psUnloaded,
    psErrors, psFailed, psUpToDate, psUpToDateForced, psBuildReady,
    psRebuildReady, psRebuildReadyForced );
  TPatchStatus  = Record
    id: TPatchStatusID;
    color: integer;
    desc: string[64];
  end;
  TPlugin = class(TObject)
  public
    _File: IwbFile;
    hasData: boolean;
    hash: string;
    setting: string;
    smashSetting: TSmashSetting;
    fileSize: Int64;
    dateModified: string;
    filename: string;
    patch: string;
    numRecords: string;
    numOverrides: string;
    author: string;
    dataPath: string;
    description: TStringList;
    masters: TStringList;
    requiredBy: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetData;
    procedure GetHash;
    procedure GetDataPath;
    function GetFormIndex: Integer;
    function IsInPatch: boolean;
    procedure LoadInfoDump(obj: ISuperObject);
    function InfoDump: ISuperObject;
  end;
  TPatch = class(TObject)
  public
    name: string;
    filename: string;
    dateBuilt: TDateTime;
    dataPath: string;
    status: TPatchStatusID;
    plugin: TPlugin;
    plugins: TStringList;
    hashes: TStringList;
    masters: TStringList;
    fails: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    function Dump: ISuperObject;
    procedure LoadDump(obj: ISuperObject);
    function GetTimeCost: integer;
    procedure UpdateHashes;
    procedure GetStatus;
    procedure GetLoadOrders;
    procedure SortPlugins;
    procedure Remove(plugin: TPlugin); overload;
    procedure Remove(pluginFilename: string); overload;
    function PluginsModified: boolean;
    function FilesExist: boolean;
    function GetStatusColor: integer;
  end;
  // SETTINGS AND STATISTICS
  TSettings = class(TObject)
  public
    [IniSection('General')]
    profile: string;
    gameMode: integer;
    gamePath: string;
    language: string;
    username: string;
    key: string;
    registered: boolean;
    simpleDictionaryView: boolean;
    simplePluginsView: boolean;
    updateDictionary: boolean;
    updateProgram: boolean;
    [IniSection('Advanced')]
    serverHost: string;
    serverPort: integer;
    dontSendStatistics: boolean;
    generalMessageColor: Int64;
    clientMessageColor: Int64;
    loadMessageColor: Int64;
    patchMessageColor: Int64;
    pluginMessageColor: Int64;
    errorMessageColor: Int64;
    logMessageTemplate: string;
    preserveTempPath: boolean;
    [IniSection('Patching')]
    patchDirectory: string;
    debugPatchStatus: boolean;
    debugMasters: boolean;
    debugArrays: boolean;
    debugSkips: boolean;
    debugTraversal: boolean;
    debugTypes: boolean;
    debugChanges: boolean;
    debugSingle: boolean;
    debugLinks: boolean;
    buildRefs: boolean;
    [IniSection('Integrations')]
    usingMO: boolean;
    MOPath: string;
    MOModsPath: string;
    constructor Create; virtual;
    procedure GenerateKey;
  end;
  TStatistics = class(TObject)
  public
    [IniSection('Statistics')]
    timesRun: integer;
    patchesBuilt: integer;
    pluginsPatched: integer;
    settingsSubmitted: integer;
    recsSubmitted: integer;
    constructor Create; virtual;
  end;
  TProfile = class(TObject)
  public
    name: string;
    gameMode: Integer;
    gamePath: string;
    constructor Create(name: string); virtual;
    procedure Clone(p: TProfile);
    procedure Delete;
    procedure Rename(name: string);
  end;

  { Initialization Methods }
  function GamePathValid(path: string; id: integer): boolean;
  procedure SetGame(id: integer);
  function GetGameID(name: string): integer;
  function GetGamePath(mode: TGameMode): string;
  procedure LoadDefinitions;
  { Bethesda Plugin Functions }
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function ExtractFormID(filename: string): string;
  function RemoveFileIndex(formID: string): string;
  function LocalFormID(aRecord: IwbMainRecord): integer;
  function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
  function etToString(et: TwbElementType): string;
  function dtToString(dt: TwbDefType): string;
  function ElementByIndexedPath(e: IwbElement; ip: string): IwbElement;
  function IndexedPath(e: IwbElement): string;
  function GetAllValues(e: IwbElement): string;
  function IsSorted(se: IwbElement): boolean;
  function OverrideCountInFiles(rec: IwbMainRecord; var files: TStringList): Integer;
  function CountOverrides(aFile: IwbFile): integer;
  procedure AddRequiredBy(filename: string; var masters: TStringList);
  procedure GetMasters(aFile: IwbFile; var sl: TStringList);
  procedure AddMasters(aFile: IwbFile; var sl: TStringList);
  function LoadBSA(filename: string): boolean;
  function INIExists(filename: string): boolean;
  function TranslationExists(filename: string): boolean;
  function FaceDataExists(filename: string): boolean;
  function VoiceDataExists(filename: string): boolean;
  function FragmentsExist(f: IwbFile): boolean;
  function ReferencesSelf(f: IwbFile): boolean;
  procedure ExtractBSA(ContainerName, folder, destination: string); overload;
  procedure ExtractBSA(ContainerName, destination: string; var ignore: TStringList); overload;
  function RemoveSelfOrContainer(const aElement: IwbElement): boolean;
  procedure UndeleteAndDisable(const aRecord: IwbMainRecord);
  function FixErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;
  { Load order functions }
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure RemovePatchdPlugins(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  { Mod Organizer methods }
  procedure ModOrganizerInit;
  function GetActiveProfile: string;
  procedure GetActiveMods(var modlist: TStringList; profileName: string);
  function GetModContainingFile(var modlist: TStringList; filename: string): string;
  { Log methods }
  procedure InitLog;
  procedure RebuildLog;
  procedure SaveLog(var Log: TList);
  function MessageEnabled(msg: TLogMessage): boolean;
  { Loading and saving methods }
  procedure LoadExcludedGroups;
  procedure LoadLanguage;
  function GetString(name: string): string;
  procedure SaveProfile(var p: TProfile);
  procedure LoadRegistrationData(var s: TSettings);
  procedure LoadSettings; overload;
  function LoadSettings(path: string): TSettings; overload;
  procedure SaveRegistrationData(var s: TSettings);
  procedure SaveSettings; overload;
  procedure SaveSettings(var s: TSettings; path: string); overload;
  procedure LoadStatistics;
  procedure SaveStatistics;
  procedure LoadDictionary;
  procedure RenameSavedPlugins;
  procedure SavePatches;
  procedure LoadPatches;
  procedure AssignPatchesToPlugins;
  procedure SaveSmashSettings;
  procedure LoadSmashSettings;
  procedure SavePluginInfo;
  procedure LoadPluginInfo;
  procedure SaveReports(var lst: TList; path: string);
  function ReportExistsFor(var plugin: TPlugin): boolean;
  procedure LoadReport(var report: TReport); overload;
  procedure LoadReport(const filename: string; var report: TReport); overload;
  { Tree methods }
  procedure SetChildren(node: TTreeNode; state: Integer);
  procedure UpdateParent(node: TTreeNode);
  procedure CheckBoxManager(node: TTreeNode);
  procedure LoadElement(var tv: TTreeView; node: TTreeNode; obj: ISuperObject;
    bWithinSingle: boolean);
  procedure LoadTree(var tv: TTreeView; aSetting: TSmashSetting);
  { Helper methods }
  procedure RenamePatches(var sl: TStringList);
  procedure GetPatchesToRename(var sl: TStringList);
  procedure RemoveSettingFromPlugins(aSetting: TSmashSetting);
  procedure DeleteTempPath;
  procedure ShowProgressForm(parent: TForm; var pf: TProgressForm; s: string);
  function GetRatingColor(rating: real): integer;
  function GetEntry(name, numRecords, version: string): TSmashSetting;
  function PluginLoadOrder(filename: string): integer;
  function SettingByName(name: string): TSmashSetting;
  function PluginByFilename(filename: string): TPlugin;
  function PatchByName(patches: TList; name: string): TPatch;
  function PatchByFilename(patches: TList; filename: string): TPatch;
  function CreateNewPatch(patches: TList): TPatch;
  function CreateNewPlugin(filename: string): TPlugin;
  function PatchPluginsCompare(List: TStringList; Index1, Index2: Integer): Integer;
  { Client methods }
  procedure InitializeClient;
  procedure ConnectToServer;
  function ServerAvailable: boolean;
  procedure SendClientMessage(var msg: TmsMessage);
  function CheckAuthorization: boolean;
  procedure SendGameMode;
  procedure SendStatistics;
  procedure ResetAuth;
  function UsernameAvailable(username: string): boolean;
  function RegisterUser(username: string): boolean;
  function GetStatus: boolean;
  function VersionCompare(v1, v2: string): boolean;
  procedure CompareStatuses;
  function UpdateChangeLog: boolean;
  function UpdateDictionary: boolean;
  function UpdateProgram: boolean;
  function DownloadProgram: boolean;
  function SendReports(var lst: TList): boolean;
  function SendPendingReports: boolean;


const
  // IMPORTANT CONSTANTS
  ProgramTesters = ' ';
  ProgramTranslators = ' ';
  xEditVersion = '3.1.1';
  bTranslationDump = false;

  // MSG IDs
  MSG_UNKNOWN = 0;
  MSG_NOTIFY = 1;
  MSG_REGISTER = 2;
  MSG_AUTH_RESET = 3;
  MSG_STATISTICS = 4;
  MSG_STATUS = 5;
  MSG_REQUEST = 6;
  MSG_REPORT = 7;

  // CHECKED STATES
  cChecked = 1;
  cUnChecked = 2;
  cPartiallyChecked = 3;

  // MERGE STATUSES
  StatusArray: array[0..10] of TPatchStatus = (
    ( id: psUnknown; color: $808080; desc: 'Unknown'; ),
    ( id: psNoPlugins; color: $0000FF; desc: 'No plugins to patch'; ),
    ( id: psDirInvalid; color: $0000FF; desc: 'Directories invalid'; ),
    ( id: psUnloaded; color: $0000FF; desc: 'Plugins not loaded'; ),
    ( id: psErrors; color: $0000FF; desc: 'Errors in plugins'; ),
    ( id: psFailed; color: $0000FF; desc: 'Patch failed'; ),
    ( id: psUpToDate; color: $900000; desc: 'Up to date'; ),
    ( id: psUpToDateForced; color: $900000; desc: 'Up to date [Forced]'; ),
    ( id: psBuildReady; color: $009000; desc: 'Ready to be built'; ),
    ( id: psRebuildReady; color: $009000; desc: 'Ready to be rebuilt'; ),
    ( id: psRebuildReadyForced; color: $009000; desc: 'Ready to be rebuilt [Forced]'; )
  );
  // STATUS TYPES
  ErrorStatuses = [psUnknown, psNoPlugins, psDirInvalid, psUnloaded, psErrors];
  UpToDateStatuses = [psUpToDate, psUpToDateForced];
  BuildStatuses = [psBuildReady, psRebuildReady, psRebuildReadyForced];
  RebuildStatuses = [psRebuildReady, psRebuildReadyForced];
  ForcedStatuses = [psUpToDateForced, psRebuildReadyForced];
  ResolveStatuses = [psNoPlugins, psDirInvalid, psUnloaded, psErrors];

  // DELAYS
  StatusDelay = 2.0 / (60.0 * 24.0); // 2 minutes
  MaxConnectionAttempts = 3;

  // GAME MODES
  GameArray: array[1..4] of TGameMode = (
    ( longName: 'Skyrim'; gameName: 'Skyrim'; gameMode: gmTES5;
      appName: 'TES5'; exeName: 'TESV.exe'; appIDs: '72850';
      bsaOptMode: 'sk'; ),
    ( longName: 'Oblivion'; gameName: 'Oblivion'; gameMode: gmTES4;
      appName: 'TES4'; exeName: 'Oblivion.exe'; appIDs: '22330,900883';
      bsaOptMode: 'ob'; ),
    ( longName: 'Fallout New Vegas'; gameName: 'FalloutNV'; gameMode: gmFNV;
      appName: 'FNV'; exeName: 'FalloutNV.exe'; appIDs: '22380,2028016';
      bsaOptMode: 'fo'; ),
    ( longName: 'Fallout 3'; gameName: 'Fallout3'; gameMode: gmFO3;
      appName: 'FO3'; exeName: 'Fallout3.exe'; appIDs: '22300,22370';
      bsaOptMode: 'fo'; )
  );

var
  dictionary, blacklist, PluginsList, PatchesList, BaseLog, Log,
  LabelFilters, GroupFilters, pluginsToHandle, patchesToBuild, SmashSettings: TList;
  TreeView: TTreeView;
  timeCosts, language, ActiveMods: TStringList;
  settings: TSettings;
  CurrentProfile: TProfile;
  statistics, sessionStatistics: TStatistics;
  status, RemoteStatus: TmsStatus;
  excludedGroups: array of string;
  bInitException, bLoadException, bChangeProfile, bForceTerminate,
  bLoaderDone, bAuthorized, bProgramUpdate, bDictionaryUpdate, bInstallUpdate,
  bConnecting, bUpdatePatchStatus, bAllowClose, bOverridesOnly, bTarget: boolean;
  TempPath, LogPath, ProgramPath, dictionaryFilename, ActiveModProfile,
  ProgramVersion, xEditLogLabel, xEditLogGroup, DataPath, GamePath,
  ProfilePath, sRecords: string;
  ConnectionAttempts: Integer;
  TCPClient: TidTCPClient;
  AppStartTime, LastStatusTime: TDateTime;
  GameMode: TGameMode;

implementation


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

{ Check if game paths are valid }
function GamePathValid(path: string; id: integer): boolean;
begin
  Result := FileExists(path + GameArray[id].exeName)
    and DirectoryExists(path + 'Data');
end;

{ Sets the game mode in the TES5Edit API }
procedure SetGame(id: integer);
begin
  GameMode := GameArray[id];
  wbGameName := GameMode.gameName;
  wbGameMode := GameMode.gameMode;
  wbAppName := GameMode.appName;
  wbDataPath := CurrentProfile.gamePath + 'Data\';
  // set general paths
  DataPath := wbDataPath;
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

{ Tries to load various registry keys }
function TryRegistryKeys(var keys: TStringList): string;
var
  i: Integer;
  path, name: string;
begin
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    // try all keys
    for i := 0 to Pred(keys.Count) do begin
      path := ExtractFilePath(keys[i]);
      name := ExtractFileName(keys[i]);
      if OpenKeyReadOnly(path) then begin
        Result := ReadString(name);
        break;
      end;
    end;
  finally
    Free;
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
  gameName: string;
  keys, appIDs: TStringList;
begin
  Result := '';

  // initialize variables
  gameName := mode.gameName;
  keys := TStringList.Create;
  appIDs := TStringList.Create;
  try
    appIDs.CommaText := mode.appIDs;

    // add keys to check
    keys.Add(sBethRegKey + gameName + '\Installed Path');
    keys.Add(sBethRegKey64 + gameName + '\Installed Path');
    for i := 0 to Pred(appIDs.Count) do begin
      keys.Add(sSteamRegKey + appIDs[i] + '\InstallLocation');
      keys.Add(sSteamRegKey64 + appIDs[i] + '\InstallLocation');
    end;

    // try to find path from registry
    Result := TryRegistryKeys(keys);
  finally
    // free memory
    keys.Free;
    appIDs.Free;
  end;

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
  end;
end;


{******************************************************************************}
{ Bethesda Plugin Functions
  Set of functions that read bethesda plugin files for various attributes.

  List of functions:
  - IsOverride
  - LocalFormID
   -LoadOrderPrefix
  - CountOverrides
  - GetMasters
  - AddMasters
  - BSAExists
  - TranslationExists
  - FaceDataExists
  - VoiceDataExists
  - FragmentsExist
  - ExtractBSA
  - CheckForErorrsLinear
  - CheckForErrors
  - PluginsModified
  - CreatSEQFile
}
{*****************************************************************************}

{ Returns true if the input record is an override record }
function IsOverride(aRecord: IwbMainRecord): boolean;
begin
  Result := not aRecord.IsMaster;
end;

function ExtractFormID(filename: string): string;
const
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
var
  i, counter: Integer;
begin
  counter := 0;
  // we loop from the back because the formID is usually at the
  // end of the filename
  for i := Length(filename) downto 1 do begin
    if (filename[i] in HexChars) then
      Inc(counter)
    else
      counter := 0;
    // set result and exit if counter has reached 8
    if counter = 8 then begin
      Result := Copy(filename, i, 8);
      exit;
    end;
  end;
end;

function RemoveFileIndex(formID: string): string;
begin
  if Length(formID) <> 8 then
    raise Exception.Create('RemoveFileIndex: FormID must be 8 characters long');
  Result := '00' + Copy(formID, 3, 6);
end;

{ Gets the local formID of a record (so no load order prefix) }
function LocalFormID(aRecord: IwbMainRecord): integer;
begin
  Result := aRecord.LoadOrderFormID and $00FFFFFF;
end;

{ Gets the load order prefix from the FormID of a record }
function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
begin
  Result := aRecord.LoadOrderFormID and $FF000000;
end;

{ Converts a TwbElementType to a string }
function etToString(et: TwbElementType): string;
begin
  case Ord(et) of
    Ord(etFile): Result := 'etFile';
    Ord(etMainRecord): Result := 'etMainRecord';
    Ord(etGroupRecord): Result := 'etGroupRecord';
    Ord(etSubRecord): Result := 'etSubRecord';
    Ord(etSubRecordStruct): Result := 'etSubRecordStruct';
    Ord(etSubRecordArray): Result := 'etSubRecordArray';
    Ord(etSubRecordUnion): Result := 'etSubRecordUnion';
    Ord(etArray): Result := 'etArray';
    Ord(etStruct): Result := 'etStruct';
    Ord(etValue): Result := 'etValue';
    Ord(etFlag): Result := 'etFlag';
    Ord(etStringListTerminator): Result := 'etStringListTerminator';
    Ord(etUnion): Result := 'etUnion';
  end;
end;

{ Converts a TwbDefType to a string }
function dtToString(dt: TwbDefType): string;
begin
  case Ord(dt) of
    Ord(dtRecord): Result := 'dtRecord';
    Ord(dtSubRecord): Result := 'dtSubRecord';
    Ord(dtSubRecordArray): Result := 'dtSubRecordArray';
    Ord(dtSubRecordStruct): Result := 'dtSubRecordStruct';
    Ord(dtSubRecordUnion): Result := 'dtSubRecordUnion';
    Ord(dtString): Result := 'dtString';
    Ord(dtLString): Result := 'dtLString';
    Ord(dtLenString): Result := 'dtLenString';
    Ord(dtByteArray): Result := 'dtByteArray';
    Ord(dtInteger): Result := 'dtInteger';
    Ord(dtIntegerFormater): Result := 'dtIntegerFormatter';
    Ord(dtFloat): Result := 'dtFloat';
    Ord(dtArray): Result := 'dtArray';
    Ord(dtStruct): Result := 'dtStruct';
    Ord(dtUnion): Result := 'dtUnion';
    Ord(dtEmpty): Result := 'dtEmpty';
  end;
end;

function ElementByIndexedPath(e: IwbElement; ip: string): IwbElement;
var
  i, index: integer;
  path: TStringList;
  c: IwbContainerElementRef;
begin
  // replace forward slashes with backslashes
  ip := StringReplace(ip, '/', '\', [rfReplaceAll]);

  // prepare path stringlist delimited by backslashes
  path := TStringList.Create;
  path.Delimiter := '\';
  path.StrictDelimiter := true;
  path.DelimitedText := ip;

  // treat e as a container
  if not Supports(e, IwbContainerElementRef, c) then
    exit;

  // traverse path
  for i := 0 to Pred(path.count) do begin
    if Pos('[', path[i]) > 0 then begin
      index := StrToInt(GetTextIn(path[i], '[', ']'));
      e := c.Elements[index];
      if not Supports(e, IwbContainerElementRef, c) then
        exit;
    end
    else begin
      e := c.ElementByPath[path[i]];
      if not Supports(e, IwbContainerElementRef, c) then
        exit;
    end;
  end;

  // set result
  Result := e;
end;

function IndexedPath(e: IwbElement): string;
var
  c: IwbContainer;
  a: string;
begin
  c := e.Container;
  while (e.ElementType <> etMainRecord) do begin
    if c.ElementType = etSubRecordArray then
      a := '['+IntToStr(c.IndexOf(e))+']'
    else
      a := e.Name;
    if Result <> '' then
      Result := a + '\' + Result
    else
      Result := a;
    e := c;
    c := e.Container;
  end;
end;

{ Returns a string hash of all of the values contained in an element }
function GetAllValues(e: IwbElement): string;
var
  i: integer;
  c: IwbContainerElementRef;
begin
  Result := e.EditValue;
  if not Supports(e, IwbContainerElementRef, c) then
    exit;

  // loop through children elements
  for i := 0 to Pred(c.ElementCount) do begin
    if (Result <> '') then
      Result := Result + ';' + GetAllValues(c.Elements[i])
    else
      Result := GetAllValues(c.Elements[i]);
  end;
end;

{ Returns true if @se is a sorted container }
function IsSorted(se: IwbElement): boolean;
var
  Container: IwbSortableContainer;
begin
  Result := false;
  if Supports(se, IwbSortableContainer, Container) then
    Result := Container.Sorted;
end;

{ Returns the number of override records in a file }
function CountOverrides(aFile: IwbFile): integer;
var
  i: Integer;
  aRecord: IwbMainRecord;
begin
  Result := 0;
  for i := 0 to Pred(aFile.GetRecordCount) do begin
    aRecord := aFile.GetRecord(i);
    if IsOverride(aRecord) then
      Inc(Result);
  end;
end;

{ Returns the number of overrides of the specified record in the specified file set }
function OverrideCountInFiles(rec: IwbMainRecord; var files: TStringList): Integer;
var
  i: Integer;
  ovr: IwbMainRecord;
begin
  Result := 0;
  for i := 0 to Pred(rec.OverrideCount) do begin
    ovr := rec.Overrides[i];
    if files.IndexOf(ovr._File.FileName) > -1 then
      Inc(Result);
  end;
end;

{ Populates required by field of @masters that are required by plugin
  @filename }
procedure AddRequiredBy(filename: string; var masters: TStringList);
var
  i: Integer;
  plugin: TPlugin;
begin
  for i := 0 to Pred(masters.Count) do begin
    plugin := PluginByFilename(masters[i]);
    if not Assigned(plugin) then
      continue;
    plugin.requiredBy.Add(filename);
  end;
end;

{ Gets the masters in an IwbFile and puts them into a stringlist }
procedure GetMasters(aFile: IwbFile; var sl: TStringList);
var
  Container, MasterFiles, MasterFile: IwbContainer;
  i: integer;
  filename: string;
begin
  Container := aFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    for i := 0 to MasterFiles.ElementCount - 1 do begin
      MasterFile := MasterFiles.Elements[i] as IwbContainer;
      filename := MasterFile.GetElementEditValue('MAST - Filename');
      if sl.IndexOf(filename) = -1 then
        sl.AddObject(filename, TObject(PluginLoadOrder(filename)));
    end;
  end;
end;

{ Gets the masters in an IwbFile and puts them into a stringlist }
procedure AddMasters(aFile: IwbFile; var sl: TStringList);
var
  i: integer;
begin
  for i := 0 to Pred(sl.Count) do begin
    if Lowercase(aFile.FileName) = Lowercase(sl[i]) then
      continue;
    aFile.AddMasterIfMissing(sl[i]);
  end;
end;

{ Checks if a BSA exists associated with the given filename and loads it into
  wbContainerHandler if found. }
function LoadBSA(filename: string): boolean;
var
  bsaFilename, ContainerName: string;
begin
  Result := false;
  bsaFilename := ChangeFileExt(filename, '.bsa');
  if FileExists(wbDataPath + bsaFilename) then begin
    ContainerName := wbDataPath + bsaFilename;
    if not wbContainerHandler.ContainerExists(ContainerName) then
      wbContainerHandler.AddBSA(ContainerName);
    Result := true;
  end;
end;

{ Check if an INI exists associated with the given filename }
function INIExists(filename: string): boolean;
var
  iniFilename: string;
begin
  iniFilename := ChangeFileExt(filename, '.ini');
  Result := FileExists(wbDataPath + iniFilename);
end;

{ Returns true if a file exists at @path matching @filename }
function MatchingFileExists(path: string; filename: string): boolean;
var
  info: TSearchRec;
begin
  Result := false;
  filename := Lowercase(filename);
  if FindFirst(path, faAnyFile, info) = 0 then begin
    repeat
      if Pos(filename, Lowercase(info.Name)) > 0 then begin
        Result := true;
        exit;
      end;
    until FindNext(info) <> 0;
    FindClose(info);
  end;
end;

{ Return true if MCM translation files for @filename are found }
function TranslationExists(filename: string): boolean;
var
  searchPath, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
begin
  searchPath := wbDataPath + 'Interface\translations\*';
  Result := MatchingFileExists(searchPath, ChangeFileExt(filename, ''));
  if Result then exit;

  // check in BSA
  if LoadBSA(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, 'Interface\translations');
    Result := ResourceList.Count > 0;
  end;
end;

{ Return true if file-specific FaceGenData files for @filename are found }
function FaceDataExists(filename: string): boolean;
var
  facetintDir, facegeomDir, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
  facetint, facegeom: boolean;
begin
  facetintDir := 'textures\actors\character\facegendata\facetint\' + filename;
  facegeomDir := 'meshes\actors\character\facegendata\facegeom\' + filename;
  facetint := DirectoryExists(wbDataPath + facetintDir);
  facegeom := DirectoryExists(wbDataPath + facegeomDir);
  Result := facetint or facegeom;
  if Result then exit;

  // check in BSA
  if LoadBSA(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, facetintDir);
    wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, facegeomDir);
    Result := ResourceList.Count > 0;
  end;
end;

{ Return true if file-specific Voice files for @filename are found }
function VoiceDataExists(filename: string): boolean;
var
  voiceDir, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
begin
  voiceDir := 'sound\voice\' + filename;
  Result := DirectoryExists(wbDataPath + voiceDir);
  if Result then exit;

  // check in BSA
  if LoadBSA(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, voiceDir);
    Result := ResourceList.Count > 0;
  end;
end;

{ Returns true if Topic Info Fragments exist in @f }
function TopicInfoFragmentsExist(f: IwbFile): boolean;
const
  infoFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Info VMAD\Script Fragments Info';
var
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  subgroup, container: IwbContainer;
  element, fragments: IwbElement;
  i, j: Integer;
begin
  Result := false;
  // exit if no DIAL records in file
  if not f.HasGroup('DIAL') then
    exit;

  // find all DIAL records
  group := f.GroupBySignature['DIAL'];
  for i := 0 to Pred(group.ElementCount) do begin
    element := group.Elements[i];
    // find all INFO records
    if not Supports(element, IwbContainer, subgroup) then
      continue;
    for j := 0 to Pred(subgroup.ElementCount) do begin
      if not Supports(subgroup.Elements[j], IwbMainRecord, rec) then
        continue;
      if not rec.IsMaster then
        continue;
      if not Supports(rec, IwbContainer, container) then
        continue;
      fragments := container.ElementByPath[infoFragmentsPath];
      if not Assigned(fragments) then
        continue;
      Result := true;
    end;
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function QuestFragmentsExist(f: IwbFile): boolean;
const
  questFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
begin
  Result := false;
  // exit if no QUST records in file
  if not f.HasGroup('QUST') then
    exit;

  // find all QUST records
  group := f.GroupBySignature['QUST'];
  for i := 0 to Pred(group.ElementCount) do begin
    if not Supports(group.Elements[i], IwbMainRecord, rec) then
      continue;
    if not rec.IsMaster then
      continue;
    if not Supports(rec, IwbContainer, container) then
      continue;
    fragments := container.ElementByPath[questFragmentsPath];
    if not Assigned(fragments) then
      continue;
    Result := true;
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function SceneFragmentsExist(f: IwbFile): boolean;
const
  sceneFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
begin
  Result := false;
  // exit if no SCEN records in file
  if not f.HasGroup('SCEN') then
    exit;

  // find all SCEN records
  group := f.GroupBySignature['SCEN'];
  for i := 0 to Pred(group.ElementCount) do begin
    if not Supports(group.Elements[i], IwbMainRecord, rec) then
      continue;
    if not rec.IsMaster then
      continue;
    if not Supports(rec, IwbContainer, container) then
      continue;
    fragments := container.ElementByPath[sceneFragmentsPath];
    if not Assigned(fragments) then
      continue;
    Result := true;
  end;
end;

{ Returns true if file-specific Script Fragments for @f are found }
function FragmentsExist(f: IwbFile): boolean;
begin
  Result := TopicInfoFragmentsExist(f) or QuestFragmentsExist(f)
    or SceneFragmentsExist(f);
end;

{ References self }
function ReferencesSelf(f: IwbFile): boolean;
var
  i: Integer;
  filename, source: string;
  scripts: IwbGroupRecord;
  container: IwbContainerElementRef;
  rec: IwbMainRecord;
begin
  // exit if has no script records in file
  Result := false;
  if not f.HasGroup('SCPT') then
    exit;

  // get scripts, and check them all for self-reference
  filename := f.FileName;
  scripts := f.GroupBySignature['SCPT'];
  if not Supports(scripts, IwbContainerElementRef, container) then
    exit;
  for i := 0 to Pred(container.ElementCount) do begin
    if not Supports(container.Elements[i], IwbMainRecord, rec) then
      continue;
    source := rec.ElementEditValues['SCTX - Script Source'];
    if Pos(filename, source) > 0 then begin
      Result := true;
      break;
    end;
  end;
end;

{ Extracts assets from @folder in the BSA @filename to @destination }
procedure ExtractBSA(ContainerName, folder, destination: string);
var
  ResourceList: TStringList;
  i: Integer;
begin
  if not wbContainerHandler.ContainerExists(ContainerName) then begin
    Tracker.Write('    '+ContainerName+' not loaded.');
    exit;
  end;
  ResourceList := TStringList.Create;
  wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, folder);
  for i := 0 to Pred(ResourceList.Count) do
    wbContainerHandler.ResourceCopy(ContainerName, ResourceList[i], destination);
end;

{ Extracts assets from the BSA @filename to @destination, ignoring assets
  matching items in @ignore }
procedure ExtractBSA(ContainerName, destination: string; var ignore: TStringList);
var
  ResourceList: TStringList;
  i, j: Integer;
  skip: boolean;
begin
  if not wbContainerHandler.ContainerExists(ContainerName) then begin
    Tracker.Write('    '+ContainerName+' not loaded.');
    exit;
  end;
  ResourceList := TStringList.Create;
  wbContainerHandler.ContainerResourceList(ContainerName, ResourceList, '');
  for i := 0 to Pred(ResourceList.Count) do begin
    skip := false;
    for j := 0 to Pred(ignore.Count) do begin
      skip := Pos(ignore[j], ResourceList[i]) > 0;
      if skip then break;
    end;

    if skip then continue;
    wbContainerHandler.ResourceCopy(ContainerName, ResourceList[i], destination);
  end;
end;

function RemoveSelfOrContainer(const aElement: IwbElement): Boolean;
var
  cElement: IwbElement;
begin
  Result := false;
  if aElement.IsRemoveable then begin
    aElement.Remove;
    Result := true;
  end
  else begin
    if not Assigned(aElement.Container) then begin
      Tracker.Write('    Element has no container!');
      exit;
    end;
    // if element isn't removable, try removing its container
    if Supports(aElement.Container, IwbMainRecord) then begin
      Tracker.Write('    Reached main record, cannot remove element');
      exit;
    end;
    Tracker.Write('    Failed to remove '+aElement.Path+', removing container');
    if Supports(aElement.Container, IwbElement, cElement) then
      Result := RemoveSelfOrContainer(cElement);
  end;
end;

procedure UndeleteAndDisable(const aRecord: IwbMainRecord);
var
  xesp: IwbElement;
  sig: string;
  container: IwbContainerElementRef;
begin
  try
    sig := aRecord.Signature;

    // undelete
    aRecord.IsDeleted := true;
    aRecord.IsDeleted := false;

    // set persistence flag depending on game
    if (wbGameMode in [gmFO3,gmFNV,gmTES5])
    and ((sig = 'ACHR') or (sig = 'ACRE')) then
      aRecord.IsPersistent := true
    else if wbGameMode = gmTES4 then
      aRecord.IsPersistent := false;

      // place it below the ground
    if not aRecord.IsPersistent then
      aRecord.ElementNativeValues['DATA\Position\Z'] := -30000;

    // remove elements
    aRecord.RemoveElement('Enable Parent');
    aRecord.RemoveElement('XTEL');

    // add enabled opposite of player (true - silent)
    xesp := aRecord.Add('XESP', True);
    if Assigned(xesp) and Supports(xesp, IwbContainerElementRef, container) then begin
      container.ElementNativeValues['Reference'] := $14; // Player ref
      container.ElementNativeValues['Flags'] := 1;  // opposite of parent flag
    end;

    // set to disable
    aRecord.IsInitiallyDisabled := true;
  except
    on x: Exception do
      Tracker.Write('    Exception: '+x.Message);
  end;
end;


function FixErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
  var errors: TStringList): IwbMainRecord;
const
  cUDR = 'Record marked as deleted but contains:';
  cUnresolved = '< Error: Could not be resolved >';
  cNULL = 'Found a NULL reference, expected:';
var
  Error: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  if Tracker.Cancel then
    exit;

  // update progress based on number of main records processed
  if Supports(aElement, IwbMainRecord) then
    Tracker.UpdateProgress(1);

  Error := aElement.Check;
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    // fix record marked as deleted errors (UDRs)
    if Pos(cUDR, Error) = 1 then begin
      if Assigned(Result) then begin
        Tracker.Write('  Fixing UDR: '+Result.Name);
        UndeleteAndDisable(Result);
      end;
    end
    else begin
      // fix unresolved FormID errors by NULLing them out
      if Pos(cUnresolved, Error) > 0 then begin
        Tracker.Write('  Fixing Unresolved FormID: '+aElement.Path);
        aElement.NativeValue := 0;
        // we may end up with an invalid NULL reference, so we Check again
        Error := aElement.Check;
        if Error = '' then exit;
      end;

      // fix invalid NULL references by removal
      if Pos(cNULL, Error) = 1 then begin
        Tracker.Write('  Removing NULL reference: '+aElement.Path);
        if RemoveSelfOrContainer(aElement) then exit;
      end;

      // unhandled error
      Tracker.Write(Format('  Unhandled error: %s -> %s', [aElement.Path, error]));
      if Assigned(Result) and (lastRecord <> Result) then begin
        lastRecord := Result;
        errors.Add(Result.Name);
      end;
      errors.Add('  '+aElement.Path + ' -> ' + Error);
    end;
  end;

  // done if element doesn't have children
  if not Supports(aElement, IwbContainerElementRef, Container) then
    exit;

  // recurse through children elements
  for i := Pred(Container.ElementCount) downto 0 do begin
    Result := FixErrors(Container.Elements[i], Result, errors);
    // break if container got deleted
    if not Assigned(Container) then break;
  end;
end;

function CheckForErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
  var errors: TStringList): IwbMainRecord;
var
  Error, msg: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  if Tracker.Cancel then
    exit;

  // update progress based on number of main records processed
  if Supports(aElement, IwbMainRecord) then
    Tracker.UpdateProgress(1);

  Error := aElement.Check;
  // log errors
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    if Assigned(Result) and (Result <> LastRecord) then begin
      Tracker.Write('  '+Result.Name);
      errors.Add(Result.Name);
    end;
    msg := '  '+aElement.Path + ' -> ' + Error;
    Tracker.Write('  '+msg);
    errors.Add(msg);
  end;

  // recursion
  if Supports(aElement, IwbContainerElementRef, Container) then
    for i := Pred(Container.ElementCount) downto 0 do
      Result := CheckForErrors(Container.Elements[i], Result, errors);
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
  i, j: integer;
  s: string;
begin
  for i := Pred(sl.Count) downto 0 do begin
    s := Trim(sl.Strings[i]);
    j := Pos('#', s);
    if j > 0 then
      System.Delete(s, j, High(Integer));
    if Trim(s) = '' then
      sl.Delete(i);
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

{ Remove patchd plugins from stringlist }
procedure RemovePatchdPlugins(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if Assigned(PatchByFilename(PatchesList, sl[i])) then
      sl.Delete(i);
end;

{ Add missing *.esp and *.esm files to list }
procedure AddMissingFiles(var sl: TStringList);
var
  F: TSearchRec;
  i: integer;
begin
  // find last master
  for i := Pred(sl.Count) downto 0 do
    if IsFileESM(sl[i]) then
      Break;

  // search for missing plugins, add to end
  if FindFirst(wbDataPath + '*.esp', faAnyFile, F) = 0 then try
    repeat
      if sl.IndexOf(F.Name) = -1 then
        sl.Add(F.Name);
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;

  // search for missing masters, add after last master
  if FindFirst(wbDataPath + '*.esm', faAnyFile, F) = 0 then try
    repeat
      if sl.IndexOf(F.Name) = -1 then begin
        sl.Insert(i, F.Name);
        Inc(i);
      end;
    until FindNext(F) <> 0;
  finally
    FindClose(F);
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
  //Logger.Write('GENERAL', 'ModOrganizer', 'ActiveMods: '#13#10+ActiveMods.Text);
end;

function GetActiveProfile: string;
var
  ini : TMemIniFile;
  fname : string;
begin
  // exit if not using MO
  Result := '';
  if not settings.usingMO then
    exit;

  // load ini file
  fname := settings.MOPath + 'ModOrganizer.ini';
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
  if not settings.usingMO then
    exit;

  // prepare to load modlist
  modlistFilePath := settings.MOPath + 'profiles/' + profileName + '/modlist.txt';
  modlist.Clear;

  // exit if modlist file doesn't exist
  if not (FileExists(modlistFilePath)) then begin
    Tracker.Write('Cannot find modlist ' + modListFilePath);
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

function GetModContainingFile(var modlist: TStringList; filename: string): string;
var
  i: integer;
  modName: string;
  filePath: string;
begin
  // exit if not using MO
  Result := '';
  if not settings.usingMO then
    exit;

  // check for file in each mod folder in modlist
  for i := 0 to Pred(modlist.Count) do begin
    modName := modlist[i];
    filePath := settings.MOModsPath + modName + '\' + filename;
    if (FileExists(filePath)) then begin
      Result := modName;
      exit;
    end;
  end;
end;


{******************************************************************************}
{ Log methods
  Set of methods for logging

  List of methods:
  - InitLog
  - RebuildLog
  - SaveLog
  - MessageGroupEnabled
}
{******************************************************************************}

procedure InitLog;
begin
  BaseLog := TList.Create;
  Log := TList.Create;
  LabelFilters := TList.Create;
  GroupFilters := TList.Create;
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

procedure RebuildLog;
var
  i: Integer;
  msg: TLogMessage;
begin
  Log.Clear;
  for i := 0 to Pred(BaseLog.Count) do begin
    msg := TLogMessage(BaseLog[i]);
    if MessageEnabled(msg) then
      Log.Add(msg);
  end;
end;

procedure SaveLog(var Log: TList);
var
  sl: TStringList;
  i: Integer;
  msg: TLogMessage;
  fdt: string;
begin
  sl := TStringList.Create;
  for i := 0 to Pred(Log.Count) do begin
    msg := TLogMessage(Log[i]);
    sl.Add(Format('[%s] (%s) %s: %s', [msg.time, msg.group, msg.&label, msg.text]));
  end;
  fdt := FormatDateTime('mmddyy_hhnnss', TDateTime(Now));
  ForceDirectories(LogPath);
  sl.SaveToFile(LogPath+'log_'+fdt+'.txt');
  sl.Free;
end;

function GetGroupFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(GroupFilters.Count) do begin
    filter := TFilter(GroupFilters[i]);
    if filter.group = msg.group then begin
      Result := filter;
      exit;
    end;
  end;
end;

function GetLabelFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(LabelFilters.Count) do begin
    filter := TFilter(LabelFilters[i]);
    if (filter.&label = msg.&label) and (filter.group = msg.group) then begin
      Result := filter;
      exit;
    end;
  end;
end;

function MessageEnabled(msg: TLogMessage): boolean;
var
  GroupFilter, LabelFilter: TFilter;
begin
  Result := true;
  GroupFilter := GetGroupFilter(msg);
  LabelFilter := GetLabelFilter(msg);
  if GroupFilter <> nil then
    Result := Result and GroupFilter.enabled;
  if LabelFilter <> nil then
    Result := Result and LabelFilter.enabled;
end;

{******************************************************************************}
{ Loading and saving methods
  Set of methods for loading and saving data.

  List of methods:
  - LoadLanguage
  - SaveProfile
  - SaveRegistrationData
  - LoadRegistrationData
  - SaveSettings
  - LoadSettings
  - SaveStatistics
  - LoadStatistics
  - LoadDictionary
  - SavePatches
  - LoadPatches
  - IndexOfDump
  - SavePluginErrors
  - LoadPluginErrors
  - SaveReports
  - ReportExists
  - LoadReport
}
{******************************************************************************}

procedure LoadExcludedGroups;
begin
  SetLength(excludedGroups, 3);
  excludedGroups[0] := 'RACE';
  excludedGroups[1] := 'NAVI';
  excludedGroups[2] := 'NAVM';
end;

procedure LoadLanguage;
const
  langFile = 'http://raw.githubusercontent.com/matortheeternal/patch-plugins/master/frontend/lang/english.lang';
  directions = 'Your english.lang file is missing.  Please download it from GitHub.  ' +
    'After you click OK, a webpage with the file will be opened.  Right-click the ' +
    'page and choose "Save page as", then save it as english.lang in the "lang\" ' +
    'folder where you have PatchPlugins.exe installed.';
var
  filename: string;
begin
  filename := Format('lang\%s.lang', [settings.language]);
  language := TStringList.Create;
  if (not FileExists(filename)) then begin
    {if settings.language <> 'english' then begin
      settings.language := 'english';
      LoadLanguage;
    end
    else begin
      MessageDlg(directions, mtConfirmation, [mbOk], 0);
      ForceDirectories(ProgramPath + 'lang\');
      ShellExecute(0, 'open', PChar(langFile), '', '', SW_SHOWNORMAL);
    end;}
  end
  else
    language.LoadFromFile(filename);
end;

function GetString(name: string): string;
begin
  if language.Values[name] <> '' then
    Result := StringReplace(language.Values[name], '#13#10', #13#10, [rfReplaceAll])
  else
    Result := name;
end;

procedure SaveProfile(var p: TProfile);
var
  path: string;
  pSettings: TSettings;
begin
  // get profile path
  path := ProgramPath + 'profiles\' + p.name + '\settings.ini';
  ForceDirectories(ExtractFilePath(path));

  // load settings if they exist, else create them
  if FileExists(path) then
    pSettings := LoadSettings(path)
  else
    pSettings := TSettings.Create;

  // save profile details to settings
  pSettings.profile := p.name;
  pSettings.gameMode := p.gameMode;
  pSettings.gamePath := p.gamePath;
  SaveSettings(pSettings, path);
  pSettings.Free;
end;

procedure SaveRegistrationData(var s: TSettings);
const
  sPatchPluginsRegKey = 'Software\\Patch Plugins\\';
  sPatchPluginsRegKey64 = 'Software\\Wow6432Node\\Patch Plugins\\';
begin
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;
    try
      Access := KEY_WRITE;
      if not OpenKey(sPatchPluginsRegKey, true) then
        if not OpenKey(sPatchPluginsRegKey64, true) then
          exit;

      WriteString('Username', s.username);
      WriteString('Key', s.key);
      WriteBool('Registered', s.registered);
    except on Exception do
      // nothing
    end;
  finally
    Free;
  end;
end;

procedure LoadRegistrationData(var s: TSettings);
const
  sPatchPluginsRegKey = 'Software\\Patch Plugins\\';
  sPatchPluginsRegKey64 = 'Software\\Wow6432Node\\Patch Plugins\\';
begin
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    try
      if (not KeyExists(sPatchPluginsRegKey))
        xor (not KeyExists(sPatchPluginsRegKey64)) then
          exit;

      if not OpenKeyReadOnly(sPatchPluginsRegKey) then
        if not OpenKeyReadOnly(sPatchPluginsRegKey64) then
          exit;

      if ReadBool('Registered') then begin
        s.username := ReadString('Username');
        s.key := ReadString('Key');
        s.registered := true;
      end;
    except on Exception do
      // nothing
    end;
  finally
    Free;
  end;
end;

procedure SaveSettings;
begin
  TRttiIni.Save(ProfilePath + 'settings.ini', settings);
  if settings.registered then
    SaveRegistrationData(settings);
end;

procedure SaveSettings(var s: TSettings; path: string);
begin
  TRttiIni.Save(path, s);
  // save registration data to registry if registered
  if (s.registered) then
    SaveRegistrationData(s);
end;

procedure LoadSettings;
begin
  settings := TSettings.Create;
  TRttiIni.Load(ProfilePath + 'settings.ini', settings);
  LoadRegistrationData(settings);
end;

function LoadSettings(path: string): TSettings;
begin
  Result := TSettings.Create;
  TRttiIni.Load(path, Result);
  LoadRegistrationData(Result);
end;

procedure SaveStatistics;
begin
  // move session statistics to general statistics
  Inc(statistics.timesRun, sessionStatistics.timesRun);
  Inc(statistics.patchesBuilt, sessionStatistics.patchesBuilt);
  Inc(statistics.pluginsPatched, sessionStatistics.pluginsPatched);
  Inc(statistics.settingsSubmitted, sessionStatistics.settingsSubmitted);
  Inc(statistics.recsSubmitted, sessionStatistics.recsSubmitted);
  // zero out session statistics
  sessionStatistics.timesRun := 0;
  sessionStatistics.patchesBuilt := 0;
  sessionStatistics.settingsSubmitted := 0;
  sessionStatistics.recsSubmitted := 0;
  // save to file
  TRttiIni.Save('statistics.ini', statistics);
end;

procedure LoadStatistics;
begin
  statistics := TStatistics.Create;
  sessionStatistics := TStatistics.Create;
  TRttiIni.Load('statistics.ini', statistics);
end;

procedure LoadDictionary;
var
  i: Integer;
  sl: TStringList;
begin
  // initialize dictionary and blacklist
  dictionary := TList.Create;
  blacklist := TList.Create;

  // don't attempt to load dictionary if it doesn't exist
  if not FileExists(dictionaryFilename) then begin
    Logger.Write('GENERAL', 'Dictionary', 'No dictionary file '+dictionaryFilename);
    exit;
  end;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile(dictionaryFilename);

  // load dictionary file into entry object
  for i := 0 to Pred(sl.Count) do begin
    // TODO: Load recommendations into dictionary
  end;

  // free temporary stringlist
  sl.Free;
end;

procedure RenameSavedPlugins;
var
  i: Integer;
  plugin: TPlugin;
  oldFileName, newFileName, bakFileName: string;
begin
  wbFileForceClosed;
  for i := Pred(PluginsList.Count) downto 0 do begin
    plugin := TPlugin(PluginsList[i]);
    try
      plugin._File._Release;
      oldFileName := plugin.dataPath + plugin.filename;
      newFileName := oldFileName + '.save';
      if FileExists(newFileName) then begin
        bakFileName := oldFileName + '.bak';
        if FileExists(bakFileName) then
          DeleteFile(bakFileName);
        RenameFile(oldFileName, bakFileName);
        RenameFile(newFileName, oldFileName);
      end;
    except
      on x: Exception do
        Tracker.Write('Failed to rename ' + plugin.filename + '.save');
    end;
  end;
end;

procedure SavePatches;
var
  i: Integer;
  patch: TPatch;
  json: ISuperObject;
  filename: string;
begin
  // initialize json
  json := SO;
  json.O['patches'] := SA([]);

  // loop through patches
  Tracker.Write('Dumping patches to JSON');
  for i := 0 to Pred(PatchesList.Count) do begin
    Tracker.UpdateProgress(1);
    patch := TPatch(PatchesList[i]);
    Tracker.Write('  Dumping '+patch.name);
    json.A['patches'].Add(patch.Dump);
  end;

  // save and finalize
  filename := ProfilePath + 'Patches.json';
  Tracker.Write(' ');
  Tracker.Write('Saving to ' + filename);
  Tracker.UpdateProgress(1);
  json.SaveTo(filename);
  json := nil;
end;

procedure LoadPatches;
const
  debug = false;
var
  patch: TPatch;
  obj, patchItem: ISuperObject;
  sl: TStringList;
  filename: string;
begin
  // don't load file if it doesn't exist
  filename := ProfilePath + 'Patches.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through patches
  for patchItem in obj['patches'] do begin
    patch := TPatch.Create;
    try
      patch.LoadDump(patchItem);
      PatchesList.Add(patch);
    except
      on x: Exception do begin
        Logger.Write('LOAD', 'Patch', 'Failed to load patch '+patch.name);
        Logger.Write('LOAD', 'Patch', x.Message);
      end;
    end;
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

procedure AssignPatchesToPlugins;
var
  i, j: Integer;
  patch: TPatch;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      if Assigned(plugin) then
        plugin.patch := patch.name;
    end;
  end;
end;

function IndexOfDump(a: TSuperArray; plugin: TPlugin): Integer;
var
  i: Integer;
  obj: ISuperObject;
begin
  Result := -1;
  for i := 0 to Pred(a.Length) do begin
    obj := a.O[i];
    if (obj.S['filename'] = plugin.filename)
    and (obj.S['hash'] = plugin.hash) then begin
      Result := i;
      exit;
    end;
  end;
end;

procedure SaveSmashSettings;
var
  aSetting: TSmashSetting;
  i: Integer;
begin
  Tracker.Write('Saving smash settings');
  for i := 0 to Pred(SmashSettings.Count) do begin
    aSetting := TSmashSetting(SmashSettings[i]);
    Tracker.Write('  Saving '+aSetting.name);
    aSetting.Save;
  end;
  Tracker.Write(' ');
end;

procedure CreateSkipSetting;
var
  skipSetting: TSmashSetting;
  index: Integer;
begin
  index := SmashSettings.Add(TSmashSetting.Create);
  skipSetting := SmashSettings[index];
  skipSetting.name := 'Skip';
  skipSetting.color := clGray;
  skipSetting.description := 'Special setting.  Any plugin with this setting '+
    'will be excluded from patch creation.';
  skipSetting.tree := SO();
  skipSetting.tree.O['records'] := SA([]);
end;

procedure LoadSmashSettings;
var
  info: TSearchRec;
  obj: ISuperObject;
  sl: TStringList;
  aSetting: TSmashSetting;
  path: String;
begin
  SmashSettings := TList.Create;
  path := ProgramPath + 'settings\';
  ForceDirectories(path);

  // load setting files from settings path
  if FindFirst(path + '*.json', faAnyFile, info) <> 0 then
    exit;
  repeat
    sl := TStringList.Create;
    try
      sl.LoadFromFile(path + info.Name);
      aSetting := TSmashSetting.Create;
      obj := SO(PChar(sl.Text));
      if Assigned(obj) then begin
        aSetting.LoadDump(obj);
        if aSetting.name <> '' then
          SmashSettings.Add(aSetting);
      end;
      sl.Free;
      obj := nil;
    except
      on x: Exception do begin
        if Assigned(sl) then sl.Free;
        obj := nil;
        Logger.Write('ERROR', 'Load', 'Failed to load smash setting '+info.Name);
      end;
    end;
  until FindNext(info) <> 0;

  // create skip setting if it isn't assigned
  if SettingByName('Skip') = nil then
    CreateSkipSetting;
end;

procedure SavePluginInfo;
var
  i, index: Integer;
  plugin: TPlugin;
  obj: ISuperObject;
  filename: string;
  sl: TStringList;
begin
  // don't load file if it doesn't exist
  filename := ProfilePath + 'PluginInfo.json';
  if FileExists(filename) then begin
    // load file text into SuperObject to parse it
    sl := TStringList.Create;
    sl.LoadFromFile(filename);
    obj := SO(PChar(sl.Text));
    sl.Free;
  end
  else begin
    // initialize new json object
    obj := SO;
    obj.O['plugins'] := SA([]);
  end;

  // loop through plugins
  Tracker.Write('Dumping plugin errors to JSON');
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := PluginsList[i];
    Tracker.UpdateProgress(1);
    Tracker.Write('  Dumping '+plugin.filename);
    index := IndexOfDump(obj.A['plugins'], plugin);
    if index = -1 then
      obj.A['plugins'].Add(plugin.InfoDump)
    else
      obj.A['plugins'].O[index] := plugin.InfoDump;
  end;

  // save and finalize
  Tracker.Write(' ');
  filename := ProfilePath + 'PluginInfo.json';
  Tracker.Write('Saving to '+filename);
  Tracker.UpdateProgress(1);
  obj.SaveTo(filename);
  obj := nil;
end;

procedure LoadPluginInfo;
var
  plugin: TPlugin;
  obj, pluginItem: ISuperObject;
  sl: TStringList;
  filename, hash: string;
begin
  // don't load file if it doesn't exist
  filename := ProfilePath + 'PluginInfo.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through patches
  filename := '';
  for pluginItem in obj['plugins'] do begin
    filename := pluginItem.AsObject.S['filename'];
    hash := pluginItem.AsObject.S['hash'];
    plugin := PluginByFileName(filename);
    if not Assigned(plugin) then
      continue;
    if (plugin.hash = hash) and (plugin.filename = filename) then
      plugin.LoadInfoDump(pluginItem);
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

procedure SaveReports(var lst: TList; path: string);
var
  i: Integer;
  report: TReport;
  fn: string;
begin
  //ForceDirectories(path);
  for i := 0 to Pred(lst.Count) do begin
    report := TReport(lst[i]);
    report.dateSubmitted := Now;
    fn := Format('%s-%s.txt', [report.filename, report.hash]);
    report.Save(path + fn);
  end;
end;

function ReportExistsFor(var plugin: TPlugin): boolean;
var
  fn, unsubmittedPath, submittedPath: string;
begin
  fn := Format('%s-%s.txt', [plugin.filename, plugin.hash]);
  unsubmittedPath := 'reports\' + fn;
  submittedPath := 'reports\submitted\' + fn;
  Result := FileExists(unsubmittedPath) or FileExists(submittedPath);
end;

procedure LoadReport(var report: TReport);
var
  fn, unsubmittedPath, submittedPath: string;
begin
  fn := Format('%s-%s.txt', [report.filename, report.hash]);
  unsubmittedPath := 'reports\' + fn;
  submittedPath := 'reports\submitted\' + fn;
  if FileExists(unsubmittedPath) then
    LoadReport(unsubmittedPath, report)
  else if FileExists(submittedPath) then
    LoadReport(submittedPath, report);
end;

procedure LoadReport(const filename: string; var report: TReport);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  report := TReport(TRttiJson.FromJson(sl.Text, report.ClassType));
  sl.Free;
end;


{******************************************************************************}
{ Tree Methods
  Helper methods for dealing with trees of nodes.
}
{******************************************************************************}

{
  SetChildren
  Sets the StateIndex attribute of all the children of @node
  to @state.  Uses recursion.
}
procedure SetChildren(node: TTreeNode; state: Integer);
var
  tmp: TTreeNode;
  e: TElementData;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // loop through children setting StateIndex to state
  // if child has children, recurse into that child
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    tmp.StateIndex := state;
    e := TElementData(tmp.Data);
    e.process := state <> cUnChecked;
    e.singleEntity := false;
    if tmp.HasChildren then
      SetChildren(tmp, state);
    tmp := tmp.getNextSibling;
  end;
end;

{
  UpdateParent
  Calculates and sets the StateIndex attribute for @node based
  on the StateIndex values of its children.  Uses recursion to
  update parents of the parent that was updated.
}
procedure UpdateParent(node: TTreeNode);
var
  tmp: TTreeNode;
  state: Integer;
  e: TElementData;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // parent state is checked if all siblings are checked
  state := cChecked;
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    if tmp.StateIndex <> cChecked then begin
      state := cPartiallyChecked;
      break;
    end;
    tmp := tmp.getNextSibling;
  end;

  // parent state is unchecked if all siblings are unchecked
  if state = cPartiallyChecked then begin
    state := cUnChecked;
    tmp := node.getFirstChild;
    while Assigned(tmp) do begin
      if tmp.StateIndex <> cUnChecked then begin
        state := cPartiallyChecked;
        break;
      end;
      tmp := tmp.getNextSibling;
    end;
  end;

  // set state, recurse to next parent
  node.StateIndex := state;
  e := TElementData(node.Data);
  e.process := state <> cUnChecked;
  e.singleEntity := false;
  tmp := node.Parent;
  UpdateParent(tmp);
end;

{
  CheckBoxManager
  Manages checkboxes in the TTreeView.  Changes the StateIndex
  of the checkbox associated with @node.  Uses SetChildren and
  UpdateParent.  Called by tvClick and tvKeyDown.
}
procedure CheckBoxManager(node: TTreeNode);
var
  e: TElementData;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // if unchecked or partially checked, set to checked and
  // set all children to checked, update parents
  if (node.StateIndex = cUnChecked)
  or (node.StateIndex = cPartiallyChecked) then begin
    node.StateIndex := cChecked;
    e := TElementData(node.Data);
    e.process := true;
    e.singleEntity := false;
    UpdateParent(node.Parent);
    SetChildren(node, cChecked);
  end
  // if checked, set to unchecked and set all children to
  // unchecked, update parents
  else if node.StateIndex = cChecked then begin
    node.StateIndex := cUnChecked;
    e := TElementData(node.Data);
    e.process := false;
    e.singleEntity := false;
    UpdateParent(node.Parent);
    SetChildren(node, cUnChecked);
  end;
end;

procedure LoadElement(var tv: TTreeView; node: TTreeNode; obj: ISuperObject;
  bWithinSingle: boolean);
var
  item: ISuperObject;
  child: TTreeNode;
  bProcess, bPreserveDeletions, bIsSingle: boolean;
  priority: Integer;
  sLinkTo, sLinkFrom: string;
  e: TElementData;
begin
  if not Assigned(obj) then
    exit;
  priority := obj.I['r'];
  bProcess := obj.I['p'] = 1;
  bPreserveDeletions := obj.I['d'] = 1;
  bIsSingle := obj.I['s'] = 1;
  bWithinSingle := bWithinSingle or bIsSingle;
  sLinkTo := obj.S['lt'];
  sLinkFrom := obj.S['lf'];
  e := TElementData.Create(priority, bProcess, bPreserveDeletions, bIsSingle,
    sLinkTo, sLinkFrom);
  child := tv.Items.AddChildObject(node, obj.S['n'], e);
  if bIsSingle then
    child.StateIndex := cPartiallyChecked
  else if bProcess then
    child.StateIndex := cChecked
  else
    child.StateIndex := cUnChecked;
  if Assigned(obj.O['c']) then try
    for item in obj['c'] do
      LoadElement(tv, child, item, bWithinSingle);
    if not bWithinSingle then
      UpdateParent(child);
  except
    on x : Exception do
      // nothing
  end;
end;

procedure LoadTree(var tv: TTreeView; aSetting: TSmashSetting);
var
  obj, item: ISuperObject;
  rootNode: TTreeNode;
  e: TElementData;
begin
  e := TElementData.Create(0, false, false, false, '', '');
  rootNode := tv.Items.AddObject(nil, 'Records', e);
  obj := aSetting.tree;
  if not Assigned(obj) then
    exit;
  if not Assigned(obj['records']) then
    exit;

  for item in obj['records'] do
    LoadElement(tv, rootNode, item, false);
end;


{******************************************************************************}
{ Helper methods
  Set of methods to help with working with Patch Plugins types.

  List of methods:
  - DeleteTempPath
  - GetRatingColor
  - GetEntry
  - IsBlacklisted
  - PluginLoadOrder
  - PluginByFilename
  - PatchByName
  - PatchByFilename
  - CreateNewPatch
  - CreateNewPlugin
  - PatchPluginsCompare
}
{******************************************************************************}

procedure RenamePatches(var sl: TStringList);
var
  i: Integer;
  oldPath, newPath: string;
begin
  for i := 0 to Pred(sl.Count) do begin
    oldPath := sl[i];
    newPath := RemoveFromEnd(oldPath, '.smash');
    if FileExists(newPath + '.bak') then
      DeleteFile(newPath + '.bak');
    RenameFile(newPath, newPath + '.bak');
    RenameFile(oldPath, newPath);
  end;
end;

procedure GetPatchesToRename(var sl: TStringList);
var
  i: Integer;
  patch: TPatch;
  path: string;
begin
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    path := patch.dataPath + patch.filename + '.smash';
    if FileExists(path) then
      sl.Add(path);
  end;
end;

procedure RemoveSettingFromPlugins(aSetting: TSmashSetting);
var
  i: Integer;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if plugin.setting = aSetting.name then begin
      plugin.setting := 'Skip';
      plugin.smashSetting := SettingByName('Skip');
    end;
  end;
end;

procedure DeleteTempPath;
begin
  DeleteDirectory(TempPath);
end;

procedure ShowProgressForm(parent: TForm; var pf: TProgressForm; s: string);
begin
  parent.Enabled := false;
  pf := TProgressForm.Create(parent);
  pf.LogPath := LogPath;
  pf.PopupParent := parent;
  pf.Caption := s;
  pf.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pf.Show;
end;

function GetRatingColor(rating: real): integer;
var
  k1, k2: real;
  r, g: byte;
begin
  if rating = -2.0 then begin
    Result := $707070;
    exit;
  end;

  if rating = -1.0 then begin
    Result := $000000;
    exit;
  end;

  if (rating > 2.0) then begin
    k2 := (rating - 2.0)/2.0;
    k1 := 1.0 - k2;
    r := Trunc($E5 * k1 + $00 * k2);
    g := Trunc($A8 * k1 + $90 * k2);
  end
  else begin
    k2 := (rating/2.0);
    k1 := 1.0 - k2;
    r := Trunc($FF * k1 + $E5 * k2);
    g := Trunc($00 * k1 + $A8 * k2);
  end;

  Result := g * 256 + r;
end;

function GetEntry(name, numRecords, version: string): TSmashSetting;
var
  i: Integer;
  entry: TSmashSetting;
begin
  Result := TSmashSetting.Create;
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TSmashSetting(dictionary[i]);
    if entry.name = name then begin
      Result := entry;
      exit;
    end;
  end;
end;

{ Gets the load order of the plugin matching the given name }
function PluginLoadOrder(filename: string): integer;
var
  i: integer;
  plugin: TPlugin;
begin
  Result := -1;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if plugin.filename = filename then begin
      Result := i;
      exit;
    end;
  end;
end;

{ Gets a smash setting matching the given name. }
function SettingByName(name: string): TSmashSetting;
var
  i: integer;
  aSetting: TSmashSetting;
begin
  Result := nil;
  for i := 0 to Pred(SmashSettings.Count) do begin
    aSetting := TSmashSetting(SmashSettings[i]);
    if aSetting.name = name then begin
      Result := aSetting;
      exit;
    end;
  end;
end;

{ Gets a plugin matching the given name. }
function PluginByFilename(filename: string): TPlugin;
var
  i: integer;
  plugin: TPlugin;
begin
  Result := nil;
  for i := 0 to Pred(PluginsList.count) do begin
    plugin := TPlugin(PluginsList[i]);
    if plugin.filename = filename then begin
      Result := plugin;
      exit;
    end;
  end;
end;

{ Gets a patch matching the given name. }
function PatchByName(patches: TList; name: string): TPatch;
var
  i: integer;
  patch: TPatch;
begin
  Result := nil;
  for i := 0 to Pred(patches.Count) do begin
    patch := TPatch(patches[i]);
    if patch.name = name then begin
      Result := patch;
      exit;
    end;
  end;
end;


{ Gets a patch matching the given name. }
function PatchByFilename(patches: TList; filename: string): TPatch;
var
  i: integer;
  patch: TPatch;
begin
  Result := nil;
  for i := 0 to Pred(patches.Count) do begin
    patch := TPatch(patches[i]);
    if patch.filename = filename then begin
      Result := patch;
      exit;
    end;
  end;
end;

{ Create a new patch with non-conflicting name and filename }
function CreateNewPatch(patches: TList): TPatch;
var
  i: Integer;
  patch: TPatch;
  name: string;
begin
  patch := TPatch.Create;

  // deal with conflicting patch names
  i := 0;
  name := patch.name;
  while Assigned(PatchByName(patches, name)) do begin
    Inc(i);
    name := 'NewPatch' + IntToStr(i);
  end;
  patch.name := name;

  // deal with conflicting patch filenames
  i := 0;
  name := patch.filename;
  while Assigned(PatchByFilename(patches, name)) do begin
    Inc(i);
    name := 'NewPatch' + IntToStr(i) + '.esp';
  end;
  patch.filename := name;

  Result := patch;
end;

{ Create a new plugin }
function CreateNewPlugin(filename: string): TPlugin;
var
  aFile: IwbFile;
  LoadOrder: integer;
  plugin: TPlugin;
begin
  Result := nil;
  LoadOrder := PluginsList.Count + 1;
  // fail if maximum load order reached
  if LoadOrder > 254 then begin
    Tracker.Write('Maximum load order reached!  Can''t create file '+filename);
    exit;
  end;

  // create new plugin file
  aFile := wbNewFile(wbDataPath + filename, LoadOrder);
  aFile._AddRef;

  // create new plugin object
  plugin := TPlugin.Create;
  plugin.filename := filename;
  plugin._File := aFile;
  PluginsList.Add(plugin);

  Result := plugin;
end;

{ Comparator for sorting plugins in patch }
function PatchPluginsCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  LO1, LO2: Integer;
begin
  LO1 := Integer(List.Objects[Index1]);
  LO2 := Integer(List.Objects[Index2]);
  Result := LO1 - LO2;
end;


{******************************************************************************}
{ Client methods
  Set of methods for communicating with the Patch Plugins server.
  - InitializeClient
  - TCPClient.Connected
  - UsernameAvailable
  - RegisterUser
  - SaveReports
  - SendReports
}
{******************************************************************************}

procedure InitializeClient;
begin
  TCPClient := TidTCPClient.Create(nil);
  TCPClient.Host := settings.serverHost;
  TCPClient.Port := settings.serverPort;
  TCPClient.ReadTimeout := 4000;
  TCPClient.ConnectTimeout := 1000;
  ConnectionAttempts := 0;
end;

procedure ConnectToServer;
begin
  if (bConnecting or TCPClient.Connected)
  or (ConnectionAttempts >= MaxConnectionAttempts) then
    exit;

  bConnecting := true;
  try
    Logger.Write('CLIENT', 'Connect', 'Attempting to connect to '+TCPClient.Host+':'+IntToStr(TCPClient.Port));
    TCPClient.Connect;
    Logger.Write('CLIENT', 'Connect', 'Connection successful!');
    CheckAuthorization;
    SendGameMode;
    GetStatus;
    CompareStatuses;
    SendPendingReports;
  except
    on x: Exception do begin
      Logger.Write('ERROR', 'Connect', 'Connection failed.');
      Inc(ConnectionAttempts);
      if ConnectionAttempts = MaxConnectionAttempts then
        Logger.Write('CLIENT', 'Connect', 'Maximum connection attempts reached.  '+
          'Click the disconnected icon in the status bar to retry.');
    end;
  end;
  bConnecting := false;
end;

function ServerAvailable: boolean;
begin
  Result := false;

  try
    if TCPClient.Connected then begin
      TCPClient.IOHandler.WriteLn('', TIdTextEncoding.Default);
      Result := true;
    end;
  except on Exception do
    // we're not connected
  end;
end;

procedure SendClientMessage(var msg: TmsMessage);
var
  msgJson: string;
begin
  msgJson := TRttiJson.ToJson(msg);
  TCPClient.IOHandler.WriteLn(msgJson, TIdTextEncoding.Default);
end;

function CheckAuthorization: boolean;
var
  msg, response: TmsMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  if settings.username = '' then
    exit;
  Logger.Write('CLIENT', 'Login', 'Checking if authenticated as "'+settings.username+'"');

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send notify request to server
    msg := TmsMessage.Create(MSG_NOTIFY, settings.username, settings.key, 'Authorized?');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(line, TmsMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = 'Yes';
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception authorizing user '+x.Message);
    end;
  end;

  // set bAuthorized boolean
  bAuthorized := Result;
end;

procedure SendGameMode;
var
  msg: TmsMessage;
begin
  if not TCPClient.Connected then
    exit;

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send notifification to server
    msg := TmsMessage.Create(MSG_NOTIFY, settings.username, settings.key, wbAppName);
    SendClientMessage(msg);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending game mode '+x.Message);
    end;
  end;
end;

procedure SendStatistics;
var
  msg, response: TmsMessage;
  LLine: string;
begin
  if not TCPClient.Connected then
    exit;

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send statistics to server
    msg := TmsMessage.Create(MSG_STATISTICS, settings.username, settings.key, TRttiJson.ToJson(sessionStatistics));
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(LLine, TmsMessage));
    Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending statistics '+x.Message);
    end;
  end;
end;

procedure ResetAuth;
var
  msg, response: TmsMessage;
  line: string;
begin
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Resetting authentication as "'+settings.username+'"');

  // attempt to reset authorization
  // throws exception if server is unavailable
  try
    // send auth reset request to server
    msg := TmsMessage.Create(MSG_AUTH_RESET, settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(line, TmsMessage));
    Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception resetting authentication '+x.Message);
    end;
  end;
end;

function UsernameAvailable(username: string): boolean;
var
  msg, response: TmsMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Checking username availability "'+username+'"');

  // attempt to register user
  // throws exception if server is unavailable
  try
    // send register request to server
    msg := TmsMessage.Create(MSG_REGISTER, username, settings.key, 'Check');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(line, TmsMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = 'Available';
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception checking username '+x.Message);
    end;
  end;
end;

function RegisterUser(username: string): boolean;
var
  msg, response: TmsMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Registering username "'+username+'"');

  // attempt to register user
  // throws exception if server is unavailable
  try
    // send register request to server
    msg := TmsMessage.Create(MSG_REGISTER, username, settings.key, 'Register');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(line, TmsMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = ('Registered ' + username);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception registering username '+x.Message);
    end;
  end;
end;

function GetStatus: boolean;
var
  msg, response: TmsMessage;
  LLine: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  if (Now - LastStatusTime) < StatusDelay then
    exit;
  LastStatusTime := Now;
  Logger.Write('CLIENT', 'Update', 'Getting update status');

  // attempt to get a status update
  // throws exception if server is unavailable
  try
    // send status request to server
    msg := TmsMessage.Create(MSG_STATUS, settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmsMessage(TRttiJson.FromJson(LLine, TmsMessage));
    RemoteStatus := TmsStatus(TRttiJson.FromJson(response.data, TmsStatus));
    //Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting status '+x.Message);
    end;
  end;
end;

function VersionCompare(v1, v2: string): boolean;
var
  sl1, sl2: TStringList;
  i, c1, c2: integer;
begin
  Result := false;

  // parse versions with . as delimiter
  sl1 := TStringList.Create;
  sl1.LineBreak := '.';
  sl1.Text := v1;
  sl2 := TStringList.Create;
  sl2.LineBreak := '.';
  sl2.Text := v2;

  // look through each version clause and perform comparisons
  i := 0;
  while (i < sl1.Count) and (i < sl2.Count) do begin
    c1 := StrToInt(sl1[i]);
    c2 := StrToInt(sl2[i]);
    if (c1 < c2) then begin
      Result := true;
      break;
    end
    else if (c1 > c2) then begin
      Result := false;
      break;
    end;
    Inc(i);
  end;

  // free ram
  sl1.Free;
  sl2.Free;
end;

procedure CompareStatuses;
begin
  if not Assigned(RemoteStatus) then
    exit;

  // handle program update
  // TODO: split string on . and do a greater than comparison for each clause
  bProgramUpdate := VersionCompare(status.programVersion, RemoteStatus.programVersion);

  // handle dictionary update based on gamemode
  case wbGameMode of
    gmTES5: begin
      bDictionaryUpdate := status.tes5Hash <> RemoteStatus.tes5Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.tes5Hash+' != '+RemoteStatus.tes5hash);
    end;
    gmTES4: begin
      bDictionaryUpdate := status.tes4Hash <> RemoteStatus.tes4Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.tes4Hash+' != '+RemoteStatus.tes4hash);
    end;
    gmFNV: begin
      bDictionaryUpdate := status.fnvHash <> RemoteStatus.fnvHash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.fnvHash+' != '+RemoteStatus.fnvhash);
    end;
    gmFO3: begin
      bDictionaryUpdate := status.fo3Hash <> RemoteStatus.fo3Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.fo3Hash+' != '+RemoteStatus.fo3hash);
    end;
  end;
end;

function UpdateChangeLog: boolean;
var
  msg: TmsMessage;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Update', 'Getting changelog');
  Tracker.Write('Getting changelog');

  // attempt to request changelog
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(MSG_REQUEST, settings.username, settings.key, 'Changelog');
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create('changelog.txt', fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load changelog from response
    Logger.Write('CLIENT', 'Update', 'Changelog recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting changelog '+x.Message);
    end;
  end;
end;

function UpdateDictionary: boolean;
var
  msg: TmsMessage;
  filename: string;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  filename := wbAppName+'Dictionary.txt';
  Logger.Write('CLIENT', 'Update',  filename);
  Tracker.Write('Updating '+filename);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(MSG_REQUEST, settings.username, settings.key, filename);
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create(filename, fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load dictionary from response
    Logger.Write('CLIENT', 'Update', filename+' recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    LoadDictionary;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception updating dictionary '+x.Message);
    end;
  end;
end;

function UpdateProgram: boolean;
var
  archive: TAbUnZipper;
begin
  // check if zip for updating exists
  Result := false;
  if not FileExists('PatchPlugins.zip') then
    exit;

  // rename program
  if FileExists('PatchPlugins.exe.bak') then
    DeleteFile('PatchPlugins.exe.bak');
  RenameFile('PatchPlugins.exe', 'PatchPlugins.exe.bak');

  // Create an instance of the TZipForge class
  archive := TAbUnZipper.Create(nil);
  try
    with archive do begin
      // The name of the ZIP file to unzip
      FileName := ProgramPath + 'PatchPlugins.zip';
      // Set base (default) directory for all archive operations
      BaseDirectory := ProgramPath;
      // Extract all files from the archive to current directory
      ExtractOptions := [eoCreateDirs, eoRestorePath];
      ExtractFiles('*.*');
      Result := true;
    end;
  except
    on x: Exception do begin
      Logger.Write('ERROR', 'Update', 'Exception ' + x.Message);
      exit;
    end;
  end;

  // clean up
  archive.Free;
  DeleteFile('PatchPlugins.zip');
end;

function DownloadProgram: boolean;
var
  msg: TmsMessage;
  filename: string;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  filename := 'PatchPlugins.zip';
  if FileExists(filename) then begin
    MessageDlg(GetString('mpOpt_PendingUpdate'), mtInformation, [mbOk], 0);
    Result := true;
    exit;
  end;

  Logger.Write('CLIENT', 'Update', 'Patch Plugins v'+RemoteStatus.programVersion);
  Tracker.Write('Updating program to v'+RemoteStatus.programVersion);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(MSG_REQUEST, settings.username, settings.key, 'Program');
    SendClientMessage(msg);

    // get response
    Logger.Write('CLIENT', 'Update', 'Downloading '+filename);
    stream := TFileStream.Create('PatchPlugins.zip', fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load dictionary from response
    Logger.Write('CLIENT', 'Update', filename+' recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception updating program '+x.Message);
    end;
  end;
end;

function SendReports(var lst: TList): boolean;
var
  i: integer;
  report: TReport;
  msg, response: TmsMessage;
  reportJson, LLine: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;

  // attempt to send reports
  // throws exception if server is unavailable
  try
    // send all reports in @lst
    for i := 0 to Pred(lst.Count) do begin
      report := TReport(lst[i]);
      if Length(report.notes) > 255 then begin
        Logger.Write('CLIENT', 'Report', 'Skipping '+report.filename+', notes too long.');
        continue;
      end;
      reportJson := TRttiJson.ToJson(report);
      Logger.Write('CLIENT', 'Report', 'Sending '+report.filename);

      // send report to server
      msg := TmsMessage.Create(MSG_REPORT, settings.username, settings.key, reportJson);
      SendClientMessage(msg);

      // get response
      LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
      response := TmsMessage.Create;
      response := TmsMessage(TRttiJson.FromJson(LLine, response.ClassType));
      Logger.Write('CLIENT', 'Response', response.data);
      Inc(sessionStatistics.settingsSubmitted);
    end;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending reports '+x.Message);
    end;
  end;
end;

function SendPendingReports: boolean;
var
  lst: TList;
  info: TSearchRec;
  report: TReport;
  path: string;
  i: Integer;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;

  // exit if no reports to load
  path := ProgramPath + 'reports\';
  if FindFirst(path + '*', faAnyFile, info) <> 0 then
    exit;  
  lst := TList.Create;
  // load reports into list
  repeat
    if IsDotFile(info.Name) then
      continue;
    if not StrEndsWith(info.Name, '.txt') then
      continue;
    try
      report := TReport.Create;
      LoadReport(path + info.Name, report);
      DeleteFile(path + info.Name);
      report.Save(path + 'submitted\' + info.Name);
      lst.Add(report);
    except
      on x: Exception do
        Logger.Write('ERROR', 'General', Format('Unable to load report at %s%s: %s', [path, info.Name, x.Message]));
    end;
  until FindNext(info) <> 0;
  FindClose(info);

  // send reports if any were found
  if lst.Count > 0 then
    Result := SendReports(lst);

  // free reports in list, then list
  for i := Pred(lst.Count) downto 0 do begin
    report := TReport(lst[i]);
    report.Free;
  end;
  lst.Free;
end;


{******************************************************************************}
{ Object methods
  Set of methods for objects.

  List of methods:
  - TLogMessage.Create
  - TmpMessage.Create
  - TReport.Create
  - TReport.TRttiJson.ToJson
  - TPlugin.Create
  - TPlugin.GetFlags
  - TPlugin.GetFlagsString
  - TPlugin.GetData
  - TPlugin.GetHash
  - TPlugin.GetDataPath
  - TPlugin.FindErrors
  - TPatch.Create
  - TPatch.Dump
  - TPatch.LoadDump
  - TPatch.GetTimeCost
  - TPatch.PluginsModified
  - TPatch.FilesExist
  - TPatch.GetStatus
  - TPatch.GetHashes
  - TPatch.GetLoadOrders
  - TPatch.SortPlugins
  - TEntry.Create
  - TSettings.Create
  - TSettings.Save
  - TSettings.Load
}
{******************************************************************************}

constructor TLogMessage.Create(time, appTime, group, &label, text: string);
begin
  self.time := time;
  self.appTime := appTime;
  self.group := group;
  self.&label := &label;
  self.text := text;
end;

{ TmpMessage Constructor }
constructor TmsMessage.Create(id: integer; username, auth, data: string);
begin
  self.id := id;
  self.username := username;
  self.auth := auth;
  self.data := data;
end;

{ Constructor for TmpStatus }
constructor TmsStatus.Create;
begin
  ProgramVersion := GetVersionMem;
  if FileExists('TES5Dictionary.txt') then
    TES5Hash := GetCRC32('TES5Dictionary.txt');
  if FileExists('TES4Dictionary.txt') then
    TES4Hash := GetCRC32('TES4Dictionary.txt');
  if FileExists('FNVDictionary.txt') then
    FNVHash := GetCRC32('FNVDictionary.txt');
  if FileExists('FO3Dictionary.txt') then
    FO3Hash := GetCRC32('FO3Dictionary.txt');

  // log messages
  Logger.Write('GENERAL', 'Status', 'ProgramVersion: '+ProgramVersion);
  case CurrentProfile.gameMode of
    1: Logger.Write('GENERAL', 'Status', 'TES5 Dictionary Hash: '+TES5Hash);
    2: Logger.Write('GENERAL', 'Status', 'TES4 Dictionary Hash: '+TES4Hash);
    3: Logger.Write('GENERAL', 'Status', 'FO3 Dictionary Hash: '+FNVHash);
    4: Logger.Write('GENERAL', 'Status', 'FNV Dictionary Hash: '+FO3Hash);
  end;
end;

{ TReport }
procedure TReport.SetNotes(notes: string);
begin
  self.notes := StringReplace(Trim(notes), #13#10, '@13', [rfReplaceAll]);
end;

function TReport.GetNotes: string;
begin
  Result := StringReplace(notes, '@13', #13#10, [rfReplaceAll]);
end;

procedure TReport.Save(const filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := TRttiJson.ToJson(self);
  try
    ForceDirectories(ExtractFilePath(filename));
    sl.SaveToFile(filename);
  except
    on x: Exception do
      ShowMessage(Format('Unabled to save report %s: %s', [filename, x.Message]));
  end;
  sl.Free;
end;

{ TPlugin Constructor }
constructor TPlugin.Create;
begin
  hasData := false;
  patch := ' ';
  setting := 'Skip';
  smashSetting := SettingByName(setting);
  description := TStringList.Create;
  masters := TStringList.Create;
  requiredBy := TStringList.Create;
end;

destructor TPlugin.Destroy;
begin
  description.Free;
  masters.Free;
  requiredBy.Free;
  inherited;
end;

{ Fetches data associated with a plugin. }
procedure TPlugin.GetData;
var
  Container: IwbContainer;
  s: string;
begin
  hasData := true;
  // get data
  filename := _File.FileName;
  Container := _File as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  author := Container.GetElementEditValue('CNAM - Author');
  numRecords := Container.GetElementEditValue('HEDR - Header\Number of Records');

  // get masters, flags
  GetMasters(_File, masters);
  AddRequiredBy(filename, masters);

  // get hash, datapath
  GetHash;
  GetDataPath;

  // get description
  s := Container.GetElementEditValue('SNAM - Description');
  description.Text := Wordwrap(s, 80);

  // get file attributes
  fileSize := GetFileSize(wbDataPath + filename);
  dateModified := DateTimeToStr(GetLastModified(wbDataPath + filename));

  // get numOverrides if not blacklisted
  if (StrToInt(numRecords) < 10000) then
    numOverrides := IntToStr(CountOverrides(_File));
end;

procedure TPlugin.GetHash;
begin
  hash := IntToHex(wbCRC32File(wbDataPath + filename), 8);
end;

procedure TPlugin.GetDataPath;
var
  modName: string;
begin
  dataPath := wbDataPath;
  if settings.usingMO then begin
    modName := GetModContainingFile(ActiveMods, filename);
    if modName <> '' then
      dataPath := settings.MOModsPath + modName + '\';
  end;
end;

function TPlugin.GetFormIndex: Integer;
var
  Container, MasterFiles: IwbContainer;
begin
  Result := 0;
  Container := self._File as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    Result := MasterFiles.ElementCount;
  end;
end;

function TPlugin.IsInPatch: boolean;
begin
  Result := patch <> ' ';
end;

function TPlugin.InfoDump: ISuperObject;
var
  obj: ISuperObject;
begin
  obj := SO;

  // filename, hash, errors
  obj.S['filename'] := filename;
  obj.S['hash'] := hash;
  obj.S['setting'] := setting;

  Result := obj;
end;

procedure TPlugin.LoadInfoDump(obj: ISuperObject);
begin
  setting := obj.AsObject.S['setting'];
  smashSetting := SettingByName(setting);
  if not Assigned(smashSetting) then begin
    setting := 'Skip';
    smashSetting := SettingByName(setting);
  end;
end;

{ TPatch Constructor }
constructor TPatch.Create;
begin
  name := 'NewPatch';
  filename := 'NewPatch.esp';
  status := psUnknown;
  dateBuilt := 0;
  plugins := TStringList.Create;
  hashes := TStringList.Create;
  masters := TStringList.Create;
  fails := TStringList.Create;
end;

destructor TPatch.Destroy;
begin
  plugins.Free;
  hashes.Free;
  masters.Free;
  fails.Free;
  inherited;
end;

{ Produces a dump of the patch. }
function TPatch.Dump: ISuperObject;
var
  obj: ISuperObject;
  i: integer;
begin
  obj := SO;

  // normal attributes
  obj.S['name'] := name;
  obj.S['filename'] := filename;
  obj.S['dateBuilt'] := DateTimeToStr(dateBuilt);

  // plugins, pluginSizes, pluginDates, masters
  obj.O['plugins'] := SA([]);
  for i := 0 to Pred(plugins.Count) do
    obj.A['plugins'].S[i] := plugins[i];
  obj.O['pluginHashes'] := SA([]);
  for i := 0 to Pred(hashes.Count) do
    obj.A['pluginHashes'].S[i] := hashes[i];
  obj.O['masters'] := SA([]);
  for i := 0 to Pred(masters.Count) do
    obj.A['masters'].S[i] := masters[i];

  // files, log, ignored dependencies
  obj.O['fails'] := SA([]);
  for i := 0 to Pred(fails.Count) do
    obj.A['fails'].S[i] := fails[i];

  Result := obj;
end;

{ Loads a dump of a patch. }
procedure TPatch.LoadDump(obj: ISuperObject);
var
  item: ISuperObject;
begin
  // load object attributes
  name := obj.AsObject.S['name'];
  filename := obj.AsObject.S['filename'];

  // try loading dateBuilt and parsing to DateTime
  try
    dateBuilt := StrToDateTime(obj.AsObject.S['dateBuilt']);
  except on Exception do
    dateBuilt := 0; // on exception set to never built
  end;

  // load array attributes
  for item in obj['plugins'] do
    plugins.Add(item.AsString);
  for item in obj['pluginHashes'] do
    hashes.Add(item.AsString);
  for item in obj['masters'] do
    masters.Add(item.AsString);
  for item in obj['fails'] do
    fails.Add(item.AsString);
end;

function TPatch.GetTimeCost: integer;
var
  i: Integer;
  plugin: TPlugin;
begin
  Result := 1;
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then
      Inc(Result, plugin._File.RecordCount);
  end;
end;

// Checks to see if the plugins in a patch have been modified since it was last
// patchd.
function TPatch.PluginsModified: boolean;
var
  plugin: TPlugin;
  i: integer;
begin
  Result := false;
  // true if number of hashes not equal to number of plugins
  if plugins.Count <> hashes.Count then begin
    Logger.Write('MERGE', 'Status', name + ' -> Plugin count changed');
    Result := true;
    exit;
  end;
  // true if any plugin hash doesn't match
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.hash <> hashes[i] then begin
        Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename + ' hash changed.');
        Result := true;
      end;
    end;
  end;
end;

// Checks if the files associated with a patch exist
function TPatch.FilesExist: boolean;
begin
  Result := FileExists(dataPath + filename);
end;

procedure TPatch.GetStatus;
var
  i: Integer;
  plugin: TPlugin;
begin
  Logger.Write('MERGE', 'Status', name + ' -> Getting status');
  status := psUnknown;

  // don't patch if no plugins to patch
  if (plugins.Count < 1) then begin
    Logger.Write('MERGE', 'Status', name + ' -> No plugins to patch');
    status := psNoPlugins;
    exit;
  end;

  // don't patch if mod destination directory is blank
  if (settings.patchDirectory = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Patch directory blank');
    status := psDirInvalid;
    exit;
  end;

  // don't patch if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.MOPath = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory blank');
    status := psDirInvalid;
    exit;
  end;

  // don't patch if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.MOPath) then begin
     Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory invalid');
     status := psDirInvalid;
     exit;
  end;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Logger.Write('MERGE', 'Status', name + ' -> Plugin '+plugins[i]+' is missing');
      if status = psUnknown then status := psUnloaded;
      continue;
    end;
  end;

  dataPath := settings.patchDirectory + name + '\';
  if (not PluginsModified) and FilesExist and (status = psUnknown) then begin
    Logger.Write('MERGE', 'Status', name + ' -> Up to date');
    status := psUpToDate;
  end;

  // status green, ready to go
  if status = psUnknown then begin
    Logger.Write('MERGE', 'Status', name + ' -> Ready to be patchd');
    if dateBuilt = 0 then
      status := psBuildReady
    else
      status := psRebuildReady;
  end;
end;

function TPatch.GetStatusColor: integer;
begin
  Result := StatusArray[Ord(status)].color;
end;

// Update the hashes list for the plugins in the patch
procedure TPatch.UpdateHashes;
var
  i: Integer;
  aPlugin: TPlugin;
begin
  hashes.Clear;
  for i := 0 to Pred(plugins.Count) do begin
    aPlugin := PluginByFilename(plugins[i]);
    if Assigned(aPlugin) then
      hashes.Add(aPlugin.hash);
  end;
end;

// Get load order for plugins in patch that don't have it
procedure TPatch.GetLoadOrders;
var
  i: integer;
begin
  for i := 0 to Pred(plugins.Count) do
    if not Assigned(plugins.Objects[i]) then
      plugins.Objects[i] := TObject(PluginLoadOrder(plugins[i]));
end;

// Sort plugins by load order position
procedure TPatch.SortPlugins;
begin
  GetLoadOrders;
  plugins.CustomSort(PatchPluginsCompare);
end;

procedure TPatch.Remove(plugin: TPlugin);
var
  index: Integer;
begin
  // clear plugin's patch property, if it's the name of this patch
  if plugin.patch = name then
    plugin.patch := ' ';
  // remove plugin from patch, if present
  index := plugins.IndexOf(plugin.filename);
  if index > -1 then
    plugins.Delete(index);
end;

procedure TPatch.Remove(pluginFilename: string);
var
  index: Integer;
begin
  index := plugins.IndexOf(pluginFilename);
  // remove plugin from patch, if present
  if index > -1 then
    plugins.Delete(index);
end;

{ TFilter }
constructor TFilter.Create(group: string; enabled: boolean);
begin
  self.group := group;
  self.enabled := enabled;
end;

constructor TFilter.Create(group, &label: string; enabled: boolean);
begin
  self.group := group;
  self.&label := &label;
  self.enabled := enabled;
end;

constructor TElementData.Create(priority: Byte; process: Boolean;
  preserveDeletions: Boolean; singleEntity: Boolean; linkTo, linkFrom: string);
begin
  self.priority := priority;
  self.process := process;
  self.preserveDeletions := preserveDeletions;
  self.singleEntity := singleEntity;
  self.linkTo := linkTo;
  self.linkFrom := linkFrom;
end;

{ TSmashSetting }
function GetUniqueSettingName(base: string): string;
var
  i: Integer;
begin
  Result := base;
  i := 1;
  while Assigned(SettingByName(Result)) do begin
    Inc(i);
    Result := base + IntToStr(i);
  end;
end;

constructor TSmashSetting.Create;
begin
  name := GetUniqueSettingName('NewSetting');
  hash := '$00000000';
  color := clBlack;
  description := '';
  records := '';
  tree := nil;
end;

destructor TSmashSetting.Destroy;
begin
  name := '';
  hash := '';
  color := 0;
  description := '';
  records := '';
  if Assigned(tree) then tree._Release;
  tree := nil;
end;

constructor TSmashSetting.Clone(s: TSmashSetting);
begin
  name := GetUniqueSettingName(s.name + '-Clone');
  hash := '$00000000';
  color := s.color;
  records := s.records;
  description := s.description;
  tree := s.tree.Clone;
end;

function TSmashSetting.GetRecordDef(sig: string): ISuperObject;
var
  item: ISuperObject;
begin
  Result := nil;
  if not Assigned(tree) then
    exit;
  // loop through record objects
  for item in tree['records'] do begin
    if item.S['n'] = sig then
      Result := item;
  end;
end;

procedure TSmashSetting.LoadDump(dump: ISuperObject);
begin
  name := dump.S['name'];
  color := dump.I['color'];
  hash := dump.S['hash'];
  description := dump.S['description'];
  records := dump.S['records'];
  tree := dump.O['tree'];
end;

function TSmashSetting.Dump: ISuperObject;
var
  obj: ISuperObject;
begin
  obj := SO;

  // tree
  obj.O['tree'] := tree;
  // normal attributes
  obj.S['records'] := records;
  obj.S['description'] := description;
  obj.I['color'] := color;
  obj.S['hash'] := hash;
  obj.S['name'] := name;

  Result := obj;
end;

procedure TSmashSetting.Save;
var
  path: string;
begin
  path := Format('%s\settings\%s.json', [ProgramPath, name]);
  ForceDirectories(ExtractFilePath(path));
  Dump.SaveTo(path);
end;

procedure TSmashSetting.Rename(newName: string);
var
  oldPath, newPath: string;
begin
  oldPath := ProgramPath + 'settings\' + name + '.json';
  newPath := ProgramPath + 'settings\' + newName + '.json';
  if FileExists(oldpath) then
    RenameFile(oldpath, newpath);
  name := newName;
end;

{ TSettings constructor }
constructor TSettings.Create;
begin
  // default settings
  language := 'English';
  serverHost := 'matorsmash.us.to';
  serverPort := 970;
  simpleDictionaryView := false;
  simplePluginsView := false;
  updateDictionary := false;
  updateProgram := false;
  usingMO := false;
  MOPath := '';
  patchDirectory := wbDataPath;
  generalMessageColor := clGreen;
  loadMessageColor := clPurple;
  clientMessageColor := clBlue;
  patchMessageColor := $000080FF;
  pluginMessageColor := $00484848;
  errorMessageColor := clRed;
  logMessageTemplate := '[{{AppTime}}] ({{Group}}) {{Label}}: {{Text}}';

  // generate a new secure key
  GenerateKey;
end;

procedure TSettings.GenerateKey;
const
  chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i: Integer;
begin
  key := '';
  for i := 0 to 31 do
    key := key + chars[Random(64)];
end;

{ TStatistics constructor }
constructor TStatistics.Create;
begin
  timesRun := 0;
  patchesBuilt := 0;
  pluginsPatched := 0;
  settingsSubmitted := 0;
  recsSubmitted := 0;
end;


{ TProfile }
constructor TProfile.Create(name: string);
begin
  self.name := name;
end;

procedure TProfile.Clone(p: TProfile);
begin
  name := p.name;
  gameMode := p.gameMode;
  gamePath := p.gamePath;
end;

procedure TProfile.Delete;
var
  path: string;
begin
  path := ProgramPath + 'profiles\' + name;
  if DirectoryExists(path) then
    RecycleDirectory(path);
end;

procedure TProfile.Rename(name: string);
var
  oldProfilePath, newProfilePath: string;
begin
  // rename old profile folder if necessary
  oldProfilePath := ProgramPath + 'profiles\' + self.name;
  newProfilePath := ProgramPath + 'profiles\' + name;
  if DirectoryExists(oldProfilePath) then
    RenameFile(oldProfilePath, newProfilePath);

  // then change name in the object
  self.name := name;
end;


end.
