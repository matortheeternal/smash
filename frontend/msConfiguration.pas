unit msConfiguration;

interface

uses
  Classes, wbInterface,
  // mte units
  mteHelpers, RttiIni;

type
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    regName: string;
    appName: string;
    exeName: string;
    appIDs: string;
    bsaOptMode: string;
  end;
  TSettings = class(TObject)
  public
    [IniSection('General')]
    profile: string;
    newProfile: boolean;
    gameMode: integer;
    gamePath: string;
    language: string;
    simpleDictionaryView: boolean;
    simplePluginsView: boolean;
    simpleSplash: boolean;
    [IniSection('Advanced')]
    generalMessageColor: Int64;
    clientMessageColor: Int64;
    loadMessageColor: Int64;
    patchMessageColor: Int64;
    pluginMessageColor: Int64;
    errorMessageColor: Int64;
    logMessageTemplate: string;
    preserveTempPath: boolean;
    allowTagging: boolean;
    smashSound: boolean;
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
    preserveITPOs: boolean;
    constructor Create; virtual;
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
  TProgramStatus = class(TObject)
  public
    bInitException, bLoadException, bChangeProfile, bForceTerminate,
    bLoaderDone, bInstallUpdate, bUpdatePatchStatus, bClose: boolean;
    GameMode: TGameMode;
    Version: String;
    constructor Create; virtual;
  end;

  procedure DeleteTempPath;
  procedure LoadLanguage;
  function GetLanguageString(name: string): string;
  procedure SaveProfile(var p: TProfile);
  procedure LoadSettings; overload;
  function LoadSettings(path: string): TSettings; overload;
  procedure SaveSettings; overload;
  procedure SaveSettings(var s: TSettings; path: string); overload;
  procedure LoadStatistics;
  procedure SaveStatistics;

var
  settings: TSettings;
  CurrentProfile: TProfile;
  statistics, sessionStatistics: TStatistics;
  language, PathList: TStringList;
  ProgramStatus: TProgramStatus;

const
  // IMPORTANT CONSTANTS
  bTranslationDump = false;

  // GAME MODES
  GameArray: array[1..6] of TGameMode = (
    ( longName: 'Skyrim'; gameName: 'Skyrim'; gameMode: gmTES5;
      regName: 'Skyrim'; appName: 'TES5'; exeName: 'TESV.exe'; appIDs: '72850';
      bsaOptMode: 'sk'; ),
    ( longName: 'Oblivion'; gameName: 'Oblivion'; gameMode: gmTES4;
      regName: 'Oblivion'; appName: 'TES4'; exeName: 'Oblivion.exe';
      appIDs: '22330,900883'; bsaOptMode: 'ob'; ),
    ( longName: 'Fallout New Vegas'; gameName: 'FalloutNV'; gameMode: gmFNV;
      regName: 'FalloutNV'; appName: 'FNV'; exeName: 'FalloutNV.exe';
      appIDs: '22380,2028016'; bsaOptMode: 'fo'; ),
    ( longName: 'Fallout 3'; gameName: 'Fallout3'; gameMode: gmFO3;
      regName: 'Fallout3'; appName: 'FO3'; exeName: 'Fallout3.exe';
      appIDs: '22300,22370'; bsaOptMode: 'fo'; ),
    ( longName: 'Fallout 4'; gameName: 'Fallout4'; gameMode: gmFO4;
      regName: 'Fallout4'; appName: 'FO4'; exeName: 'Fallout4.exe';
      appIDs: '377160'; bsaOptMode: ''; ),
    ( longName: 'Skyrim Special Edition'; gameName: 'Skyrim'; gameMode: gmSSE;
      regName: 'Skyrim Special Edition'; appName: 'SSE';
      exeName: 'SkyrimSE.exe'; appIDs: '489830'; bsaOptMode: ''; )
  );

implementation

uses
  Graphics, SysUtils, Dialogs, ShellAPI, Windows, Registry;

{ TSettings }
constructor TSettings.Create;
begin
  // default settings
  newProfile := true;
  language := 'English';
  simpleDictionaryView := false;
  simplePluginsView := false;
  patchDirectory := wbDataPath;
  generalMessageColor := clGreen;
  loadMessageColor := clPurple;
  clientMessageColor := clBlue;
  patchMessageColor := $000080FF;
  pluginMessageColor := $00484848;
  errorMessageColor := clRed;
  logMessageTemplate := '[{{AppTime}}] ({{Group}}) {{Label}}: {{Text}}';
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
  path := PathList.Values['ProgramPath'] + 'profiles\' + name;
  if DirectoryExists(path) then
    DeleteToRecycleBin(path, false);
end;

procedure TProfile.Rename(name: string);
var
  oldProfilePath, newProfilePath: string;
begin
  // rename old profile folder if necessary
  oldProfilePath := PathList.Values['ProgramPath'] + 'profiles\' + self.name;
  newProfilePath := PathList.Values['ProgramPath'] + 'profiles\' + name;
  if DirectoryExists(oldProfilePath) then
    RenameFile(oldProfilePath, newProfilePath);

  // then change name in the object
  self.name := name;
end;

{ TProgramStatus }
constructor TProgramStatus.Create;
begin
  bInitException  := false;
  bLoadException := false;
  bChangeProfile := false;
  bForceTerminate := false;
  bUpdatePatchStatus := false;
  bClose := false;
  Version := GetVersionMem;
end;

{ CONFIGURATION HELPERS }

procedure DeleteTempPath;
begin
  DeleteDirectory(PathList.Values['TempPath']);
end;

procedure LoadLanguage;
const
  langFile = 'http://raw.githubusercontent.com/matortheeternal/smash/master/frontend/lang/english.lang';
  directions = 'Your english.lang file is missing.  Please download it from GitHub.  ' +
    'After you click OK, a webpage with the file will be opened.  Right-click the ' +
    'page and choose "Save page as", then save it as english.lang in the "lang\" ' +
    'folder where you have MatorSmash.exe installed.';
  accessMessage = 'It looks like Smash doesn''t have permission to read files ' +
    'from disk.  Try running the program as administrator or disabling your antivirus.';
var
  filename: string;
begin
  filename := Format('lang\%s.lang', [settings.language]);
  language := TStringList.Create;
  if (not FileExists(filename)) then begin
    if settings.language <> 'english' then begin
      settings.language := 'english';
      LoadLanguage;
    end
    else if not FileExists('MatorSmash.exe') then
      MessageDlg(accessMessage, mtConfirmation, [mbOk], 0)
    else begin
      MessageDlg(directions, mtConfirmation, [mbOk], 0);
      ForceDirectories(PathList.Values['ProgramPath'] + 'lang\');
      ShellExecute(0, 'open', PChar(langFile), '', '', SW_SHOWNORMAL);
    end;
  end
  else
    language.LoadFromFile(filename);
end;

function GetLanguageString(name: string): string;
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
  path := PathList.Values['ProgramPath'] + 'profiles\' + p.name + '\settings.ini';
  ForceDirectories(ExtractFilePath(path));

  // load settings if they exist, else create them
  if FileExists(path) then
    pSettings := LoadSettings(path)
  else begin
    pSettings := TSettings.Create;
    pSettings.patchDirectory := p.gamePath + 'Data\';
  end;

  // save profile details to settings
  pSettings.profile := p.name;
  pSettings.gameMode := p.gameMode;
  pSettings.gamePath := p.gamePath;
  SaveSettings(pSettings, path);
end;

procedure SaveSettings;
begin
  // user saving settings from options form
  settings.newProfile := false;
  TRttiIni.Save(PathList.Values['ProfilePath'] + 'settings.ini', settings);
end;

procedure SaveSettings(var s: TSettings; path: string);
begin
  TRttiIni.Save(path, s);
end;

procedure LoadSettings;
begin
  settings := TSettings.Create;
  TRttiIni.Load(PathList.Values['ProfilePath'] + 'settings.ini', settings);
end;

function LoadSettings(path: string): TSettings;
begin
  Result := TSettings.Create;
  TRttiIni.Load(path, Result);
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

initialization
begin
  ProgramStatus := TProgramStatus.Create;
  PathList := TStringList.Create;
end;

finalization
begin
  ProgramStatus.Free;
  PathList.Free;
end;

end.
