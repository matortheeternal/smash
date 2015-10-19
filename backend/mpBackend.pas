unit mpBackend;

interface

uses
  Windows, SysUtils, ShlObj, ShellApi, Classes, IniFiles, Dialogs, Masks,
  Controls, Registry, DateUtils, Graphics, ComCtrls, CommCtrl, RTTI,
  // zeosdbo components
  ZConnection, ZDataset, ZDbcCache, ZAbstractRODataset, ZDbcMySQL, ZSequence,
  ZDbcPostgreSQL, DB, ZSqlUpdate, ZDbcInterbase6, ZSqlMonitor, ZAbstractDataset,
  // abbrevia components
  AbBase, AbBrowse, AbZBrows, AbZipper, AbArcTyp,
  // superobject
  superobject,
  // mte components
  CRC32, RttiIni, mteHelpers, mteLogger, mteTracker;

type
  TTaskProcedures = class
  public
    class procedure SQLHeartbeat;
    class procedure RebuildDictionaries;
    class procedure RefreshBlacklist;
  end;
  TLogMessage = class (TObject)
  public
    time: string;
    group: string;
    &label: string;
    text: string;
    constructor Create(time, group, &label, text: string); Overload;
  end;
  TUserStatistics = class(TObject)
  public
    timesRun: integer;
    mergesBuilt: integer;
    pluginsChecked: integer;
    pluginsFixed: integer;
    pluginsMerged: integer;
    reportsSubmitted: integer;
  end;
  TUser = class(TObject)
  public
    ip: string;
    username: string;
    auth: string;
    firstSeen: TDateTime;
    lastSeen: TDateTime;
    timesSeen: integer;
    download: Int64;
    upload: Int64;
    timesRun: integer;
    mergesBuilt: integer;
    pluginsChecked: integer;
    pluginsFixed: integer;
    pluginsMerged: integer;
    reportsSubmitted: integer;
    constructor Create(ip: string); Overload;
    constructor Create(const fields: TFields); Overload;
    procedure UpdateStatistics(stats: TUserStatistics);
  end;
  TBlacklistEntry = class(TObject)
  public
    ip: string;
    username: string;
    created: TDateTime;
    expires: TDateTime;
    constructor Create(ip, username: string; duration: real); Overload;
    constructor Create(const fields: TFields); Overload;
    function IsExpired: boolean;
  end;
  TFilter = class(TObject)
  public
    group: string;
    &label: string;
    enabled: boolean;
    constructor Create(group: string; enabled: boolean); Overload;
    constructor Create(group, &label: string; enabled: boolean); Overload;
  end;
  TmpMessage = class(TObject)
  public
    id: integer;
    username: string;
    auth: string;
    data: string;
    constructor Create; Overload;
    constructor Create(id: integer; username, auth, data: string); Overload;
  end;
  TmpStatus = class(TObject)
  public
    programVersion: string;
    tes5Hash: string;
    tes4Hash: string;
    fnvHash: string;
    fo3Hash: string;
    procedure Refresh;
  end;
  TReport = class(TObject)
  public
    game: string;
    username: string;
    filename: string;
    hash: string;
    recordCount: integer;
    rating: integer;
    mergeVersion: string;
    notes: string;
    dateSubmitted: TDateTime;
    constructor Create(const fields: TFields); Overload;
  end;
  TEntry = class(TObject)
  public
    filename: string;
    hash: string;
    records: string;
    version: string;
    rating: string;
    reports: string;
    notes: string;
    constructor Create; Overload;
    constructor Create(const s: string); Overload;
    function ToText: string;
  end;
  TSettings = class(TObject)
  public
    [IniSection('SQL')]
    sqlUser: string;
    sqlPassword: string;
    sqlDatabase: string;
    sqlHost: string;
    sqlPort: string;
    [IniSection('General')]
    simpleLogView: boolean;
    simpleReportsView: boolean;
    simpleDictionaryView: boolean;
    serverMessageColor: TColor;
    initMessageColor: TColor;
    SQLMessageColor: TColor;
    dataMessageColor: TColor;
    taskMessageColor: TColor;
    errorMessageColor: TColor;
    [IniSection('Dictionary')]
    bSeparateHashes: boolean;
    bSeparateRecords: boolean;
    bSeparateVersions: boolean;
    templateHash: string;
    templateNoHash: string;
    constructor Create; Overload;
  end;
  TServerStatistics = class(TObject)
  public
    [IniSection('Statistics')]
    timesRun: integer;
    uniqueIPs: TStringList;
    dictionaryUpdates: integer;
    programUpdates: integer;
    reportsRecieved: integer;
    reportsApproved: integer;
    reportsDenied: integer;
    totalBandwidth: Int64;
    totalUptime: TDateTime;
    tes5Reports: integer;
    tes4Reports: integer;
    fnvReports: integer;
    fo3Reports: integer;
    tes5Logins: integer;
    tes4Logins: integer;
    fnvLogins: integer;
    fo3Logins: integer;
  end;

  { MySQL methods }
  procedure DBLogin(userID, password, database, host, port: string);
  procedure SQLQuery(query: string);
  function SanitizeForSQL(const s: string): string;
  //==USERS==
  procedure DBQueryUsers;
  function UserWhereClause(user: TUser): string;
  function UserSetClause(user: TUser): string;
  function UserValuesClause(user: TUser): string;
  procedure DBUpdateUser(SetClause, WhereClause: string); Overload;
  procedure DBAddUser(user: TUser); Overload;
  procedure DBRemoveUser(user: TUser);
  //==BLACKLIST==
  procedure DBQueryBlacklist;
  function BlacklistWhereClause(entry: TBlacklistEntry): string;
  function BlacklistSetClause(entry: TBlacklistEntry): string;
  function BlacklistValuesClause(entry: TBlacklistEntry): string;
  procedure DBUpdateBlacklist(SetClause, WhereClause: string);
  procedure DBAddBlacklist(entry: TBlacklistEntry);
  procedure DBRemoveBlacklist(entry: TBlacklistEntry);
  //==REPORTS==
  procedure DBQueryReports;
  function ReportWhereClause(report: TReport): string;
  function ReportSetClause(report: TReport): string;
  procedure DBUpdateReport(report: TReport; SetClause, WhereClause: string);
  procedure DBAddReport(report: TReport; table: string);
  procedure DBRemoveReport(report: TReport; table: string);
  { Data methods }
  procedure LoadSettings;
  procedure SaveSettings;
  procedure LoadStatistics;
  procedure SaveStatistics;
  function GetSessionUptime: TDateTime;
  { Dictionary methods }
  procedure EntryNotes(var sl: TStringList; var report: TReport);
  procedure RebuildDictionary(game: string; var lst: TList);
  procedure UpdateRebuildBooleans(report: TReport);
  function GetDictionary(name: string): string;
  function GetDictionaryHash(name: string): string;
  procedure LoadDictionary(var lst: TList; var sl: TStringList; filename: string);
  procedure LoadPluginBlacklist(var lst, dictionary: TList);
  function GetRatingColor(rating: real): integer;
  function GetEntry(var dictionary: TList; pluginName, numRecords, version: string): TEntry;
  function ReportExists(var lst: TList; var report: TReport): boolean;
  procedure RemoveExistingReports(var lst: TList; var report: TReport);
  function CompareReportsForBuild(P1, P2: Pointer): Integer;
  function CompareApprovedReports(P1, P2: Pointer): Integer;
  function CompareUnapprovedReports(P1, P2: Pointer): Integer;
  { Log methods }
  procedure InitLog;
  procedure RebuildLog;
  procedure SaveLog(var Log: TList);
  function MessageEnabled(msg: TLogMessage): boolean;
  { User methods }
  function Authorized(ip, username, auth: string): boolean;
  function ResetAuth(ip, username, auth: string): boolean;
  function UsernameAvailable(username: string): boolean;
  function GetUser(ip, username, auth: string): TUser; Overload;
  function GetUser(ip: string): TUser; Overload;
  function AddUser(ip: string): TUser; Overload;
  procedure UpdateUser(ip, username, auth: string); Overload;
  function IsBlacklisted(ip: string): boolean;
  function UserString(user: TUser): string;

const
  // COLUMNS IN SQL TABLES
  REPORT_COLUMNS = 'game,username,filename,hash,record_count,rating,'+
    'merge_version,notes,date_submitted';
  USER_COLUMNS = 'ip,username,auth,firstSeen,lastSeen,timesSeen,download,'+
    'upload,timesRun,mergesBuilt,pluginsChecked,pluginsMerged,reportsSubmitted';
  BLACKLIST_COLUMNS = 'ip,username,created,expires';

  // MAXIMUM DICTIONARY ENTRY NOTES LENGTH
  MAX_NOTES_LENGTH = 4096;

  // MSG IDs
  MSG_NOTIFY = 1;
  MSG_REGISTER = 2;
  MSG_AUTH_RESET = 3;
  MSG_STATISTICS = 4;
  MSG_STATUS = 5;
  MSG_REQUEST = 6;
  MSG_REPORT = 7;

  // MSG Strings
  MSG_STRINGS: array[1..7] of string = (
    'MSG_NOTIFY',
    'MSG_REGISTER',
    'MSG_AUTH_RESET',
    'MSG_STATISTICS',
    'MSG_STATUS',
    'MSG_REQUEST',
    'MSG_REPORT'
  );

var
  TES5Dictionary, TES4Dictionary, FO3Dictionary, FNVDictionary,
  ApprovedReports, UnapprovedReports, Users, Blacklist, BaseLog, Log,
  LabelFilters, GroupFilters: TList;
  slTES5Dictionary, slTES4Dictionary, slFO3Dictionary, slFNVDictionary,
  slConnectedIPs: TStringList;
  statistics: TServerStatistics;
  settings: TSettings;
  status: TmpStatus;
  LogPath, ProgramPath, ProgramVersion: string;
  bLoginSuccess, bRebuildTES5, bRebuildTES4, bRebuildFNV,
  bRebuildFO3, bApprovedAscending, bUnapprovedAscending: boolean;
  wbStartTime: TDateTime;
  sessionBandwidth: Int64;
  Connection: TZConnection;
  aApprovedColumnToSort, aUnapprovedColumnToSort: integer;

implementation

{******************************************************************************}
{ Task Procedures
  Procedures that are executed as regular tasks.
}
{******************************************************************************}

class procedure TTaskProcedures.SQLHeartbeat;
begin
  if Connection.Connected then
    SQLQuery('SELECT @@Version;');
end;

class procedure TTaskProcedures.RebuildDictionaries;
begin
  Logger.Write('DATA', 'Dictionary', 'Checking for dictionaries to rebuild');
  if bRebuildTES5 then RebuildDictionary('TES5', TES5Dictionary);
  if bRebuildTES4 then RebuildDictionary('TES4', TES4Dictionary);
  if bRebuildFNV then RebuildDictionary('FNV', FNVDictionary);
  if bRebuildFO3 then RebuildDictionary('FO3', FO3Dictionary);
  if not (bRebuildTES5 or bRebuildTES4 or bRebuildFNV or bRebuildFO3) then
    Logger.Write('DATA', 'Dictionary', 'No dictionaries need to be updated');
end;

class procedure TTaskProcedures.RefreshBlacklist;
var
  i: Integer;
  entry: TBlacklistEntry;
begin
  Logger.Write('DATA', 'Blacklist', 'Removing expired entries');
  for i := Pred(Blacklist.Count) downto 0 do begin
    entry := TBlacklistEntry(Blacklist[i]);
    if entry.IsExpired then begin
      DBRemoveBlacklist(entry);
      Blacklist.Remove(entry);
    end;
  end;
end;


{******************************************************************************}
{ SQL Methods
  Methods for interacting with the SQL database.

  List of methods:
  - DoLogin
  - SQLQuery
  ==USERS==
  - QueryUsers
  - UserWhereClause
  - UserSetClause
  - UserValuesClause
  - UpdateUser
  - AddUser
  - RemoveUser
  ==BLACKLIST==
  - QueryBlacklist
  - BlacklistWhereClause
  - BlacklistSetClause
  - BlacklistValuesClause
  - UpdateBlacklist
  - AddBlacklist
  - RemoveBlacklist
  ==REPORTS==
  - QueryReports
  - ReportWhereClause
  - ReportSetClause
  - ReportValuesClause
  - UpdateReport
  - AddReport
  - RemoveReport
}
{******************************************************************************}

{ Attempt to login to MySQL database }
procedure DBLogin(userID, password, database, host, port: string);
begin
  // attempt to connect to mysql
  bLoginSuccess := false;
  try
    Connection := TZConnection.Create(nil);
    Connection.User := userID;
    Connection.Port := StrToInt(port);
    Connection.Database := database;
    Connection.Password := password;
    Connection.HostName := host;
    Connection.Protocol := 'mysql';
    Connection.Connect;
    bLoginSuccess := Connection.Connected;
    if not bLoginSuccess then
      ShowMessage('Failed to connect to database.');
  except
    on x : Exception do begin
      ShowMessage('Failed to connect: '#13#10+x.Message);
      Connection.Free;
    end;
  end;
end;

procedure SQLQuery(query: string);
var
  ZQuery: TZQuery;
begin
  try
    Logger.Write('SQL', 'Query', query);
    ZQuery := TZQuery.Create(nil);
    ZQuery.Connection := Connection;
    ZQuery.Fields.Clear;
    ZQuery.SQL.Add(query);
    ZQuery.ExecSQL;
    ZQuery.Free;
  except
    on x : Exception do begin
      if x.Message = 'SQL Error: MySQL server has gone away' then begin
        Logger.Write('ERROR', 'SQL', 'MySQL server is unavailable');
        Connection.Disconnect;
        Connection.Connect;
        if Connection.Connected then
          SQLQuery(query);
      end
      else
        Logger.Write('ERROR', 'SQL', x.Message);
    end;
  end;
end;

function StringReplaceExt(const s: string; OldPattern, NewPattern: array of string;
  Flags: TReplaceFlags): string;
var
  i : integer;
begin
  Assert(Length(OldPattern) = (Length(NewPattern)));
  Result := S;
  for  i := Low(OldPattern) to High(OldPattern) do
    Result := StringReplace(Result, OldPattern[i], NewPattern[i], Flags);
end;

function SanitizeForSQL(const s: string): string;
begin
  Result := StringReplaceExt(s,
    ['\', #39, #34, #0, #10, #13, #26], ['\\','\'#39,'\'#34,'\0','\n','\r','\Z'] ,
    [rfReplaceAll]
  );
end;


//========================================
// USERS
//========================================

procedure DBQueryUsers;
var
  Dataset: TZQuery;
  user: TUser;
  count: integer;
begin
  Users := TList.Create;

  // get users
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+USER_COLUMNS+' FROM users');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into Users list
  count := 0;
  Dataset.First;
  while not Dataset.EOF do begin
    Inc(count);
    user := TUser.Create(Dataset.Fields);
    Users.Add(user);
    Dataset.Next;
  end;
  Logger.Write('SQL', 'Users', 'Loaded '+IntToStr(count)+' records');

  // clean up
  Dataset.Close;
  Dataset.Free;
end;

function UserWhereClause(user: TUser): string;
begin
  Result := 'WHERE '+
    'ip='''+user.ip+''' AND '+
    'username='''+user.username+'''';
end;

function UserSetClause(user: TUser): string;
begin
  Result := 'SET '+
    'ip='''+user.ip+''', '+
    'username='''+user.username+''', '+
    'auth='''+user.auth+''', '+
    'firstSeen='''+DateTimeToSQL(user.firstSeen)+''', '+
    'lastSeen='''+DateTimeToSQL(user.lastSeen)+''', '+
    'timesSeen='+IntToStr(user.timesSeen)+', '+
    'download='+IntToStr(user.download)+', '+
    'upload='+IntToStr(user.upload)+', '+
    'timesRun='+IntToStr(user.timesRun)+', '+
    'mergesBuilt='+IntToStr(user.mergesBuilt)+', '+
    'pluginsChecked='+IntToStr(user.pluginsChecked)+', '+
    'pluginsMerged='+IntToStr(user.pluginsMerged)+', '+
    'reportsSubmitted='+IntToStr(user.reportsSubmitted);
end;

function UserValuesClause(user: TUser): string;
begin
  Result := '('+USER_COLUMNS+') '+
    'VALUES ('''+
    user.ip+''','''+
    user.username+''','''+
    user.auth+''','''+
    DateTimeToSQL(user.firstSeen)+''','''+
    DateTimeToSQL(user.lastSeen)+''','+
    IntToStr(user.timesSeen)+','+
    IntToStr(user.download)+','+
    IntToStr(user.upload)+','+
    IntToStr(user.timesRun)+','+
    IntToStr(user.mergesBuilt)+','+
    IntToStr(user.pluginsChecked)+','+
    IntToStr(user.pluginsMerged)+','+
    IntToStr(user.reportsSubmitted)+')';
end;

procedure DBUpdateUser(SetClause, WhereClause: string);
var
  query: string;
begin
  Logger.Write('SQL', 'Users', 'Update '+WhereClause);
  query := 'UPDATE users '+SetClause+' '+WhereClause+';';
  SQLQuery(query);
end;

procedure DBAddUser(user: TUser);
var
  query, ValuesClause: string;
begin
  // execute SQL
  Logger.Write('SQL', 'Users', 'Add '+user.ip+' ('+user.username+')');
  ValuesClause := UserValuesClause(user);
  query := 'INSERT INTO users '+ValuesClause+';';
  SQLQuery(query);
end;

procedure DBRemoveUser(user: TUser);
var
  query, WhereClause: string;
begin
  // execute SQL
  WhereClause := UserWhereClause(user);
  Logger.Write('SQL', 'Users', 'Delete '+user.ip+' ('+user.username+')');
  query := 'DELETE FROM users '+WhereClause+';';
  SQLQuery(query);
end;

//========================================
// BLACKLIST
//========================================

procedure DBQueryBlacklist;
var
  Dataset: TZQuery;
  entry: TBlacklistEntry;
  count: integer;
begin
  Blacklist := TList.Create;

  // get blacklist
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+BLACKLIST_COLUMNS+' FROM blacklist');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into Blacklist
  Dataset.First;
  count := 0;
  while not Dataset.EOF do begin
    Inc(count);
    entry := TBlacklistEntry.Create(Dataset.Fields);
    Blacklist.Add(entry);
    Dataset.Next;
  end;
  Logger.Write('SQL', 'Blacklist', 'Loaded '+IntToStr(count)+' records');

  // clean up
  Dataset.Close;
  Dataset.Free;
end;

function BlacklistWhereClause(entry: TBlacklistEntry): string;
begin
  Result := 'WHERE '+
    'ip='''+entry.ip+''' AND '+
    'username='''+entry.username+'''';
end;

function BlacklistSetClause(entry: TBlacklistEntry): string;
begin
  Result := 'SET '+
    'ip='''+entry.ip+''', '+
    'username='''+entry.username+''', '+
    'created='''+DateTimeToSQL(entry.created)+''', '+
    'expires='''+DateTimeToSQL(entry.expires)+'''';
end;

function BlacklistValuesClause(entry: TBlacklistEntry): string;
begin
  Result := '('+BLACKLIST_COLUMNS+') '+
    'VALUES ('''+
    entry.ip+''','''+
    entry.username+''','''+
    DateTimeToSQL(entry.created)+''','''+
    DateTimeToSQL(entry.expires)+''')';
end;

procedure DBUpdateBlacklist(SetClause, WhereClause: string);
var
  query: string;
begin
  query := 'UPDATE blacklist '+SetClause+' '+WhereClause+';';
  Logger.Write('SQL', 'Blacklist', 'Update '+WhereClause);
  SQLQuery(query);
end;

procedure DBAddBlacklist(entry: TBlacklistEntry);
var
  query, ValuesClause: string;
begin
  // execute SQL
  ValuesClause := BlacklistValuesClause(entry);
  query := 'INSERT INTO blacklist '+ValuesClause+';';
  Logger.Write('SQL', 'Blacklist', 'Add '+entry.ip+' ('+entry.username+')');
  SQLQuery(query);
end;

procedure DBRemoveBlacklist(entry: TBlacklistEntry);
var
  query, WhereClause: string;
begin
  // execute SQL
  WhereClause := BlacklistWhereClause(entry);
  query := 'DELETE FROM blacklist '+WhereClause+';';
  Logger.Write('SQL', 'Blacklist', 'Delete '+entry.ip+' ('+entry.username+')');
  SQLQuery(query);
end;

//========================================
// REPORTS
//========================================

{ Query database for Approved and Unapproved reports }
procedure DBQueryReports;
var
  Dataset: TZQuery;
  report: TReport;
  count: integer;
begin
  // initialize lists
  ApprovedReports := TList.Create;
  UnapprovedReports := TList.Create;

  // get approved_reports
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+REPORT_COLUMNS+' FROM approved_reports');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into ApprovedReports list
  count := 0;
  Dataset.First;
  while not Dataset.EOF do begin
    Inc(count);
    report := TReport.Create(Dataset.Fields);
    ApprovedReports.Add(report);
    Dataset.Next;
  end;
  Logger.Write('SQL', 'approved_reports', 'Loaded '+IntToStr(count)+' records');
  Dataset.Close;

  // get unapproved_reports
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+REPORT_COLUMNS+' FROM unapproved_reports');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into UnapprovedReports list
  count := 0;
  Dataset.First;
  while not Dataset.EOF do begin
    Inc(count);
    report := TReport.Create(Dataset.Fields);
    UnapprovedReports.Add(report);
    Dataset.Next;
  end;
  Logger.Write('SQL', 'unapproved_reports', 'Loaded '+IntToStr(count)+' records');
  Dataset.Close;
  Dataset.Free;
end;

function ReportWhereClause(report: TReport): string;
begin
  Result := 'WHERE '+
    'game='''+report.game+''' AND '+
    'username='''+report.username+''' AND '+
    'filename='''+report.filename+''' AND '+
    'merge_version='''+report.mergeVersion+'''';
end;

function ReportSetClause(report: TReport): string;
begin
  Result := 'SET '+
    'game='''+report.game+''', '+
    'username='''+report.username+''', '+
    'filename='''+report.filename+''', '+
    'hash='''+report.hash+''', '+
    'record_count='+IntToStr(report.recordCount)+', '+
    'rating='+IntToStr(report.rating)+', '+
    'merge_version='''+report.mergeVersion+''', '+
    'notes='''+StringReplace(Trim(report.notes), #13#10, '@13', [rfReplaceAll])+'''';
end;

function ReportValuesClause(report: TReport): string;
begin
  Result := '('+REPORT_COLUMNS+') '+
    'VALUES ('''+
    report.game+''','''+
    report.username+''','''+
    report.filename+''','''+
    report.hash+''','+
    IntToStr(report.recordCount)+','+
    IntToStr(report.rating)+','''+
    report.mergeVersion+''','''+
    StringReplace(Trim(report.notes), #13#10, '@13', [rfReplaceAll])+''','''+
    DateTimeToSQL(report.dateSubmitted)+''')';
end;

procedure DBUpdateReport(report: TReport; SetClause, WhereClause: string);
var
  query: string;
begin
  query := 'UPDATE approved_reports '+SetClause+' '+WhereClause+';';
  Logger.Write('SQL', 'approved_reports', 'Update '+WhereClause);
  SQLQuery(query);
end;

procedure DBAddReport(report: TReport; table: string);
var
  query, ValuesClause: string;
begin
  // execute SQL
  ValuesClause := ReportValuesClause(report);
  query := 'INSERT INTO '+table+' '+ValuesClause+';';
  Logger.Write('SQL', table, 'Add '+report.game+', '+report.username+', '+report.filename);
  SQLQuery(query);
end;

procedure DBRemoveReport(report: TReport; table: string);
var
  query, WhereClause: string;
begin
  // execute SQL
  WhereClause := ReportWhereClause(report);
  query := 'DELETE FROM '+table+' '+WhereClause+';';
  Logger.Write('SQL', table, 'Delete '+report.game+', '+report.username+', '+report.filename);
  SQLQuery(query);
end;


{******************************************************************************}
{ Data methods
  Set of methods for working with data.

  List of methods:
  - LoadSettings
  - SaveSettings
  - LoadStatistics
  - SaveStatistics
  - LoadDictionary
  - GetRatingColor
  - GetRating
  - IsBlackListed
  - GetEntry
}
{******************************************************************************}

procedure LoadSettings;
begin
  settings := TSettings.Create;
  TRttiIni.Load('settings.ini', settings);
end;

procedure SaveSettings;
begin
  TRttiIni.Save('settings.ini', settings);
end;

procedure LoadStatistics;
begin
  statistics := TServerStatistics.Create;
  TRttiIni.Load('statistics.ini', statistics);
end;

procedure SaveStatistics;
begin
  TRttiIni.Save('statistics.ini', statistics);
end;

{ Returns the time the application has been running for }
function GetSessionUptime: TDateTime;
begin
  Result := Now - wbStartTime;
end;

procedure EntryNotes(var sl: TStringList; var report: TReport);
var
  header, notes: string;
  slHeader: TStringList;
begin
  // prepare header
  slHeader := TStringList.Create;
  slHeader.Values['user'] := report.username;
  slHeader.Values['hash'] := report.hash;
  slHeader.Values['records'] := IntToStr(report.recordCount);
  slHeader.Values['version'] := report.mergeVersion;
  slHeader.Values['rating'] := IntToStr(report.rating);
  slHeader.Values['date'] := DateToStr(report.dateSubmitted);
  if (report.hash <> '0') then
    header := ApplyTemplate(settings.templateHash, slHeader)
  else
    header := ApplyTemplate(settings.templateNoHash, slHeader);

  // add to entry notes if doing so won't exceed the maximum notes length
  notes := Report.notes + '@13';
  if (Length(header) + Length(notes) + Length(sl.Text) < MAX_NOTES_LENGTH) then begin
    sl.Add(header);
    sl.Add(notes);
  end;

  // clean up
  slHeader.Free;
end;

procedure SaveDictionary(game: string; var lst: TList);
var
  i: Integer;
  sl: TStringList;
  entry: TEntry;
begin
  sl := TStringList.Create;
  for i := 0 to Pred(lst.Count) do begin
    entry := TEntry(lst[i]);
    sl.Add(entry.ToText);
  end;
  sl.SaveToFile(game + 'Dictionary.txt');
  sl.Free;
end;

procedure RebuildDictionary(game: string; var lst: TList);
var
  i, n: integer;
  report: TReport;
  entry: TEntry;
  rating: real;
  sl: TStringList;
  bFilenameMatch, bHashMatch, bRecordsMatch, bVersionMatch: boolean;
begin
  Logger.Write('DATA', 'Dictionary', 'Building '+game+' Dictionary');
  // sort reports so we can build dictionary entries faster
  bApprovedAscending := false;
  aApprovedColumnToSort := 1;
  ApprovedReports.Sort(CompareReportsForBuild);

  // prepare to make new dictionary file
  lst.Clear;
  sl := TStringList.Create;
  rating := 0;
  n := 0;
  bFilenameMatch := false;
  bHashMatch := false;
  bRecordsMatch := false;
  bVersionMatch := false;
  entry := nil;

  // loop through approved reports
  for i := 0 to Pred(ApprovedReports.Count) do begin
    report := TReport(ApprovedReports[i]);
    // skip approved reports not for the game we're making the dictionary for
    if report.game <> game then
      continue;

    // process reports separately based on dictionary consolidation settings
    if Assigned(entry) then begin
      bFilenameMatch := SameText(entry.filename, report.filename);
      bHashMatch := (not settings.bSeparateHashes) or SameText(entry.hash, report.hash);
      bRecordsMatch := (not settings.bSeparateRecords) or (StrToInt(entry.records) = report.recordCount);
      bVersionMatch := (not settings.bSeparateVersions) or SameText(entry.version, report.mergeVersion);
    end;

    // thanks to sorting, we can make a single dictionary entry at a time
    if (bFilenameMatch and bHashMatch and bRecordsMatch and bVersionMatch) then begin
      Inc(n);
      rating := rating + report.rating;
      EntryNotes(sl, report);
    end
    else begin
      // add built entry to dictionary if it exists
      if (entry <> nil) then begin
        entry.rating := FormatFloat('0.0#', (rating / (n * 1.0)));
        entry.reports := IntToStr(n);
        entry.notes := StringReplace(sl.Text, #13#10, '@13', [rfReplaceAll]);
        sl.Clear;
        lst.Add(entry);
      end;
      // prepare new entry
      entry := TEntry.Create;
      entry.filename := report.filename;
      entry.hash := report.hash;
      entry.records := IntToStr(report.recordCount);
      entry.version := report.mergeVersion;
      rating := report.rating;
      EntryNotes(sl, report);
      n := 1;
    end;
  end;

  // add last built entry to dictionary if it exists
  if (entry <> nil) then begin
    entry.rating := FormatFloat('0.0#', (rating / (n * 1.0)));
    entry.reports := IntToStr(n);
    entry.notes := StringReplace(sl.Text, #13#10, '@13', [rfReplaceAll]);
    sl.Clear;
    lst.Add(entry);
  end;

  // clean up
  sl.Free;
  // save dictionary
  SaveDictionary(game, lst);
  // refresh status
  status.Refresh;
end;

procedure UpdateRebuildBooleans(report: TReport);
begin
  if report.game = 'TES5' then bRebuildTES5 := true;
  if report.game = 'TES4' then bRebuildTES4 := true;
  if report.game = 'FNV' then bRebuildFNV := true;
  if report.game = 'FO3' then bRebuildFO3 := true;
end;

function GetDictionary(name: string): string;
begin
  if name = 'TES5Dictionary.txt' then
    Result := slTES5Dictionary.Text
  else if name = 'TES4Dictionary.txt' then
    Result := slTES4Dictionary.Text
  else if name = 'FNVDictionary.txt' then
    Result := slFNVDictionary.Text
  else if name = 'FO3Dictionary.txt' then
    Result := slFO3Dictionary.Text;
end;

function GetDictionaryHash(name: string): string;
begin
  if name = 'TES5Dictionary.txt' then
    Result := status.tes5hash
  else if name = 'TES4Dictionary.txt' then
    Result := status.tes4hash
  else if name = 'FNVDictionary.txt' then
    Result := status.fnvhash
  else if name = 'FO3Dictionary.txt' then
    Result := status.fo3hash;
end;

procedure LoadDictionary(var lst: TList; var sl: TStringList; filename: string);
var
  i: Integer;
  entry: TEntry;
begin
  // don't attempt to load dictionary if it doesn't exist
  if not FileExists(filename) then begin
    Logger.Write('ERROR', 'Init', 'No dictionary file '+filename);
    exit;
  end;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile(filename);

  // load dictionary file into entry object
  for i := 0 to Pred(sl.Count) do begin
    entry := TEntry.Create(sl[i]);
    lst.Add(entry);
  end;
end;

procedure LoadPluginBlacklist(var lst, dictionary: TList);
var
  i: Integer;
  entry: TEntry;
begin
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if entry.rating = '-1' then
      lst.Add(entry);
  end;
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

function GetEntry(var dictionary: TList; pluginName, numRecords, version: string): TEntry;
var
  i: Integer;
  entry: TEntry;
begin
  Result := TEntry.Create;
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if entry.filename = pluginName then begin
      Result := entry;
      exit;
    end;
  end;
end;

procedure InitLog;
begin
  BaseLog := TList.Create;
  Log := TList.Create;
  LabelFilters := TList.Create;
  GroupFilters := TList.Create;
  // INITIALIZE GROUP FILTERS
  GroupFilters.Add(TFilter.Create('INIT', true));
  GroupFilters.Add(TFilter.Create('SQL', true));
  GroupFilters.Add(TFilter.Create('SERVER', true));
  GroupFilters.Add(TFilter.Create('DATA', true));
  GroupFilters.Add(TFilter.Create('TASK', true));
  GroupFilters.Add(TFilter.Create('ERROR', true));
  // INITIALIZE LABEL FILTERS
  LabelFilters.Add(TFilter.Create('INIT', 'Dictionary', true));
  LabelFilters.Add(TFilter.Create('INIT', 'Status', true));
  LabelFilters.Add(TFilter.Create('INIT', 'Log', true));
  LabelFilters.Add(TFilter.Create('SQL', 'Blacklist', true));
  LabelFilters.Add(TFilter.Create('SQL', 'Users', true));
  LabelFilters.Add(TFilter.Create('SQL', 'approved_reports', true));
  LabelFilters.Add(TFilter.Create('SQL', 'unapproved_reports', true));
  LabelFilters.Add(TFilter.Create('SQL', 'Query', false));
  LabelFilters.Add(TFilter.Create('SERVER', 'Connected', true));
  LabelFilters.Add(TFilter.Create('SERVER', 'Message', true));
  LabelFilters.Add(TFilter.Create('SERVER', 'Response', true));
  LabelFilters.Add(TFilter.Create('SERVER', 'Disconnected', true));
  LabelFilters.Add(TFilter.Create('SERVER', 'Terminated', true));
  LabelFilters.Add(TFilter.Create('TASK', 'Init', true));
  LabelFilters.Add(TFilter.Create('TASK', 'Execute', false));
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
    if filter.&label = msg.&label then begin
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

function CompareReportsForBuild(P1, P2: Pointer): Integer;
var
  report1, report2: TReport;
begin
  report1 := TReport(P1);
  report2 := TReport(P2);

  Result := AnsiCompareText(report1.filename, report2.filename);
  if Result = 0 then
    Result := AnsiCompareText(report2.mergeVersion, report1.mergeVersion);
  if Result = 0 then
    Result := report2.recordCount - report1.recordCount;
end;

function CompareApprovedReports(P1, P2: Pointer): Integer;
var
  report1, report2: TReport;
begin
  Result := 0;
  report1 := TReport(P1);
  report2 := TReport(P2);

  if aApprovedColumnToSort = 0 then
    Result := AnsiCompareText(report1.game, report2.game)
  else if aApprovedColumnToSort = 1 then
    Result := AnsiCompareText(report1.filename, report2.filename)
  else if aApprovedColumnToSort = 2 then
    Result := Trunc(report1.dateSubmitted) - Trunc(report2.dateSubmitted)
  else if aApprovedColumnToSort = 3 then
    Result := AnsiCompareText(report1.username, report2.username)
  else if aApprovedColumnToSort = 4 then
    Result := report1.rating - report2.rating;

  if bApprovedAscending then
    Result := -Result;
end;

function CompareUnapprovedReports(P1, P2: Pointer): Integer;
var
  report1, report2: TReport;
begin
  Result := 0;
  report1 := TReport(P1);
  report2 := TReport(P2);

  if aUnapprovedColumnToSort = 0 then
    Result := AnsiCompareText(report1.game, report2.game)
  else if aUnapprovedColumnToSort = 1 then
    Result := AnsiCompareText(report1.filename, report2.filename)
  else if aUnapprovedColumnToSort = 2 then
    Result := Trunc(report1.dateSubmitted) - Trunc(report2.dateSubmitted)
  else if aUnapprovedColumnToSort = 3 then
    Result := AnsiCompareText(report1.username, report2.username)
  else if aUnapprovedColumnToSort = 4 then
    Result := report1.rating - report2.rating;

  if bUnapprovedAscending then
    Result := -Result;
end;

function ReportExists(var lst: TList; var report: TReport): boolean;
var
  i: Integer;
  r: TReport;
begin
  Result := false;
  for i := 0 to Pred(lst.Count) do begin
    r := TReport(lst[i]);
    Result := (r.game = report.game) and (r.username = report.username) and
      (r.filename = report.filename);
    if Result then break;
  end;
end;

procedure RemoveExistingReports(var lst: TList; var report: TReport);
var
  i: Integer;
  r: TReport;
begin
  for i := Pred(lst.Count) downto 0 do begin
    r := TReport(lst[i]);
    if (r.game = report.game) and (r.username = report.username) and
      (r.filename = report.filename) then lst.Remove(r);
  end;
end;

{******************************************************************************}
{ User methods
  Set of methods for handling users.

  List of method:
  - Authorized
  - ResetAuth
  - GetUser
  - GetUser
}
{******************************************************************************}


function Authorized(ip, username, auth: string): boolean;
var
  i: Integer;
  user: TUser;
  WhereClause, SetClause: string;
begin
  Result := false;
  for i := 0 to Pred(Users.Count) do begin
    user := TUser(Users[i]);
    if SameText(user.username, username) then begin
      Result := SameText(user.auth, auth);
      if Result and not SameText(user.ip, ip) then begin
        WhereClause := UserWhereClause(user);
        user.ip := ip;
        SetClause := UserSetClause(user);
        DBUpdateUser(SetClause, WhereClause);
      end;
      exit;
    end;
  end;
end;

function ResetAuth(ip, username, auth: string): boolean;
var
  i: Integer;
  user: TUser;
  SetClause, WhereClause: string;
begin
  Result := false;
  for i := 0 to Pred(Users.Count) do begin
    user := TUser(Users[i]);
    if SameText(user.username, username) then begin
      Result := SameText(user.ip, ip);
      if Result then begin
        WhereClause := UserWhereClause(user);
        user.auth := auth;
        SetClause := UserSetClause(user);
        DBUpdateUser(SetClause, WhereClause);
      end;
      exit;
    end;
  end;
end;

function UsernameAvailable(username: string): boolean;
var
  i: Integer;
  user: TUser;
begin
  // assume false
  Result := false;

  // check if username is too short or too long
  if (Length(username) < 4) or (Length(username) > 24) then
    exit;

  // check if username is already taken
  for i := 0 to Pred(Users.Count) do begin
    user := TUser(Users[i]);
    if SameText(user.username, username) then
      exit;
  end;

  // all checks passed, true
  Result := true;
end;

function GetUser(ip, username, auth: string): TUser;
var
  i: Integer;
  user: TUser;
begin
  Result := nil;
  for i := 0 to Pred(Users.Count) do begin
    user := TUser(Users[i]);
    if SameText(user.username, username) then begin
      if SameText(user.auth, auth) and SameText(user.ip, ip) then
        Result := user;
      exit; // exit if user auth or ip differs with stored values
    end;
  end;
end;

function GetUser(ip: string): TUser;
var
  i: Integer;
  user: TUser;
begin
  Result := nil;
  for i := 0 to Pred(Users.Count) do begin
    user := TUser(Users[i]);
    if SameText(user.ip, ip) then begin
      Result := user;
      exit;
    end;
  end;
end;

function AddUser(ip: string): TUser;
var
  user: TUser;
begin
  user := TUser.Create(ip);
  Users.Add(user);
  DBAddUser(user);
  Result := user;
end;

procedure UpdateUser(ip, username, auth: string);
var
  WhereClause, SetClause: string;
  user: TUser;
begin
  user := GetUser(ip);
  WhereClause := UserWhereClause(user);
  user.username := username;
  user.auth := auth;
  SetClause := UserSetClause(user);
  DBUpdateUser(SetClause, WhereClause);
end;

function IsBlacklisted(ip: string): boolean;
var
  i: Integer;
  entry: TBlacklistEntry;
begin
  Result := false;
  for i := 0 to Pred(Blacklist.Count) do begin
    entry := TBlacklistEntry(Blacklist[i]);
    if SameText(ip, entry.ip) then begin
      Result := true;
      exit;
    end;
  end;
end;

function UserString(user: TUser): string;
begin
  Result := user.ip;
  if user.username <> '' then
    Result := Result + ' ('+user.username+')';
end;


{******************************************************************************}
{ Object methods
  Set of methods for various general objects

  List of methods:
  - TLogMessage.Create
  - TBlacklistEntry.Create
  - TBlacklistEntry.Create
  - TUser.Create
  - TUser.Dump
  - TUser.LoadDump
  - TmpMessage.Create
  - TReport.Create
  - TReport.Create
  - TEntry.Create
  - TSettings.Create
  - TSettings.Save
  - TSettings.Load
  - TStatistics.Create
  - TStatistics.Save
  - TStatistics.Load
}
{******************************************************************************}

constructor TLogMessage.Create(time, group, &label, text: string);
begin
  self.time := time;
  self.group := group;
  self.&label := &label;
  self.text := text;
end;

{ TBlacklistEntry }
constructor TBlacklistEntry.Create(ip, username: string; duration: real);
begin
  self.ip := ip;
  self.username := username;
  created := Now;
  expires := created + duration;
end;

constructor TBlacklistEntry.Create(const fields: TFields);
begin
  ip := fields[0].AsString;
  username := fields[1].AsString;
  created := fields[2].AsDateTime;
  expires := fields[3].AsDateTime;
end;

function TBlacklistEntry.IsExpired: boolean;
begin
  Result := Now > expires;
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

{ TUser }
constructor TUser.Create(ip: string);
begin
  self.ip := ip;
  firstSeen := Now;
  lastSeen := Now;
end;

constructor TUser.Create(const fields: TFields);
begin
  ip := fields[0].AsString;
  username := fields[1].AsString;
  auth := fields[2].AsString;
  firstSeen := fields[3].AsDateTime;
  lastSeen := fields[4].AsDateTime;
  timesSeen := fields[5].AsInteger;
  download := fields[6].AsLargeInt;
  upload := fields[7].AsLargeInt;
  timesRun := fields[8].AsInteger;
  mergesBuilt := fields[9].AsInteger;
  pluginsChecked := fields[10].AsInteger;
  pluginsMerged := fields[11].AsInteger;
  reportsSubmitted := fields[12].AsInteger;
end;

procedure TUser.UpdateStatistics(stats: TUserStatistics);
begin
  timesRun := timesRun + stats.timesRun;
  mergesBuilt := mergesBuilt + stats.mergesBuilt;
  pluginsChecked := pluginsChecked + stats.pluginsChecked;
  pluginsMerged := pluginsMerged + stats.pluginsMerged;
end;

{ TmpMessage }
constructor TmpMessage.Create;
begin
  id := 0;
end;

constructor TmpMessage.Create(id: integer; username, auth, data: string);
begin
  self.id := id;
  self.username := username;
  self.auth := auth;
  self.data := data;
end;

{ TmpStatus }
procedure TmpStatus.Refresh;
var
  NewVersion: string;
  Zipper: TAbZipper;
begin
  if FileExists('MergePlugins.exe') then begin
    NewVersion := FileVersion('MergePlugins.exe');
    if (ProgramVersion <> NewVersion) then begin
      Zipper := TAbZipper.Create(nil);
      Zipper.AutoSave := true;
      Zipper.FileName := 'MergePlugins.zip';
      Zipper.StoreOptions := [soStripDrive, soRemoveDots, soReplace];
      Zipper.AddFiles('MergePlugins.exe', 0);
      Zipper.AddFiles('lang\*.lang', 0);
      Zipper.AddFiles('doc\*.*', 0);
      ProgramVersion := NewVersion;
      Logger.Write('INIT', 'Status', 'Client Version: '+ProgramVersion);
      Zipper.Free;
    end;
  end;
  if FileExists('TES5Dictionary.txt') then begin
    NewVersion := GetCRC32('TES5Dictionary.txt');
    if (TES5Hash <> NewVersion) then begin
      TES5Hash := NewVersion;
      Logger.Write('INIT', 'Status', 'TES5Dictionary Hash: '+TES5Hash);
    end;
  end;
  if FileExists('TES4Dictionary.txt') then begin
    NewVersion := GetCRC32('TES4Dictionary.txt');
    if (TES4Hash <> NewVersion) then begin
      TES4Hash := NewVersion;
      Logger.Write('INIT', 'Status', 'TES4Dictionary Hash: '+TES4Hash);
    end;
  end;
  if FileExists('FNVDictionary.txt') then begin
    NewVersion := GetCRC32('FNVDictionary.txt');
    if (FNVHash <> NewVersion) then begin
      FNVHash := NewVersion;
      Logger.Write('INIT', 'Status', 'FNVDictionary Hash: '+FNVHash);
    end;
  end;
  if FileExists('FO3Dictionary.txt') then begin
    NewVersion := GetCRC32('FO3Dictionary.txt');
    if (FO3Hash <> NewVersion) then begin
      FO3Hash := NewVersion;
      Logger.Write('INIT', 'Status', 'FO3Dictionary Hash: '+FO3Hash);
    end;
  end;
end;

{ TReport }
constructor TReport.Create(const fields: TFields);
var
  s: string;
begin
  game := fields[0].AsString;
  username := fields[1].AsString;
  filename := fields[2].AsString;
  hash := fields[3].AsString;
  recordCount := fields[4].AsInteger;
  rating := fields[5].AsInteger;
  mergeVersion := fields[6].AsString;
  s := StringReplace(fields[7].AsString, '@13', #13#10, [rfReplaceAll]);
  notes := Wordwrap(s, 70);
  dateSubmitted := fields[8].AsDateTime;
end;

{ TEntry }
constructor TEntry.Create;
begin
  reports := '0';
  rating := 'No rating';
end;

constructor TEntry.Create(const s: string);
var
  i, lastIndex, ct: Integer;
begin
  lastIndex := 1;
  ct := 0;
  for i := 1 to Length(s) do begin
    if s[i] = ';' then begin
      if ct = 0 then
        filename := Copy(s, lastIndex, i - lastIndex)
      else if ct = 1 then
        records := Copy(s, lastIndex, i - lastIndex)
      else if ct = 2 then
        version := Copy(s, lastIndex, i - lastIndex)
      else if ct = 3 then
        rating := Copy(s, lastIndex, i - lastIndex)
      else if ct = 4 then begin
        reports := Copy(s, lastIndex, i - lastIndex);
        notes := Copy(s, i + 1, Length(s));
      end;
      LastIndex := i + 1;
      Inc(ct);
    end;
  end;
end;

function TEntry.ToText: string;
begin
  Result := filename + ';' + records + ';' + version + ';' + rating + ';' +
    reports + ';' + notes;
end;

{ TSettings }
constructor TSettings.Create;
begin
  serverMessageColor := clBlue;
  initMessageColor := clGreen;
  SQLMessageColor := clPurple;
  dataMessageColor := $000080FF;
  taskMessageColor := clBlack;
  errorMessageColor := clRed;
end;

end.
