unit msClient;

interface

uses
  Windows, Classes, SysUtils,
  // indy components
  IdTCPClient, IdStack, IdGlobal;

type
  // CLIENT OBJECTS
  TmsMessageIDs = ( MSG_UNKNOWN, MSG_NOTIFY, MSG_REGISTER, MSG_AUTH_RESET,
    MSG_STATISTICS, MSG_STATUS, MSG_REQUEST, MSG_REPORT );
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
  procedure CompareStatuses;
  function UpdateDictionary: boolean;
  function UpdateProgram: boolean;
  function DownloadProgram: boolean;
  function UpdateChangeLog: boolean;

const
  // DELAYS
  StatusDelay = 2.0 / (60.0 * 24.0); // 2 minutes
  MaxConnectionAttempts = 3;

var
  TCPClient: TidTCPClient;
  LocalStatus, RemoteStatus: TmsStatus;
  LastStatusTime: TDateTime;
  ConnectionAttempts: Integer;

implementation

uses
  Dialogs,
  // abbrevia components
  AbZBrows, AbUnZper, AbArcTyp, AbMeter, AbBrowse, AbBase,
  // mte units
  CRC32, mteHelpers, mteLogger, mteTracker, RttiJson,
  // mp units
  msConfiguration, msCore,
  // xedit units
  wbInterface;

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
    TES5Hash := FileCRC32('TES5Dictionary.txt');
  if FileExists('TES4Dictionary.txt') then
    TES4Hash := FileCRC32('TES4Dictionary.txt');
  if FileExists('FNVDictionary.txt') then
    FNVHash := FileCRC32('FNVDictionary.txt');
  if FileExists('FO3Dictionary.txt') then
    FO3Hash := FileCRC32('FO3Dictionary.txt');

  // log messages
  Logger.Write('GENERAL', 'Status', 'ProgramVersion: '+ProgramVersion);
  case CurrentProfile.gameMode of
    1: Logger.Write('GENERAL', 'Status', 'TES5 Dictionary Hash: '+TES5Hash);
    2: Logger.Write('GENERAL', 'Status', 'TES4 Dictionary Hash: '+TES4Hash);
    3: Logger.Write('GENERAL', 'Status', 'FO3 Dictionary Hash: '+FNVHash);
    4: Logger.Write('GENERAL', 'Status', 'FNV Dictionary Hash: '+FO3Hash);
  end;
end;


{******************************************************************************}
{ Client methods
  Set of methods for communicating with the Mator Smash server.
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
  if ProgramStatus.bOfflineMode
  or ProgramStatus.bConnecting
  or TCPClient.Connected
  or (ConnectionAttempts >= MaxConnectionAttempts) then
    exit;

  ProgramStatus.bConnecting := true;
  try
    Logger.Write('CLIENT', 'Status', 'Attempting to connect to '+TCPClient.Host+':'+IntToStr(TCPClient.Port));
    TCPClient.Connect;
    Logger.Write('CLIENT', 'Status', 'Connection successful!');
    CheckAuthorization;
    SendGameMode;
    GetStatus;
    CompareStatuses;
    //SendPendingReports;
  except
    on x: Exception do begin
      Logger.Write('ERROR', 'Status', 'Connection failed.');
      Inc(ConnectionAttempts);
      if ConnectionAttempts = MaxConnectionAttempts then
        Logger.Write('CLIENT', 'Status', 'Maximum connection attempts reached.  '+
          'Click the disconnected icon in the status bar to retry.');
    end;
  end;
  ProgramStatus.bConnecting := false;
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
    msg := TmsMessage.Create(Ord(MSG_NOTIFY), settings.username, settings.key, 'Authorized?');
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
  ProgramStatus.bAuthorized := Result;
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
    msg := TmsMessage.Create(Ord(MSG_NOTIFY), settings.username, settings.key, wbAppName);
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
    msg := TmsMessage.Create(Ord(MSG_STATISTICS), settings.username, settings.key, TRttiJson.ToJson(sessionStatistics));
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
    msg := TmsMessage.Create(Ord(MSG_AUTH_RESET), settings.username, settings.key, '');
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
    msg := TmsMessage.Create(Ord(MSG_REGISTER), username, settings.key, 'Check');
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
    msg := TmsMessage.Create(Ord(MSG_REGISTER), username, settings.key, 'Register');
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
    msg := TmsMessage.Create(Ord(MSG_STATUS), settings.username, settings.key, '');
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

procedure CompareStatuses;
begin
  if not Assigned(RemoteStatus) then
    exit;

  // handle program update
  ProgramStatus.bProgramUpdate := VersionCompare(LocalStatus.programVersion,
    RemoteStatus.programVersion);

  // handle dictionary update based on gamemode
  case wbGameMode of
    gmTES5: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.tes5Hash <> RemoteStatus.tes5Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.tes5Hash+' != '+RemoteStatus.tes5hash);
    end;
    gmTES4: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.tes4Hash <> RemoteStatus.tes4Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.tes4Hash+' != '+RemoteStatus.tes4hash);
    end;
    gmFNV: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.fnvHash <> RemoteStatus.fnvHash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.fnvHash+' != '+RemoteStatus.fnvhash);
    end;
    gmFO3: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.fo3Hash <> RemoteStatus.fo3Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.fo3Hash+' != '+RemoteStatus.fo3hash);
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

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, filename);
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create(filename, fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load dictionary from response
    Logger.Write('CLIENT', 'Update', filename+' recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    //LoadDictionary;
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
  if not FileExists('MatorSmash.zip') then
    exit;

  // rename program
  if FileExists('MatorSmash.exe.bak') then
    DeleteFile('MatorSmash.exe.bak');
  RenameFile('MatorSmash.exe', 'MatorSmash.exe.bak');

  // Create an instance of the TZipForge class
  archive := TAbUnZipper.Create(nil);
  try
    with archive do begin
      // The name of the ZIP file to unzip
      FileName := PathList.Values['ProgramPath'] + 'MatorSmash.zip';
      // Set base (default) directory for all archive operations
      BaseDirectory := PathList.Values['ProgramPath'];
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
  DeleteFile('MatorSmash.zip');
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
  filename := 'MatorSmash.zip';
  if FileExists(filename) then begin
    MessageDlg(GetLanguageString('mpOpt_PendingUpdate'), mtInformation, [mbOk], 0);
    Result := true;
    exit;
  end;

  Logger.Write('CLIENT', 'Update', 'Mator Smash v'+RemoteStatus.programVersion);
  Tracker.Write('Updating program to v'+RemoteStatus.programVersion);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, 'Program');
    SendClientMessage(msg);

    // get response
    Logger.Write('CLIENT', 'Update', 'Downloading '+filename);
    stream := TFileStream.Create('MatorSmash.zip', fmCreate + fmShareDenyNone);
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

function UpdateChangeLog: boolean;
var
  msg: TmsMessage;
  stream: TFileStream;
begin
  Result := false;
  // don't update changelog if we've updated it within a day
  if FileExists('changelog.txt') then begin
    if Now - GetLastModified('changelog.txt') < 1.0 then
      exit;
  end;

  // exit if we aren't connected to the server
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Update', 'Getting changelog');
  Tracker.Write('Getting changelog');

  // attempt to request changelog
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmsMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, 'Changelog');
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


end.

