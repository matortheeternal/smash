unit mteHelpers;

interface

uses
  Windows, Forms, Classes, ComCtrls, Grids, StdCtrls, Types;

type
  TCallback = procedure of object;
  TAppHelpers = class
    class procedure GetHelp(var Msg: TMsg; var Handled: Boolean);
    class function HandleHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
  end;

  { General functions }
  function ShortenVersion(vs: string; numClauses: Integer): string;
  function IfThenInt(AValue: boolean; ATrue: Integer = 1; AFalse: Integer = 0): Integer;
  function TitleCase(sText: String): String;
  function SentenceCase(sText: string): string;
  function csvText(s: string): string;
  function CopyFromTo(str: string; first, last: Integer): string;
  function GetTextIn(str: string; open, close: char): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function DateTimeToSQL(date: TDateTime): string;
  function SQLToDateTime(date: string): TDateTime;
  function RateStr(date: TDateTime): string;
  function TimeStr(date: TDateTime): string;
  function AppendIfMissing(str, substr: string): string;
  function StrEndsWith(s1, s2: string): boolean;
  function RemoveFromEnd(s1, s2: string): string;
  function IntegerListSum(list: TStringList; maxIndex: integer): integer;
  function Wordwrap(s: string; charCount: integer): string;
  function ExtractPath(path: string; levels: integer): string;
  function ContainsMatch(var sl: TStringList; const s: string): boolean;
  function IsURL(s: string): boolean;
  function IsDotFile(fn: string): boolean;
  procedure SaveStringToFile(s: string; fn: string);
  function ApplyTemplate(const template: string; var map: TStringList): string;
  function VersionCompare(v1, v2: string): boolean;
  procedure TryToFree(obj: TObject);
  procedure FreeList(var lst: TList);
  { Windows API functions }
  procedure ForceForeground(hWnd: THandle);
  function GetDriveList: TStringDynArray;
  function DOSDrive(const sDrive: String ): Integer;
  function DriveReady(const sDrive: String): Boolean;
  function TryRegistryKeys(var keys: TStringList): string;
  function FileNameValid(filename: string): boolean;
  function DirectoryValid(dir: string): boolean;
  function UpDirectory(sPath: string): string;
  function DeleteToRecycleBin(const path: string; Confirm: Boolean): Boolean;
  procedure ExecNewProcess(ProgramName: string; synchronous: Boolean);
  procedure BrowseForFile(var ed: TEdit; filter, initDir: string);
  procedure BrowseForFolder(var ed: TEdit; initDir: string);
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function GetFileSize(const aFilename: String): Int64;
  function GetLastModified(const aFileName: String): TDateTime;
  function SearchPathsForFile(sPaths, sFileName: string): string;
  function MultFileSearch(paths, filenames, ignore: array of string;
    maxDepth: integer): string;
  function RecursiveFileSearch(aPath: string; filenames, ignore: array of string;
   maxDepth: integer): string;
  procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
  procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
  procedure CopyFiles(src, dst: string; var list: TStringList);
  function GetVersionMem: string;
  function FileVersion(const FileName: string): String;
  procedure DeleteDirectory(const path: string);
  procedure PerformFileSystemTests(sBasePath: string);
  { GUI Helper Functions }
  procedure StringGrid_CorrectWidth(var sg: TStringGrid);
  procedure ListView_CorrectWidth(var lv: TListView);
  function ListView_NextMatch(ListView: TListView; sSearch: string;
    iIndex: Integer): Integer;
  procedure ListView_HandleMatch(ListView: TListView; iFoundIndex: Integer;
    var sBuffer: string; sTempBuffer: string);

const
  wndBorderSide = 8;
  wndBorderTop = 30;

  // TIME TRACKING
  days = 1.0;
  hours = 1.0 / 24.0;
  minutes = hours / 60.0;
  seconds = minutes / 60.0;

var
  bAllowHelp: boolean;

implementation

uses
  SysUtils, Controls, Masks, Dialogs, StrUtils, FileCtrl, ShellApi,
  Messages, CommCtrl, DateUtils, shlObj, IOUtils, Registry;


{******************************************************************************}
{ Application Helpers
  General helpers for applications
}
{******************************************************************************}

class procedure TAppHelpers.GetHelp(var Msg: TMsg; var Handled: Boolean);
var
  control: TControl;
  sKeyword: string;
begin
  if (Msg.message = WM_KEYDOWN) and (LoWord(Msg.wParam) = VK_F1) then begin
    Screen.Cursor := crHelp;
    Handled := true;
  end
  else if (Msg.message = WM_LBUTTONDOWN) and (Screen.Cursor = crHelp) then begin
    // get control the user clicked on
    control := FindVCLWindow(Mouse.CursorPos);
    // if we found a control, jump to help keyword for that control
    if Assigned(control) then begin
      bAllowHelp := true;
      sKeyword := control.HelpKeyword;
      while (sKeyword = '') and Assigned(control.Parent) do begin
        control := control.Parent;
        sKeyword := control.HelpKeyword;
      end;
      Application.HelpKeyword(sKeyword);
      Screen.Cursor := crDefault;
      Handled := true;
    end;
  end;
end;

class function TAppHelpers.HandleHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := bAllowHelp;
  bAllowHelp := false;
  Result := true;
end;


{******************************************************************************}
{ General functions
  Set of functions that help with converting data types and handling strings.

  List of functions:
  - IfThenInt
  - TitleCase
  - SentenceCase
  - csvText
  - CopyFromTo
  - GetTextIn
  - FormatByteSize
  - DateBuiltString
  - DateTimeToSQL
  - SQLToDateTime
  - RateStr
  - TimeStr
  - AppendIfMissing
  - StrEndsWith
  - RemoveFromEnd
  - IntegerListSum
  - Wordwrap
  - ExtractPath
  - ContainsMatch
  - IsURL
  - IsDotFile
  - SaveStringToFile
  - ApplyTemplate
}
{*****************************************************************************}

function ShortenVersion(vs: string; numClauses: Integer): string;
var
  i, numDots: Integer;
begin
  Result := '';
  numDots := 0;
  for i := 1 to Pred(Length(vs)) do begin
    if vs[i] = '.' then
      Inc(numDots);
    if numDots = numClauses then
      break;
    Result := Result + vs[i];
  end;
end;

{ Returns one of two integers based on a boolean argument.
  Like IfThen from StrUtils, but returns an Integer. }
function IfThenInt(AValue: boolean; ATrue: Integer = 1; AFalse: Integer = 0): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{ Capitalizes the first letter of each word }
function TitleCase(sText: String): String;
const
  cDelimiters = [#9, #10, #13, ' ', ',', '.', ':', ';', '"',
                 '\', '/', '(', ')', '[', ']', '{', '}'];
var
  iLoop: Integer;
begin
  Result := sText;
  if (Result <> '') then begin
    Result := LowerCase(Result);

    Result[1] := UpCase(Result[1]);
    for iLoop := 2 to Length(Result) do
      if (Result[iLoop - 1] in cDelimiters) then
        Result[iLoop] := UpCase(Result[iLoop]);
  end;
end;

{ Capitalizes first character of each sentence }
function SentenceCase(sText: string): string;
const
  cTerminators = ['!', '.', '?'];
var
  iLoop: Integer;
  bTerminated: boolean;
begin
  Result := sText;
  if (Result <> '') then begin
    Result := LowerCase(Result);

    Result[1] := UpCase(Result[1]);
    bTerminated := false;
    for iLoop := 2 to Length(Result) do begin
      if (Result[iLoop - 1] in cTerminators) then
        bTerminated := true;
      if bTerminated and (Result[iLoop] <> ' ') then
        Result[iLoop] := UpCase(Result[iLoop]);
    end;
  end;
end;

{ Replaces newlines with a comma and space }
function csvText(s: string): string;
begin
  result := StringReplace(Trim(s), #13, ', ', [rfReplaceAll]);
end;

{ Copies a substring in a string between two indexes }
function CopyFromTo(str: string; first, last: Integer): string;
begin
  Result := Copy(str, first, (last - first) + 1);
end;

{ Returns a substring of @str between characters @open and @close }
function GetTextIn(str: string; open, close: char): string;
var
  i, openIndex: integer;
  bOpen: boolean;
begin
  Result := '';
  bOpen := false;
  openIndex := 0;
  for i := 0 to Length(str) do begin
    if not bOpen and (str[i] = open) then begin
      openIndex := i;
      bOpen := true;
    end;
    if bOpen and (str[i] = close) then begin
      Result := CopyFromTo(str, openIndex + 1, i - 1);
      break;
    end;
  end;
end;

{ Format file byte size }
function FormatByteSize(const bytes: Int64): string;
const
 B = 1; //byte
 KB = 1024 * B; //kilobyte
 MB = 1024 * KB; //megabyte
 GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    result := FormatFloat('#.## GB', bytes / GB)
  else
    if bytes > MB then
      result := FormatFloat('#.## MB', bytes / MB)
    else
      if bytes > KB then
        result := FormatFloat('#.## KB', bytes / KB)
      else
        if bytes > 0 then
          result := FormatFloat('#.## bytes', bytes)
        else
          result := '0 bytes';
end;

{ Converts a TDateTime to a string, with 0 being the string 'Never' }
function DateBuiltString(date: TDateTime): string;
begin
  if date = 0 then
    Result := 'Never'
  else begin
    Result := DateTimeToStr(date);
  end;
end;

function DateTimeToSQL(date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', date);
end;

function SQLToDateTime(date: string): TDateTime;
var
  fs: TFormatSettings;
begin
  GetLocaleFormatSettings(GetThreadLocale, fs);
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-mm-dd';
  fs.TimeSeparator := ':';
  fs.LongTimeFormat := 'hh:nn:ss';
  Result := StrToDateTime(date, fs);
end;

{ Converts a TDateTime to a rate string, e.g. Every 24.0 hours }
function RateStr(date: TDateTime): string;
begin
  if date > 1.0 then
    Result := Format('Every %0.2f days', [date])
  else if date * 24.0 > 1.0 then
    Result := Format('Every %0.1f hours', [date * 24.0])
  else if date * 24.0 * 60.0 > 1.0 then
    Result := Format('Every %0.1f minutes', [date * 24.0 * 60.0])
  else
    Result := Format('Every %0.1f seconds', [date * 24.0 * 60.0 * 60.0]);
end;

{ Converts a TDateTime to a time string, e.g. 19d 20h 3m 30s }
function TimeStr(date: TDateTime): string;
begin
  Result := Format('%dd %dh %dm', [Trunc(date), HourOf(date), MinuteOf(date)]);
end;

{
  StrEndsWith:
  Checks to see if a string ends with an entered substring.

  Example usage:
  s := 'This is a sample string.';
  if StrEndsWith(s, 'string.') then
    AddMessage('It works!');
}
function StrEndsWith(s1, s2: string): boolean;
var
  n1, n2: integer;
begin
  Result := false;

  n1 := Length(s1);
  n2 := Length(s2);
  if n1 < n2 then exit;

  Result := (Copy(s1, n1 - n2 + 1, n2) = s2);
end;

{
  AppendIfMissing:
  Appends substr to the end of str if it's not already there.

  Example usage:
  s := 'This is a sample string.';
  Logger.Write(AppendIfMissing(s, 'string.')); //'This is a sample string.'
  Logger.Write(AppendIfMissing(s, '  Hello.')); //'This is a sample string.  Hello.'
}
function AppendIfMissing(str, substr: string): string;
begin
  Result := str;
  if not StrEndsWith(str, substr) then
    Result := str + substr;
end;

{
  RemoveFromEnd:
  Creates a new string with s1 removed from the end of s2, if found.

  Example usage:
  s := 'This is a sample string.';
  AddMessage(RemoveFromEnd(s, 'string.')); //'This is a sample '
}
function RemoveFromEnd(s1, s2: string): string;
begin
  Result := s1;
  if StrEndsWith(s1, s2) then
    Result := Copy(s1, 1, Length(s1) - Length(s2));
end;

{ Calculates the integer sum of all values in a TStringList to maxIndex }
function IntegerListSum(list: TStringList; maxIndex: integer): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to maxIndex do
    Inc(result, StrToInt(list[i]));
end;

{ Inserts line breaks in string @s before @charCount has been exceeded }
function Wordwrap(s: string; charCount: integer): string;
var
  i, lastSpace, counter: Integer;
begin
  counter := 0;
  lastSpace := 0;
  for i := 1 to Length(s) - 1 do begin
    Inc(counter);
    if (s[i] = ' ') or (s[i] = ',') then
      lastSpace := i;
    if (s[i] = #13) or (s[i] = #10)
    or (s[i + 1] = #13) or (s[i + 1] = #10) then begin
      lastSpace := 0;
      counter := 0;
    end;
    if (counter = charCount) and (lastSpace > 0) then begin
      Insert(#13#10, s, lastSpace + 1);
      lastSpace := 0;
      counter := 0;
    end;
  end;
  Result := s;
end;

{ Like ExtractFilePath, but will allow the user to specify how many @levels
  they want to traverse back.  Specifying @levels = 0 is equivalent to
  ExtractFilePath.

  Example usage:
  path := 'C:\Program Files (x86)\Test\Test.exe';
  ShowMessage(ExtractPath(path, 0)); // 'C:\Program Files (x86)\Test\'
  ShowMessage(ExtractPath(path, 1)); // 'C:\Program Files (x86)\'
  ShowMessage(ExtractPath(path, 2)); // 'C:\'
}
function ExtractPath(path: string; levels: integer): string;
var
  i, n: integer;
begin
  n := 0;
  for i := Length(path) downto 1 do
    if IsPathDelimiter(path, i) then begin
      if n = levels then
        break
      else
        Inc(n);
    end;
  Result := Copy(path, 1, i);
end;

{ Checks to see if any mask in @sl matches the string @s }
function ContainsMatch(var sl: TStringList; const s: string): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Pred(sl.Count) do
    if MatchesMask(s, sl[i]) then begin
      Result := true;
      break;
    end;
end;

{ Returns true if the string is an http:// or https:// url }
function IsURL(s: string): boolean;
begin
  Result := (Pos('http://', s) = 1) or (Pos('https://', s) = 1);
end;

{ Returns true if @fn is . or .. }
function IsDotFile(fn: string): boolean;
begin
  Result := (fn = '.') or (fn = '..');
end;

{ Saves a string @s to a file at @fn }
procedure SaveStringToFile(s: string; fn: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := s;
  sl.SaveToFile(fn);
  sl.Free;
end;

function ApplyTemplate(const template: string; var map: TStringList): string;
const
  openTag = '{{';
  closeTag = '}}';
var
  i: Integer;
  name, value: string;
begin
  Result := template;
  for i := 0 to Pred(map.Count) do begin
    name := map.Names[i];
    value := map.ValueFromIndex[i];
    Result := StringReplace(Result, openTag + name + closeTag, value, [rfReplaceAll]);
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

procedure TryToFree(obj: TObject);
begin
  if Assigned(obj) then try
    obj.Free;
  except
    on x: Exception do // nothing
  end;
end;

procedure FreeList(var lst: TList);
var
  i: Integer;
  obj: TObject;
begin
  for i := Pred(lst.Count) downto 0 do begin
    obj := TObject(lst[i]);
    TryToFree(obj);
  end;
  lst.Free;
end;


{******************************************************************************}
{ Windows API functions
  Set of functions that help deal with the Windows File System.

  List of functions:
  - ForceForeground
  - FileNameValid
  - RecycleDirectory
  - ExecNewProcess
  - BrowseForFile
  - BrowseForFolder
  - GetCSIDLShellFolder
  - GetFileSize
  - GetLastModified
  - MultFileSearch
  - RecursiveFileSearch
  - CopyDirectory
  - GetFilesList
  - CopyFiles
  - CorrectListViewWidth
  - GetVersionMem
  - FileVersion
  - DeleteDirectory
}
{******************************************************************************}

{
  ForceForeground:
  Forces a hWnd to the foreground.
}
procedure ForceForeground(hWnd: THandle);
begin
  SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
  SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
end;

{
  GetDriveList:
  Returns an array filled wit the assigned
  drive letters on the current computer.
}

function GetDriveList: TStringDynArray;
var
  Buff: array[0..128] of Char;
  ptr: PChar;
  Idx: Integer;
begin
  if (GetLogicalDriveStrings(Length(Buff), Buff) = 0) then
    RaiseLastOSError;
  // There can't be more than 26 lettered drives (A..Z).
  SetLength(Result, 26);

  Idx := 0;
  ptr := @Buff;
  while StrLen(ptr) > 0 do
  begin
    Result[Idx] := ptr;
    ptr := StrEnd(ptr);
    Inc(ptr);
    Inc(Idx);
  end;
  SetLength(Result, Idx);
end;

{
  DOSDrive:
  Converts a drive letter into the integer drive #
  required by DiskSize().
}
function DOSDrive( const sDrive: String ): Integer;
begin
  if (Length(sDrive) < 1) then
    Result := -1
  else
    Result := (Ord(UpCase(sDrive[1])) - 64);
end;

{
  DriveReady:
  Tests the status of a drive to see if it's ready
  to access.
}
function DriveReady(const sDrive: String): Boolean;
var
  ErrMode: Word;
begin
  ErrMode := SetErrorMode(0);
  SetErrorMode(ErrMode or SEM_FAILCRITICALERRORS);
  try
    Result := (DiskSize(DOSDrive(sDrive)) > -1);
  finally
    SetErrorMode(ErrMode);
  end;
end;

{
  TryRegistryKeys:
  Tries to load various registry keys.
}
function TryRegistryKeys(var keys: TStringList): string;
var
  i: Integer;
  path, name: string;
begin
  Result := '';
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

{
  DirectoryValid:
  Returns true if the input directory path is valid.
}
function DirectoryValid(dir: string): boolean;
begin
  Result := false;
  if (dir = '') then
    exit;

  dir := ExcludeTrailingPathDelimiter(dir);
{$IFDEF MSWINDOWS}
  if (Length(dir) < 3) or (ExtractFilePath(dir) = dir) then
    exit; // avoid 'xyz:\' problem.
{$ENDIF}
{$IFDEF POSIX}
  if (dir = '') then
    exit;
{$ENDIF POSIX};
  Result := true;
end;

{
  UpDirectory:
  Returns the path of the directory holding a directory.
}
function UpDirectory(sPath: string): string;
begin
  if not StrEndsWith(sPath, '\') then
    sPath := ExtractFilePath(sPath);
  Result := ExtractFilePath(RemoveFromEnd(sPath, '\'));
end;

{
  FileNameValid:
  Returns true if the input filename is valid.
}
function FileNameValid(filename: string): boolean;
begin
  Result := (Length(Trim(filename)) > 0) and
    TPath.HasValidFileNameChars(filename, false);
end;

{
  ExecNewProcess:
  Create a new synchronous or asynchronous process.
}
procedure ExecNewProcess(ProgramName: string; synchronous: Boolean);
var
  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;
  CreateOK : Boolean;
begin
  { fill with known state }
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb := SizeOf(TStartupInfo);
  CreateOK := CreateProcess(PChar(ProgramName), nil, nil, nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, nil, StartInfo, ProcInfo);

  // check if successful
  if CreateOK then begin
    if synchronous then
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);
  end
  else
    ShowMessage('Unable to run '+ProgramName);

  // close handles
  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

{
  BrowseForFile:
  Links a file selection through a TOpenDialog to the text stored in @ed,
  applying filter @filter.
}
procedure BrowseForFile(var ed: TEdit; filter, initDir: string);
var
  openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(ed.Parent);

  if FileExists(ed.Text) then
    openDialog.InitialDir := ExtractFilePath(ed.Text)
  else if DirectoryExists(ed.Text) then
    openDialog.InitialDir := ed.Text
  else
    openDialog.InitialDir := initDir;

  openDialog.Filter := filter;
  if openDialog.Execute then
    ed.Text := openDialog.FileName;
end;

{
  BrowseForFolder:
  Links a file selection through a TOpenDialog to the text stored in @ed,
  applying filter @filter
}
procedure BrowseForFolder(var ed: TEdit; initDir: string);
var
  s: string;
begin
  // start in current directory value if valid
  if DirectoryExists(ed.Text) then
    s := ed.Text
  else
    s := initDir;
  // prompt user to select a directory
  SelectDirectory('Select a directory', '', s, []);

  // save text to TEdit
  if s <> '' then
    ed.Text := AppendIfMissing(s, '\');
end;

{
  GetCSIDLShellFolder:
  Gets a folder by its integer CSID.
}
function GetCSIDLShellFolder(CSIDLFolder: integer): string;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(0, PChar(Result), CSIDLFolder, True);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then
    Result := IncludeTrailingBackslash(Result);
end;

{
  GetFileSize:
  Gets the size of a file at @aFilename through the windows API.
}
function GetFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    EXIT;

  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

{
  GetLastModified:
  Gets the last time a file was modified.
}
function GetLastModified(const aFileName: String): TDateTime;
var
  info: TWin32FileAttributeData;
  FileTime: TFileTime;
  LocalTime, SystemTime: TSystemTime;
begin
  result := 0;
  // exit if can't get attributes
  if not GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    exit;

  // get last modified
  FileTime := info.ftLastWriteTime;

  // convert to system time
  if not FileTimeToSystemTime(FileTime, SystemTime) then
    RaiseLastOSError;
  if not SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime) then
    RaiseLastOSError;

  Result := SystemTimeToDateTime(LocalTime);
end;

{
  SearchPathsForFile:
  Searches for a file @sFileName in each path in @sPaths.
}
function SearchPathsForFile(sPaths, sFileName: string): string;
var
  slPaths: TStringList;
  i: Integer;
  info: TSearchRec;
begin
  slPaths := TStringList.Create;
  try
    while (Pos(';', sPaths) > 0) do begin
      slPaths.Add(Copy(sPaths, 1, Pos(';', sPaths) - 1));
      sPaths := Copy(sPaths, Pos(';', sPaths) + 1, Length(sPaths));
    end;
    for i := 0 to slPaths.Count - 1 do begin
      if FindFirst(slPaths[i] + '\*', faDirectory, info) = 0 then begin
        repeat
          Result := FileSearch(sFileName, slPaths[i] + '\' + info.Name);
          if (Result <> '') then
            break;
        until FindNext(info) <> 0;
        FindClose(info);
        // break if we found it
        if (Result <> '') then
          break;
      end;
    end;
  finally
    slPaths.Free;
  end;
end;

{
  MultFileSearch:
  Wraps around RecursiveFileSearch, allowing the searching of multiple paths.
}
function MultFileSearch(paths, filenames, ignore: array of string; maxDepth: integer): string;
var
  i: Integer;
  path: string;
begin
  for i := Low(paths) to High(paths) do begin
    path := RecursiveFileSearch(paths[i], filenames, ignore, maxDepth);
    if path <> '' then
      break;
  end;
  Result := path;
end;

{
  RecursiveFileSearch:
  Recursively searches a path for a file matching @filenames, ignoring
  directories in @ignore, and not traversing deeper than maxDepth.

  Example usage:
  p := RecursiveFileSearch(GamePath, filenames, ignore, 1);
  AddMessage(p);
}
function RecursiveFileSearch(aPath: string; filenames, ignore: array of string;
  maxDepth: integer): string;
var
  skip: boolean;
  i: integer;
  info: TSearchRec;
begin
  Result := '';
  aPath := AppendIfMissing(aPath, PathDelim);
  if Result <> '' then exit;

  // exit if no files in path
  if FindFirst(aPath + '*', faAnyFile, info) <> 0 then
    exit;
  // else loop through all files in path
  repeat
    if IsDotFile(info.Name) then
      continue; // skip . and ..
    skip := false;
    for i := Low(ignore) to High(ignore) do begin
      skip := Lowercase(info.Name) = ignore[i];
      if skip then
        break;
    end;
    if not skip then begin
      if ((info.attr and faDirectory) = faDirectory) and (maxDepth > 0) then begin
        Result := RecursiveFileSearch(aPath+info.Name, filenames, ignore, maxDepth - 1);
      end
      else if MatchStr(info.Name, filenames) then
        Result := aPath + info.Name;
    end;
    if (Result <> '') then break;
  until FindNext(info) <> 0;
  FindClose(info);
end;

{
  CopyDirectory:
  Recursively copies all of the contents of a directory.

  Example usage:
  slIgnore := TStringList.Create;
  slIgnore.Add('mteFunctions.pas');
  CopyDirectory(ScriptsPath, 'C:\ScriptsBackup', slIgnore);
}
procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
var
  info: TSearchRec;
  isDirectory: boolean;
begin
  src := AppendIfMissing(src, PathDelim);
  dst := AppendIfMissing(dst, PathDelim);

  // if no files in source path, exit
  if (FindFirst(src + '*', faAnyFile, info) <> 0) then
    exit;
  repeat
    isDirectory := (info.Attr and faDirectory = faDirectory);
    // skip . and ..
    if (info.Name = '.') or (info.Name = '..') then
      continue;

    // skip if ignored
    if isDirectory and ContainsMatch(dIgnore, info.Name) then
      continue
    else if ContainsMatch(fIgnore, info.Name) then
      continue;

    // copy the file or recurse
    ForceDirectories(dst);
    if isDirectory then
      CopyDirectory(src+info.Name, dst+info.Name, fIgnore, dIgnore)
    else
      CopyFile(PChar(src+info.Name), PChar(dst+info.Name), false);
  until FindNext(info) <> 0;

  FindClose(info);
end;

{
  GetFilesList:
  Searches @path, recursively traversing subdirectories that don't match a mask
  in @dIgnore, adding files that don't match a mask in @fIgnore to @list.

  Example usage:
  FilesList := TStringList.Create;
  fileIgnore := TStringList.Create;
  fileIgnore.Add('*.esp');
  dirIgnore := TStringList.Create;
  dirIgnore.Add('translations');
  GetFilesList(wbDataPath, fileIgnore, dirIgnore, FilesList);
}
procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
var
  info: TSearchRec;
  isDirectory: boolean;
begin
  path := AppendIfMissing(path, PathDelim);

  // if no files in source path, exit
  if (FindFirst(path + '*', faAnyFile, info) <> 0) then
    exit;
  repeat
    isDirectory := (info.Attr and faDirectory = faDirectory);
    // skip . and ..
    if (info.Name = '.') or (info.Name = '..') then
      continue;

    // skip if ignored
    if isDirectory then begin
      if ContainsMatch(dIgnore, info.Name) then
        continue;
    end
    else if ContainsMatch(fIgnore, info.Name) then
      continue;

    // copy the file or recurse
    if isDirectory then
      GetFilesList(path + info.Name, fIgnore, dIgnore, list)
    else
      list.Add(path + info.Name);
  until FindNext(info) <> 0;

  FindClose(info);
end;

{ Copies files in @list from @src to @dst }
procedure CopyFiles(src, dst: string; var list: TStringList);
var
  i: Integer;
  srcFile, dstFile: string;
begin
  src := AppendIfMissing(src, PathDelim);
  dst := AppendIfMissing(dst, PathDelim);
  for i := 0 to Pred(list.Count) do begin
    srcFile := list[i];
    dstFile := StringReplace(srcFile, src, dst, []);
    ForceDirectories(ExtractFilePath(dstFile));
    CopyFile(PChar(srcFile), PChar(dstFile), false);
  end;
end;

{ Get program version from memory }
function GetVersionMem: string;
var
  verblock: PVSFIXEDFILEINFO;
  versionMS, versionLS, verlen: cardinal;
  rs: TResourceStream;
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    rs := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
    try
      m.CopyFrom(rs, rs.Size);
    finally
      rs.Free;
    end;
    m.Position := 0;
    if VerQueryValue(m.Memory, '\', Pointer(verblock), verlen) then begin
      VersionMS := verblock.dwFileVersionMS;
      VersionLS := verblock.dwFileVersionLS;
      Result := Format('%s.%s.%s.%s', [IntToStr(versionMS shr 16),
        IntToStr(versionMS and $FFFF), IntToStr(VersionLS shr 16),
        IntToStr(VersionLS and $FFFF)]);
    end;
  finally
    m.Free;
  end;
end;

{ Get program version from disk }
function FileVersion(const FileName: string): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('%d.%d.%d.%d', [
            HiWord(dwFileVersionMS), //Major
            LoWord(dwFileVersionMS), //Minor
            HiWord(dwFileVersionLS), //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

{ Sends the file/directory at @path to the recycle bin }
function DeleteToRecycleBin(const path: string; Confirm: Boolean): Boolean;
var
  sh: TSHFileOpStruct;
begin
  FillChar(sh, SizeOf(sh), 0);
  with sh do begin
    Wnd := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(path + #0);
    fFlags := FOF_SILENT or FOF_ALLOWUNDO;
    if not Confirm then
      fFlags := fFlags or FOF_NOCONFIRMATION;
  end;
  Result := SHFileOperation(sh) = 0;
end;

{ Deletes the directory at @path and all files it contains }
procedure DeleteDirectory(const path: string);
var
  ShOp: TSHFileOpStruct;
begin
  ShOp.Wnd := 0;
  ShOp.wFunc := FO_DELETE;
  ShOp.pFrom := PChar(path + #0);
  ShOp.pTo := nil;
  ShOp.fFlags := FOF_NOCONFIRMATION or FOF_ALLOWUNDO or FOF_NO_UI;
  SHFileOperation(ShOp);
end;

{ Performs tests of directory creation and deletion, and file creation,
  reading, writing, and deletion at the specified @sBasePath }
procedure PerformFileSystemTests(sBasePath: string);
var
  sl1, sl2: TStringList;
  sExceptionBase, sPath, sTask: string;
begin
  // initialize stringlists
  sl1 := TStringList.Create;
  sl2 := TStringList.Create;
  sExceptionBase := 'Could not %s at path "%s"';

  try
    // try to create a new directory
    sTask := 'create directory';
    sPath := sBasePath + 'test\';
    ForceDirectories(sPath);
    if not DirectoryExists(sPath) then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));

    // try to create a new file
    sTask := 'create file';
    sPath := sBasePath + 'test\Test.txt';
    sl1.Text := sBasePath;
    sl1.SaveToFile(sPath);

    // if file doesn't exist after saving, raise an exception
    if not FileExists(sPath) then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));

    // try to read the file
    sTask := 'read file';
    sl2.LoadFromFile(sPath);
    if sl2.Text <> sl1.Text then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));

    // try to write to the file
    sTask := 'write to file';
    sl1.Text := 'Testing 123abc';
    sl1.SaveToFile(sPath);
    sl2.LoadFromFile(sPath);
    if sl2.Text <> sl1.Text then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));

    // try to delete the file
    sTask := 'delete file';
    DeleteFile(sPath);
    if FileExists(sPath) then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));

    // try to delete the directory
    sTask := 'delete directory';
    sPath := sBasePath + 'test\';
    DeleteDirectory(sPath);
    if DirectoryExists(sPath) then
      raise Exception.Create(Format(sExceptionBase, [sTask, sPath]));
  finally
    // always free memory
    sl1.Free;
    sl2.Free;
  end;
end;


{******************************************************************************}
{ GUI Helper Functions
  - ListView_CorrectWidth
  - ListView_FindNextMatch
  - ListView_HandleMatch
}
{******************************************************************************}

procedure StringGrid_CorrectWidth(var sg: TStringGrid);
var
  w: Integer;
begin
  w := sg.ClientWidth;
  Dec(w, sg.ColWidths[0]);
  sg.ColWidths[1] := w;
end;

{ Fixes @lv's width to fit client width if it has autosizable columns,
  which resolves an issue where autosize doesn't work on virtual vsReport
  TListViews when a scroll bar becomes visible. }
procedure ListView_CorrectWidth(var lv: TListView);
var
  i, w: Integer;
  col: TListColumn;
  AutoSizedColumns: TList;
begin
  AutoSizedColumns := TList.Create;
  w := lv.ClientWidth;

  // loop through columns keeping track of remaining width
  for i := 0 to Pred(lv.Columns.Count) do begin
    col := lv.Columns[i];
    if col.AutoSize then
      AutoSizedColumns.Add(col)
    else
      Dec(w, ListView_GetColumnWidth(lv.Handle, i));
  end;

  // set auotsized columns to fit client width
  for i := 0 to Pred(AutoSizedColumns.Count) do begin
    col := TListColumn(AutoSizedColumns[i]);
    col.Width := w div AutoSizedColumns.Count;
  end;

  // clean up
  AutoSizedColumns.Free;
end;

{ If @iIndex = 0, returns the index of the next @ListView item
  matching the input @sSearch string.  Else returns the index
  of the next @ListView subitem at index @iIndex - 1 matching
  the input @sSearch string. }
function ListView_NextMatch(ListView: TListView; sSearch: string;
  iIndex: Integer): Integer;
var
  i, iStart: Integer;
  ListItem: TListITem;
  sCaption, sCompare: string;
begin
  Result := -1;

  // Start at selected item's index, if there
  // is an item selected
  if Assigned(ListView.Selected) then
    iStart := ListView.Selected.Index
  // Else start at 0, the first item
  else
    iStart := 0;

  // Loop through items looking for a match
  for i := iStart to Pred(ListView.Items.Count) do begin
    ListItem := ListView.Items[i];
    if iIndex = 0 then
      sCaption := ListItem.Caption
    else
      sCaption := ListItem.SubItems[iIndex - 1];
    sCompare := Copy(sCaption, 1, Length(sSearch));
    if SameText(sSearch, sCompare) then begin
      Result := i;
      break;
    end;
  end;
end;

{ Sets @sBuffer to @sTempBuffer, then selects and jumps to the item
  at @iFoundIndex in @ListView }
procedure ListView_HandleMatch(ListView: TListView; iFoundIndex: Integer;
  var sBuffer: string; sTempBuffer: string);
begin
  // Set the actual buffer to our temporary buffer
  // and jump to the item we found
  sBuffer := sTempBuffer;
  if Assigned(ListView.Selected) then
    ListView.ClearSelection;
  ListView.Selected := ListView.Items[iFoundIndex];
  ListView.Items[iFoundIndex].MakeVisible(false);
end;

initialization
begin
  bAllowHelp := false;
end;

end.