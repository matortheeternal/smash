unit mteBase;

interface

uses
  Classes, Menus,
  // third party libraries
  superobject,
  // mte units
  mteTracker,
  // xEdit units
  wbHelpers, wbInterface, wbImplementation;

type
  TSmashType = ( stUnknown, stRecord, stString, stInteger, stFlag, stFloat,
    stStruct, stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray, stByteArray, stUnion );
  TBasePlugin = class(TObject)
  public
    _File: IwbFile;
    hasData: boolean;
    hash: string;
    fileSize: Int64;
    dateModified: string;
    filename: string;
    numRecords: Integer;
    numOverrides: Integer;
    author: string;
    dataPath: string;
    description: TStringList;
    masters: TStringList;
    requiredBy: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetData(var lst: TList);
    procedure UpdateData; virtual;
    procedure GetHash;
    function GetFormIndex: Integer;
  end;
  TPluginHelpers = class
    class function CreateNewBasePlugin(var list: TList; filename: string): TBasePlugin;
    class function BasePluginByFilename(var list: TList; filename: string): TBasePlugin;
    class function BasePluginLoadOrder(var list: TList; filename: string): integer;
  end;
  THeaderHelpers = class
    class procedure LoadPluginHeaders(var sl: TStringList);
    class procedure GetPluginMasters(filename: string; var sl: TStringList);
    class procedure GetPluginDependencies(filename: string; var sl: TStringList);
  end;

  { General Helper Functions }
  function etToString(et: TwbElementType): string;
  function dtToString(dt: TwbDefType): string;
  function ctToString(ct: TConflictThis): string;
  function stToString(st: TSmashType): string;
  function SmashType(def: IwbNamedDef): TSmashtype;
  function GetSmashType(element: IwbElement): TSmashType;
  function ElementByIndexedPath(e: IwbElement; ip: string): IwbElement;
  function IndexedPath(e: IwbElement): string;
  function GetAllValues(e: IwbElement): string;
  function IsSortedDef(def: IwbNamedDef): boolean;
  function IsSorted(e: IwbElement): boolean;
  function HasStructChildren(e: IwbElement): boolean;
  function HasStructChildrenDef(def: IwbNamedDef): boolean;
  function WinningOverrideInFiles(rec: IwbMainRecord;
    var sl: TStringList): IwbMainRecord;
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function ExtractFormID(filename: string): string;
  function RemoveFileIndex(formID: string): string;
  function LocalFormID(aRecord: IwbMainRecord): integer;
  function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
  function CountOverrides(aFile: IwbFile): integer;
  function OverrideCountInFiles(rec: IwbMainRecord; var files: TStringList): Integer;
  procedure AddRequiredBy(var lst: TList; filename: string;
    var masters: TStringList);
  procedure GetMasters(aFile: IwbFile; var sl: TStringList);
  procedure AddMasters(aFile: IwbFile; var sl: TStringList);
  function RemoveSelfOrContainer(const aElement: IwbElement): boolean;
  procedure UndeleteAndDisable(const aRecord: IwbMainRecord);
  function LoadOrderCompare(List: TStringList; Index1, Index2: Integer): Integer;

  { Record Prototyping Functions }
  function GetElementObj(var obj: ISuperObject; name: string): ISuperObject;
  function CreateRecordObj(var tree: ISuperObject; rec: IwbMainRecord): ISuperObject;
  function GetRecordObj(var tree: ISuperObject; name: string): ISuperObject;
  function GetRecordDef(sig: TwbSignature): TwbRecordDefEntry;
  function BuildDef(def: IwbNamedDef; name: string): ISuperObject;
  function BuildRecordDef(sName: string; mrDef: IwbRecordDef; out recObj: ISuperObject): boolean; overload;
  function BuildRecordDef(sName: string; out recObj: ISuperObject): boolean; overload;
  function GetEditableFileContainer: IwbContainerElementRef;

  { Plugin Error Functions }
  function FixErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;

  { Asset Handling Functions }
  procedure ExtractBSA(ContainerName, folder, destination: string); overload;
  procedure ExtractBSA(ContainerName, destination: string; var ignore: TStringList); overload;
  function BSAExists(filename: string): boolean;
  function INIExists(filename: string): boolean;
  function TranslationExists(filename: string): boolean;
  function FaceDataExists(filename: string): boolean;
  function VoiceDataExists(filename: string): boolean;
  function FragmentsExist(f: IwbFile): boolean;
  function ReferencesSelf(f: IwbFile): boolean;

var
  PluginsList: TList;
  HeaderList: TList;

implementation

uses
  SysUtils, Dialogs,
  mteHelpers,
  msConfiguration;

constructor TBasePlugin.Create;
begin
  hasData := false;
  dataPath := wbDataPath;
  description := TStringList.Create;
  masters := TStringList.Create;
  requiredBy := TStringList.Create;
end;

destructor TBasePlugin.Destroy;
begin
  description.Free;
  masters.Free;
  requiredBy.Free;
end;

procedure TBasePlugin.GetData(var lst: TList);
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
  // we have to subtract 1 because this count includes the
  // file header for some reason
  numRecords := Container.GetElementNativeValue('HEDR - Header\Number of Records') - 1;

  // get masters, required by
  GetMasters(_File, masters);
  AddRequiredBy(lst, filename, masters);

  // get description
  s := Container.GetElementEditValue('SNAM - Description');
  description.Text := Wordwrap(s, 80);

  // get file attributes
  fileSize := GetFileSize(wbDataPath + filename);
  dateModified := DateTimeToStr(GetLastModified(wbDataPath + filename));
end;

procedure TBasePlugin.UpdateData;
begin
  // virtual method to be overridden
end;

procedure TBasePlugin.GetHash;
begin
  hash := IntToHex(wbCRC32File(wbDataPath + filename), 8);
end;

function TBasePlugin.GetFormIndex: Integer;
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


{*****************************************************************************}
{ PLUGIN HELPERS
  Helper methods for dealing with TBasePlugins.
}
{*****************************************************************************}

{ Create a new plugin }
class function TPluginHelpers.CreateNewBasePlugin(var list: TList; filename: string): TBasePlugin;
var
  aFile: IwbFile;
  LoadOrder: integer;
  plugin: TBasePlugin;
begin
  Result := nil;
  LoadOrder := PluginsList.Count + 1;
  // fail if maximum load order reached
  if LoadOrder > 254 then begin
    Tracker.Write('Maximum load order reached!  Can''t create file '+filename);
    exit;
  end;

  // create new plugin file
  SysUtils.FormatSettings.DecimalSeparator := '.';
  aFile := wbNewFile(wbDataPath + filename, LoadOrder);
  aFile._AddRef;

  // create new plugin object
  plugin := TBasePlugin.Create;
  plugin.filename := filename;
  plugin._File := aFile;
  Result := plugin;
end;

{ Gets the load order of the plugin matching the given name }
class function TPluginHelpers.BasePluginLoadOrder(var list: TList; filename: string): integer;
var
  i: integer;
  plugin: TBasePlugin;
begin
  Result := -1;
  for i := 0 to Pred(list.Count) do begin
    plugin := TBasePlugin(list[i]);
    if plugin.filename = filename then begin
      Result := i;
      exit;
    end;
  end;
end;

{ Gets a plugin matching the given name. }
class function TPluginHelpers.BasePluginByFilename(var list: TList; filename: string): TBasePlugin;
var
  i: integer;
  plugin: TBasePlugin;
begin
  Result := nil;
  for i := 0 to Pred(list.count) do begin
    plugin := TBasePlugin(list[i]);
    if plugin.filename = filename then begin
      Result := plugin;
      exit;
    end;
  end;
end;

class procedure THeaderHelpers.LoadPluginHeaders(var sl: TStringList);
var
  i: Integer;
  aFile: IwbFile;
  plugin: TBasePlugin;
begin
  // create header list
  HeaderList := TList.Create;

  // load plugin headers for each plugin in @sl
  for i := 0 to Pred(sl.Count) do try
    aFile := wbFile(wbDataPath + sl[i], -1, '', False, True);
    plugin := TBasePlugin.Create;
    plugin._File := aFile;
    HeaderList.Add(plugin);
  except
    on x: Exception do begin
      Tracker.Write('Failed to load '+sl[i]);
    end;
  end;

  // get data for each plugin in the header list
  for i := 0 to Pred(HeaderList.Count) do begin
    plugin := TBasePlugin(HeaderList[i]);
    plugin.GetData(HeaderList);
  end;
end;

class procedure THeaderHelpers.GetPluginMasters(filename: string;
  var sl: TStringList);
var
  plugin: TBasePlugin;
  i: integer;
begin
  // get plugin
  plugin := TPluginHelpers.BasePluginByFilename(HeaderList, filename);
  if not Assigned(plugin) then exit;
  // add its masters to @sl
  for i := 0 to Pred(plugin.masters.Count) do begin
    if sl.IndexOf(plugin.masters[i]) > -1 then continue;
    sl.Add(plugin.masters[i]);
    GetPluginMasters(plugin.masters[i], sl);
  end;
end;

class procedure THeaderHelpers.GetPluginDependencies(filename: string;
  var sl: TStringList);
var
  plugin: TBasePlugin;
  i: integer;
begin
  // get plugin
  plugin := TPluginHelpers.BasePluginByFilename(HeaderList, filename);
  if not Assigned(plugin) then exit;
  // add its required by to @sl
  for i := 0 to Pred(plugin.requiredBy.Count) do begin
    if sl.IndexOf(plugin.requiredBy[i]) > -1 then continue;
    sl.Add(plugin.requiredBy[i]);
    GetPluginDependencies(plugin.requiredBy[i], sl);
  end;
end;


{******************************************************************************}
{ General Helper Functions
  Set of functions that read bethesda plugin files for various attributes.

  List of functions:
  - etToString
  - dtToString
  - ctToString
  - stToString
  - GetSmashType
  - ElementByIndexedPath
  - IndexedPath
  - GetAllValues
  - IsSorted
  - HasStructChildren
  - WinningOverrideInFiles
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

function ctToString(ct: TConflictThis): string;
begin
  case Ord(ct) of
    Ord(ctUnknown): Result := 'ctUnknown';
    Ord(ctIgnored): Result := 'ctIgnored';
    Ord(ctNotDefined): Result := 'ctNotDefined';
    Ord(ctIdenticalToMaster): Result := 'ctIdenticalToMaster';
    Ord(ctOnlyOne): Result := 'ctOnlyOne';
    Ord(ctHiddenByModGroup): Result := 'ctHiddenByModGroup';
    Ord(ctMaster): Result := 'ctMaster';
    Ord(ctConflictBenign): Result := 'ctConflictBenign';
    Ord(ctOverride): Result := 'ctOverride';
    Ord(ctIdenticalToMasterWinsConflict): Result := 'ctIdenticalToMasterWinsConflict';
    Ord(ctConflictWins): Result := 'ctConflictWins';
    Ord(ctConflictLoses): Result := 'ctConflictLoses';
  end;
end;

function stToString(st: TSmashType): string;
begin
  case Ord(st) of
    Ord(stUnknown): Result := 'Unknown';
    Ord(stRecord): Result := 'Record';
    Ord(stString): Result := 'String';
    Ord(stInteger): Result := 'Integer';
    Ord(stFlag): Result := 'Flag';
    Ord(stFloat): Result := 'Float';
    Ord(stStruct): Result := 'Struct';
    Ord(stUnsortedArray): Result := 'Unsorted Array';
    Ord(stUnsortedStructArray): Result := 'Unsorted Struct Array';
    Ord(stSortedArray): Result := 'Sorted Array';
    Ord(stSortedStructArray): Result := 'Sorted Struct Array';
    Ord(stByteArray): Result := 'Byte Array';
    Ord(stUnion): Result := 'Union';
    else Result := 'Unknown';
  end;
end;

function SmashType(def: IwbNamedDef): TSmashtype;
var
  subDef: IwbSubRecordDef;
  dt: TwbDefType;
  bIsSorted, bHasStructChildren: boolean;
begin
  dt := def.DefType;
  if Supports(def, IwbSubrecordDef, subDef) then
    dt := subDef.GetValue.DefType;
  case Ord(dt) of
    Ord(dtRecord): Result := stRecord;
    Ord(dtSubRecord): Result := stUnknown;
    Ord(dtSubRecordStruct): Result := stStruct;
    Ord(dtSubRecordUnion): Result := stUnion;
    Ord(dtString): Result := stString;
    Ord(dtLString): Result := stString;
    Ord(dtLenString): Result := stString;
    Ord(dtByteArray): Result := stByteArray;
    Ord(dtInteger): Result := stInteger;
    Ord(dtIntegerFormater): Result := stInteger;
    Ord(dtIntegerFormaterUnion): Result := stInteger;
    Ord(dtFlag): Result := stFlag;
    Ord(dtFloat): Result := stFloat;
    Ord(dtSubRecordArray), Ord(dtArray): begin
      bIsSorted := IsSortedDef(def);
      bHasStructChildren := HasStructChildrenDef(def);
      if bIsSorted then begin
        if bHasStructChildren then
          Result := stSortedStructArray
        else
          Result := stSortedArray;
      end
      else begin
        if bHasStructChildren then
          Result := stUnsortedStructArray
        else
          Result := stUnsortedArray;
      end;
    end;
    Ord(dtStruct): Result := stStruct;
    Ord(dtUnion): Result := stUnion;
    Ord(dtEmpty): Result := stUnknown;
    Ord(dtStructChapter): Result := stStruct;
    else Result := stUnknown;
  end;
end;

function GetSmashType(element: IwbElement): TSmashType;
begin
  Result := SmashType(element.Def);
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

function IsSortedDef(def: IwbNamedDef): boolean;
var
  sraDef: IwbSubRecordArrayDef;
  arDef: IwbArrayDef;
begin
  Result := false;
  if Supports(def, IwbSubRecordArrayDef, sraDef) then
    Result := Supports(sraDef.Element, IwbHasSortKeyDef)
  else if Supports(def, IwbArrayDef, arDef) then
    Result := Supports(arDef.Element, IwbHasSortKeyDef);
end;

{ Returns true if @e is a sorted container }
function IsSorted(e: IwbElement): boolean;
var
  Container: IwbSortableContainer;
begin
  Result := false;
  if Supports(e, IwbSortableContainer, Container) then
    Result := Container.Sorted;
end;

function HasStructChildrenDef(def: IwbNamedDef): boolean;
begin
  Result := Supports(def, IwbSubRecordArrayDef);
end;

{ Returns true if @e is a container with struct children }
function HasStructChildren(e: IwbElement): boolean;
var
  Container: IwbContainerElementRef;
begin
  Result := false;
  if Supports(e, IwbContainerElementRef, Container)
  and (Container.ElementCount > 0) then
    Result := GetSmashType(Container.Elements[0]) = stStruct;
end;

{ Returns the most-winning override of @rec from the
  files listed in @sl }
function WinningOverrideInFiles(rec: IwbMainRecord;
  var sl: TStringList): IwbMainRecord;
var
  i: Integer;
  ovr: IwbMainRecord;
begin
  Result := rec;
  for i := Pred(rec.OverrideCount) downto 0 do begin
    ovr := rec.Overrides[i];
    if sl.IndexOf(ovr._file.FileName) > -1 then begin
      Result := ovr;
      exit;
    end;
  end;
end;

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
procedure AddRequiredBy(var lst: TList; filename: string;
  var masters: TStringList);
var
  i: Integer;
  plugin: TBasePlugin;
begin
  for i := 0 to Pred(masters.Count) do begin
    plugin := TPluginHelpers.BasePluginByFilename(lst, masters[i]);
    if not Assigned(plugin) then
      continue;
    plugin.requiredBy.Add(filename);
  end;
end;

{ Gets the masters in an IwbFile and puts them into a stringlist }
procedure GetMasters(aFile: IwbFile; var sl: TStringList);
var
  Container, MasterFiles, MasterFile: IwbContainer;
  i, iLoadOrder: integer;
  filename: string;
begin
  Container := aFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    for i := 0 to MasterFiles.ElementCount - 1 do begin
      MasterFile := MasterFiles.Elements[i] as IwbContainer;
      filename := MasterFile.GetElementEditValue('MAST - Filename');
      if sl.IndexOf(filename) = -1 then begin
        iLoadOrder := TPluginHelpers.BasePluginLoadOrder(PluginsList, filename);
        sl.AddObject(filename, TObject(iLoadOrder));
      end;
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

{ Checks if a BSA exists associated with the given filename }
function BSAExists(filename: string): boolean;
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
  if BSAExists(filename) then begin
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
  if BSAExists(filename) then begin
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
  if BSAExists(filename) then begin
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

{ Comparator for sorting plugins }
function LoadOrderCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  LO1, LO2: Integer;
begin
  LO1 := Integer(List.Objects[Index1]);
  LO2 := Integer(List.Objects[Index2]);
  Result := LO1 - LO2;
end;


{******************************************************************************}
{ Record Prototyping Functions
  - GetElementObj
  - CreateRecordObj
  - GetRecordObj
  - GetRecordDef
  - BuildElementDef
  - BuildRecordDef
  - GetEditableFileContainer
}
{******************************************************************************}

{
  GetElementObj:
  Gets the child json object from a node in a TSmashSetting tree
  @obj matching @name.  Returns nil if a matching child is not
  found.
}
function GetElementObj(var obj: ISuperObject; name: string): ISuperObject;
var
  item: ISuperObject;
begin
  Result := nil;
  if not Assigned(obj) then
    exit;
  if not Assigned(obj['c']) then
    exit;
  for item in obj['c'] do begin
    if item.S['n'] = name then begin
      Result := item;
      exit;
    end;
  end;
end;

function CreateRecordObj(var tree: ISuperObject; rec: IwbMainRecord): ISuperObject;
var
  item: ISuperObject;
begin
  item := SO;
  item.S['n'] := rec.Signature;
  item.I['t'] := Ord(stRecord);
  tree.A['records'].Add(item);
  Result := item;
end;

function GetRecordObj(var tree: ISuperObject; name: string): ISuperObject;
var
  aSignature: TwbSignature;
  item: ISuperObject;
begin
  Result := nil;
  aSignature := StrToSignature(name);
  for item in tree['records'] do begin
    if StrToSignature(item.S['n']) = aSignature then
      Result := item;
  end;
end;

function GetRecordDef(sig: TwbSignature): TwbRecordDefEntry;
var
  i: Integer;
  def: TwbRecordDefEntry;
begin
  for i := Low(wbRecordDefs) to High(wbRecordDefs) do begin
    def := wbRecordDefs[i];
    if def.rdeSignature = sig then begin
      Result := def;
      exit;
    end;
  end;
end;

function BuildElementDef(element: IwbElement): ISuperObject;
var
  container: IwbContainerElementRef;
  i: Integer;
  childElement: IwbElement;
begin
  // release object if something goes wrong
  Result := SO;
  try
    Result.S['n'] := element.Name;
    Result.I['t'] := Ord(GetSmashType(element));

    // populate element children, if it supports them
    if not Supports(element, IwbContainerElementRef, container) then
      exit;
    // assign to container if it doesn't have element but can hold them
    if (container.ElementCount = 0)
    and container.CanAssign(High(Integer), nil, false) then try
      container.Assign(High(Integer), nil, false);
    except
      // oops, container assignment failed
      // this catches an assertion error when assigning to a DOBJ record
      on x: Exception do
        exit;
    end;

    // if we have children, make children array and recurse
    if container.ElementCount > 0 then begin
      Result.O['c'] := SA([]);
      // traverse children
      for i := 0 to Pred(container.ElementCount) do begin
        childElement := container.Elements[i];
        Result.A['c'].Add(BuildElementDef(childElement));
      end;
    end;
  except
    on x: Exception do begin
      Result._Release;
      raise x;
    end;
  end;
end;

function IsUnionDef(def: IwbNamedDef; out unionDef: IwbUnionDef): Boolean;
var
  subDef: IwbSubRecordDef;
begin
  if Supports(def, IwbSubRecordDef, subDef) then
    Result := Supports(subDef.GetValue, IwbUnionDef, unionDef)
  else
    Result := Supports(def, IwbUnionDef, unionDef);
end;

function HasDef(recObj: ISuperObject; name: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(recObj.A['c'].Length) do
    if recObj.A['c'].O[i].S['n'] = name then begin
      Result := True;
      exit;
    end;
end;

procedure AddDefIfMissing(recObj: ISuperObject; def: IwbNamedDef; name: String);
begin
  if not HasDef(recObj, name) then
    recObj.A['c'].Add(BuildDef(def, name));
end;

function SigToStr(sig: TwbSignature): String;
var
  i: Integer;
begin
  for i := Low(sig) to High(sig) do
    if Ord(sig[i]) < 32 then
      sig[i] := AnsiChar(Ord('a') + Ord(sig[i]));
  Result := sig;
end;

procedure BuildChildDef(def: IwbNamedDef; recObj: ISuperObject);
var
  i: Integer;
  unionDef: IwbUnionDef;
  sigDef: IwbSignatureDef;
  recDef: IwbRecordDef;
  name: String;
begin
  if IsUnionDef(def, unionDef) then begin
    for i := 0 to Pred(unionDef.MemberCount) do
      BuildChildDef(unionDef.Members[i] as IwbNamedDef, recObj);
  end
  else if Supports(def, IwbSubRecordUnionDef) and Supports(def, IwbRecordDef, recDef) then begin
    for i := 0 to Pred(recDef.MemberCount) do
      BuildChildDef(recDef.Members[i] as IwbNamedDef, recObj);
  end
  else if Supports(def, IwbSignatureDef, sigDef) then begin
    name := SigToStr(sigDef.DefaultSignature) + ' - ' + sigDef.Name;
    AddDefIfMissing(recObj, def, name);
  end
  else
    AddDefIfMissing(recObj, def, def.Name);
end;

procedure BuildChildDefs(obj: ISuperObject; def: IwbNamedDef);
var
  i: Integer;
  subDef: IwbSubRecordDef;
  recDef: IwbRecordDef;
  unionDef: IwbUnionDef;
  structDef: IwbStructDef;
  intDef: IwbIntegerDefFormaterUnion;
  sraDef: IwbSubRecordArrayDef;
  aDef: IwbArrayDef;
begin
  // try SubRecordDef ValueDef
  if Supports(def, IwbSubRecordDef, subDef) then
    BuildChildDefs(obj, subDef.GetValue as IwbNamedDef)
  // try IwbRecordDef
  else if Supports(def, IwbRecordDef, recDef) then begin
    if recDef.MemberCount = 0 then exit;
    obj.O['c'] := SA([]);
    for i := 0 to Pred(recDef.MemberCount) do
      BuildChildDef(recDef.Members[i] as IwbNamedDef, obj);
  end
  // try IwbUnionDef
  else if Supports(def, IwbUnionDef, unionDef) then begin
    if unionDef.MemberCount = 0 then exit;
    obj.O['c'] := SA([]);
    for i := 0 to Pred(unionDef.MemberCount) do
      BuildChildDef(unionDef.Members[i] as IwbNamedDef, obj);
  end
  // try IwbStructDef
  else if Supports(def, IwbStructDef, structDef) then begin
    if structDef.MemberCount = 0 then exit;
    obj.O['c'] := SA([]);
    for i := 0 to Pred(structDef.MemberCount) do
      BuildChildDef(structDef.Members[i] as IwbNamedDef, obj);
  end
  // try IwbIntegerDefFormaterUnion
  else if Supports(def, IwbIntegerDefFormaterUnion, intDef) then begin
    if intDef.MemberCount = 0 then exit;
    obj.O['c'] := SA([]);
    for i := 0 to Pred(intDef.MemberCount) do
      BuildChildDef(intDef.Members[i] as IwbNamedDef, obj);
  end
  // try IwbSubRecordArrayDef
  else if Supports(def, IwbSubRecordArrayDef, sraDef) then begin
    obj.O['c'] := SA([]);
    BuildChildDef(sraDef.Element as IwbNamedDef, obj);
  end
  // try IwbArrayDef
  else if Supports(def, IwbArrayDef, aDef) then begin
    obj.O['c'] := SA([]);
    BuildChildDef(aDef.Element as IwbNamedDef, obj);
  end;
end;

function BuildDef(def: IwbNamedDef; name: string): ISuperObject;
begin
  // release object if something goes wrong
  Result := SO;
  try
    Result.S['n'] := name;
    Result.I['t'] := Ord(SmashType(def));
    BuildChildDefs(Result, def);
  except
    on x: Exception do begin
      Result._Release;
      raise x;
    end;
  end;
end;

function BuildRecordDef(sName: string; mrDef: IwbRecordDef; out recObj: ISuperObject): boolean; overload;
var
  i: Integer;
begin
  recObj := SO;
  try
    recObj.S['n'] := sName;
    recObj.I['t'] := Ord(stRecord);
    recObj.O['c'] := SA([]);
    for i := 0 to Pred(mrDef.MemberCount) do
      BuildChildDef(mrDef.Members[i] as IwbNamedDef, recObj);
  except
    on x: Exception do begin
      recObj._Release;
      raise x;
    end;
  end;

  // if everything completed, result is object we made
  Result := true;
end;

function BuildRecordDef(sName: string; out recObj: ISuperObject): boolean;
var
  def: TwbRecordDefEntry;
begin
  def := GetRecordDef(StrToSignature(sName));
  Result := BuildRecordDef(sName, def.rdeDef, recObj);
end;

function GetEditableFileContainer: IwbContainerElementRef;
var
  i: Integer;
  aPlugin: TBasePlugin;
  aFile: IwbFile;
  Container: IwbContainerElementRef;
begin
  Result := nil;
  i := 0;
  repeat
    // exit if max index reached
    if i > Pred(PluginsList.Count) then
      exit;

    // get next plugin
    aPlugin := TBasePlugin(PluginsList[i]);
    Inc(i);

    // exit if file is invalid
    aFile := aPlugin._File;
    if not Supports(aFile, IwbContainerElementRef, Container) then
      exit;
  until Container.IsElementEditable(nil);
  Result := Container;
end;

procedure PopulateAddList(var AddItem: TMenuItem; Event: TNotifyEvent);
var
  i: Integer;
  RecordDef: PwbRecordDef;
  item: TMenuItem;
begin
  // populate wbGroupOrder to additem
  with TStringList.Create do try
    Sorted := True;
    Duplicates := dupIgnore;

    // initialize list contents
    AddStrings(wbGroupOrder);
    Sorted := False;

    // get record def names, if available
    for i := Pred(Count) downto 0 do
      if wbFindRecordDef(AnsiString(Strings[i]), RecordDef) then
        Strings[i] := Strings[i] + ' - ' + RecordDef.Name
      else
        Delete(i);

    // populate menu items
    for i := 0 to Pred(Count) do begin
      if Length(Strings[i]) < 4 then
        continue;
      item := TMenuItem.Create(AddItem);
      item.Caption := Strings[i];
      item.OnClick := Event;
      AddItem.Add(item);
    end;
  finally
    Free;
  end;
end;

initialization
begin
  PluginsList := TList.Create;
end;

finalization
begin
  FreeList(PluginsList);
end;

end.
