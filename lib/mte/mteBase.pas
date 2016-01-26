unit mteBase;

interface

uses
  Classes,
  // mte units
  mteTracker,
  // xEdit units
  wbHelpers, wbInterface, wbImplementation;

type
  TBasePlugin = class(TObject)
  public
    _File: IwbFile;
    hasData: boolean;
    hash: string;
    fileSize: Int64;
    dateModified: string;
    filename: string;
    numRecords: string;
    numOverrides: string;
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

  { Bethesda Plugin Functions }
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function ExtractFormID(filename: string): string;
  function RemoveFileIndex(formID: string): string;
  function LocalFormID(aRecord: IwbMainRecord): integer;
  function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
  function CountOverrides(aFile: IwbFile): integer;
  procedure AddRequiredBy(var lst: TList; filename: string;
    var masters: TStringList);
  procedure GetMasters(aFile: IwbFile; var sl: TStringList);
  procedure AddMasters(aFile: IwbFile; var sl: TStringList);
  function BSAExists(filename: string): boolean;
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
  function LoadOrderCompare(List: TStringList; Index1, Index2: Integer): Integer;

var
  PluginsList: TList;
  HeaderList: TList;

implementation

uses
  SysUtils,
  mteHelpers,
  mpConfiguration;

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
  numRecords := Container.GetElementEditValue('HEDR - Header\Number of Records');

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
    aFile := wbFile(wbDataPath + sl[i], -1, '', True, False);
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
begin
  // get plugin
  plugin := TPluginHelpers.BasePluginByFilename(HeaderList, filename);
  // add its masters to list
  if Assigned(plugin) then
    sl.AddStrings(plugin.masters);
end;

class procedure THeaderHelpers.GetPluginDependencies(filename: string;
  var sl: TStringList);
var
  plugin: TBasePlugin;
begin
  // get plugin
  plugin := TPluginHelpers.BasePluginByFilename(HeaderList, filename);
  // add its required by to @sl
  if Assigned(plugin) then
    sl.AddStrings(plugin.requiredBy);
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

initialization
begin
  PluginsList := TList.Create;
end;

finalization
begin
  FreeList(PluginsList);
end;

end.
