unit msCore;

interface

uses
  Classes, Menus, ComCtrls, RegularExpressions,
  // third party libraries
  superobject,
  // mte units
  mteBase;

type
  // SMASH CLASSES
  TElementData = class(TObject)
  public
    priority: byte;
    process: boolean;
    preserveDeletions: boolean;
    overrideDeletions: boolean;
    forceValue: boolean;
    singleEntity: boolean;
    smashType: TSmashType;
    linkTo: string;
    linkFrom: string;
    constructor Create; overload;
    constructor Create(priority: byte; process, preserveDeletions,
      overrideDeletions, singleEntity, forceValue: boolean;
      smashType: TSmashType; linkTo, linkFrom: string); overload;
  end;
  TSmashSetting = class(TObject)
  public
    name: string;
    hash: string;
    description: string;
    records: string;
    tree: ISuperObject;
    color: Int64;
    bVirtual: boolean;
    constructor Create;
    destructor Destroy; override;
    constructor Clone(s: TSmashSetting);
    function GetRecordDef(sig: string): ISuperObject;
    procedure LoadDump(dump: ISuperObject);
    function Dump: ISuperObject;
    procedure UpdateHash;
    procedure UpdateRecords;
    procedure Save;
    procedure Delete;
    procedure Rename(newName: string);
    function MatchesHash(hash: string): boolean;
    function GetTags: String;
    function GetCombinedTags: String;
  end;
  {TRecommendation = class(TObject)
  public
    game: string;
    username: string;
    filename: string;
    hash: string;
    setting: string;
    settingHash: string;
    recordCount: integer;
    rating: integer;
    smashVersion: string;
    notes: string;
    dateSubmitted: TDateTime;
    procedure SetNotes(notes: string);
    function GetNotes: string;
    procedure Save(const filename: string);
  end; }
  // SMASH CORE CLASSES
  TPatchStatusID = ( psUnknown, psNoPlugins, psDirInvalid, psUnloaded,
    psErrors, psFailed, psUpToDate, psUpToDateForced, psBuildReady,
    psRebuildReady, psRebuildReadyForced );
  TPatchStatus  = Record
    id: TPatchStatusID;
    color: integer;
    desc: string[64];
  end;
  TPlugin = class(TBasePlugin)
  public
    setting: string;
    smashSetting: TSmashSetting;
    patch: string;
    constructor Create; override;
    procedure GetMsData;
    procedure GetDataPath;
    function GetFormIndex: Integer;
    function IsInPatch: boolean;
    procedure LoadInfoDump(obj: ISuperObject);
    function InfoDump: ISuperObject;
    function HasTags: Boolean;
    procedure ApplySettingTags;
    procedure SetSmashSetting(aSetting: TSmashSetting);
    procedure LoadTags(sSettingName: String; var sl: TStringList;
      var sTagGroup: String);
    procedure GetSettingTag;
    procedure WriteDescription;
    procedure Save;
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
    smashSettings: TStringList;
    masters: TStringList;
    fails: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    function Dump: ISuperObject;
    procedure LoadDump(obj: ISuperObject);
    function GetTimeCost: integer;
    procedure UpdateHashes;
    procedure UpdateSettings;
    procedure GetStatus;
    procedure UpdateDataPath;
    procedure GetLoadOrders;
    procedure SortPlugins;
    procedure Remove(plugin: TPlugin); overload;
    procedure Remove(pluginFilename: string); overload;
    function PluginsModified: boolean;
    function FilesExist: boolean;
    function GetStatusColor: integer;
  end;
  TSettingHelpers = class
    class function SettingByName(name: String): TSmashSetting;
    class function SettingByHash(hash: String): TSmashSetting;
    class function GetSmashSetting(setting: string): TSmashSetting;
  end;
  TPatchHelpers = class
    class function CreateNewPatch(var patches: TList): TPatch;
    class function GetPatchForPlugin(filename: string): string;
    class procedure AssignPatchesToPlugins;
    class function PatchByName(var patches: TList; name: string): TPatch;
    class function PatchByFilename(var patches: TList; filename: string): TPatch;
  end;

  // Loading/Saving Functions
  procedure RenameSavedPlugins;
  procedure SavePatches;
  procedure LoadPatches;
  procedure SaveSmashSettings;
  procedure LoadSmashSettings;
  procedure SavePluginInfo;
  procedure LoadPluginInfo;
  procedure LoadSettingTags;
  // Helper Functions
  procedure HandleCanceled(msg: string);

  procedure UpdatePluginData;
  function CreateNewPlugin(sFilename: string): TPlugin;
  function PluginLoadOrder(sFilename: string): Integer;
  function PluginByFilename(sFilename: string): TPlugin;
  procedure PopulateAddList(var AddItem: TMenuItem; Event: TNotifyEvent);
  procedure AddAllRecords(currentSetting: TSmashSetting; var tv: TTreeView);
  procedure RemoveSettingFromPlugins(aSetting: TSmashSetting);
  function GetTagString(var slTags: TStringList): String;
  // Tree Helper Functions
  procedure BuildTreeFromPlugins(var tv: TTreeView; var sl: TStringList;
    tree: ISuperObject);
  procedure SetChildren(node: TTreeNode; state: Integer);
  procedure UpdateParent(node: TTreeNode);
  procedure CheckBoxManager(node: TTreeNode);
  procedure LoadElement(var tv: TTreeView; node: TTreeNode; obj: ISuperObject;
    bWithinSingle: boolean);
  procedure LoadTree(var tv: TTreeView; aSetting: TSmashSetting);
  function GetRecordObject(tree: ISuperObject; sig: string): ISuperObject;
  function GetChild(obj: ISuperObject; name: string): ISuperObject;
  procedure MergeChildren(srcObj, dstObj: ISuperObject);
  function CreateCombinedSetting(var sl: TStringList; name: string;
    bVirtual: boolean = false): TSmashSetting;
  function CombineSettingTrees(var lst: TList; var slSettings: TStringList): boolean;
  // Tag Helper Functions
  function ClearTags(sDescription: String): String;
  procedure GetMissingTags(var slPresent, slMissing: TStringList);
  procedure ExtractTags(var match: TMatch; var sl: TStringList;
    var sTagGroup: String);
  procedure ParseTags(description: string; var sl: TStringList);


const
  // IMPORTANT CONSTANTS
  ProgramTesters = ' ';
  ProgramTranslators = ' ';
  xEditVersion = '3.1.1';

  // CHECKBOX STATES
  csUnknown = 0;
  csChecked = 1;
  csUnChecked = 2;
  csPartiallyChecked = 3;

  // SMASH TYPE ARRAYS
  stArrays = [ stUnsortedArray, stUnsortedStructArray,
    stSortedArray, stSortedStructArray ];
  stValues = [ stString, stFloat, stInteger, stByteArray ];

  // PATCH STATUSES
  StatusArray: array[0..10] of TPatchStatus = (
    ( id: psUnknown; color: $808080; desc: 'Unknown'; ),
    ( id: psNoPlugins; color: $0000FF; desc: 'Need two or more plugins to patch'; ),
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
  BuildStatuses = [psBuildReady, psRebuildReady, psRebuildReadyForced, psFailed];
  RebuildStatuses = [psRebuildReady, psRebuildReadyForced, psFailed];
  ForcedStatuses = [psUpToDateForced, psRebuildReadyForced];
  ResolveStatuses = [psNoPlugins, psDirInvalid, psUnloaded, psErrors];
  FailedStatuses = [psFailed];

var
  PatchesList, SmashSettings, pluginsToHandle, patchesToBuild: TList;
  ActiveMods, SavedPluginPaths: TStringList;
  ActiveModProfile, xEditLogGroup, xEditLogLabel, DictionaryFilename: string;

implementation

uses
  Windows, SysUtils, Dialogs, Graphics,
  // mte units
  mteLogger, mteTracker, mteHelpers, mteLogging, RttiJson, CRC32,
  // mp units
  msConfiguration,
  // xEdit units
  wbInterface, wbImplementation;

{ TPlugin Constructor }
constructor TPlugin.Create;
begin
  patch := ' ';
  inherited;
end;

{ Fetches data associated with a plugin. }
procedure TPlugin.GetMsData;
begin
  hasData := true;

  // get data path and hash
  GetDataPath;
  GetHash;

   // get numOverrides if not blacklisted
  if (numRecords < 10000) then
    numOverrides := CountOverrides(_File);

  // call get data method
  GetData(PluginsList);
end;

procedure TPlugin.GetDataPath;
begin
  dataPath := wbDataPath;
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
var
  aSetting: TSmashSetting;
begin
  aSetting := TSettingHelpers.SettingByName(obj.AsObject.S['setting']);
  SetSmashSetting(aSetting);
end;

procedure TPlugin.SetSmashSetting(aSetting: TSmashSetting);
begin
  if not Assigned(aSetting) then begin
    setting := 'Skip';
    smashSetting := TSettingHelpers.SettingByName(setting);
  end
  else begin
    setting := aSetting.name;
    smashSetting := aSetting;
    Logger.Write('PLUGIN', 'Settings', 'Using '+setting+' for '+filename);
  end;
end;

function TPlugin.HasTags: Boolean;
var
  regex: TRegex;
  match: TMatch;
begin
  regex := TRegEx.Create('{{([a-zA-Z]{1,10}:){0,1}([^}]*)}}');
  match := regex.Match(description.Text);
  Result := match.Success;
end;

procedure TPlugin.ApplySettingTags;
var
  sTags: String;
begin
  description.Text := ClearTags(description.Text);
  sTags := smashSetting.GetTags;

  // write tags to the description
  description.Add(' ');
  description.Add(sTags);
  description.Text := Trim(description.Text);
  WriteDescription;
end;

procedure TPlugin.LoadTags(sSettingName: String; var sl: TStringList;
  var sTagGroup: String);
var
  slRecords: TStringList;
  aSetting: TSmashSetting;
  settingsToCombine: TList;
  i: Integer;
begin
  // if only one setting present, use it
  if sl.Count = 1 then begin
    aSetting := TSettingHelpers.GetSmashSetting(sl[0]);
    SetSmashSetting(aSetting);
  end
  // else make a combined setting
  else begin
    settingsToCombine := TList.Create;

    // loop through found settings
    for i := Pred(sl.Count) downto 0 do begin
      aSetting := TSettingHelpers.GetSmashSetting(sl[i]);
      if not Assigned(aSetting) then begin
        sl.Delete(i);
        continue;
      end;
      settingsToCombine.Add(aSetting);
    end;

    // if settingsToCombine has 0 settings, set to skip setting
    if settingsToCombine.Count = 0 then
      SetSmashSetting(nil)
    // if settingToCombine has only 1 setting, use that setting
    else if settingsToCombine.Count = 1 then
      SetSmashSetting(settingsToCombine[0])
    // else build a combined setting
    else begin
      Logger.Write('PLUGIN', 'Settings', 'Building combined setting');
      slRecords := TStringList.Create;
      CombineSettingTrees(settingsToCombine, slRecords);
      if sTagGroup <> '' then
        sSettingName := sTagGroup + '.' + sSettingName;
      aSetting := CreateCombinedSetting(slRecords, sSettingName, true);
      SetSmashSetting(aSetting);
    end;
  end;
end;

procedure TPlugin.GetSettingTag;
var
  regex: TRegEx;
  match: TMatch;
  sTagGroup: String;
  sl: TStringList;
begin
  // get setting tags from description
  regex := TRegEx.Create('{{([a-zA-Z]{1,10}:){0,1}([^}]*)}}');
  match := regex.Match(description.Text);
  sl := TStringList.Create;

  // set to skip setting if no tag is found
  if not match.Success then begin
    Logger.Write('PLUGIN', 'Tags', 'No tags found for '+filename);
    setting := 'Skip';
    smashSetting := TSettingHelpers.SettingByName(setting);
  end
  // else parse settings from tag
  else begin
    Logger.Write('PLUGIN', 'Tags', 'Found tag '+match.Value+' for '+filename);
    ExtractTags(match, sl, sTagGroup);
    LoadTags(match.Groups.Item[2].Value, sl, sTagGroup);
  end;

  // free memory
  sl.Free;
end;

procedure TPlugin.WriteDescription;
var
  Container: IwbContainer;
begin
  Container := _File as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  Container.SetElementEditValue('SNAM - Description', description.Text);
end;

procedure TPlugin.Save;
var
  path: string;
  FileStream: TFileStream;
begin
  // save plugin
  path := dataPath + filename + '.save';
  Tracker.Write(' ');
  Tracker.Write('Saving: ' + path);
  Logger.Write('PLUGIN', 'Save', path);
  FileStream := nil;
  try
    FileStream := TFileStream.Create(path, fmCreate);
    _File.WriteToStream(FileStream, False);
    if SavedPluginPaths.IndexOf(path) = -1 then
      SavedPluginPaths.Add(dataPath + filename);
  except
    on x: Exception do
      Tracker.Write('Failed to save: '+x.Message);
  end;
  TryToFree(FileStream);
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
  smashSettings := TStringList.Create;
  masters := TStringList.Create;
  fails := TStringList.Create;
end;

destructor TPatch.Destroy;
begin
  plugins.Free;
  hashes.Free;
  smashSettings.Free;
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

  // plugins, pluginHashes, pluginSettings, masters
  obj.O['plugins'] := SA([]);
  for i := 0 to Pred(plugins.Count) do
    obj.A['plugins'].S[i] := plugins[i];
  obj.O['pluginHashes'] := SA([]);
  for i := 0 to Pred(hashes.Count) do
    obj.A['pluginHashes'].S[i] := hashes[i];
  obj.O['pluginSettings'] := SA([]);
  for i := 0 to Pred(smashSettings.Count) do
    obj.A['pluginSettings'].S[i] := smashSettings[i];
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
  try
    for item in obj['pluginSettings'] do
      smashSettings.Add(item.AsString);
  except
    on x: Exception do
      // nothing
  end;
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
  Result := 10000;
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
  if (plugins.Count <> hashes.Count)
  or (plugins.Count <> smashSettings.Count) then begin
    Logger.Write('PATCH', 'Status', name + ' -> Plugin count changed');
    Result := true;
    exit;
  end;
  // true if any plugin hash doesn't match
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.hash <> hashes[i] then begin
        Logger.Write('PATCH', 'Status', name + ' -> '+plugin.filename + ' hash changed.');
        Result := true;
      end;
    end;
  end;
  // true if any plugin setting doesn't match
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.setting <> smashSettings[i] then begin
        Logger.Write('PATCH', 'Status', name + ' -> '+plugin.filename + ' smash setting changed.');
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

procedure TPatch.UpdateDataPath;
begin
  dataPath := settings.patchDirectory;
  if not SameText(dataPath, wbDataPath) then
    dataPath := dataPath + name + '\';
end;

procedure TPatch.GetStatus;
var
  i: Integer;
  plugin: TPlugin;
begin
  Logger.Write('PATCH', 'Status', name + ' -> Getting status');
  status := psUnknown;

  // don't patch if there aren't two or more plugins to patch
  if (plugins.Count < 2) then begin
    Logger.Write('PATCH', 'Status', name + ' -> Need two or more plugins to patch');
    status := psNoPlugins;
    exit;
  end;

  // don't patch if mod destination directory is blank
  if (settings.patchDirectory = '') then begin
    Logger.Write('PATCH', 'Status', name + ' -> Patch directory blank');
    status := psDirInvalid;
    exit;
  end;

  // update the patch's data path
  UpdateDataPath;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Logger.Write('PATCH', 'Status', name + ' -> Plugin '+plugins[i]+' is missing');
      if status = psUnknown then status := psUnloaded;
      continue;
    end;
  end;

  // check plugins were modified or files were deleted before
  // giving patch the up to date status
  if (not PluginsModified) and FilesExist and (status = psUnknown) then begin
    Logger.Write('PATCH', 'Status', name + ' -> Up to date');
    status := psUpToDate;
  end;

  // status green, ready to go
  if status = psUnknown then begin
    Logger.Write('PATCH', 'Status', name + ' -> Ready to be patchd');
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

// Update the settings list for the plugins in the patch
procedure TPatch.UpdateSettings;
var
  i: Integer;
  aPlugin: TPlugin;
begin
  smashSettings.Clear;
  for i := 0 to Pred(plugins.Count) do begin
    aPlugin := PluginByFilename(plugins[i]);
    if Assigned(aPlugin) then
      smashSettings.Add(aPlugin.setting);
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
  plugins.CustomSort(LoadOrderCompare);
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

{ TSettingHelpers }

{ Gets a smash setting matching the given name. }
class function TSettingHelpers.SettingByName(name: string): TSmashSetting;
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

{ Gets a smash setting matching the given hash. }
class function TSettingHelpers.SettingByHash(hash: string): TSmashSetting;
var
  i: integer;
  aSetting: TSmashSetting;
begin
  Result := nil;
  for i := 0 to Pred(SmashSettings.Count) do begin
    aSetting := TSmashSetting(SmashSettings[i]);
    if aSetting.MatchesHash(hash) then begin
      Result := aSetting;
      exit;
    end;
  end;
end;

{ Gets a smash setting matching a name or hash }
class function TSettingHelpers.GetSmashSetting(setting: string): TSmashSetting;
var
  sl: TStringList;
  smashSetting: TSmashSetting;
begin
  // default result
  Result := nil;

  // parse setting name and hash
  if Pos('|', setting) > 0 then begin
    sl := TStringList.Create;
    try
      sl.Delimiter := '|';
      sl.StrictDelimiter := true;
      sl.DelimitedText := setting;

      // if we have a setting name, use it to get a smash setting
      if Length(sl[0]) > 0 then begin
        smashSetting := SettingByName(sl[0]);
        // and return it if the hash matches
        if Assigned(smashSetting) and smashSetting.MatchesHash(sl[1]) then
          Result := smashSetting;
      end
      // else just get the setting from the hash
      else
        Result := SettingByHash(sl[1]);
    finally
      sl.Free;
    end;
  end
  // else just return SettingByName
  else
    Result := SettingByName(setting);
end;

{ TPatchHelpers }

class function TPatchHelpers.GetPatchForPlugin(filename: string): string;
var
  i: Integer;
  patch: TPatch;
begin
  Result := ' ';
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    if patch.plugins.IndexOf(filename) > -1 then begin
      Result := patch.name;
      break;
    end;
  end;
end;

class procedure TPatchHelpers.AssignPatchesToPlugins;
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

{ Gets a patch matching the given name. }
class function TPatchHelpers.PatchByName(var patches: TList; name: string): TPatch;
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


{ Gets a patch matching the given filename. }
class function TPatchHelpers.PatchByFilename(var patches: TList; filename: string): TPatch;
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
class function TPatchHelpers.CreateNewPatch(var patches: TList): TPatch;
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

{ TElementData }

constructor TElementData.Create;
begin
  self.priority := 0;
  self.smashType := TSmashType(0);
  self.linkTo := '';
  self.linkFrom := '';
end;

constructor TElementData.Create(priority: Byte; process, preserveDeletions,
  overrideDeletions, singleEntity, forceValue: Boolean; smashType: TSmashType;
  linkTo, linkFrom: string);
begin
  self.priority := priority;
  self.process := process;
  self.preserveDeletions := preserveDeletions;
  self.overrideDeletions := overrideDeletions;
  self.singleEntity := singleEntity;
  self.forceValue := forceValue;
  self.smashType := smashType;
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
  while Assigned(TSettingHelpers.SettingByName(Result)) do begin
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
  tree := SO;
  tree.O['records'] := SA([]);
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
begin
  Result := nil;
  if not Assigned(tree) then
    exit;
  Result := GetRecordObj(tree, sig);
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

procedure TSmashSetting.UpdateHash;
begin
  hash := StrCRC32(tree.AsJSon);
end;

procedure TSmashSetting.UpdateRecords;
var
  item: ISuperObject;
  sl: TStringList;
begin
  // prepare comma delimited stringlist
  sl := TStringList.Create;
  sl.Delimiter := ',';
  sl.StrictDelimiter := true;

  try
    // loop through records and add their signatures
    // to the stringlist if they are set to be processed
    for item in tree['records'] do begin
      if item.I['p'] = 1 then
        sl.Add(Copy(item.S['n'], 1, 4));
    end;

    // assign records the delimited signatures
    records := sl.DelimitedText;
  finally
    // free memory
    sl.Free;
  end;
end;

procedure TSmashSetting.Save;
var
  path: string;
begin
  UpdateHash;
  path := Format('%s\settings\%s\%s.json',
    [PathList.Values['ProgramPath'], ProgramStatus.GameMode.gameName, name]);
  ForceDirectories(ExtractFilePath(path));
  Dump.SaveTo(path);
end;

procedure TSmashSetting.Delete;
var
  path: string;
begin
  path := Format('%s\settings\%s\%s.json',
    [PathList.Values['ProgramPath'], ProgramStatus.GameMode.gameName, name]);
  if FileExists(path) then
    DeleteToRecycleBin(path, false);
end;

procedure TSmashSetting.Rename(newName: string);
var
  oldPath, newPath: string;
begin
  oldPath := Format('%s\settings\%s\%s.json',
    [PathList.Values['ProgramPath'], ProgramStatus.GameMode.gameName, name]);
  newPath := Format('%s\settings\%s\%s.json',
    [PathList.Values['ProgramPath'], ProgramStatus.GameMode.gameName, newName]);
  if FileExists(oldpath) then
    RenameFile(oldpath, newpath);
  name := newName;
end;

function TSmashSetting.MatchesHash(hash: string): boolean;
begin
  // result is true if hash is blank
  if hash = '' then begin
    Result := true;
    exit;
  end;

  // else result is true if the input hash matches setting's hash
  // starting at the first hexadecimal digit
  Result := Pos(hash, self.hash) = 2;
end;

function TSmashSetting.GetTags: String;
var
  index: Integer;
begin
  if Pos('Combined setting:', description) = 1 then
      Result := GetCombinedTags
  // else handle a normal setting
  else begin
    index := Pos('.', name);
    if (index > 0) and (index < 11) then
      Result := Format('{{%s}}', [StringReplace(name, '.', ':', [])])
    else
      Result := Format('{{%s}}', [name]);
  end;
end;

function TSmashSetting.GetCombinedTags: String;
var
  sl, slTags: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  slTags := TStringList.Create;
  try
    sl.Text := description;
    // parse tags from description
    slTags.CommaText := sl[1];

    // return tag string
    Result := GetTagString(slTags);
  finally
    sl.Free;
    slTags.Free;
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
  Tracker.Write(' ');
  Tracker.Write('Dumping patches to JSON');
  for i := 0 to Pred(PatchesList.Count) do begin
    Tracker.UpdateProgress(1);
    patch := TPatch(PatchesList[i]);
    Tracker.Write('  Dumping '+patch.name);
    json.A['patches'].Add(patch.Dump);
  end;

  // save and finalize
  filename := PathList.Values['ProfilePath'] + 'Patches.json';
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
  filename := PathList.Values['ProfilePath'] + 'Patches.json';
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
    if aSetting.bVirtual then
      continue;
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
  path := Format('%ssettings\%s\', [PathList.Values['ProgramPath'], ProgramStatus.GameMode.gameName]);
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
  if TSettingHelpers.SettingByName('Skip') = nil then
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
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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
  for i := 0 to Pred(PluginsList.Count) do try
    plugin := PluginsList[i];
    Tracker.UpdateProgress(1);
    if not Assigned(plugin.smashSetting) then
      continue;
  	index := IndexOfDump(obj.A['plugins'], plugin);
    if plugin.smashSetting.bVirtual or (plugin.setting = 'Skip') then begin
	    if index <> -1 then
	      obj.A['plugins'].Delete(index);
	    continue;
    end;
    Tracker.Write('  Dumping '+plugin.filename);
    if index = -1 then
      obj.A['plugins'].Add(plugin.InfoDump)
    else
      obj.A['plugins'].O[index] := plugin.InfoDump;
  except
    on x: Exception do
      Tracker.Write('  Exception '+x.Message);
  end;

  // save and finalize
  Tracker.Write(' ');
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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

procedure LoadSettingTags;
var
  i: Integer;
  plugin: TPlugin;
begin
  // loop through loaded plugins
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if plugin.setting <> '' then
      continue;
    plugin.GetSettingTag;
  end;
end;

procedure HandleCanceled(msg: string);
begin
  if Tracker.Cancel then
    raise Exception.Create(msg);
end;

procedure RenameSavedPlugins;
var
  i: Integer;
  oldFileName, newFileName, bakFileName: string;
begin
  // tracker message
  Tracker.Write(' ');
  Tracker.Write('Renaming saved plugins');

  for i := Pred(SavedPluginPaths.Count) downto 0 do try
    oldFileName := SavedPluginPaths[i];
    newFileName := oldFileName + '.save';
    bakFileName := oldFileName + '.bak';
    Tracker.Write(Format('    Renaming %s to %s', [ExtractFileName(newFileName), ExtractFileName(oldFileName)]));
    if FileExists(bakFileName) then
      DeleteFile(bakFileName);
    RenameFile(oldFileName, bakFileName);
    RenameFile(newFileName, oldFileName);
  except
    on x: Exception do
      Tracker.Write('      Failed to rename ' + newFileName);
  end;
end;

procedure UpdatePluginData;
var
  i: Integer;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    plugin.UpdateData;
  end;
end;

function CreateNewPlugin(sFilename: string): TPlugin;
begin
  Result := TPlugin(TPluginHelpers.CreateNewBasePlugin(PluginsList, sFilename));
end;

function PluginLoadOrder(sFilename: string): Integer;
begin
  Result := TPluginHelpers.BasePluginLoadOrder(PluginsList, sFilename);
end;

function PluginByFilename(sFilename: string): TPlugin;
begin
  Result := TPlugin(TPluginHelpers.BasePluginByFilename(PluginsList, sFilename));
end;

function DefDisplayName(var def: TwbRecordDefEntry): String;
var
  sig: String;
begin
  sig := String(def.rdeSignature);
  Result := def.rdeDef.Name;
  if (Result <> sig) then
    Result := sig + ' - ' + Result;
end;

procedure PopulateAddList(var AddItem: TMenuItem; Event: TNotifyEvent);
var
  i: Integer;
  recordDef: TwbRecordDefEntry;
  item: TMenuItem;
begin
  for i := Low(wbRecordDefs) to High(wbRecordDefs) do begin
    recordDef := wbRecordDefs[i];
    item := TMenuItem.Create(AddItem);
    item.Caption := DefDisplayName(recordDef);
    item.OnClick := Event;
    AddItem.Add(item);
  end;
end;

procedure AddAllRecords(currentSetting: TSmashSetting; var tv: TTreeView);
var
  i: Integer;
  recordDef: TwbRecordDefEntry;
  groupName: String;
  recObj: ISuperObject;
begin
  for i := Low(wbRecordDefs) to High(wbRecordDefs) do begin
    recordDef := wbRecordDefs[i];
    groupName := DefDisplayName(recordDef);
    recObj := GetRecordObj(currentSetting.tree, groupName);
    if Assigned(recObj) then continue;
    if not BuildRecordDef(groupName, recObj) then continue;
    currentSetting.tree.A['records'].Add(recObj);
    LoadElement(tv, tv.Items[0], recObj, false);
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
      plugin.smashSetting := TSettingHelpers.SettingByName('Skip');
    end;
  end;
end;

function GetTagString(var slTags: TStringList): String;
var
  i, index: Integer;
  tag, sTagGroup, sGroup: String;
begin
  for i := 0 to Pred(slTags.Count) do begin
    tag := slTags[i];
    index := Pos('.', tag);
    sTagGroup := Copy(tag, 1, index - 1);
    if (index > 0) and (index < 11) and (SameText(sGroup, sTagGroup) or (i = 0)) then
      sGroup := sTagGroup
    else
      sGroup := '';
  end;

  // generate the string of tags
  if sGroup <> '' then begin
    Result := StringReplace(slTags.CommaText, sGroup + '.', '', [rfReplaceAll, rfIgnoreCase]);
    Result := Format('{{%s:%s}}', [UpperCase(sGroup), Result])
  end
  else
    Result := Format('{{%s}}', [slTags.CommaText]);
end;


{******************************************************************************}
{ Tree Helper Functions
  - BuildTreeFromPlugins
  - SetChildren
  - UpdateParent
  - CheckBoxManager
  - LoadElement
}
{******************************************************************************}

procedure BuildTreeFromPlugins(var tv: TTreeView; var sl: TStringList;
  tree: ISuperObject);
var
  i, j: Integer;
  plugin: TPlugin;
  rec: IwbMainRecord;
  RecordDef: PwbRecordDef;
  def: TwbRecordDefEntry;
  sName, sSignature: string;
  slRecordSignatures: TStringList;
  recObj: ISuperObject;
begin
  slRecordSignatures := TStringList.Create;
  slRecordSignatures.Sorted := true;
  slRecordSignatures.Duplicates := dupIgnore;
  try
    // loop through plugins
    for i := 0 to Pred(sl.Count) do begin
      plugin := PluginByFileName(sl[i]);
      if not Assigned(plugin) then
        continue;
      if not plugin._File.IsEditable then
        continue;
      // loop through records
      for j := 0 to Pred(plugin._File.RecordCount) do begin
        rec := plugin._File.Records[j];
        // skip non-override records
        if rec.IsMaster then
          continue;
        sSignature := rec.Signature;

        // skip record signatures we've already seen
        if (slRecordSignatures.IndexOf(sSignature) > -1) then
          continue;
        slRecordSignatures.Add(sSignature);
        // skip records that aren't defined
        if not wbFindRecordDef(AnsiString(sSignature), RecordDef) then
          continue;

        // get record def object if it exists
        sName := sSignature + ' - ' + RecordDef.Name;
        recObj := GetRecordObj(tree, sName);

        // build record def if it doesn't exist
        if not Assigned(recObj) then begin
          def := GetRecordDef(rec.Signature);
          if not BuildRecordDef(sName, def.rdeDef, recObj) then
            continue;
          tree.A['records'].Add(recObj);
          LoadElement(tv, tv.Items[0], recObj, false);
        end;
      end;
    end;
  finally
    tv.Repaint;
    slRecordSignatures.Free;
  end;
end;

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
    e.process := state <> csUnChecked;
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
  // or if not is set to be treated as a single entity
  if not Assigned(node) then exit;
  e := TElementData(node.Data);
  if not Assigned(e) then exit;
  if e.singleEntity then exit;

  // parent state is checked if all siblings are checked
  state := csChecked;
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    if tmp.StateIndex <> csChecked then begin
      state := csPartiallyChecked;
      break;
    end;
    tmp := tmp.getNextSibling;
  end;

  // parent state is unchecked if all siblings are unchecked
  if state = csPartiallyChecked then begin
    state := csUnChecked;
    tmp := node.getFirstChild;
    while Assigned(tmp) do begin
      if tmp.StateIndex <> csUnChecked then begin
        state := csPartiallyChecked;
        break;
      end;
      tmp := tmp.getNextSibling;
    end;
  end;

  // set state, recurse to next parent
  node.StateIndex := state;
  e.process := state <> csUnChecked;
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
  if (node.StateIndex = csUnChecked)
  or (node.StateIndex = csPartiallyChecked) then begin
    node.StateIndex := csChecked;
    e := TElementData(node.Data);
    e.process := true;
    e.singleEntity := false;
    UpdateParent(node.Parent);
    SetChildren(node, csChecked);
  end
  // if checked, set to unchecked and set all children to
  // unchecked, update parents
  else if node.StateIndex = csChecked then begin
    node.StateIndex := csUnChecked;
    e := TElementData(node.Data);
    e.process := false;
    e.singleEntity := false;
    UpdateParent(node.Parent);
    SetChildren(node, csUnChecked);
  end;
end;

procedure LoadElement(var tv: TTreeView; node: TTreeNode; obj: ISuperObject;
  bWithinSingle: boolean);
var
  item: ISuperObject;
  child, nextChild: TTreeNode;
  bProcess, bPreserveDeletions, bOverrideDeletions, bIsSingle,
  bForceValue: boolean;
  priority: Integer;
  oSmashType: TSmashType;
  sName, sLinkTo, sLinkFrom: string;
  e: TElementData;
begin
  if not Assigned(obj) then
    exit;

  // load data from json
  sName := obj.S['n'];
  priority := obj.I['r'];
  bProcess := obj.I['p'] = 1;
  bPreserveDeletions := obj.I['d'] = 1;
  bOverrideDeletions := obj.I['o'] = 1;
  bIsSingle := obj.I['s'] = 1;
  bForceValue := obj.I['f'] = 1;
  bWithinSingle := bWithinSingle or bIsSingle;
  oSmashType := TSmashType(obj.I['t']);
  sLinkTo := obj.S['lt'];
  sLinkFrom := obj.S['lf'];

  // create child
  e := TElementData.Create(priority, bProcess, bPreserveDeletions,
    bOverrideDeletions, bIsSingle, bForceValue, oSmashType, sLinkTo, sLinkFrom);
  // nodes insert in sorted order for record nodes
  if (node.Level = 0) and node.hasChildren then begin
    child := node.getFirstChild;
    while (AnsiCompareText(child.Text, sName) < 0) do begin
      nextChild := child.getNextSibling;
      if not Assigned(nextChild) then
        break;
      child := nextChild;
    end;
    child := tv.Items.InsertObject(child, sName, e);
  end
  // else just add them in the order they were found
  else
    child := tv.Items.AddChildObject(node, sName, e);

  // set check state
  if bIsSingle then
    child.StateIndex := csPartiallyChecked
  else if bProcess then
    child.StateIndex := csChecked
  else
    child.StateIndex := csUnChecked;

  // recurse into children
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
  e := TElementData.Create;
  rootNode := tv.Items.AddObject(nil, 'Records', e);
  obj := aSetting.tree;
  if not Assigned(obj) then
    exit;
  if not Assigned(obj['records']) then
    exit;

  for item in obj['records'] do
    LoadElement(tv, rootNode, item, false);
end;

function GetRecordObject(tree: ISuperObject; sig: string): ISuperObject;
var
  item: ISuperObject;
begin
  Result := nil;
  for item in tree['records'] do begin
    if Copy(item.S['n'], 1, 4) = sig then begin
      Result := item;
      break;
    end;
  end;
end;

function GetChild(obj: ISuperObject; name: string): ISuperObject;
var
  child: ISuperObject;
begin
  Result := nil;
  for child in obj['c'] do begin
    if child.S['n'] = name then begin
      Result := child;
      exit;
    end;
  end;
end;

procedure MergeChildren(srcObj, dstObj: ISuperObject);
var
  srcChild, dstChild: ISuperObject;
begin
  for srcChild in srcObj['c'] do begin
    dstChild := GetChild(dstObj, srcChild.S['n']);
    if not Assigned(dstChild) then
      dstObj.A['c'].Add(srcChild.Clone)
    else begin
      // merge force value
      if srcChild.I['f'] = 1 then
        dstChild.I['f'] := 1;
      // merge treat as single
      if srcChild.I['s'] = 1 then
        dstChild.I['s'] := 1;
      // merge preserve deletions
      if srcChild.I['d'] = 1 then
        dstChild.I['d'] := 1;
      // merge override deletions
      if srcChild.I['o'] = 1 then
        dstChild.I['o'] := 1;
      // merge process
      if srcChild.I['p'] = 1 then
        dstChild.I['p'] := 1;
      // merge links
      if srcChild.S['lt'] <> '' then
        dstChild.S['lt'] := srcChild.S['lt'];
      if srcChild.S['lf'] <> '' then
        dstChild.S['lf'] := srcChild.S['lf'];
      // recurse into children if present
      if Assigned(srcChild.A['c']) then begin
        if Assigned(dstChild.A['c']) then
          MergeChildren(srcChild, dstChild)
        else
          dstChild.O['c'] := srcChild.O['c'].Clone;
      end;
    end;
  end;
end;

function CreateCombinedSetting(var sl: TStringList; name: string;
  bVirtual: boolean = false): TSmashSetting;
var
  i, index: Integer;
  newSetting, aSetting: TSmashSetting;
  recordObj, existingRecordObj: ISuperObject;
begin
  newSetting := TSmashSetting.Create;
  newSetting.tree := SO;
  newSetting.tree.O['records'] := SA([]);

  for i := 0 to Pred(sl.Count) do begin
    aSetting := TSmashSetting(sl.Objects[i]);
    recordObj := GetRecordObject(aSetting.tree, sl[i]);
    existingRecordObj := GetRecordObject(newSetting.tree, sl[i]);
    // if record object matching record signature already exists
    // merge the record objects
    if Assigned(existingRecordObj) then
      MergeChildren(recordObj, existingRecordObj)
    // else just add it to the tree
    else
      newSetting.tree.A['records'].Add(recordObj.Clone);
  end;
  newSetting.UpdateRecords;

  // set other attributes
  newSetting.UpdateHash;
  newSetting.bVirtual := bVirtual;
  newSetting.description := 'Combined setting:'#13#10 + name;
  index := Pos('.', name);
  if (index > 0) and (index < 11) then
    newSetting.name := Format('%sCombined-%s', [Copy(name, 1, index), newSetting.hash])
  else
    newSetting.name := 'Combined-'+newSetting.hash;

  // add new setting to SmashSettings list
  aSetting := TSettingHelpers.SettingByName(newSetting.name);
  if not Assigned(aSetting) then begin
    SmashSettings.Add(newSetting);
    Result := newSetting;
  end
  else begin
    newSetting.Free;
    Result := aSetting;
  end;
end;

function CombineSettingTrees(var lst: TList; var slSettings: TStringList): boolean;
var
  setting: TSmashSetting;
  sl: TStringList;
  i, j: Integer;
begin
  sl := TStringList.Create;
  Result := false;
  for i := 0 to Pred(lst.Count) do begin
    setting := TSmashSetting(lst[i]);
    sl.CommaText := setting.records;
    for j := 0 to Pred(sl.Count) do begin
      if slSettings.IndexOf(sl[j]) > -1 then
        Result := true;
      slSettings.AddObject(sl[j], TObject(setting));
    end;
  end;

  // free memory
  sl.Free;
end;


{******************************************************************************}
{ Tag Helper Functions
  - ClearTags
  - GetMissingTags
  - ExtractTags
  - GetTags
}
{******************************************************************************}

function ClearTags(sDescription: String): String;
var
  regex: TRegex;
  match: TMatch;
begin
  // find tags
  regex := TRegex.Create('{{([^}]*)}}');
  match := regex.Match(sDescription);

  // delete tags
  while match.Success do begin
    sDescription := StringReplace(sDescription, match.Value, '', []);
    match := match.NextMatch;
  end;

  // set description to the memo
  Result := Trim(sDescription);
end;

procedure GetMissingTags(var slPresent, slMissing: TStringList);
var
  i: Integer;
  aSetting: TSmashSetting;
begin
  for i := 0 to Pred(SmashSettings.Count) do begin
    aSetting := TSmashSetting(SmashSettings[i]);
    if slPresent.IndexOf(aSetting.name) = -1 then
      slMissing.Add(aSetting.name);
  end;
end;

procedure ExtractTags(var match: TMatch; var sl: TStringList;
  var sTagGroup: String);
var
  i: Integer;
begin
  sTagGroup := '';

  // split tag on commas
  sl.Delimiter := ',';
  sl.StrictDelimiter := true;
  sl.DelimitedText := match.Groups.Item[2].Value;

  // trim leading or trailing whitespace from tags
  for i := 0 to Pred(sl.Count) do
    sl[i] := Trim(sl[i]);

  // if tags are presented under a group, append the group name
  // and a . to the beginning of each setting name in the tag
  if match.Groups.Item[1].Value <> '' then begin
    sTagGroup := TitleCase(match.Groups.Item[1].Value);
    SetLength(sTagGroup, Length(sTagGroup) - 1);
    Logger.Write('PLUGIN', 'Tags', 'Parsing as ' + sTagGroup + ' tags');
    for i := 0 to Pred(sl.Count) do
      sl[i] := Format('%s.%s', [sTagGroup, sl[i]]);
  end;
end;

procedure ParseTags(description: string; var sl: TStringList);
var
  regex: TRegEx;
  match: TMatch;
  sTagGroup: String;
begin
  // get setting tags from description
  regex := TRegEx.Create('{{([a-zA-Z]{1,10}:){0,1}([^}]*)}}');
  match := regex.Match(description);

  // if match found, put the tags into the stringlist
  if match.success then
    ExtractTags(match, sl, sTagGroup);
end;

initialization
begin
  PatchesList := TList.Create;
  SmashSettings := TList.Create;
  SavedPluginPaths := TStringList.Create;
end;

finalization
begin
  FreeList(PatchesList);
  FreeList(SmashSettings);
  SavedPluginPaths.Free;
end;

end.
