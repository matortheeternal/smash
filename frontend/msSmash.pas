unit msSmash;

interface

uses
  Windows, SysUtils, Classes, ShellAPI, Controls, Dialogs,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteTracker, mteBase,
  // ms units
  msCore, msConfiguration, msConflict, msAlgorithm,
  // xEdit units
  wbInterface, wbImplementation;

  procedure BuildPatch(var patch: TPatch);
  procedure RebuildPatch(var patch: TPatch);

implementation

procedure BuildPluginsList(var patch: TPatch; var lst: TList);
var
  i: Integer;
  plugin: TPlugin;
begin
  for i := 0 to Pred(patch.plugins.Count) do begin
    plugin := PluginByFileName(patch.plugins[i]);
    if not Assigned(plugin) then
      raise Exception.Create('Couldn''t find plugin '+patch.plugins[i]);
    lst.Add(plugin);
  end;
end;

procedure SetPatchAttributes(var patch: TPatch);
var
  patchFile: IwbFile;
  fileHeader: IwbContainer;
begin
  patchFile := patch.plugin._File;
  fileHeader := patchFile.Elements[0] as IwbContainer;
  // set author
  fileHeader.ElementEditValues['CNAM'] := 'Mator Smash v' + ProgramStatus.Version;
  // set description
  fileHeader.ElementEditValues['SNAM'] := 'Smashed patch:'#13#10 + patch.plugins.Text;
end;

function GetPatchFile(var patch: TPatch; var lst: TList): IwbFile;
var
  plugin: TPlugin;
  bUsedExistingFile: boolean;
  patchFile: IwbFile;
  i: Integer;
begin
  // get plugin if it exists
  // else create it
  plugin := PluginByFilename(patch.filename);
  patch.plugin := nil;
  if Assigned(plugin) then begin
    bUsedExistingFile := true;
    patch.plugin := plugin;
  end
  else begin
    bUsedExistingFile := false;
    patch.plugin := CreateNewPlugin(patch.filename);
  end;

  // don't patch if patchFile not assigned
  if not Assigned(patch.plugin) then 
    raise Exception.Create('Couldn''t assign patch file');

  // don't patch if patchFile is at an invalid load order position relative
  // to the plugins being patched
  if bUsedExistingFile then begin
    for i := 0 to Pred(lst.Count) do begin
      plugin := TPlugin(lst[i]);
      if PluginsList.IndexOf(plugin) > PluginsList.IndexOf(patch.plugin) then
        raise Exception.Create(Format('%s is at a lower load order position than %s',
          [plugin.filename, patch.filename]));
    end;

    // clean up the patch file
    patchFile := patch.plugin._File;
    for i := Pred(patchFile.RecordCount) downto 0 do
      patchFile.Records[i].Remove;
  end;

  // set result
  Result := patch.plugin._File;
  Tracker.Write(' ');
  Tracker.Write('Patch is using plugin: '+patch.plugin.filename);
end;

procedure AddRequiredMasters(var patch: TPatch; var lst: TList);
var
  slMasters: TStringList;
  i: Integer;
  plugin: TPlugin;
begin
  slMasters := TStringList.Create;
  try
    Tracker.Write('Adding masters...');
    for i := 0 to Pred(lst.Count) do begin
      plugin := TPlugin(lst[i]);
      GetMasters(plugin._File, slMasters);
      slMasters.AddObject(plugin.filename, patch.plugins.Objects[i]);
    end;
    try
      slMasters.CustomSort(LoadOrderCompare);
      AddMasters(patch.plugin._File, slMasters);
      if settings.debugMasters then begin
        Tracker.Write('Masters added:');
        Tracker.Write(slMasters.Text);
        slMasters.Clear;
        GetMasters(patch.plugin._File, slMasters);
        Tracker.Write('Actual masters:');
        Tracker.Write(slMasters.Text);
      end;
    except
      on x: Exception do begin
        Tracker.Write('Critical exception adding masters!');
        Tracker.Write(x.Message);
        raise x;
      end;
    end;
  finally
    slMasters.Free;
    if Tracker.Cancel then
      raise Exception.Create('User cancelled smashing.');
    Tracker.Write('Done adding masters');
  end;
end;

procedure BuildOverridesList(var patch: TPatch; var lst: TList;
  var records: TInterfaceList);
var
  i, j, recCount: Integer;
  plugin: TPlugin;
  aSetting: TSmashSetting;
  aFile: IwbFile;
  rec: IwbMainRecord;
  recObj: ISuperObject;
begin
  Tracker.Write(' ');
  Tracker.Write('Processing files');
  for i := 0 to Pred(lst.Count) do begin
    if Tracker.Cancel then break;
    plugin := TPlugin(lst[i]);

    // get file and setting for later use
    aFile := plugin._File;
    aSetting := plugin.smashSetting;

    // loop through file records
    Tracker.Write('Processing '+plugin.filename);
    recCount := Pred(aFile.RecordCount);
    for j := 0 to recCount do begin
      if Tracker.Cancel then break;
      rec := aFile.Records[j];
      if j mod 500 = 499 then
        Tracker.UpdateProgress(500);

      try
        // skip non-override records
        if rec.IsMaster then
          continue;
        rec := rec.Master;

        if OverrideCountInFiles(rec, patch.plugins) < 2 then
          continue;

        // skip records according to smash setting
        recObj := aSetting.GetRecordDef(rec.Signature);
        if not Assigned(recObj) then
          continue;
        if (recObj.I['p'] <> 1) then
          continue;
        // skip non-conflicting records
        if ConflictAllForMainRecord(rec) < caConflict then
          continue;

        // add record to overrides list
        if records.IndexOf(rec) = -1 then
          records.Add(rec);
      except
        on x: Exception do begin
          Tracker.Write('  Error processing ' + rec.Name + ', ' + x.Message);
          continue;
        end;
      end;
    end;
    
    // update progress bar for file
    Tracker.UpdateProgress(recCount mod 500);
  end;
end;

procedure UpdateCounts(var rec: IwbMainRecord);
var
  container, arrayContainer: IwbContainerElementRef;
  i: Integer;
  element, nextElement: IwbElement;
begin
  // if reocrd is not editable, exit
  if not rec.IsEditable then
    exit;

  // if record can't be treated as a container, exit
  if not Supports(rec, IwbContainerElementRef, container) then
    exit;

  // loop through top-level elements
  for i := 0 to container.ElementCount - 2 do begin
    element := container.Elements[i];
    nextElement := container.Elements[i + 1];
    if not Supports(nextElement, IwbContainerElementRef, arrayContainer) then
      continue;

    // if next element is an array element and current element has the
    // word count in its name update the count to be the number of elements
    // in the array
    if (GetSmashType(nextElement) in stArrays)
    and (Pos('Count', element.Name) > 0) then try
      element.NativeValue := arrayContainer.ElementCount;
    except
      on x: Exception do
        Tracker.Write('    Exception updating count at '+element.Path);
    end;
  end;
end;

function HasPartialFormFlag(rec: IwbMainRecord): Boolean;
begin
  Result := ((rec.Signature = 'QUST') or (rec.Signature = 'LCTN')) and
    (rec.Flags._Flags and $00004000 <> 0);
end;

procedure SmashRecords(var patch: TPatch; var records: TInterfaceList);
var
  i, j: Integer;
  incProgress, currentProgress: Real;
  rec, mst, ovr, patchRec: IwbMainRecord;
  f, patchFile, forceFile: IwbFile;
  plugin: TPlugin;
  aSetting: TSmashSetting;
  recObj: ISuperObject;
  e, eCopy: IwbElement;
  bDeletions, bOverride, bForce: boolean;
begin
  Tracker.Write(' ');
  Tracker.Write('Smashing records');
  patchFile := patch.plugin._File;

  // loop through records to smash
  currentProgress := Tracker.GetProgress;
  incProgress := (Tracker.GetMaxProgress - Tracker.GetProgress) / records.Count;
  for i := 0 to Pred(records.Count) do begin
    if Tracker.Cancel then break;
    if not Supports(records[i], IwbMainRecord, rec) then
      exit;
    Tracker.StatusMessage(Format('Smashing records (%d/%d)',
      [i + 1, records.Count]));
    currentProgress := currentProgress + incProgress;
    Tracker.SetProgress(Round(currentProgress));

    // loop through record's overrides
    patchRec := nil;
    forceFile := nil;
    for j := 0 to Pred(rec.OverrideCount) do begin
      if Tracker.Cancel then break;
      ovr := rec.Overrides[j];
      f := ovr._File;
      // skip overrides that are in plugins we aren't patching
      if patch.plugins.IndexOf(f.FileName) = -1 then
        continue;
      plugin := PluginByFileName(f.FileName);
      if not Assigned(plugin) then
        continue;
      // skip ctIdenticalToMaster overrides
      if (ConflictThisForMainRecord(ovr) = ctIdenticalToMaster) then
        continue;

      // skip plugins that have the skip setting
      if plugin.setting = 'Skip' then
        continue;

      // skip overrides according to smash setting
      aSetting := plugin.smashSetting;
      recObj := aSetting.GetRecordDef(ovr.Signature);
      if not Assigned(recObj) then
        continue;
      if (recObj.I['p'] <> 1) then
        continue;
      bForce := recObj.I['f'] = 1;
      if bForce then begin
        if Assigned(patchRec) then begin
          patchRec.Remove;
          patchRec := nil;
        end;
        forceFile := f;
      end;

      // copy record to smashed patch if it hasn't been copied yet
      if not Assigned(patchRec) then try
        if bForce then
          e := ovr
        else
          e := WinningOverrideInFiles(rec, patch.plugins);
        Tracker.Write(Format('  [%d] Copying record %s', [i + 1, e.Name]));
        eCopy := wbCopyElementToFile(e, patchFile, false, true, '', '' ,'');
        patchRec := eCopy as IwbMainRecord;
        if bForce then continue;
      except
        on x: Exception do begin
          Tracker.Write('      Exception copying record '+ovr.Name+' : '+x.Message);
          patch.fails.Add('Exception copying record '+ovr.Name+' : '+x.Message);
          continue;
        end;
      end;

      // skip if we're forcing and plugin doesn't require forceFile
      if Assigned(forceFile) and not bForce
      and (plugin.masters.IndexOf(forceFile.FileName) = -1) then
        continue;

      // finally, recursively copy overridden elements
      try
        bDeletions := recObj.I['d'] = 1;
        if (wbGameMode = gmFO4) and HasPartialFormFlag(ovr) then
          bDeletions := False;
        bOverride := recObj.I['o'] = 1;
        if bForce then
          mst := e as IwbMainRecord
        else
          mst := WinningOverrideInFiles(rec, plugin.masters);
        Tracker.Write(Format('    Smashing override from: %s, master: %s',
          [f.FileName, mst._File.FileName]));
        rcore(IwbElement(ovr), IwbElement(mst), IwbElement(patchRec), patchRec,
          recObj, false, bDeletions, bOverride);
      except
        on x: Exception do begin
          Tracker.Write('      Exception smashing record: '+ovr.Name+' : '+x.Message);
          patch.fails.Add('Exception smashing record: '+ovr.Name+' : '+x.Message);
        end;
      end;

      // update any count elements on the record
      UpdateCounts(patchRec);
    end;
  end;
end;

function IsChildGroup(group: IwbGroupRecord): Boolean;
begin
  Result := group.GroupType in [1,6,7];
end;

function NativeContainer(element: IwbElement): IwbContainer;
var
  group: IwbGroupRecord;
begin
  if Supports(element, IwbGroupRecord, group) and IsChildGroup(group) then
    Result := group.ChildrenOf as IwbContainer
  else
    Result := element.Container;
  if not Assigned(Result) then
    raise Exception.Create('Could not find container for ' + element.Name);
end;

procedure RemoveEmptyContainers(aContainer: IwbContainer);
var
  container, nextContainer: IwbContainer;
  rec: IwbMainRecord;
  bITM, bITPO: Boolean;
begin
  container := aContainer;
  // traverse up container until we find an IwbMainRecord
  while Assigned(container) and 
  not Supports(container, IwbMainRecord, rec) do begin
    // break if container still has elements in it
    if container.ElementCount > 0 then
      exit;

    // else remove it and traverse up to next container
    nextContainer := NativeContainer(container);
    container.Remove;
    container := nextContainer;
  end;

  // exit if record is not ITM or ITPO
  bITM := IsITM(rec);
  bITPO := IsITPO(rec);
  if not (bITM or bITPO) then
    exit;

  // else remove MainRecord and recurse
  if bITM then
    Tracker.Write('    Removing ITM: ' + rec.Name)
  else
    Tracker.Write('    Removing ITPO: ' + rec.Name);
  nextContainer := rec.Container;
  rec.Remove;
  RemoveEmptyContainers(nextContainer);
end;

procedure RemoveITPOs(aFile: IwbFile);
var
  i, CountITPO: Integer;
  e, m: IwbMainRecord;
  container: IwbContainer;
  ITPOs: TDynMainRecords;
begin
  Tracker.Write(' ');
  Tracker.Write('Removing ITPO records from patch');
  CountITPO := 0;

  // loop through file's records
  for i := Pred(aFile.RecordCount) downto 0 do begin
    if Tracker.Cancel then break;
    e := aFile.Records[i];
    m := e.MasterOrSelf;

    // skip master records
    if e.IsMaster then
      continue;

    // skip records that have elements in child group (WRLD, CELL, DIAL)
    if Assigned(e.ChildGroup) and (e.ChildGroup.ElementCount > 0) then
      continue;

    // remove record if no conflicts
    if IsITPO(e) then begin
      Tracker.Write('    Removing ITPO: ' + e.Name);

      // add ITPO to list of records to remove
      SetLength(ITPOs, CountITPO + 1);
      ITPOs[CountITPO] := e;
      Inc(CountITPO);
    end;
  end;

  // remove the records
  for i := Pred(Length(ITPOs)) downto 0 do begin
    e := ITPOs[i];
    container := e.Container;
    e.Remove;
    try
      RemoveEmptyContainers(container);
    except
      on x: Exception do
        Tracker.Write('      Exception removing empty containers: '+x.Message);
    end;
  end;

  // finalization message
  Tracker.Write(Format('    Removed %d ITPO records', [CountITPO]));
end;

procedure CleanPatch(var patch: TPatch);
var
  patchFile: IwbFile;
begin
  patchFile := patch.plugin._File;

  // remove ITPOs
  try
    if not settings.preserveITPOs then
      RemoveITPOs(patchFile);
  except
    on x: Exception do
      Tracker.Write('    Exception removing ITPOs: '+x.Message);
  end;
end;

procedure SavePatchFiles(var patch: TPatch);
var
  patchFilePrefix, patchPath: string;
begin
  // update patch plugin hashes and settings
  patch.UpdateHashes;
  patch.UpdateSettings;

  // get path to save file at
  patchPath := patch.dataPath + 'smash\';
  ForceDirectories(patchPath);

  // save patch plugin
  patch.plugin.dataPath := patch.dataPath;
  patch.plugin.Save;

  // save files, fails, plugins
  patchFilePrefix := patchPath + ChangeFileExt(patch.filename, '');
  patch.fails.SaveToFile(patchFilePrefix+'_fails.txt');
  patch.plugins.SaveToFile(patchFilePrefix+'_plugins.txt');
end;

procedure BuildPatch(var patch: TPatch);
var
  patchFile: IwbFile;
  pluginsToPatch: TList;
  recordsList: TInterfaceList;
  time: TDateTime;
  msg: string;
begin
  // initialize
  Tracker.Write('Building patch: '+patch.name);
  time := Now;
  patch.fails.Clear;
  pluginsToPatch := TList.Create;
  msg := 'User cancelled smashing patches.';

  try
    // build list of plugins to patch
    BuildPluginsList(patch, pluginsToPatch);
    HandleCanceled(msg);
    
    // identify or create patch file
    patchFile := GetPatchFile(patch, pluginsToPatch);
    SetPatchAttributes(patch);

    // add masters to patch file
    AddRequiredMasters(patch, pluginsToPatch);
    HandleCanceled(msg);

    // build list of overrides
    recordsList := TInterfaceList.Create;
    BuildOverridesList(patch, pluginsToPatch, recordsList);
    HandleCanceled(msg);

    // stop smashing if no records to smash
    if recordsList.Count = 0 then
      raise Exception.create('No records to patch!');

    // smash records
    SmashRecords(patch, recordsList);
    HandleCanceled(msg);

    // clean patch (ITPOs)
    CleanPatch(patch);

    // save patch and associated files
    SavePatchFiles(patch);

    // update statistics
    if patch.status = psBuildReady then
      Inc(sessionStatistics.pluginsPatched, patch.plugins.Count);
    Inc(sessionStatistics.patchesBuilt);

    // finalization messages
    time := (Now - time) * 86400;
    patch.dateBuilt := Now;
    patch.status := psUpToDate;
    Tracker.Write(Format('Done smashing %s (%.3f)', [patch.name, Real(time)]));
  except
    on x: Exception do begin
      patch.status := psFailed;
      Tracker.Write(Format('Failed to patch %s, %s', [patch.name, x.Message]));
    end;
  end;

  // clean up
  TryToFree(pluginsToPatch);
  TryToFree(recordsList);
end;

procedure RebuildPatch(var patch: TPatch);
begin
  BuildPatch(patch);
end;

end.
