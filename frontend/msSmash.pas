unit msSmash;

interface

uses
  Windows, SysUtils, Classes, ShellAPI, Controls, Dialogs,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteTracker,
  // ms units
  msFrontend, msConflict, msAlgorithm,
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
  fileHeader.ElementEditValues['CNAM'] := 'Mator Smash v'+ProgramVersion;
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
    patchFile.CleanMasters;
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
      slMasters.CustomSort(PatchPluginsCompare);
      AddMasters(patch.plugin._File, slMasters);
      if settings.debugMasters then begin
        Tracker.Write('Masters added:');
        Tracker.Write(slMasters.Text);
        slMasters.Clear;
        GetMasters(patch.plugin._File, slMasters);
        Tracker.Write('Actual masters:');
        Tracker.Write(slMasters.Text);
      end;
    except on Exception do
      // nothing
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
  i, j, skips: Integer;
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
    // skip plugins that have the skip setting
    if plugin.setting = 'Skip' then
      continue;

    // get file and setting for later use
    aFile := plugin._File;
    aSetting := plugin.smashSetting;

    // loop through file records
    Tracker.Write('Processing '+plugin.filename);
    skips := 0;
    for j := 0 to Pred(aFile.RecordCount) do begin
      if Tracker.Cancel then break;
      rec := aFile.Records[j];
      // skip non-override records
      if rec.IsMaster then begin
        Inc(skips);
        continue;
      end;
      // skip records that have 1 or fewer overrides
      rec := rec.Master;
      if OverrideCountInFiles(rec, patch.plugins) < 2 then begin
        Inc(skips);
        continue;
      end;
      // skip records according to smash setting
      recObj := aSetting.GetRecordDef(rec.Signature);
      if not Assigned(recObj) then begin
        Inc(skips);
        continue;
      end;
      if (recObj.I['p'] <> 1) then begin
        Inc(skips);
        continue;
      end;

      // TODO: skip records that are ITM or ITPO
      // add record to overrides list
      if records.IndexOf(rec) = -1 then
        records.Add(rec);
    end;
    
    // update progress bar for file
    Tracker.UpdateProgress(skips);
  end;
end;

procedure SmashRecords(var patch: TPatch; var records: TInterfaceList);
var
  i: Integer;
  rec, ovr, patchRec: IwbMainRecord;
  j: Integer;
  f, patchFile: IwbFile;
  plugin: TPlugin;
  aSetting: TSmashSetting;
  recObj: ISuperObject;
  e, eCopy: IwbElement;
  bDeletions: boolean;
begin
  Tracker.Write(' ');
  Tracker.Write('Smashing records');
  patchFile := patch.plugin._File;
  // loop through records to smash
  for i := 0 to Pred(records.Count) do begin
    if Tracker.Cancel then break;
    if not Supports(records[i], IwbMainRecord, rec) then
      exit;
    Tracker.StatusMessage(Format('Smashing records (%d/%d)',
      [i + 1, records.Count]));
    Tracker.UpdateProgress(1);
    // loop through record's overrides
    patchRec := nil;
    for j := 0 to Pred(rec.OverrideCount) do begin
      if Tracker.Cancel then break;
      ovr := rec.Overrides[j];
      f := ovr._File;
      plugin := PluginByFileName(f.FileName);
      if not Assigned(plugin) then
        continue;
      if patch.plugins.IndexOf(plugin.filename) = -1 then
        continue;
      // skip overrides in plugins that are skipped
      if plugin.setting = 'Skip' then
        continue;
      // skip ctIdenticalToMaster overrides
      if (ConflictThisForMainRecord(ovr) = ctIdenticalToMaster) then
        continue;

      // skip overrides according to smash setting
      aSetting := plugin.smashSetting;
      recObj := aSetting.GetRecordDef(ovr.Signature);
      if not Assigned(recObj) then
        continue;
      if (recObj.I['p'] <> 1) then
        continue;

      // copy record to smashed patch if it hasn't been copied yet
      if not Assigned(patchRec) then try
        e := ovr.WinningOverride as IwbElement;
        Tracker.Write('  Copying record '+e.Name);
        eCopy := wbCopyElementToFile(e, patchFile, false, true, '', '' ,'');
        patchRec := eCopy as IwbMainRecord;
      except on x: Exception do
        Tracker.Write('      Exception copying record '+ovr.Name+' : '+x.Message);
      end;

      // finally, recursively copy overridden elements
      try
        Tracker.Write('    Smashing record '+ovr.Name+' from file: '+f.FileName);
        bDeletions := recObj.I['d'] = 1;
        rcore(IwbElement(ovr), IwbElement(rec), IwbElement(patchRec), patchRec,
          recObj, false, bDeletions);
      except
        on x: Exception do
          Tracker.Write('      Exception smashing record: '+ovr.Name+' : '+x.Message);
      end;
    end;
  end;
end;

procedure RemoveITPOs(aFile: IwbFile);
var
  i, j, CountITPO: Integer;
  e, m, prevovr, ovr: IwbMainRecord;
begin
  Tracker.Write(' ');
  Tracker.Write('Removing ITPO records from patch');
  CountITPO := 0;

  // loop through file's records
  for i := Pred(aFile.RecordCount) downto 0 do begin
    if Tracker.Cancel then break;
    e := aFile.Records[i];

    // skip master records
    if e.IsMaster then
      continue;

    // skip records that have elements in child group (WRLD, CELL, DIAL)
    if Assigned(e.ChildGroup) and (e.ChildGroup.ElementCount <> 0) then
      continue;

    m := e.MasterOrSelf;

    // find previous override record in a list of overrides for master record
    prevovr := m;
    for j := 0 to Pred(m.OverrideCount) do begin
      ovr := m.Overrides[j];
      if ovr.Equals(e) then
        Break;
      prevovr := ovr;
    end;

    // remove record if no conflicts
    if ConflictAllForElements(prevovr, e, False, False) <= caNoConflict then begin
      Tracker.Write('    Removing ITPO: ' + e.Name);
      e.Remove;
      Inc(CountITPO);
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
  patchFile.CleanMasters;
  RemoveITPOs(patchFile);
end;

procedure SavePatchFiles(var patch: TPatch);
var
  FileStream: TFileStream;
  path, patchFilePrefix: string;
  patchFile: IwbFile;
begin
  // update patch plugin hashes
  patch.UpdateHashes;
  patchFile := patch.plugin._File;
  patchFilePrefix := patch.dataPath + 'smash\'+ChangeFileExt(patch.filename, '');

  // save patch plugin
  path := patch.dataPath + patch.filename;
  Tracker.Write(' ');
  Tracker.Write('Saving: ' + path);
  try
    FileStream := TFileStream.Create(path, fmCreate);
    patchFile.WriteToStream(FileStream, False);
  except
    on x: Exception do begin
      path := path + '.smash';
      FileStream := TFileStream.Create(path, fmCreate);
      Tracker.Write('Failed to save, saving to: '+path);
      patchFile.WriteToStream(FileStream, False);
    end;
  end;
  TryToFree(FileStream);

  // save files, fails, plugins
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
  
  // set up directories
  patch.DataPath := settings.patchDirectory + patch.name + '\';
  ForceDirectories(patch.dataPath + 'smash\');

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

    // smash records
    SmashRecords(patch, recordsList);
    HandleCanceled(msg);

    // clean patch (Masters, ITPOs)
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
    Tracker.Write(Format('Done smashing %s (%.3f)',
      [patch.name, Real(time)]));
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
