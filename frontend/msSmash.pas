unit msSmash;

interface

uses
  Windows, SysUtils, Classes, ShellAPI, Controls, Dialogs,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteTracker,
  // ms units
  msFrontend, msAlgorithm,
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
  // to the plugins being patchd
  if bUsedExistingFile then
    for i := 0 to Pred(lst.Count) do begin
      plugin := TPlugin(lst[i]);
      if PluginsList.IndexOf(plugin) > PluginsList.IndexOf(patch.plugin) then
        raise Exception.Create(Format('%s is at a lower load order position than %s', 
          [plugin.filename, patch.filename]));
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
      raise Exception.Create('User cancelled merge.');
    Tracker.Write('Done adding masters');
  end;
end;

procedure BuildOverridesList(var patch: TPatch; var lst: TList;
  var records: TInterfaceList);
var
  i, j: Integer;
  plugin: TPlugin;
  aSetting: TSmashSetting;
  aFile: IwbFile;
  rec: IwbMainRecord;
  recObj: ISuperObject;
begin
  for i := 0 to Pred(lst.Count) do begin
    plugin := TPlugin(lst[i]);
    // skip plugins that have the skip setting
    if plugin.setting = 'Skip' then
      continue;

    // get file and setting for later use
    aFile := plugin._File;
    aSetting := plugin.smashSetting;

    // loop through file records
    Tracker.Write('Processing '+plugin.filename);
    for j := 0 to Pred(aFile.RecordCount) do begin
      rec := aFile.Records[j];
      // skip non-override records
      if rec.IsMaster then
        continue;
      // skip records that have 1 or fewer overrides
      rec := rec.MasterOrSelf;
      if rec.OverrideCount < 2 then
        continue;
      // skip records according to smash setting
      recObj := aSetting.GetRecordDef(rec.Signature);
      if not Assigned(recObj) then
        continue;
      // add record to overrides list
      if records.IndexOf(rec) = -1 then
        records.Add(rec);
    end;
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
begin
  patchFile := patch.plugin._File;
  // loop through records to smash
  for i := 0 to Pred(records.Count) do begin
    if not Supports(records[i], IwbMainRecord, rec) then
      exit;
    Tracker.StatusMessage(Format('Smashing record (%d/%d)',
      [i + 1, records.Count]));
    // loop through record's overrides
    patchRec := nil;
    for j := 0 to Pred(rec.OverrideCount) do begin
      ovr := rec.Overrides[j];
      f := ovr._File;
      plugin := PluginByFileName(f.FileName);
      if not Assigned(plugin) then
        continue;
      // skip overrides in plugins that are skipped
      if plugin.setting = 'Skip' then
        continue;
      // skip ctIdenticalToMaster overrides
      {if (ConflictThisForMainRecord(ovr) = ctIdenticalToMaster) then
        continue;}

      // skip overrides according to smash setting
      aSetting := plugin.smashSetting;
      recObj := aSetting.GetRecordDef(ovr.Signature);
      if not Assigned(recObj) then
        continue;

      // copy record to smashed patch if it hasn't been copied yet
      if not Assigned(patchRec) then try
        e := ovr.WinningOverride as IwbElement;
        Tracker.Write('    Copying record '+e.Name);
        eCopy := wbCopyElementToFile(e, patchFile, false, true, '', '' ,'');
        patchRec := eCopy as IwbMainRecord;
      except on x: Exception do
        Tracker.Write('      Exception copying record '+ovr.Name+' : '+x.Message);
      end;

      // finally, recursively copy overridden elements
      try
        Tracker.Write('    Smashing record '+ovr.Name+' from file: '+f.FileName);
        rcore(IwbElement(ovr), IwbElement(rec), IwbElement(patchRec), patchRec,
          1, recObj);
      except
        on x: Exception do
          Tracker.Write('      Exception smashing record: '+ovr.Name+' : '+x.Message);
      end;
    end;
  end;
end;

procedure CleanPatch(var patch: TPatch);
var
  patchFile: IwbFile;
begin
  patchFile := patch.plugin._File;
  patchFile.CleanMasters;
  // TODO: Handle ITPOs
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
  FileStream := TFileStream.Create(path, fmCreate);
  try
    Tracker.Write(' ');
    Tracker.Write('Saving: ' + path);
    patchFile.WriteToStream(FileStream, False);
    patch.files.Add(path);
  finally
    FileStream.Free;
  end;

  // save files, fails, plugins
  patch.files.SaveToFile(patchFilePrefix+'_files.txt');
  patch.fails.SaveToFile(patchFilePrefix+'_fails.txt');
  patch.plugins.SaveToFile(patchFilePrefix+'_plugins.txt');
end;

procedure BuildPatch(var patch: TPatch);
var
  patchFile: IwbFile;
  pluginsToPatch: TList;
  recordsList: TInterfaceList;
  time: TDateTime;
begin
  // initialize
  Tracker.Write('Building patch: '+patch.name);
  time := Now;
  patch.fails.Clear;
  pluginsToPatch := TList.Create;
  
  // set up directories
  patch.DataPath := settings.patchDirectory + patch.name + '\';
  ForceDirectories(patch.dataPath + 'smash\');

  try
    // build list of plugins to patch
    BuildPluginsList(patch, pluginsToPatch);
    
    // identify or create patch file
    patchFile := GetPatchFile(patch, pluginsToPatch);
    SetPatchAttributes(patch);

    // add masters to patch file
    AddRequiredMasters(patch, pluginsToPatch);

    // build list of overrides
    recordsList := TInterfaceList.Create;
    BuildOverridesList(patch, pluginsToPatch, recordsList);

    // smash records
    SmashRecords(patch, recordsList);

    // clean patch (Masters, ITPOs)
    //CleanPatch(patch);

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
    Tracker.Write(Format('Done smashing %s (%ss)', 
      [patch.name, FormatFloat('0.###', time)]));
  except
    on x: Exception do begin
      patch.status := psFailed;
      Tracker.Write(Format('Failed to patch %s, %s', [patch.name, x.Message]));
    end;
  end;

  // clean up
  if Assigned(pluginsToPatch) then
    pluginsToPatch.Free;
  if Assigned(recordsList) then
    recordsList.Free;
end;

procedure RebuildPatch(var patch: TPatch);
begin
  // TODO: Clear records?
  BuildPatch(patch);
end;

end.
