unit msSmash;
interface
uses
  Windows, SysUtils, Classes, ShellAPI, Controls, Dialogs, Generics.Collections,
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
  for i := 0 to Pred(patch.plugins.Count) do
  begin
    plugin := PluginByFileName(patch.plugins[i]);
    if not Assigned(plugin) then
      raise Exception.Create('Couldn''t find plugin ' + patch.plugins[i]);
    lst.Add(plugin);
  end;
end;
procedure SetPatchAttributes(var patch: TPatch);
var
  patchFile: IwbFile;
  fileHeader: IwbMainRecord;
begin
  patchFile := patch.plugin._File;
  fileHeader := patchFile.Elements[0] as IwbMainRecord;
  // set author
  fileHeader.ElementEditValues['CNAM'] := 'Mator Smash v' +
    ProgramStatus.Version;
  // set description
  fileHeader.ElementEditValues['SNAM'] := 'Smashed patch:'#13#10 +
    patch.plugins.Text;
  // set ESL flag
  fileHeader.IsESL := settings.flagESL;
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
  plugin := PluginByFileName(patch.filename);
  patch.plugin := nil;
  if Assigned(plugin) then
  begin
    bUsedExistingFile := true;
    patch.plugin := plugin;
  end
  else
  begin
    bUsedExistingFile := false;
    patch.plugin := CreateNewPlugin(patch.filename);
  end;
  // don't patch if patchFile not assigned
  if not Assigned(patch.plugin) then
    raise Exception.Create('Couldn''t assign patch file');
  // don't patch if patchFile is at an invalid load order position relative
  // to the plugins being patched
  if bUsedExistingFile then
  begin
    for i := 0 to Pred(lst.Count) do
    begin
      plugin := TPlugin(lst[i]);
      if PluginsList.IndexOf(plugin) > PluginsList.IndexOf(patch.plugin) then
        raise Exception.Create
          (Format('%s is at a lower load order position than %s',
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
  Tracker.Write('Patch is using plugin: ' + patch.plugin.filename);
end;
function CompareLoadOrder(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if Index1 = Index2 then
  begin
    Result := 0;
    Exit;
  end;
  Result := CmpI32(IwbFile(Pointer(List.Objects[Index1])).LoadOrder,
    IwbFile(Pointer(List.Objects[Index2])).LoadOrder);
end;
procedure AddRequiredMasters(var aFile: IwbFile; const el: IwbElement);
var
  slMasters: TStringList;
  i, j: Integer;
begin
  slMasters := TStringList.Create;
  slMasters.Sorted := true;
  slMasters.Duplicates := dupIgnore;
  try
    try
      el.ReportRequiredMasters(slMasters, false, true, false);
      if settings.debugMasters then
        Tracker.Write('    Element ' + el.Name + ' from ' + el._File.filename +
          ' requires masters: ' + slMasters.CommaText);
      for i := 0 to Pred(aFile.MasterCount[true]) do
        if slMasters.Find(aFile.Masters[i, true].filename, j) then
          slMasters.Delete(j);
      if slMasters.Find(aFile.filename, j) then
        slMasters.Delete(j);
      if slMasters.Count > 0 then
      begin
        for i := 0 to Pred(slMasters.Count) do
          if IwbFile(Pointer(slMasters.Objects[i])).LoadOrder >= aFile.LoadOrder
          then
            raise Exception.Create('The required master "' + slMasters[i] +
              '" can not be added to "' + aFile.filename +
              '" as it has a higher load order');
        slMasters.Sorted := false;
        slMasters.CustomSort(CompareLoadOrder);
        if (aFile.MasterCount[true] + slMasters.Count >= 253) then
          aFile.CleanMasters;
        aFile.AddMasters(slMasters);
        Logger.Write('PATCH', 'MASTERS', 'Added masters: ' +
          slMasters.CommaText);
        if settings.debugMasters then
          Tracker.Write('    Adding masters: ' + slMasters.CommaText);
      end;
    except
      on x: Exception do
      begin
        Tracker.Write('Critical exception adding masters!');
        Tracker.Write(x.Message);
        raise x;
      end;
    end
  finally
    slMasters.Free;
    if Tracker.Cancel then
      raise Exception.Create('User cancelled smashing.');
  end;
end;
procedure ListParents(const rec: IwbMainRecord; var parents: TInterfaceList);
var
  grup: IwbGroupRecord;
begin
  if Supports(rec.Container, IwbGroupRecord, grup) and Assigned(grup.ChildrenOf)
  then
  begin
    if parents.IndexOf(grup.ChildrenOf) = -1 then
    begin
      parents.Add(grup.ChildrenOf);
      ListParents(grup.ChildrenOf, parents);
    end
  end;
end;
procedure AddParents(var patchFile: IwbFile; const rec: IwbElement);
var
  grup: IwbGroupRecord;
begin
  if Supports(rec.Container, IwbGroupRecord, grup) and Assigned(grup.ChildrenOf)
  then
  begin
    if not Assigned(patchFile.RecordByFormID[grup.ChildrenOf.FormID,
      true, true]) then
    begin
      AddParents(patchFile, grup.ChildrenOf);
      Tracker.Write(Format('    Copying parent record %s of %s',
        [grup.ChildrenOf.Name, rec.Name]));
      AddRequiredMasters(patchFile, grup.ChildrenOf);
      grup.ChildrenOf.CopyInto(patchFile, false, false, '',
        '', '', '');
    end
  end;
end;
procedure BuildOverridesList(var patch: TPatch; var lst: TList;
  var Records: TInterfaceList);
var
  i, j, recCount: Integer;
  plugin: TPlugin;
  aSetting: TSmashSetting;
  aFile: IwbFile;
  rec, ovr: IwbMainRecord;
  recObj: ISuperObject;
begin
  Tracker.Write(' ');
  Tracker.Write('Processing files');
  for i := 0 to Pred(lst.Count) do
  begin
    if Tracker.Cancel then
      break;
    plugin := TPlugin(lst[i]);
    // get file and setting for later use
    aFile := plugin._File;
    aSetting := plugin.smashSetting;
    Tracker.Write('Processing ' + aFile.Name);
    // loop through file records
    recCount := Pred(aFile.RecordCount);
    if aFile.Name.EndsWith(csDotGhost) then begin
      for j := 0 to recCount do begin
        rec := aFile.Records[j];
        // add parent record(s) to list first so they get smashed first?
        ListParents(rec, Records);
        rec := rec.Master;
        // add record to overrides list
        if Records.IndexOf(rec) = -1 then
          Records.Add(rec);
      end;
      // update progress bar for file
      Tracker.UpdateProgress(recCount);
    end
    else begin
      for j := 0 to recCount do
      begin
        if Tracker.Cancel then
          break;
        rec := aFile.Records[j];
        ovr := rec;
        if j mod 500 = 499 then
          Tracker.UpdateProgress(500);
        try
          // skip non-override records
          if rec.IsMaster then
            continue;
          rec := rec.Master;
          {
          if (OverrideCountInFiles(rec, patch.plugins) < 2) then
            continue;
          }
          // skip records according to smash setting
          recObj := aSetting.GetRecordDef(rec.Signature);
          if not Assigned(recObj) then
            continue;
          if (recObj.i['p'] <> 1) then
            continue;
          // skip non-conflicting records
          if ConflictAllForMainRecord(rec) < caConflict then
            continue;
          // add parent record(s) to list first so they get smashed first?
          ListParents(ovr, Records);
          // add record to overrides list
          if Records.IndexOf(rec) = -1 then
            Records.Add(rec);
        except
          on x: Exception do
          begin
            Tracker.Write('  Error processing ' + rec.Name + ', ' + x.Message);
            continue;
          end;
        end;
      end;
      // update progress bar for file
      Tracker.UpdateProgress(recCount mod 500);
    end;
  end;
end;
procedure UpdateCounts(var rec: IwbMainRecord);
var
  Container, arrayContainer: IwbContainerElementRef;
  i: Integer;
  element, nextElement: IwbElement;
begin
  // if reocrd is not editable, exit
  if not rec.IsEditable then
    Exit;
  // if record can't be treated as a container, exit
  if not Supports(rec, IwbContainerElementRef, Container) then
    Exit;
  // loop through top-level elements
  for i := 0 to Container.ElementCount - 2 do
  begin
    element := Container.Elements[i];
    nextElement := Container.Elements[i + 1];
    if not Supports(nextElement, IwbContainerElementRef, arrayContainer) then
      continue;
    // if next element is an array element and current element has the
    // word count in its name update the count to be the number of elements
    // in the array
    if (GetSmashType(nextElement) in stArrays) and
      (GetSmashType(element) = stInteger) and (Pos('Count', element.Name) > 0)
    then
      try
        element.NativeValue := arrayContainer.ElementCount;
      except
        on x: Exception do
          Tracker.Write('    Exception updating count at ' + element.FullPath);
      end;
  end;
end;
function HasPartialFormFlag(rec: IwbMainRecord): boolean;
begin
  Result := ((rec.Signature = 'QUST') or (rec.Signature = 'LCTN')) and
    (rec.Flags._Flags and $00004000 <> 0);
end;
procedure SmashRecords(var patch: TPatch; var Records: TInterfaceList);
var
  i, j, k, ndx: Integer;
  incProgress, currentProgress: Real;
  rec, mst, ovr, patchRec: IwbMainRecord;
  msts: TList<IwbMainRecord>;
  ovrs: TStringList;
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
  incProgress := (Tracker.GetMaxProgress - Tracker.GetProgress) / Records.Count;
  for i := 0 to Pred(Records.Count) do
  begin
    if Tracker.Cancel then
      break;
    if not Supports(Records[i], IwbMainRecord, rec) then
      Exit;
    Tracker.StatusMessage(Format('Smashing records (%d/%d)',
      [i + 1, Records.Count]));
    currentProgress := currentProgress + incProgress;
    Tracker.SetProgress(Round(currentProgress));
    // loop through record's overrides to find which ones to smash
    ovrs := TStringList.Create;
    try
      for j := 0 to Pred(rec.OverrideCount) do
      begin
        ovr := rec.Overrides[j];
        f := ovr._File;
        // skip overrides that are in plugins we aren't patching
        if patch.plugins.IndexOf(f.filename) = -1 then
          continue;
        plugin := PluginByFileName(f.filename);
        if not Assigned(plugin) then
          continue;
        // skip plugins that have the skip setting
        if plugin.setting = 'Skip' then
          continue;
        // skip overrides according to smash setting
        aSetting := plugin.smashSetting;
        recObj := aSetting.GetRecordDef(ovr.Signature);
        if not Assigned(recObj) then
          continue;
        if (recObj.i['p'] <> 1) then
          continue;
        // this is an override to smash
        ovrs.AddObject(f.Name, Pointer(ovr));
        // don't bother to smash this override's masters?
        for k := 0 to Pred(f.MasterCount[false]) do
          if ovrs.Find(f.Masters[k,false].Name, ndx) then
            ovrs.Delete(ndx);
      end;
      // If only one override to smash just copy it in
      if ovrs.Count = 1 then begin
        ovr := IwbMainRecord(Pointer(ovrs.Objects[0]));
        {
        if ovr.IsWinningOverride then
          continue;
        }
        try
          // be sure we include the parent?
          AddParents(patchFile, ovr);
          Tracker.Write(Format('  [%d] Copying record %s from %s',
            [i + 1, ovr.Name, ovr._File.Name]));
          AddRequiredMasters(patchFile, ovr);
          ovr.CopyInto(patchFile, false, false, '', '', '', '');
        except
          on x: Exception do
          begin
            Tracker.Write('      Exception copying record ' + e.Name +
              ' from file ' + e._File.filename + ': ' + x.Message);
            patch.fails.Add('Exception copying record ' + ovr.Name + ' : ' +
              x.Message);
          end;
        end;
        continue;
      end;
      // loop through overrides to smash
      patchRec := nil;
      forceFile := nil;
      for j := 0 to Pred(ovrs.Count) do
      begin
        if Tracker.Cancel then
          break;
        ovr := IwbMainRecord(Pointer(ovrs.Objects[j]));
        f := ovr._File;
        bForce := recObj.i['f'] = 1;
        if bForce then
        begin
          if Assigned(patchRec) then
          begin
            patchRec.Remove;
            patchRec := nil;
          end;
          forceFile := f;
        end;
        // copy record to smashed patch if it hasn't been copied yet
        if not Assigned(patchRec) then
          try
            if bForce then
              e := ovr
            else
              e := WinningOverrideInFiles(rec, patch.plugins);
            // be sure we include the parent?
            AddParents(patchFile, e);
            Tracker.Write(Format('  [%d] Copying record %s from %s',
              [i + 1, e.Name, e._File.Name]));
            AddRequiredMasters(patchFile, e);
            eCopy := e.CopyInto(patchFile, false, false, '', '', '', '');
            patchRec := eCopy as IwbMainRecord;
            if bForce then
              continue;
          except
            on x: Exception do
            begin
              Tracker.Write('      Exception copying record ' + e.Name +
                ' from file ' + e._File.filename + ': ' + x.Message);
              patch.fails.Add('Exception copying record ' + ovr.Name + ' : ' +
                x.Message);
              continue;
            end;
          end;
        // skip if we're forcing and plugin doesn't require forceFile
        if Assigned(forceFile) and not bForce and
          (plugin.Masters.IndexOf(forceFile.filename) = -1) then
          continue;
        // finally, recursively copy overridden elements
        try
          bDeletions := recObj.i['d'] = 1;
          if (wbGameMode = gmFO4) and HasPartialFormFlag(ovr) then
            bDeletions := false;
          bOverride := recObj.i['o'] = 1;
          msts := TList<IwbMainRecord>.Create;
          try
            if bForce then
              msts.Add(e as IwbMainRecord)
            else
              OverridesInMasters(ovr, msts);
            for k := 0 to Pred(msts.Count) do begin
              mst := msts.Items[k];
              Tracker.Write(Format('    Smashing override of %s from: %s, master: %s',
                [ovr.Name, f.filename, mst._File.filename]));
              AddRequiredMasters(patch.plugin._File, ovr);
              rcore(IwbElement(ovr), IwbElement(mst), IwbElement(patchRec), patchRec,
                recObj, false, bDeletions, bOverride);
          end;
          finally
            msts.Free;
          end
        except
          on x: Exception do
          begin
            Tracker.Write('      Exception smashing record: ' + ovr.Name + ' : ' +
              x.Message);
            patch.fails.Add('Exception smashing record: ' + ovr.Name + ' : ' +
              x.Message);
          end;
        end;
        // update any count elements on the record
        UpdateCounts(patchRec);
      end;
    finally
      ovrs.Free;
    end;
  end;
end;
function IsChildGroup(group: IwbGroupRecord): boolean;
begin
  Result := group.GroupType in [1, 6, 7];
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
  Container, nextContainer: IwbContainer;
  rec: IwbMainRecord;
  bITM, bITPO: boolean;
begin
  Container := aContainer;
  // traverse up container until we find an IwbMainRecord
  while Assigned(Container) and not Supports(Container, IwbMainRecord, rec) do
  begin
    // break if container still has elements in it
    if Container.ElementCount > 0 then
      Exit;
    // else remove it and traverse up to next container
    nextContainer := NativeContainer(Container);
    Container.Remove;
    Container := nextContainer;
  end;
  // exit if record is not ITM or ITPO
  bITM := IsITM(rec);
  bITPO := IsITPO(rec);
  if not(bITM or bITPO) then
    Exit;
  // else remove MainRecord and recurse
  if bITM then
    Tracker.Write('    Removing ITM: ' + rec.Name)
  else
    Tracker.Write('    Removing ITPO: ' + rec.Name);
  nextContainer := rec.Container;
  rec.Remove;
  RemoveEmptyContainers(nextContainer);
end;
function FindITPO(e: IwbMainRecord): boolean;
begin
  // skip master records
  if e.IsMaster then
    Exit(false);
  // skip records that have elements in child group (WRLD, CELL, DIAL)
  if Assigned(e.ChildGroup) and (e.ChildGroup.ElementCount > 0) then
    Exit(false);
  // remove record if no conflicts
  if not IsITPO(e) then
    Exit(false);
  Result := true;
  Tracker.Write('    Removing ITPO: ' + e.Name);
end;
type
  TITPOThread = class(TThread)
  private
    Fe: IwbMainRecord;
    procedure Execute; override;
  public
    constructor Create(const e: IwbMainRecord);
    property ReturnValue;
  end;
constructor TITPOThread.Create;
begin
  inherited Create(false);
  Fe := e;
end;
procedure TITPOThread.Execute;
begin
  if FindITPO(Fe) then
    ReturnValue := 1
  else
    ReturnValue := 0;
end;
procedure RemoveITPOs(aFile: IwbFile);
var
  i, CountITPO: Integer;
  e: IwbMainRecord;
  Container: IwbContainer;
  ITPOs: TDynMainRecords;
  ThreadRefs: array of TITPOThread;
  ThreadHandles: array of THandle;
begin
  Tracker.Write(' ');
  Tracker.Write('Removing ITPO records from patch');
  CountITPO := 0;
  if settings.multiThreadedSmash then
  begin
    SetLength(ThreadRefs, aFile.RecordCount);
    SetLength(ThreadHandles, aFile.RecordCount);
    // loop through file's records
    for i := Pred(aFile.RecordCount) downto 0 do
    begin
      if Tracker.Cancel then
        break;
      e := aFile.Records[i];
      ThreadRefs[i] := TITPOThread.Create(e);
      ThreadHandles[i] := ThreadRefs[i].Handle;
    end;
    // Wait for Threads
    WaitForMultipleObjects(Length(ThreadRefs), Pointer(ThreadHandles), true,
      INFINITE);
    // loop through threads and get results
    for i := Pred(Length(ThreadRefs)) downto 0 do
    begin
      // remove record if no conflicts
      if ThreadRefs[i].ReturnValue = 1 then
      begin
        // add ITPO to list of records to remove
        SetLength(ITPOs, CountITPO + 1);
        ITPOs[CountITPO] := e;
        Inc(CountITPO);
      end;
    end;
  end
  else
  begin
    // loop through file's records
    for i := Pred(aFile.RecordCount) downto 0 do
    begin
      e := aFile.Records[i];
      if FindITPO(e) then
      begin
        // add ITPO to list of records to remove
        SetLength(ITPOs, CountITPO + 1);
        ITPOs[CountITPO] := e;
        Inc(CountITPO);
      end;
    end;
  end;
  // remove the records
  for i := Pred(Length(ITPOs)) downto 0 do
  begin
    e := ITPOs[i];
    Container := e.Container;
    e.Remove;
    try
      RemoveEmptyContainers(Container);
    except
      on x: Exception do
        Tracker.Write('      Exception removing empty containers: ' +
          x.Message);
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
  try
    // remove ITPOs
    if not settings.preserveITPOs then
      RemoveITPOs(patchFile);
    // Mator didn't like cleaning the masters unnecessarily
    // patchFIle.CleanMasters;
  except
    on x: Exception do
      Tracker.Write('    Exception removing ITPOs: ' + x.Message);
  end;
  Tracker.Write('Sorting patch masters according to current load order');
  patchFile.SortMasters;
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
  patch.fails.SaveToFile(patchFilePrefix + '_fails.txt');
  patch.plugins.SaveToFile(patchFilePrefix + '_plugins.txt');
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
  Tracker.Write('Building patch: ' + patch.Name);
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
    // AddRequiredMasters(patch, pluginsToPatch);
    // HandleCanceled(msg);
    // build list of overrides
    recordsList := TInterfaceList.Create;
    BuildOverridesList(patch, pluginsToPatch, recordsList);
    HandleCanceled(msg);
    // stop smashing if no records to smash
    if recordsList.Count = 0 then
      raise Exception.Create('No records to patch!');
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
    Tracker.Write(Format('Done smashing %s (%.3f)', [patch.Name, Real(time)]));
  except
    on x: Exception do
    begin
      patch.status := psFailed;
      Tracker.Write(Format('Failed to patch %s, %s', [patch.Name, x.Message]));
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
