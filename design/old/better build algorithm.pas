procedure LoadElementData(var sl: TStringList; container: IwbContainerElementRef);
var
  innerContainer: IwbContainerElementRef;
  element: IwbElement;
begin
  // loop through container's elements
  for i := 0 to Pred(container.ElementCount) do begin
    element := container.Elements[i];
    if bOverridesOnly and (ConflictThis(element) = ctIdenticalToMaster) then
      continue;
    index := sl.IndexOf(element.Name);
    // create new element name item if missing
    if index = -1 then
      index := sl.Add(element.Name);
    // traverse children of element, if it has any
    if Supports(element, IwbContainerElementRef, innerContainer) then
      if innerContainer.ElementCount > 0 then begin
        if not Assigned(sl.Objects[index]) then 
          sl.Objects[index] := TStringList.Create;
        LoadElementData(TStringList(sl.Objects[index]), innerContainer); 
      end;
  end;
end;

procedure LoadRecordData(var sl: TStringList; f: IwbFile);
var
  i, index: Integer;
  container: IwbContainerElementRef;
  rec: IwbMainRecord;
begin
  // loop through file's records
  for i := 0 to Pred(f.RecordCount) do begin
    rec := f.Records[i];
    // skip excluded signatures
    if HasSignature(rec.Signature, excludedSignatures) then 
      continue;
    // skip non-override records
    if bOverridesOnly and not IsOverride(rec) then
      continue;
    index := sl.IndexOf(rec.Signature);
    // add new record signature item if missing
    if index = -1 then
      index := sl.AddObject(rec.Signature, TStringList.Create);
    if Supports(rec, IwbContainerElementRef, container) then
      LoadElementData(TStringList(sl.Objects[index]), container);
  end;
end;

procedure StringsToNodes(node: TTreeNode; var sl: TStringList);
var
  child: TTreeNode;
  i: Integer;
begin
  for i := 0 to Pred(sl.Count) do begin
    child := TreeView.Items.AddChild(node, sl[i]);
    child.Data := TElementData.Create(0, false, false, false);
    if Assigned(sl.Objects[i]) then
      StringsToNodes(child, TStringList(sl.Objects[i]));
  end;
end;

procedure TTreeThread.Execute;
var
  i: Integer;
  plugin: TPlugin;
begin
  // ?
  
  // BODY
  sl := TStringList.Create;
  sl.Sorted := true;
  for i := 0 to Pred(pluginsToHandle.Count) do begin
    plugin := TPlugin(pluginsToHandle[i]);
    LoadRecordData(sl, plugin._File);
  end;
  rootNode := TreeView.Items.Add(nil, 'Records');
  StringsToNodes(rootNode, sl);
  
  //?
end;

function GetConflictThis(element: IwbElement): TConflictThis;
var
  mainRecord, master, winning: IwbMainRecord;
begin
  mainRecord := element.ContainingMainRecord;
  if mainRecord.ConflictPriority = cpIgnore then
    Result := ctIgnored
  else if mainRecord.OverrideCount = 0 then
    Result := ctOnlyOne
  else begin  
    master := mainRecord.Master;
    winning := mainRecord.WinningOverride;
    
    masterElement := master.ElementByPath(element.Path);
    winningElement := winning.ElementByPath(element.Path);
  end;
end;