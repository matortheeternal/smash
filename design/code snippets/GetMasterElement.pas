{for j := 0 to ElementCount(ae) - 1 do begin
  ne := ebi(ae, j);
  if (SortKey(ne, false) = sk) then begin
    Result := ne;
    break;
  end;
end;}

{for j := 0 to ElementCount(ae) - 1 do begin
  ne := ebi(ae, j);
  if (gav(ne) = sk) then begin
    Result := ne;
    break;
  end;
end;}

function ElementByKey(e: IwbElement; key: string; bUseSortKey: boolean): IwbElement;
var
  i: Integer;
  c: IwbContainerElementRef;
begin
  if not Supports(e, IwbContainerElementRef, c) then 
    exit;
  
  // loop through children elements
  for i := 0 to Pred(c.ElementCount) do begin
    element := c.Elements[i];
    // get sort key if bUseSortKey, else get values key
    if bUseSortKey then
      eKey := element.SortKey[false]
    else
      eKey := GetAllValues(element);
    // if keys match result is current element
    if eKey = key then begin
      Result := element;
      break;
    end;
  end;
end;

function GetMasterElement(src, se: IwbElement; dstRec: IwbMainRecord): IwbElement;
var
  i: integer;
  path, key: string;
  mstRec, ovr: IwbMainRecord;
  mst: IwbElement;
  sorted: boolean;
begin
  Result := nil;
  mstRec := MasterOrSelf(dstRec);
  path := IndexedPath(src);
  bSorted := IsSorted(src);
  
  // if sorted, use SortKey, else use GetAllValues
  if bSorted then 
    key := se.SortKey[false]
  else
    key := GetAllValues(se);
    
  // debug message
  if debugGetMaster then 
    Tracker.Write('  Called GetMasterElement at path '+p+' looking for Key '+key);
    
  // loop from override 0 to the second to last override
  // last override is in our patch, we don't want to process that one
  for i := 0 to mstRec.OverrideCount - 2 do begin
    ovr := mst.Overrides[i];
    mst := ElementByIndexedPath[(mstRec, p);
    Result := ElementByKey(mst, key, bSorted);
    
    // break if we found a subrecord matching the sortkey
    if Result <> nil then
      break;
  end;
end;
