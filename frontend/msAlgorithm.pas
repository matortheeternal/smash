unit msAlgorithm;

interface

uses
  SysUtils, Classes,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteTracker,
  // ms units
  msFrontend,
  // xEdit components
  wbInterface, wbImplementation;

  function HandleElementLife(srcCont, dstCont, mstCont: IwbContainerElementRef;
    dstRec: IwbMainRecord; bSingle, bDeletions: boolean): boolean;
  function HandleArray(se, me, de: IwbElement; et: TwbElementType;
    dstrec: IwbMainRecord; obj: ISuperObject;
    bSingle, bDeletions: boolean): boolean;
  procedure CopyElementValue(se, me, de: IwbElement);
  function HandleElement(se, me, de: IwbElement; dstRec: IwbMainRecord;
    obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
  function rcore(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
    obj: ISuperObject; bSingle, bDeletions: boolean): boolean;

const
  valueElements: set of TwbDefType =
    [dtInteger,
     dtFloat,
     dtUnion,
     dtByteArray,
     dtString,
     dtLString,
     dtLenString,
     dtSubRecord];

implementation

{ 
  ElementByKey: 
  Gets an element from a container, @e, matching a specified @key.
  If @bUseSortKey is true uses SortKey, else uses GetAllValues.
}
function ElementByKey(e: IwbElement; key: string; bUseSortKey: boolean): IwbElement;
var
  i: Integer;
  c: IwbContainerElementRef;
  element: IwbElement;
  eKey: string;
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

{ 
  GetChildObj: 
  Gets the child json object from a node in a TSmashSetting tree
  @obj matching @name.  Returns nil if a matching child is not 
  found.
}
function GetChildObj(obj: ISuperObject; name: string): ISuperObject;
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

{
  HandleElementLife:
  Called by rcore.  Handles the creation/deletion of elemnents in the
  destination patch record @dstRec.
}
function HandleElementLife(srcCont, dstCont, mstCont: IwbContainerElementRef;
  dstRec: IwbMainRecord; bSingle, bDeletions: boolean): boolean;
var
  i: Integer;
  element: IwbElement;
begin
  Result := false;

  // handle element creation
  for i := 0 to Pred(srcCont.ElementCount) do begin
    element := srcCont.Elements[i];
    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    if not (Assigned(dstCont.ElementByName[element.Name])
    or Assigned(mstCont.ElementByName[element.Name])) then begin
      Result := true;
      if bSingle then 
        exit;
      wbCopyElementToRecord(element, dstRec, false, true);
    end;
  end;

  // handle element deletion
  if bDeletions then begin
    for i := Pred(dstCont.ElementCount) downto 0 do begin
      element := dstCont.Elements[i];
      if Assigned(mstCont.ElementByName[element.Name]) and 
      not Assigned(srcCont.ElementByName[element.Name]) then begin
        Result := true;
        if bSingle then
          exit;
        dstCont.RemoveElement(element);
      end;
    end;
  end;
end;

{ 
  BuildSortKeyList: 
  Puts the sort keys of elements in @element in a stringlist @sl.
}
procedure BuildSortKeyList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IwbElement;
  sk, skAdj: string;
  container: IwbContainerElementRef;
begin
  if not Supports(element, IwbContainerElementRef, container) then
    exit;

  // loop through children elements
  for i := 0 to Pred(container.ElementCount) do begin
    childElement := container.Elements[i];
    sk := childElement.SortKey[false];
    skAdj := sk;
    n := 0;
    while sl.IndexOf(skAdj) > -1 do begin
      Inc(n);
      skAdj := sk + '-' + IntTostr(n);
    end;
    if settings.debugArrays and (n > 0) then
      Tracker.Write('        Adjusted SortKey: '+skAdj);
    sl.Add(skAdj);
  end;
end;

// GetMasterElement: Gets the first instance of an element (the master)
function GetMasterElement(src, se: IwbElement; dstRec: IwbMainRecord): IwbElement;
const
  debugGetMaster = false;
var
  i: integer;
  path, key: string;
  mstRec, ovr: IwbMainRecord;
  mst: IwbElement;
  bSorted: boolean;
begin
  Result := nil;
  mstRec := dstRec.MasterOrSelf;
  path := IndexedPath(src);
  bSorted := IsSorted(src);
  
  // if sorted, use SortKey, else use GetAllValues
  if bSorted then 
    key := se.SortKey[false]
  else
    key := GetAllValues(se);
    
  // debug message
  if debugGetMaster then 
    Tracker.Write('      Called GetMasterElement at path '+path+' looking for key '+key);
    
  // loop from override 0 to the second to last override
  // last override is in our patch, we don't want to process that one
  for i := 0 to mstRec.OverrideCount - 2 do begin
    ovr := mstRec.Overrides[i];
    mst := ElementByIndexedPath(mstRec, path);
    Result := ElementByKey(mst, key, bSorted);
    
    // break if we found a subrecord matching the sortkey
    if Result <> nil then
      break;
  end;
end;

function RemoveArrayElements(var slSrc, slMst, slDst: TStringList;
  dstCont: IwbContainerElementRef): boolean;
var
  i, s_ndx, d_ndx: Integer;
begin
  // loop through
  for i := 0 to Pred(slMst.Count) do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);

    if (s_ndx = -1) and (d_ndx > -1) then begin
      if settings.debugArrays then
        Tracker.Write('        > Removing element '+dstCont.Elements[d_ndx].Path+' with key: '+slDst[d_ndx]);
      dstCont.RemoveElement(d_ndx);
      slDst.Delete(d_ndx);
    end;
  end;
end;

function MergeSorted(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  i, m_ndx, d_ndx: integer;
  me, se: IwbElement;
  slMst, slDst, slSrc: TStringList;
  srcCont, dstCont: IwbContainerElementRef;
  et: TwbElementType;
  dt: TwbDefType;
begin
  Result := false;

  if not Supports(src, IwbContainerElementRef, srcCont) then
    exit;
  if not Supports(dst, IwbContainerElementRef, dstCont) then
    exit;

  // Step 1: build lists of elements in each array for easy comparison
  slSrc := TStringList.Create;
  slMst := TStringList.Create;
  slDst := TStringList.Create;
  slDst.Sorted := true;
  BuildSortKeyList(src, slSrc);
  BuildSortKeyList(mst, slMst);
  BuildSortKeyList(dst, slDst);

  // Step 2: Remove elements that are in mst and dst, but missing from src
  if bDeletions then
    Result := RemoveArrayElements(slSrc, slMst, slDst, dstCont);

  // Step 3: Copy array elements in src that aren't in mst.
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);

    se := srcCont.Elements[i];
    dt := se.Def.DefType;
    et := se.ElementType;
    if (d_ndx = -1) and (m_ndx = -1) then begin
      if settings.debugArrays then
        Tracker.Write('        > Adding element '+IntToStr(i)+' at '+dst.Path+' with key: '+slSrc[i]);
      me := dstCont.Assign(dstCont.ElementCount, se, false);
      slDst.Add(slSrc[i]);
    end
    // Step 3.5: If array element is in dst and has subelements, traverse it.
    else if (d_ndx > -1) and Supports(se, IwbContainer) then begin
	    if settings.debugTraversal then
        Tracker.Write('        > Traversing element '+se.Path+' with key: '+slSrc[i]);
      if settings.debugTraversal and settings.debugArrays then begin
        Tracker.Write('        > Source Element: '+GetAllValues(se));
        Tracker.Write('        > Destination Element: '+GetAllValues(dstCont.Elements[d_ndx]));
      end;
      try
        rcore(se, GetMasterElement(src, se, dstrec), dstCont.Elements[d_ndx],
          dstrec, GetChildObj(obj, se.Name), bSingle, bDeletions);
      except on x : Exception do begin
          Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
        end;
      end;
    end
    else if (d_ndx > -1) and (et = etSubRecordStruct) then begin
	    if settings.debugTraversal then
        Tracker.Write('        > Traversing element '+se.Path+' with key: '+slSrc[i]);
      if settings.debugTraversal and settings.debugArrays then begin
        Tracker.Write('        > Source Element: '+GetAllValues(se));
        Tracker.Write('        > Destination Element: '+GetAllValues(dstCont.Elements[d_ndx]));
      end;
      try
        rcore(se, GetMasterElement(src, se, dstrec), dstCont.Elements[d_ndx], dstrec,
          GetChildObj(obj, se.Name), bSingle, bDeletions);
      except on x : Exception do begin
          Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
        end;
      end;
    end;
  end;

  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

// BuildElementList: puts the values of elements in a stringlist
procedure BuildElementList(element: IwbElement; var sl: TStringList);
var
  i, n: integer;
  childElement: IwbElement;
  values, valuesAdj: string;
  container: IwbContainerElementRef;
begin
  if not Supports(element, IwbContainerElementRef, container) then
    exit;

  // loop through children elements
  for i := 0 to Pred(container.ElementCount) do begin
    childElement := container.Elements[i];
    values := GetAllValues(childElement);
    valuesAdj := values;
    n := 0;
    while (sl.IndexOf(valuesAdj) > -1) do begin
      Inc(n);
      valuesAdj := values + IntToStr(n);
    end;
    sl.Add(valuesAdj);
  end;
end;

function MergeUnsorted(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  srcCont, mstCont, dstCont: IwbContainerElementRef;
  slMst, slSrc, slDst: TStringList;
  i, s_ndx, m_ndx, d_ndx: Integer;
  se: IwbElement;
begin
  Result := false;
  
  // exit if source or destination can't be treated as a container
  if not Supports(src, IwbContainerElementRef, srcCont) then
    exit;
  if not Supports(mst, IwbContainerElementRef, mstCont) then
    exit;
  if not Supports(dst, IwbContainerElementRef, dstCont) then
    exit;
  
  // Build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  try
    BuildElementList(mst, slMst);
    BuildElementList(src, slSrc);
    BuildElementList(dst, slDst);
    
    // ELEMENT DELETION:
    // Remove elements that are in master and destination, but 
    // missing from source
    if bDeletions then
      for i := 0 to Pred(slMst.Count) do begin
        s_ndx := slSrc.IndexOf(slMst[i]);
        d_ndx := slDst.IndexOf(slMst[i]);
        
        // element from master isn't in source, but is in destination
        if (s_ndx = -1) and (d_ndx > -1) then begin
          Result := true;
          // if we're in a treat as single, exit without removing anything 
          if bSingle then 
            exit;
          // remove element from destination
          if settings.debugArrays then
            Tracker.Write('        > Removing element at '+dst.Path+' with values: '+slMst[i]);
          dstCont.RemoveElement(d_ndx);
          slDst.Delete(d_ndx);
        end;
      end;
    
    // ELEMENT ADDITION:
    // Copy array elements in source that aren't in master 
    // or destination
    for i := 0 to slSrc.Count - 1 do begin
      d_ndx := slDst.IndexOf(slSrc[i]);
      m_ndx := slMst.IndexOf(slSrc[i]);
      se := srcCont.Elements[i];
      
      if (m_ndx = -1) and (d_ndx = -1) then begin
        Result := true;
        // if we're in a treat as single, exit without adding anything 
        if bSingle then 
          exit;
        // add element to destination
        if settings.debugArrays then
          Tracker.Write('        > Adding element at '+dst.Path+' with values: '+slSrc[i]);
        dstCont.Assign(dstCont.ElementCount, se, false);
        slDst.Add(slSrc[i]);
      end;
    end;
  finally
    // free lists.
    slMst.Free;
    slSrc.Free;
    slDst.Free;
  end;
end;

function HandleArray(se, me, de: IwbElement; et: TwbElementType;
  dstrec: IwbMainRecord; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
begin
  Result := false;

  // if sorted, deal with sorted array
  if IsSorted(se) then begin
    if settings.debugArrays then
      Tracker.Write('      Sorted array found: '+se.Path);
    try
      Result := MergeSorted(me, se, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('        MergeSortedArray: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else deal with unsorted etSubRecordArray
  else if (et = etSubRecordArray) then begin
    if settings.debugArrays then
      Tracker.Write('      Unsorted etSubRecordArray found: '+se.Path);
    try
      Result := MergeUnsorted(me, se, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('        MergeUnsortedArray: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else deal with unsorted dtArray
  else begin
    if settings.debugArrays then
      Tracker.Write('      Unsorted dtArray found: '+se.Path);
    try
      Result := MergeUnsorted(me, se, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('        MergeUnsortedArray: Exception at '+se.Path+': '+x.Message);
    end;
  end;
end;

{ 
  CopyElementValue:
  Copies the edit value of @se to @de.
}
procedure CopyElementValue(se, me, de: IwbElement);
begin
  if Assigned(me) and settings.debugChanges then begin
    if (not settings.debugTraversal) then
      Tracker.Write('      '+se.Path);
    Tracker.Write('      > Found differing values: '+se.EditValue+', '+me.EditValue);
  end;

  // try to copy element value to destination element from source element
  try
    de.EditValue := se.EditValue;
  except on x : Exception do
    Tracker.Write('      CopyElementValue: Exception '+x.Message);
  end;
end;

{
  HandleElement:
  Wrapper function around the logic for recursing into child 
  elements (rcore), handling an array, or copying element values.
}
function HandleElement(se, me, de: IwbElement; dstRec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  et: TwbElementType;
  dt: TwbDefType;
  container: IwbContainer;
begin
  Result := false;

  // deftype and elementtype
  et := se.ElementType;
  dt := se.Def.DefType;

  // debug messages
  if settings.debugTraversal then
    Tracker.Write('      '+se.Path);
  if settings.debugTypes then
    Tracker.Write('      ets: '+etToString(et)+'  dts: '+dtToString(dt));

  // handle array, recurse deeper, or copy element value
  if (et = etSubRecordArray) or (dt = dtArray) then
    Result := HandleArray(se, me, de, et, dstRec, obj, bSingle, bDeletions)
  else if (dt <> dtInteger) and Supports(se, IwbContainer, container)
  and (container.ElementCount > 0) then begin
    Tracker.Write('        Treating as container');
    try
      Result := rcore(se, me, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
    end;
  end
  else if {(dt in ValueElements) and} (se.EditValue <> me.EditValue) then begin
    if not bSingle then
      CopyElementValue(se, me, de);
    Result := true;
  end
  else if settings.debugChanges then
    Tracker.Write('        No changes found');
end;

{
  rcore:
  Recursively copy overridden elements.  Recursively traverses elements,
  comparing between a source element (from an override record) @src, a 
  master element @mst, and a destination element @dst.  Tracks the 
  destination patch record for element copying through @dstRec.  Uses a 
  json tree @obj to determine user settings for traversal.  Uses @bSingle 
  to determine when we're in a "single entity" as specified by the user, 
  and @bDeletions to determine when it's ok to delete removed elements 
  from the destination patch record.
  
  - Uses HandleElementExistence to resolve element creation/deletion.
  - Uses HandleElement to handle arrays, recurse deeper, or copy element
    values
}
function rcore(src, mst, dst: IwbElement; dstRec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  i: integer;
  srcCont, dstCont, mstCont: IwbContainerElementRef;
  se, me, de: IwbElement;
  process, eSingle, eDeletions: boolean;
  eObj: ISuperObject;
begin
  Result := false;

  // prepare containers
  if not Supports(src, IwbContainerElementRef, srcCont) then
    exit;
  if not Supports(dst, IwbContainerElementRef, dstCont) then
    exit;
  if not Supports(mst, IwbContainerElementRef, mstCont) then
    exit;

  // copy elements from source to destination if missing AND
  // delete elements missing from source if found in master and destination
  Result := HandleElementLife(srcCont, dstCont, mstCont, dstRec, bSingle, bDeletions);
  if bSingle and Result then
    exit;

  // loop through subelements
  for i := 0 to Pred(srcCont.ElementCount) do begin
    // assign source, destination, master elements
    se := srcCont.Elements[i];
    de := dstCont.ElementByName[se.Name];
    me := mstCont.ElementByName[se.Name];

    // skip according to setting
    eObj := GetChildObj(obj, se.Name);
    process := Assigned(eObj) and (eObj.I['p'] = 1);
    if not process then begin
      if settings.debugSkips then
        Tracker.Write('      Skipping '+se.Path);
      continue;
    end;

    // set element treat as single entity / ignore deletions booleans
    eSingle := bSingle or (eObj.I['s'] = 1);
    eDeletions := bDeletions or (eObj.I['i'] = 1);

    // handle element
    Result := HandleElement(se, me, de, dstRec, eObj, eSingle, eDeletions);

    // if we're in a single entity and an element is changed, break
    // we don't need to handle anything anymore
    if bSingle and Result then
      break;

    // if the element we're processing has the single entity flag set
    // and we're not currently in a single entity, copy entire element
    if eSingle and (not bSingle) and Result then
      wbCopyElementToRecord(se, dstRec, false, true);
  end;
end;

end.
