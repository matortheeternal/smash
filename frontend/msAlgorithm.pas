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
     dtLenString];

implementation

var
  firstLink: string;

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
      try
        wbCopyElementToRecord(element, dstRec, false, true);
      except
        on x: Exception do
          Tracker.Write('        HandleElementLife: Failed to copy '+element.Path+', '+x.Message);
      end;
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

{
  BuildKeyList:
  Creates a list of element keys for elements in @container in a stringlist @sl.
  Uses SortKey if @bUseSortKeys is true, else uses GetAllValues.
}
procedure BuildKeyList(container: IwbContainerElementRef; var sl: TStringList;
  bUseSortKeys: boolean);
var
  i, n: integer;
  childElement: IwbElement;
  key, adjustedKey: string;
begin
  // loop through children elements
  for i := 0 to Pred(container.ElementCount) do begin
    childElement := container.Elements[i];

    // use sort if bUseSortKeys is true, else use GetAllValues
    if bUseSortKeys then
      key := childElement.SortKey[false]
    else
      key := GetAllValues(childElement);

    // find a non-colliding key
    n := 0;
    adjustedKey := key;
    while (sl.IndexOf(adjustedKey) > -1) do begin
      Inc(n);
      adjustedKey := key + IntToStr(n);
    end;

    // add adjusted key to stringlist
    sl.Add(adjustedKey);
  end;
end;

{
  MergeArray:
  Merges a sorted or unsorted arrray element by comparing element
  keys.  Compares a source array @src, against a master array @mst and
  a destination array @dst.  Returns true if any changes are made.
  Exits before making any changes if @bSingle is true.  Preserves
  deletions if @bDeletions if true.  Tracks @dstRec and @obj for
  calling rcore on element containers in sorted arrays.
}
function MergeArray(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  i, s_ndx, m_ndx, d_ndx: integer;
  se: IwbElement;
  slMst, slDst, slSrc: TStringList;
  srcCont, dstCont, mstCont, seCont: IwbContainerElementRef;
  bSorted: boolean;
begin
  Result := false;

  // exit if input array elements can't be treated as a containers
  if not Supports(src, IwbContainerElementRef, srcCont) then
    exit;
  if not Supports(mst, IwbContainerElementRef, mstCont) then
    exit;
  if not Supports(dst, IwbContainerElementRef, dstCont) then
    exit;

  // determine if we're handling a sorted array
  bSorted := IsSorted(src);

  // Build lists of element keys in each array for easy comparison
  slSrc := TStringList.Create;
  slMst := TStringList.Create;
  slDst := TStringList.Create;
  try
    BuildKeyList(srcCont, slSrc, bSorted);
    BuildKeyList(mstCont, slMst, bSorted);
    BuildKeyList(dstCont, slDst, bSorted);

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
    for i := 0 to Pred(slSrc.Count) do begin
      d_ndx := slDst.IndexOf(slSrc[i]);
      m_ndx := slMst.IndexOf(slSrc[i]);
      se := srcCont.Elements[i];


      if (d_ndx = -1) and (m_ndx = -1) then begin
        Result := true;
        // if we're in a treat as single, exit without adding anything
        if bSingle then
          exit;
        // add element to destination
        if settings.debugArrays then
          Tracker.Write('        > Adding element at '+dst.Path+' with values: '+slSrc[i]);
        dstCont.Assign(dstCont.ElementCount, se, false);
        slDst.Add(slSrc[i]);
      end

      // Special handling for sorted arrays
      // Traverses elements that may have been modified without their sortkey changing
      else if bSorted and (d_ndx > -1) and Supports(se, IwbContainerElementRef, seCont)
      and (seCont.ElementCount > 0) then begin
        if settings.debugArrays then begin
          Tracker.Write('        > Traversing element '+se.Path+' with key: '+slSrc[i]);
          Tracker.Write('        > Source Element: '+GetAllValues(se));
          Tracker.Write('        > Destination Element: '+GetAllValues(dstCont.Elements[d_ndx]));
        end;
        // traverse element
        try
          Result := rcore(se, GetMasterElement(src, se, dstrec), dstCont.Elements[d_ndx],
            dstrec, GetChildObj(obj, se.Name), bSingle, bDeletions);
          if Result and bSingle then
            exit;
        except on x : Exception do begin
            Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
          end;
        end;
      end;
    end;
  finally
    // free lists
    slMst.Free;
    slSrc.Free;
    slDst.Free;
  end;
end;

{ 
  CopyElementValue:
  Copies the edit value of @se to @de.
}
procedure CopyElementValue(se, me, de: IwbElement);
begin
  if not de.IsEditable then begin
    if settings.debugChanges then begin
      Tracker.Write('      Unable to copy element value on '+se.path);
      Tracker.Write('      Element is not editable');
    end;
    exit;
  end;

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
  CopyLinkedElement:
  Copies the linked element named eLink, and then copies any elements it is
  linked to.
}
procedure CopyLinkedElement(srcCont, dstCont: IwbContainerElementRef;
  eLink: string; obj: ISuperObject; dstRec: IwbMainRecord);
var
  cObj: ISuperObject;
  le, de: IwbElement;
  cLink: string;
begin
  // exit if we reached the first link in a chain
  if eLink = firstLink then
    exit;

  // else try to copy the linked element
  try
    cObj := GetChildObj(obj, eLink);
    le := srcCont.ElementByName[eLink];
    de := dstCont.ElementByName[eLink];
    if Assigned(le) then begin
      if settings.debugLinks then
        Tracker.Write('      Copying linked element '+le.Path);
      if Assigned(de) and de.IsRemoveable then
        de.Remove;
      wbCopyElementToRecord(le, dstRec, false, true);
      // follow chain
      cLink := cObj.S['lf'];
      if cLink <> '' then
        CopyLinkedElement(srcCont, dstCont, cLink, obj, dstRec);
    end;
  except
    on x: Exception do
      Tracker.Write('        CopyLinkedElement: Failed to copy '+eLink+', '+x.Message);
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
  srcType, dstType: TwbDefType;
  container: IwbContainer;
  subDef: IwbSubRecordDef;
begin
  Result := false;

  // deftype and elementtype
  et := se.ElementType;
  srcType := se.Def.DefType;
  dstType := de.Def.DefType;

  // get the deftype of value held by subrecorddefs
  if Supports(se.Def, IwbSubRecordDef, subDef) then
    srcType := subDef.Value.DefType;
  if Supports(de.Def, IwbSubRecordDef, subDef) then
    dstType := subDef.Value.DefType;

  // exit if srcType <> dstType
  if srcType <> dstType then begin
    if settings.debugSkips then begin
      Tracker.Write('      Source and destination types don''t match');
      Tracker.Write('      '+dtToString(srcType)+' != '+dtToString(dstType));
      Tracker.Write('      Skipping '+se.Path);
    end;
    exit;
  end;

  // debug messages
  if settings.debugTraversal then
    Tracker.Write('      '+se.Path);
  if settings.debugTypes then
    Tracker.Write('      ets: '+etToString(et)+'  dts: '+dtToString(srcType));

  // merge array
  if (et = etSubRecordArray) or ((srcType = dtArray) and IsSorted(se)) then begin
    if settings.debugTraversal then
      Tracker.Write('         Merging array');
    try
      Result := MergeArray(se, me, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('        MergeArray: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else recurse deeper
  else if (srcType <> dtInteger) and Supports(se, IwbContainer, container)
  and (container.ElementCount > 0) then begin
    if settings.debugTraversal then
      Tracker.Write('        Recursing deeper');
    try
      Result := rcore(se, me, de, dstrec, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else copy element value
  else if (srcType in ValueElements) and (se.EditValue <> me.EditValue) then begin
    if not bSingle then
      CopyElementValue(se, me, de);
    Result := true;
  end;
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
  eLink: string;
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

    // skip if master element not assigned
    if not Assigned(me) then begin
      if settings.debugSkips then begin
        Tracker.Write('      Master element not found!');
        Tracker.Write('      Skipping '+se.Path);
      end;
      continue;
    end;

    // skip if destination element not assigned
    if not Assigned(de) then begin
      if settings.debugSkips then begin
        Tracker.Write('      Destination element not found!');
        Tracker.Write('      Skipping '+se.Path);
      end;
      continue;
    end;

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
    eDeletions := bDeletions or (eObj.I['d'] = 1);

    // handle element
    Result := HandleElement(se, me, de, dstRec, eObj, eSingle, eDeletions);

    // if we're in a single entity and an element is changed, break
    // we don't need to handle anything anymore
    if bSingle and Result then
      break;

    // if the element we're processing has the single entity flag set
    // and we're not currently in a single entity, copy entire element
    if eSingle and (not bSingle) and Result then try
      if settings.debugSingle then
        Tracker.Write('      Copying single entity '+se.path);
      wbCopyElementToRecord(se, dstRec, false, true);
    except
      on x: Exception do
        Tracker.Write('        rcore: Failed to copy '+se.Path+', '+x.Message);
    end;

    // if another element is linked to the element being processed
    // and the element being processed has been modified, copy the linked
    // element
    eLink := eObj.S['lf'];
    firstLink := eLink;
    if Result and (eLink <> '') then
      CopyLinkedElement(srcCont, dstCont, eLink, obj, dstRec);
  end;
end;

end.
