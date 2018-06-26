unit msAlgorithm;

interface

uses
  SysUtils, Classes,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteTracker, mteBase,
  // ms units
  msCore, msConfiguration,
  // xEdit components
  wbInterface, wbImplementation;

  procedure CopyLinkedElement(srcCont, dstCont: IwbContainerElementRef;
    eLink: string; obj: ISuperObject; dstRec: IwbMainRecord);
  function rcore(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
    obj: ISuperObject; bSingle, bDeletions, bOverride: boolean): boolean;

implementation

var
  firstLink: string;
  bLinkProcessed: boolean;

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
  HandleElementLife:
  Called by rcore.  Handles the creation/deletion of elements in the
  destination patch record @dstRec.
}
function HandleElementLife(srcCont, dstCont, mstCont: IwbContainerElementRef;
  dstRec: IwbMainRecord; obj: ISuperObject; bSingle, bDeletions, bOverride: boolean): boolean;
var
  i: Integer;
  element: IwbElement;
  eObj: ISuperObject;
  process, bInDestination, bInMaster: boolean;
  eLink: String;
begin
  Result := false;

  // handle element creation
  for i := 0 to Pred(srcCont.ElementCount) do begin
    element := srcCont.Elements[i];

    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    bInDestination := Assigned(dstCont.ElementByName[element.Name]);
    bInMaster := Assigned(mstCont.ElementByName[element.Name]);
    if (not bInDestination) and (bOverride or not bInMaster) then begin
      Result := true;
      if bSingle then 
        exit;

      // skip according to setting
      eObj := GetElementObj(obj, element.Name);
      process := Assigned(eObj) and (eObj.I['p'] = 1);
      if not process then begin
        if settings.debugSkips then
          Tracker.Write('      Skipping element creation at '+element.Path);
        continue;
      end;

      // copy element
      try
        if settings.debugChanges then
          Tracker.Write('      Created element at '+element.Path);
        wbCopyElementToRecord(element, dstRec, false, true);

        // if another element is linked to the element, copy it
        eLink := eObj.S['lf'];
        if (eLink <> '') then begin
          firstLink := eLink;
          bLinkProcessed := false;
          CopyLinkedElement(srcCont, dstCont, eLink, obj, dstRec);
        end;
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

        // skip according to setting
        eObj := GetElementObj(obj, element.Name);
        process := Assigned(eObj) and (eObj.I['p'] = 1);
        if not process then begin
          if settings.debugSkips then
            Tracker.Write('      Skipping element deletion at '+element.Path);
          continue;
        end;

        // remove element
        if settings.debugChanges then
          Tracker.Write('      Deleted element at '+element.Path);
        dstCont.RemoveElement(element);

        // if another element is linked to the element, copy it
        eLink := eObj.S['lf'];
        if (eLink <> '') then begin
          firstLink := eLink;
          bLinkProcessed := false;
          CopyLinkedElement(srcCont, dstCont, eLink, obj, dstRec);
        end;
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
  obj: ISuperObject; bSingle, bDeletions, bOverride: boolean): boolean;
var
  i, s_ndx, m_ndx, d_ndx: integer;
  se, de: IwbElement;
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

        // element from master isn't in source
        if (s_ndx = -1) then begin
          Result := true;
          // if we're in a treat as single, exit without removing anything
          if bSingle then exit;
          // if element is present in destination, remove it
          d_ndx := slDst.IndexOf(slMst[i]);
          if (d_ndx = -1) then continue;
          if settings.debugArrays then
            Tracker.Write('        > Removing element at '+dst.Path+' with key: '+slMst[i]);
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


      if (d_ndx = -1) and ((m_ndx = -1) or bOverride) then begin
        Result := true;
        // if we're in a treat as single, exit without adding anything
        if bSingle then exit;
        // add element to destination
        if settings.debugArrays then
          Tracker.Write('        > Adding element at '+dst.Path+' with key: '+slSrc[i]);
        de := dstCont.Assign(dstCont.ElementCount, se, false);
        if bSorted then
          slDst.Insert(dstCont.IndexOf(de), slSrc[i])
        else
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
            dstrec, GetElementObj(obj, se.Name), bSingle, bDeletions, bOverride);
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
  if (eLink = firstLink) then begin
    if bLinkProcessed then
      exit;
    bLinkProcessed := true;
  end;

  // else try to copy the linked element
  try
    cObj := GetElementObj(obj, eLink);
    le := srcCont.ElementByName[eLink];
    de := dstCont.ElementByName[eLink];
    if Assigned(le) then begin
      if settings.debugLinks then
        Tracker.Write('      Copying linked element '+le.Path);
      if Assigned(de) then
        de.Assign(Low(Integer), le, false)
      else
        dstCont.AddIfMissing(le, true, true, '', '', '');
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
  obj: ISuperObject; bSingle, bDeletions, bOverride: boolean): boolean;
var
  srcType, dstType: TSmashType;
  container: IwbContainerElementRef;
  bCanAdd, bIsContainer: boolean;
  seVal, meVal: string;
begin
  Result := false;

  // deftype and elementtype
  srcType := GetSmashType(se);
  dstType := GetSmashType(de);

  // other type information
  bIsContainer := Supports(se, IwbContainerElementRef, container)
    and (container.ElementCount > 0);
  bCanAdd := se.CanAssign(High(Integer), nil, True)
    and not (esNotSuitableToAddTo in se.ElementStates);

  // exit if srcType <> dstType, returning true
  if srcType <> dstType then begin
    if settings.debugSkips then begin
      Tracker.Write('      Source and destination types don''t match');
      Tracker.Write('      '+stToString(srcType)+' != '+stToString(dstType));
      Tracker.Write('      Skipping '+se.Path);
    end;
    Result := True;
    exit;
  end;

  // debug messages
  if settings.debugTraversal then
    Tracker.Write('      '+se.Path);
  if settings.debugTypes then begin
    Tracker.Write('      bCanAdd: '+BoolToStr(bCanAdd, true));
    Tracker.Write('      SmashType: '+stToString(srcType));
  end;

  // merge array
  if bCanAdd and (srcType in stArrays) then begin
    if settings.debugTraversal then
      Tracker.Write('         Merging array');
    try
      Result := MergeArray(se, me, de, dstrec, obj, bSingle, bDeletions, bOverride);
    except on x : Exception do
      Tracker.Write('        MergeArray: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else recurse deeper
  else if bIsContainer and (srcType <> stInteger) then begin
    if settings.debugTraversal then
      Tracker.Write('        Recursing deeper');
    try
      Result := rcore(se, me, de, dstrec, obj, bSingle, bDeletions, bOverride);
    except on x : Exception do
      Tracker.Write('      rcore: Exception at '+se.Path+': '+x.Message);
    end;
  end
  // else copy element value
  else if (srcType in stValues) then begin
    seVal := se.EditValue;
    meVal := me.EditValue;
    if (seVal <> meVal) then begin
      if not bSingle then
        CopyElementValue(se, me, de);
      Result := true;
    end
    else if settings.debugSkips then begin
      Tracker.Write(Format('         Skipping, "%s" = "%s"', [seVal, meVal]));
    end;
  end
  else if settings.debugSkips then begin
    Tracker.Write(Format('         Skipping, %s is not a value type',
      [stToString(srcType)]));
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
  
  - Uses HandleElementLife to resolve element creation/deletion.
  - Uses HandleElement to handle arrays, recurse deeper, or copy element
    values
}
function rcore(src, mst, dst: IwbElement; dstRec: IwbMainRecord;
  obj: ISuperObject; bSingle, bDeletions, bOverride: boolean): boolean;
var
  i: integer;
  srcCont, dstCont, mstCont: IwbContainerElementRef;
  se, me, de: IwbElement;
  process, eSingle, eDeletions, eOverride: boolean;
  eObj: ISuperObject;
  eLink: string;
begin
  Result := false;

  // prepare containers
  if not Supports(src, IwbContainerElementRef, srcCont) then begin
    if settings.debugSkips then begin
      Tracker.Write('      Source element not a container.');
      Tracker.Write('      Skipping '+src.Path);
    end;
    exit;
  end;
  if not Supports(dst, IwbContainerElementRef, dstCont) then begin
    if settings.debugSkips then begin
      Tracker.Write('      Destination element not a container.');
      Tracker.Write('      Skipping '+src.Path);
    end;
    exit;
  end;
  if not Supports(mst, IwbContainerElementRef, mstCont) then begin
    if settings.debugSkips then begin
      Tracker.Write('      Master element not a container.');
      Tracker.Write('      Skipping '+src.Path);
    end;
    exit;
  end;

  // copy elements from source to destination if missing AND
  // delete elements missing from source if found in master and destination
  Result := HandleElementLife(srcCont, dstCont, mstCont, dstRec, obj, bSingle, bDeletions, bOverride);
  if bSingle and Result then begin
    if settings.debugSingle then
      Tracker.Write('      Single entity change found at '+src.Path);
    exit;
  end;

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
    eObj := GetElementObj(obj, se.Name);
    process := Assigned(eObj) and (eObj.I['p'] = 1);
    if not process then begin
      if settings.debugSkips then
        Tracker.Write('      Skipping '+se.Path);
      continue;
    end;

    // set element treat as single entity / ignore deletions booleans
    eSingle := bSingle or (eObj.I['s'] = 1);
    eDeletions := bDeletions or (eObj.I['d'] = 1);
    eOverride := bOverride or (eObj.I['o'] = 1);

    // handle element
    Result := HandleElement(se, me, de, dstRec, eObj, eSingle, eDeletions, eOverride);

    // if we're in a single entity and an element is changed, break
    // we don't need to handle anything anymore
    if bSingle and Result then begin
      if settings.debugSingle then
        Tracker.Write('      Single entity change found at '+se.Path);
      break;
    end;

    // if the element we're processing has the single entity flag set
    // and we're not currently in a single entity, copy entire element
    if eSingle and (not bSingle) and Result then try
      if settings.debugSingle then
        Tracker.Write(Format('      Copying single entity %s', [se.path]));
      de.Assign(Low(Integer), se, false);
    except
      on x: Exception do
        Tracker.Write('        rcore: Failed to copy '+se.Path+', '+x.Message);
    end;

    // if another element is linked to the element being processed
    // and the element being processed has been modified, copy the linked
    // element
    eLink := eObj.S['lf'];
    if Result and (eLink <> '') then begin
      firstLink := eLink;
      bLinkProcessed := false;
      CopyLinkedElement(srcCont, dstCont, eLink, obj, dstRec);
    end;
  end;
end;

end.
