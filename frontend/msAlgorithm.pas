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

  procedure AddElementsToList(element: IwbElement; var sl: TStringList);
  function HandleElementLife(srcCont, dstCont, mstCont: IwbContainerElementRef;
    dstRec: IwbMainRecord; bSingle, bDeletions: boolean): boolean;
  function HandleArray(se, me, de: IwbElement; et: TwbElementType;
    dstrec: IwbMainRecord; depth: Integer; obj: ISuperObject;
    bSingle, bDeletions: boolean): boolean;
  procedure CopyElementValue(se, me, de: IwbElement; depth: Integer);
  function HandleElement(se, me, de: IwbElement; dstRec: IwbMainRecord;
    depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
  function rcore(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
    depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;

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

procedure AddElementsToList(element: IwbElement; var sl: TStringList);
var
  i: integer;
  childElement: IwbElement;
  container: IwbContainerElementRef;
begin
  // exit if element isn't a container
  if not Supports(element, IwbContainerElementRef, container) then
    exit;

  // loop through children
  for i := 0 to Pred(container.ElementCount) do begin
    childElement := container.Elements[i];
    sl.Add(childElement.Name);
  end;
end;

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
      if bSingle then begin
        Result := true;
        break;
      end;
      wbCopyElementToRecord(element, dstRec, false, true);
    end;
  end;

  // handle element deletion
  if bDeletions then begin
    for i := Pred(dstCont.ElementCount) downto 0 do begin
      element := dstCont.Elements[i];
      if Assigned(mstCont.ElementByName[element.Name])
      and not Assigned(srcCont.ElementByName[element.Name]) then
        dstCont.RemoveElement(element);
    end;
  end;
end;

// BuildSortKeyList: puts the sort keys of elements in a stringlist
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
      Tracker.Write('    Adjusted SortKey: '+skAdj);
    sl.Add(skAdj);
  end;
end;

// GetMasterElement: Gets the first instance of an element (the master)
function GetMasterElement(src, se: IwbElement; dstRec: IwbMainRecord): IwbElement;
const
  debugGetMaster = false;
var
  i, j: integer;
  p, sk: string;
  ovrElement, me: IwbElement;
  ovr, mst: IwbMainRecord;
  ovrCont: IwbContainerElementRef;
  sorted: boolean;
begin
  Result := nil;
  mst := dstRec.MasterOrSelf;
  p := IndexedPath(src);
  sk := se.SortKey[false];
  sorted := not (sk = '');
  // if sorted, look for an element matching sort key
  if sorted then begin
    if debugGetMaster then
      Tracker.Write('  Called GetMasterElement at path '+p+
        ' looking for SortKey '+se.SortKey[false]);
    // loop from override 0 to the second to last override
    for i := 0 to mst.OverrideCount - 2 do begin
      ovr := mst.Overrides[i];
      ovrElement := ElementByIndexedPath(mst, p);
      if not Supports(ovrElement, IwbContainerElementRef, ovrCont) then
        continue;
      for j := 0 to Pred(ovrCont.ElementCount) do begin
        me := ovrCont.Elements[j];
        if (me.SortKey[false] = sk) then begin
          Result := me;
          exit;
        end;
      end;
    end;
  end
  // if unsorted, look for the element using GetAllValues
  else begin
    sk := GetAllValues(se);
    if debugGetMaster then
      Tracker.Write('  Called GetMasterElement at path '+p+' looking for '+sk);
    ovrElement := mst.ElementByPath[p];
    for i := 0 to mst.OverrideCount - 2 do begin
      ovr := mst.Overrides[i];
      ovrElement := ElementByIndexedPath(mst, p);
      if not Supports(ovrElement, IwbContainerElementRef, ovrCont) then
        continue;
      for j := 0 to Pred(ovrCont.ElementCount) do begin
        me := ovrCont.Elements[j];
        if (GetAllValues(me) = sk) then begin
          Result := me;
          break;
        end;
      end;
    end;
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
        Tracker.Write('      > Removing element '+dstCont.Elements[d_ndx].Path+
          ' with key: '+slDst[d_ndx]);
      dstCont.RemoveElement(d_ndx);
      slDst.Delete(d_ndx);
    end;
  end;
end;

function MergeSorted(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
  depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  i, m_ndx, s_ndx, d_ndx: integer;
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
        Tracker.Write('      > Adding element '+IntToStr(i)+' at '+dst.Path+
          ' with key: '+slSrc[i]);
      me := dstCont.Assign(dstCont.ElementCount, se, false);
      if settings.debugArrays then
        Tracker.Write('      > '+GetAllValues(me));
      slDst.Add(slSrc[i]);
    end
    // Step 3.5: If array element is in dst and has subelements, traverse it.
    else if (d_ndx > -1) and ((dt = dtStruct) or (et = etSubRecordArray)) then begin
	    if settings.debugTraversal then
        Tracker.Write('      > Traversing element '+se.Path+' with key: '+slSrc[i]);
      if settings.debugTraversal and settings.debugArrays then
        Tracker.Write('      > Source Element: '+GetAllValues(se)+
          #13#10'      > Destination Element: '+GetAllValues(dstCont.Elements[d_ndx]));
      try
        rcore(se, GetMasterElement(src, se, dstrec), dstCont.Elements[d_ndx],
          dstrec, depth + 2, obj, bSingle, bDeletions);
      except on x : Exception do begin
          Tracker.Write('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    else if (d_ndx > -1) and (et = etSubRecordStruct) then begin
	    if settings.debugTraversal then
        Tracker.Write('      > Traversing element '+se.Path+' with key: '+slSrc[i]);
      if settings.debugTraversal and settings.debugArrays then
        Tracker.Write('      > Source Element: '+GetAllValues(se)+
        #13#10'      > Destination Element: '+GetAllValues(dstCont.Elements[d_ndx]));
      try
        rcore(se, GetMasterElement(src, se, dstrec), dstCont.Elements[d_ndx], dstrec,
          depth + 2, obj, bSingle, bDeletions);
      except on x : Exception do begin
          Tracker.Write('      !! rcore exception: '+x.Message);
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
  depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  i, m_ndx, s_ndx, d_ndx: integer;
  se: IwbElement;
  slMst, slSrc, slDst: TStringList;
  srcCont, dstCont: IwbContainerElementRef;
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
  BuildElementList(src, slSrc);
  BuildElementList(mst, slMst);
  BuildElementList(dst, slDst);

  // Step 2: Remove elements that are in mst and dst, but missing from src
  if bDeletions then
    Result := RemoveArrayElements(slSrc, slMst, slDst, dstCont);


  // Step 3: Copy array elements in src that aren't in mst or dst
  // TODO: Make into separate method
  for i := 0 to Pred(slSrc.Count) do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    se := srcCont.Elements[i];

    if (m_ndx = -1) and (d_ndx = -1) then begin
      if settings.debugArrays then
        Tracker.Write('      > Adding element at '+dst.Path+' with values: '+slSrc[i]);
      dstCont.Assign(dstCont.ElementCount, se, false);
      slDst.Add(slSrc[i]);
    end;
  end;

  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

function HandleArray(se, me, de: IwbElement; et: TwbElementType;
  dstrec: IwbMainRecord; depth: Integer; obj: ISuperObject;
  bSingle, bDeletions: boolean): boolean;
var
  indent: string;
begin
  Result := false;
  indent := StringOfChar(' ', depth * 2);

  // if sorted, deal with sorted array
  if IsSorted(se) then begin
    if settings.debugArrays then
      Tracker.Write(indent+'Sorted array found: '+se.Path);
    try
      Result := MergeSorted(me, se, de, dstrec, depth, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write(indent+'MergeSortedArray: Exception '+x.Message);
    end;
  end
  // else deal with unsorted etSubRecordArray
  else if (et = etSubRecordArray) then begin
    if settings.debugArrays then
      Tracker.Write(indent+'Unsorted etSubRecordArray found: '+se.Path);
    try
      Result := MergeUnsorted(me, se, de, dstrec, depth, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write(indent+'MergeUnsortedArray: Exception '+x.Message);
    end;
  end
  // else deal with unsorted dtArray
  else begin
    if settings.debugArrays then
      Tracker.Write(indent+'Unsorted dtArray found: '+se.Path);
    try
      Result := rcore(se, me, de, dstrec, depth + 1, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write(indent+'rcore: Exception '+x.Message);
    end;
  end;
end;

procedure CopyElementValue(se, me, de: IwbElement; depth: Integer);
var
  indent: string;
begin
  indent := StringOfChar(' ', depth * 2);

  if Assigned(me) and settings.debugChanges then begin
    if (not settings.debugTraversal) then
      Tracker.Write(indent+se.Path);
    Tracker.Write(indent+'> Found differing values: '+se.EditValue+', '+me.EditValue);
  end;

  // try to copy element value to destination element from source element
  try
    de.EditValue := se.EditValue;
  except on x : Exception do
    Tracker.Write(indent+'CopyElementValue: Exception '+x.Message);
  end;
end;

function HandleElement(se, me, de: IwbElement; dstRec: IwbMainRecord;
  depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
var
  et: TwbElementType;
  dt: TwbDefType;
  indent: string;
begin
  Result := false;

  // deftype and elementtype
  et := se.ElementType;
  dt := se.Def.DefType;

  // debug messages
  indent := StringOfChar(' ', depth * 2);
  if settings.debugTraversal then
    Tracker.Write(indent+se.Path);
  if settings.debugTypes then
    Tracker.Write(indent+'ets: '+etToString(et)+'  dts: '+dtToString(dt));

  // handle array, recurse deeper, or copy element value
  if (et = etSubRecordArray) or (dt = dtArray) then
    Result := HandleArray(se, me, de, et, dstRec, depth, obj, bSingle, bDeletions)
  else if Supports(se, IwbContainer) and (dt <> dtInteger) then begin
    try
      Result := rcore(se, me, de, dstrec, depth + 1, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write(indent+'rcore: Exception at '+se.Path+': '+x.Message);
    end;
  end
  else if (dt in ValueElements) and (se.EditValue <> me.EditValue) then begin
    if not bSingle then
      CopyElementValue(se, me, de, depth);
    Result := true;
  end;
end;

//======================================================================
// rcore: Recursively Copy Overridden Elements
function rcore(src, mst, dst: IwbElement; dstRec: IwbMainRecord;
  depth: Integer; obj: ISuperObject; bSingle, bDeletions: boolean): boolean;
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
        Tracker.Write(StringOfChar(' ', depth * 2) + 'Skipping '+se.Path);
      continue;
    end;

    // set element treat as single entity / ignore deletions booleans
    eSingle := bSingle or (eObj.I['s'] = 1);
    eDeletions := bDeletions or (eObj.I['i'] = 1);

    // handle element
    Result := HandleElement(se, me, de, dstRec, depth, eObj, eSingle, eDeletions);

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
