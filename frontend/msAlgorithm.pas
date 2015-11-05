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
  srcElements, dstElements, mstElements: TStringList;
begin
  Result := false;

  // build element lists
  srcElements := TStringList.Create;
  dstElements := TStringList.Create;
  mstElements := TStringList.Create;
  AddElementsToList(srcCont, srcElements);
  AddElementsToList(dstCont, dstElements);
  AddElementsToList(mstCont, mstElements);

  // handle element creation
  for i := 0 to Pred(srcCont.ElementCount) do begin
    element := srcCont.Elements[i];
    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    if (dstElements.IndexOf(element.Name) = -1)
    and (mstElements.IndexOf(element.Name) = -1) then begin
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
      if (srcElements.IndexOf(element.Name) = -1)
      and (mstElements.IndexOf(element.Name) > -1) then
        dstCont.RemoveElement(element);
    end;
  end;

  // clean up
  srcElements.Free;
  dstElements.Free;
  mstElements.Free;
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
      //Result := MergeSorted(me, se, de, dstrec, depth, obj, bSingle, bDeletions);
    except on x : Exception do
      Tracker.Write(indent+'MergeSortedArray: Exception '+x.Message);
    end;
  end
  // else deal with unsorted etSubRecordArray
  else if (et = etSubRecordArray) then begin
    if settings.debugArrays then
      Tracker.Write(indent+'Unsorted etSubRecordArray found: '+se.Path);
    try
      //Result := MergeUnsorted(me, se, de, dstrec, depth, obj, bSingle, bDeletions);
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
