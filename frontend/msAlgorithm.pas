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
  procedure CopyElementsIfMissing(srcContainer, dstContainer,
    mstContainer: IwbContainerElementRef; dstRec: IwbMainRecord);
  procedure HandleArray(me, se, de: IwbElement; et: TwbElementType;
    dstrec: IwbMainRecord; depth: Integer; obj: ISuperObject);
  procedure CopyElementValue(se, me, de: IwbElement);
  procedure RecurseDeeper(se, me, de: IwbElement; dstRec: IwbMainRecord;
    depth: Integer; obj: ISuperObject);
  procedure rcore(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
    depth: Integer; obj: ISuperObject);

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

procedure CopyElementsIfMissing(srcContainer, dstContainer,
  mstContainer: IwbContainerElementRef; dstRec: IwbMainRecord);
var
  i: Integer;
  element: IwbElement;
  dstElements, mstElements: TStringList;
begin
  // build element lists
  dstElements := TStringList.Create;
  mstElements := TStringList.Create;
  AddElementsToList(dstContainer, dstElements);
  AddElementsToList(mstContainer, mstElements);

  for i := 0 to Pred(srcContainer.ElementCount) do begin
    element := srcContainer.Elements[i];
    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    if (dstElements.IndexOf(element.Name) = -1)
    and (mstElements.IndexOf(element.Name) = -1) then
      wbCopyElementToRecord(element, dstRec, false, true);
  end;

  // clean up
  dstElements.Free;
  mstElements.Free;
end;

procedure HandleArray(me, se, de: IwbElement; et: TwbElementType;
  dstrec: IwbMainRecord; depth: Integer; obj: ISuperObject);
begin
  // if sorted, deal with sorted array
  if IsSorted(se) then begin
    if settings.debugArrays then
      Tracker.Write('    Sorted array found: '+se.Path);
    try
      //MergeSortedArray(me, se, de, dstrec, depth, obj);
    except on x : Exception do
      Tracker.Write('      !! MergeSortedArray exception: '+x.Message);
    end;
  end
  // else deal with unsorted etSubRecordArray
  else if (et = etSubRecordArray) then begin
    if settings.debugArrays then
      Tracker.Write('    Unsorted etSubRecordArray found: '+se.Path);
    try
      //MergeUnsortedArray(me, se, de, dstrec, depth, obj);
    except on x : Exception do
      Tracker.Write('      !! MergeUnsortedArray exception: '+x.Message);
    end;
  end
  // else deal with unsorted dtArray
  else begin
    if settings.debugArrays then
      Tracker.Write('    Unsorted dtArray found: '+se.Path);
    try
      rcore(se, me, de, dstrec, depth + 1, obj);
    except on x : Exception do
      Tracker.Write('      !! rcore exception: '+x.Message);
    end;
  end;
end;

procedure CopyElementValue(se, me, de: IwbElement);
begin
  if Assigned(me) and settings.debugChanges then begin
    if (not settings.debugTraversal) then
      Tracker.Write('    '+se.Path);
    Tracker.Write('      > Found differing values: '+se.EditValue+' and '+me.EditValue);
  end;
  // try to copy element value to destination element from source element
  try
    de.EditValue := se.EditValue;
  except on x : Exception do
    Tracker.Write('      !! Copy element value exception: '+x.Message);
  end;
end;

procedure RecurseDeeper(se, me, de: IwbElement; dstRec: IwbMainRecord;
  depth: Integer; obj: ISuperObject);
begin
  try
    rcore(se, me, de, dstrec, depth + 1, obj);
  except on x : Exception do
    Tracker.Write('      !! rcore exception in element '+se.Path+': '+x.Message);
  end;
end;

//======================================================================
// rcore: Recursively Copy Overridden Elements
procedure rcore(src, mst, dst: IwbElement; dstrec: IwbMainRecord;
  depth: Integer; obj: ISuperObject);
var
  i, j: integer;
  srcContainer, dstContainer, mstContainer: IwbContainerElementRef;
  se, me, de: IwbElement;
  et: TwbElementType;
  dt: TwbDefType;
  skip: boolean;
  elementObj: ISuperObject;
begin
  // prepare containers
  if not Supports(src, IwbContainerElementRef, srcContainer) then
    exit;
  if not Supports(dst, IwbContainerElementRef, dstContainer) then
    exit;
  if not Supports(mst, IwbContainerElementRef, mstContainer) then
    exit;

  // copy elements from source to destination if missing
  CopyElementsIfMissing(srcContainer, dstContainer, mstContainer, dstRec);

  // loop through subelements
  i := 0;
  j := 0;
  while i < srcContainer.ElementCount do begin
    // assign source, destination, master elements
    se := srcContainer.Elements[i];
    de := dstContainer.ElementByName[se.Name];
    me := mstContainer.ElementByName[se.Name];

    // deftype and elementtype
    et := se.ElementType;
    dt := se.Def.DefType;

    // skip according to setting
    elementObj := GetChildObj(obj, se.Name);
    skip := not (Assigned(elementObj) and (elementObj.I['p'] = 1));
    if skip then begin
      if settings.debugSkips then
        Tracker.Write('  Skipping '+se.Path);
      Inc(i);
      Inc(j);
      continue;
    end;

    // debug messages
    if settings.debugTraversal then
      Tracker.Write('    '+se.Path);
    if settings.debugTypes then
      Tracker.Write('    ets: '+etToString(et)+'  dts: '+dtToString(dt));

    // if destination element doesn't match source element
    if (se.Name <> de.Name) then begin
      // if we're not at the end of the destination elements
      // proceed to next destination element
      // else proceed to next source element
      if (j < dstContainer.ElementCount) then
        Inc(j)
      else
        Inc(i);
      continue;
    end;

    // handle array, recurse deeper, or copy element value
    if (et = etSubRecordArray) or (dt = dtArray) then
      HandleArray(me, se, de, et, dstrec, depth, elementObj)
    else if Supports(se, IwbContainer) and (dt <> dtInteger) then
      RecurseDeeper(se, me, de, dstRec, depth, elementObj)
    else if (dt in ValueElements) and (se.EditValue <> me.EditValue) then
      CopyElementValue(se, me, de);

    // proceed to next element
    Inc(i);
    Inc(j);
  end;
end;

end.
