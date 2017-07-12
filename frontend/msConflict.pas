unit msConflict;

interface

uses
  Classes, SysUtils,
  // mte units
  mteBase,
  // ms units
  msCore,
  // xEdit units
  wbInterface, wbImplementation, wbHelpers;

type
  // States used in InitNode to indicate states a node shall initially have.
  TVirtualNodeInitState = (
    ivsDisabled,
    ivsExpanded,
    ivsHasChildren,
    ivsMultiline,
    ivsSelected,
    ivsFiltered,
    ivsReInit{>>>},
    ivsHidden{<<<}
  );
  TVirtualNodeInitStates = set of TVirtualNodeInitState;

  // navnode flags
  TNavNodeFlag = (
    nnfInjected,
    nnfNotReachable,
    nnfReferencesInjected
  );

  TNavNodeFlags = set of TNavNodeFlag;

  // navnode data
  PNavNodeData = ^TNavNodeData;
  TNavNodeData = record
    Element      : IwbElement;
    Container    : IwbContainer;
    ConflictAll  : TConflictAll;
    ConflictThis : TConflictThis;
    Flags        : TNavNodeFlags;
  end;

  // node flags
  TViewNodeFlag = (
    vnfDontShow,
    vnfIgnore
  );
  TViewNodeFlags = set of TViewNodeFlag;

  // node data
  PViewNodeData = ^TViewNodeData;
  TViewNodeData = record
    Element: IwbElement;
    Container: IwbContainerElementRef;
    ConflictAll: TConflictAll;
    ConflictThis: TConflictThis;
    ViewNodeFlags: TViewNodeFlags;
  end;

  // collections of node datas
  TViewNodeDatas = array[Word] of TViewNodeData;
  PViewNodeDatas = ^TViewNodeDatas;

  TDynViewNodeDatas = array of TViewNodeData;

  // exposed methods
  procedure ConflictLevelForMainRecord(const aMainRecord: IwbMainRecord;
    out aConflictAll: TConflictAll; out aConflictThis: TConflictThis);
  function ConflictLevelForChildNodeDatas(const aNodeDatas: TDynViewNodeDatas;
    aSiblingCompare, aInjected: Boolean): TConflictAll;
  function ConflictLevelForNodeDatas(const aNodeDatas: PViewNodeDatas;
    aNodeCount: Integer; aSiblingCompare, aInjected: Boolean): TConflictAll;
  function ConflictAllForElements(e1, e2: IwbElement; aSiblingCompare,
    aInjected: Boolean): TConflictAll;
  function ConflictThisForMainRecord(aMainRecord: IwbMainRecord): TConflictThis;
  function ConflictAllForMainRecord(aMainRecord: IwbMainRecord): TConflictAll;
  function IsITPO(rec: IwbMainRecord): Boolean;
  function IsITM(rec: IwbMainRecord): Boolean;

implementation

procedure AppendToNodeDatas(var NodeDatas: TDynViewNodeDatas; e: IwbElement);
var
  Container: IwbContainerElementRef;
begin
  SetLength(NodeDatas, Succ(Length(NodeDatas)));
  NodeDatas[Pred(Length(NodeDatas))].Element := e;
  if Supports(e, IwbContainerElementRef, Container) and (Container.ElementCount > 0) then
    NodeDatas[Pred(Length(NodeDatas))].Container := Container;
end;

function ConflictAllForElements(e1, e2: IwbElement; aSiblingCompare,
  aInjected: Boolean): TConflictAll;
var
  NodeDatas: TDynViewNodeDatas;
begin
  // prepare node datas
  AppendToNodeDatas(NodeDatas, e1);
  AppendToNodeDatas(NodeDatas, e2);

  // compute the conflict level
  Result := caUnknown;
  if Length(NodeDatas) > 0 then
    if Assigned(NodeDatas[0].Container) then
      Result := ConflictLevelForChildNodeDatas(NodeDatas, aSiblingCompare, aInjected)
    else
      Result := ConflictLevelForNodeDatas(@NodeDatas[0], Length(NodeDatas), aSiblingCompare, aInjected);
end;

function NodeDatasForMainRecord(const aMainRecord: IwbMainRecord): TDynViewNodeDatas;
var
  Master        : IwbMainRecord;
  Rec           : IwbMainRecord;
  i, j          : Integer;
  Records       : TStringList;
  AnyHidden     : Boolean;
  IsNonOverride : Boolean;
  EditorID      : string;
  FormID        : Cardinal;
  LoadOrder     : Integer;
  Group         : IwbGroupRecord;
  Signature     : TwbSignature;
  plugin        : TPlugin;
  aFile         : IwbFile;
begin
  Assert(wbLoaderDone);
  IsNonOverride := False;
  AnyHidden := False;

  if aMainRecord.Signature = 'GMST' then begin
    IsNonOverride := True;
    EditorID := aMainRecord.EditorID;
    SetLength(Result, PluginsList.Count);
    Master := nil;
    for i := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      aFile := plugin._File;
      Group := aFile.GroupBySignature['GMST'];
      if Assigned(Group) then begin
        Rec := Group.MainRecordByEditorID[EditorID];
        if Assigned(Rec) then begin
          if not Assigned(Master) then
            Master := Rec;
          Result[i].Element := Rec;
        end;
      end;
    end;

  end else if (aMainRecord.Signature = 'NAVI') (* or (aMainRecord.Signature = 'TES4') *) then begin
    IsNonOverride := True;
    Signature := aMainRecord.Signature;
    FormID := aMainRecord.FormID;
    LoadOrder := aMainRecord.GetFile.LoadOrder;
    SetLength(Result, 0);
    Master := nil;
    for i := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      aFile := plugin._File;
      if aFile.LoadOrder = LoadOrder then begin
        Group := aFile.GroupBySignature[Signature];
        if Assigned(Group) then begin
          Rec := Group.MainRecordByFormID[FormID];
          if Assigned(Rec) then begin
            j := Length(Result);
            SetLength(Result, j+1);
            if not Assigned(Master) then
              Master := Rec;
            Result[j].Element := Rec;
          end;
        end;
      end;
    end;

  end else if (aMainRecord.Signature = 'TES4') then begin
    IsNonOverride := True;
    Signature := aMainRecord.Signature;
    LoadOrder := aMainRecord.GetFile.LoadOrder;
    SetLength(Result, 0);
    Master := nil;
    for i := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      aFile := plugin._File;
      if aFile.LoadOrder = LoadOrder then begin
        // header of .dat file, show only itself
        if SameText(ExtractFileExt(aMainRecord.GetFile.FileName), '.dat') and not SameText(ExtractFileExt(aFile.FileName), '.dat') then
          Continue;
        // skip .dat file header by default
        if not SameText(ExtractFileExt(aMainRecord.GetFile.FileName), '.dat') and SameText(ExtractFileExt(aFile.FileName), '.dat') then
          Continue;
        Rec := aFile.Elements[0] as IwbMainRecord;
        if Assigned(Rec) then begin
          j := Length(Result);
          SetLength(Result, j+1);
          if not Assigned(Master) then
            Master := Rec;
          Result[j].Element := Rec;
        end;
      end;
    end;

  end else begin
    Master := aMainRecord.MasterOrSelf;

    SetLength(Result, Succ(Master.OverrideCount));

    AnyHidden := Master.IsHidden;
    if not AnyHidden then
      for i := 0 to Pred(Master.OverrideCount) do begin
        AnyHidden := Master.Overrides[i].IsHidden;
        if AnyHidden then
          Break;
      end;
  end;

  if (Length(Result) > 1) and ({ModGroupsEnabled or }AnyHidden) or IsNonOverride then begin

    Records := TStringList.Create;
    try
      if IsNonOverride then begin
        for i := Low(Result) to High(Result) do
          if Supports(Result[i].Element, IwbMainRecord, Rec) then
             Records.AddObject(Rec._File.FileName, Pointer(Rec));
        Result := nil;
      end else begin
        Records.AddObject(Master._File.FileName, Pointer(Master));
        for i := 0 to Pred(Master.OverrideCount) do begin
          Rec := Master.Overrides[i];
          Records.AddObject(Rec._File.FileName, Pointer(Rec));
        end;
      end;

      {f ModGroupsEnabled then repeat
        MadeChanges := False;
        sl := TStringList.Create;
        try
          for i := 0 to Pred(ModGroups.Count) do begin
            sl.Assign(TStrings(ModGroups.Objects[i]));
            for j := Pred(sl.Count) downto 0 do begin
              k := Records.IndexOf(sl[j]);
              if K > 0 then // >, not >=, never hide the original master
                sl.Objects[j] := TObject(k)
              else
                sl.Delete(j);
            end;
            if sl.Count > 1 then begin
              k := Integer(sl.Objects[0]);
              j := 1;
              if k = 0 then begin
                while (j < sl.Count) and (Integer(sl.Objects[j]) = k + 1) do begin
                  Records.Objects[Integer(sl.Objects[Pred(j)])] := nil;
                  Inc(k);
                  Inc(j);
                end;
                Inc(j);
              end;
              while (j < sl.Count) do begin
                Records.Objects[Integer(sl.Objects[Pred(j)])] := nil;
                Inc(j);
              end;
              for j := Pred(Records.Count) downto 0 do
                if Records.Objects[j] = nil then begin
                  Records.Delete(j);
                  MadeChanges := True;
                end;
            end;
            if Records.Count < 2 then
              Break;
          end;
        finally
          sl.Free;
        end;
      until not MadeChanges;}

      i := 0;
      while (i < Records.Count) and (Records.Count > 1) do
        if IwbElement(Pointer(Records.Objects[i])).IsHidden then
          Records.Delete(i)
        else
          Inc(i);

      SetLength(Result, Records.Count);
      for i := 0 to Pred(Records.Count) do
        with Result[i] do begin
          Rec := IwbMainRecord(Pointer(Records.Objects[i]));
          if i = 0 then
            Master := Rec;

          Container := Rec as IwbContainerElementRef;
          Element := Container;
          if (Container.ElementCount = 0) or (Rec.Signature <> Master.Signature) then
            Container := nil;
        end;

    finally
      FreeAndNil(Records);
    end;

    Exit;
  end;

  Result[0].Element := Master;
  Result[0].Container := Master as IwbContainerElementRef;
  if Master.ElementCount < 1 then
    Result[0].Container := nil;

  for i := 0 to Pred(Master.OverrideCount) do
    with Result[Succ(i)] do begin
      Container := Master.Overrides[i] as IwbContainerElementRef;
      Element := Container;
      if (Container.ElementCount = 0) or (Master.Overrides[i].Signature <> Master.Signature) then
        Container := nil;
    end;
end;

procedure ConflictLevelForMainRecord(const aMainRecord: IwbMainRecord;
  out aConflictAll: TConflictAll; out aConflictThis: TConflictThis);

  procedure Fix(const aMainRecord: IwbMainRecord);
  begin
    with aMainRecord do begin
      ConflictAll := aConflictAll;
      if ConflictThis = ctUnknown then begin
        ConflictThis := ctHiddenByModGroup;
      end;
    end;
  end;

var
  NodeDatas                   : TDynViewNodeDatas;
  i                           : Integer;
  Master                      : IwbMainRecord;
  KeepAliveRoot               : IwbKeepAliveRoot;
begin
  KeepAliveRoot := wbCreateKeepAliveRoot;

  aConflictAll := aMainRecord.ConflictAll;
  aConflictThis := aMainRecord.ConflictThis;

  if aConflictAll > caUnknown then
    Exit;

  Master := aMainRecord.MasterOrSelf;
  if (Master.OverrideCount = 0) and not wbTranslationMode and not (Master.Signature = 'GMST') then begin
    aConflictAll := caOnlyOne;
    aConflictThis := ctOnlyOne;
    aMainRecord.ConflictAll := aConflictAll;
    aMainRecord.ConflictThis := aConflictThis;
  end else begin
    NodeDatas := NodeDatasForMainRecord(aMainRecord);
    if Length(NodeDatas) = 1 then begin
      aConflictAll := caOnlyOne;
      NodeDatas[0].ConflictAll := caOnlyOne;
      NodeDatas[0].ConflictThis := ctOnlyOne;
    {end else if wbQuickShowConflicts and (Length(NodeDatas) = 2) then begin
      aConflictAll := caOverride;
      NodeDatas[0].ConflictAll := caOverride;
      NodeDatas[1].ConflictAll := caOverride;
      NodeDatas[0].ConflictThis := ctMaster;
      NodeDatas[1].ConflictThis := ctOverride;}
    end else
      aConflictAll := ConflictLevelForChildNodeDatas(NodeDatas, False, (aMainRecord.MasterOrSelf.IsInjected and not (aMainRecord.Signature = 'GMST')) );

    for i := Low(NodeDatas) to High(NodeDatas) do
      with NodeDatas[i] do
        if Assigned(Element) then
          with (Element as IwbMainRecord) do begin
            ConflictAll := aConflictAll;
            ConflictThis := NodeDatas[i].ConflictThis;
          end;

    Fix(Master);
    for i := 0 to Pred(Master.OverrideCount) do
      Fix(Master.Overrides[i]);

    aConflictThis := aMainRecord.ConflictThis;
  end;
end;

procedure InitChilds(const aNodeDatas: PViewNodeDatas; aNodeCount: Integer;
  var aChildCount: Cardinal);
var
  NodeData                    : PNavNodeData;
  Container                   : IwbContainer;
  FirstContainer              : IwbContainer;
  SortableContainer           : IwbSortableContainer;
  Element                     : IwbElement;
  i, j, k                     : Integer;
  SortedCount                 : Integer;
  NonSortedCount              : Integer;
  SortedKeys                  : array of TnxFastStringListCS;
  Sortables                   : array of IwbSortableContainer;
  SortKey                     : string;
  LastSortKey                 : string;
  DupCounter                  : Integer;
begin
  SortedCount := 0;
  NonSortedCount := 0;
  FirstContainer := nil;
  for i := 0 to Pred(aNodeCount) do begin
    NodeData := @aNodeDatas[i];
    Container := NodeData.Container;
    if not Assigned(FirstContainer) then
      FirstContainer := Container;
    if Assigned(Container) then
      if Supports(Container, IwbSortableContainer, SortableContainer) and SortableContainer.Sorted then
        Inc(SortedCount)
      else
        Inc(NonSortedCount);
  end;

  if (NonSortedCount > 0) and (SortedCount > 0) then begin
    if Assigned(FirstContainer) then
      ;//PostAddMessage('Warning: Comparing sorted and unsorted entry for "' + FirstContainer.Path + '" in "'+FirstContainer.ContainingMainRecord.Name+'"');
    SortedCount := 0;
  end;

  if SortedCount > 0 then begin
//    Assert(NonSortedCount = 0);

    SetLength(SortedKeys, Succ(aNodeCount));
    for i := Low(SortedKeys) to High(SortedKeys) do begin
      SortedKeys[i] := TnxFastStringListCS.Create;
      SortedKeys[i].Sorted := True;
      SortedKeys[i].Duplicates := dupError;
    end;

    try
      SortedKeys[aNodeCount].Duplicates := dupIgnore;

      SetLength(Sortables, aNodeCount);

      for i := 0 to Pred(aNodeCount) do
        if Supports(aNodeDatas[i].Container, IwbSortableContainer, Sortables[i]) then begin
          SortableContainer := Sortables[i];
          DupCounter := 0;
          LastSortKey := '';
          for j := 0 to Pred(SortableContainer.ElementCount) do begin
            Element := SortableContainer.Elements[j];
            SortKey := Element.SortKey[False];
            if SameStr(LastSortKey, SortKey) then
              Inc(DupCounter)
            else begin
              DupCounter := 0;
              LastSortKey := SortKey;
            end;

            SortKey := SortKey + '<' + IntToHex64(DupCounter, 4) + '>';

            SortedKeys[i].AddObject(SortKey, Pointer(Element));
            SortedKeys[aNodeCount].Add(SortKey);
          end;
        end;

      aChildCount := SortedKeys[aNodeCount].Count;

      for j := 0 to Pred(aChildCount) do begin
        SortKey := SortedKeys[aNodeCount].Strings[j];
        for i := 0 to Pred(aNodeCount) do
          if SortedKeys[i].Find(SortKey, k) then
            IwbElement(Pointer(SortedKeys[i].Objects[k])).SortOrder := j;
      end;

    finally

      for i := Low(SortedKeys) to High(SortedKeys) do
        FreeAndNil(SortedKeys[i]);

    end;

  end
  else
    for i := 0 to Pred(aNodeCount) do begin
      NodeData := @aNodeDatas[i];
      Container := NodeData.Container;

      if Assigned(Container) then begin
        case Container.ElementType of
          etMainRecord, etSubRecordStruct: begin
              aChildCount := (Container.Def as IwbRecordDef).MemberCount;
              Inc(aChildCount, Container.AdditionalElementCount);
              if Cardinal(Container.ElementCount) > aChildCount then begin
                //PostAddMessage('Error: Container.ElementCount {'+IntToStr(Container.ElementCount)+'} > aChildCount {'+IntToStr(aChildCount)+'} for ' + Container.Path + ' in ' + Container.ContainingMainRecord.Name);
                //for j := 0 to Pred(Container.ElementCount) do
                //PostAddMessage('  #'+IntToStr(j)+': ' + Container.Elements[j].Name);
                //Assert(Cardinal(Container.ElementCount) <= aChildCount);
              end;
            end;
          etSubRecordArray, etArray, etStruct, etSubRecord, etValue, etUnion, etStructChapter:
            if aChildCount < Cardinal(Container.ElementCount) then
              aChildCount := Container.ElementCount;
        end;
      end;
    end;
end;

procedure InitNodes(const aNodeDatas: PViewNodeDatas;
  const aParentDatas: PViewNodeDatas;
  aNodeCount: Integer;
  aIndex: Cardinal;
  var aInitialStates: TVirtualNodeInitStates);
var
  NodeData                    : PViewNodeData;
  ParentData                  : PViewNodeData;
  Container                   : IwbContainerElementRef;
  SortableContainer           : IwbSortableContainer;
  i                           : Integer;
begin
  for i := 0 to Pred(aNodeCount) do begin
    NodeData := @aNodeDatas[i];
    ParentData := @aParentDatas[i];

    Container := ParentData.Container;
    if Assigned(Container) then begin
      if Supports(Container, IwbSortableContainer, SortableContainer) and SortableContainer.Sorted then
        NodeData.Element := Container.ElementBySortOrder[aIndex]
      else
        case Container.ElementType of
          etMainRecord, etSubRecordStruct:
            NodeData.Element := Container.ElementBySortOrder[aIndex];
          etSubRecordArray, etArray, etStruct, etSubRecord, etValue, etUnion, etStructChapter:
            if aIndex < Cardinal(Container.ElementCount) then
              NodeData.Element := Container.Elements[aIndex];
        end;
    end;
    if Assigned(NodeData.Element) and NodeData.Element.DontShow then begin
      NodeData.Element := nil;
      Include(NodeData.ViewNodeFlags, vnfDontShow);
    end;
  end;

  aInitialStates := [ivsDisabled];
  for i := 0 to Pred(aNodeCount) do
    with aNodeDatas[i] do begin
      if Assigned(Element) then
        Exclude(aInitialStates, ivsDisabled)
      else
        if Assigned(aParentDatas) and ((vnfIgnore in aParentDatas[i].ViewNodeFlags) or (Assigned(aParentDatas[i].Element) and (aParentDatas[i].Element.ConflictPriority = cpIgnore))) then
          Include(ViewNodeFlags, vnfIgnore);

      if not Assigned(Container) then
        if Supports(Element, IwbContainerElementRef, Container) then begin
          //          if Container.ElementCount = 0 then
          //            Container := nil;
        end;

      if Assigned(Container) then
        if Container.ElementCount > 0 then
          Include(aInitialStates, ivsHasChildren);
    end;
end;

function ConflictLevelForChildNodeDatas(const aNodeDatas: TDynViewNodeDatas;
  aSiblingCompare, aInjected: Boolean): TConflictAll;
var
  ChildCount                  : Cardinal;
  i, j                        : Integer;
  NodeDatas                   : TDynViewNodeDatas;
  InitialStates               : TVirtualNodeInitStates;
  ConflictAll                 : TConflictAll;
  ConflictThis                : TConflictThis;
  Element                     : IwbElement;
begin
  case Length(aNodeDatas) of
    0: Result := caUnknown;
    1: begin
      Result := caOnlyOne;
      aNodeDatas[0].ConflictThis := ctOnlyOne;
    end;
  else
    Result := caNoConflict;
  end;

  if wbTranslationMode then begin
    if Result < caOnlyOne then
      Exit;
  end
  else begin
    if Result < caNoConflict then
      Exit;
  end;

  ChildCount := 0;
  InitChilds(@aNodeDatas[0], Length(aNodeDatas), ChildCount);
  if ChildCount > 0 then
    for i := 0 to Pred(ChildCount) do begin
      NodeDatas := nil;
      SetLength(NodeDatas, Length(aNodeDatas));
      InitialStates := [];
      InitNodes(@NodeDatas[0], @aNodeDatas[0], Length(aNodeDatas), i, InitialStates);
      if not (ivsDisabled in InitialStates) then begin

        if ivsHasChildren in InitialStates then
          ConflictAll := ConflictLevelForChildNodeDatas(NodeDatas, aSiblingCompare, aInjected)
        else
          ConflictAll := ConflictLevelForNodeDatas(@NodeDatas[0], Length(NodeDatas), aSiblingCompare, aInjected);

        if ConflictAll > Result then
          Result := ConflictAll;

        for j := Low(aNodeDatas) to High(aNodeDatas) do
          if NodeDatas[j].ConflictThis > aNodeDatas[j].ConflictThis then
            aNodeDatas[j].ConflictThis := NodeDatas[j].ConflictThis;

      end
      else begin

        ConflictThis := ctNotDefined;

        for j := Low(aNodeDatas) to High(aNodeDatas) do begin
          Element := aNodeDatas[j].Container;
          if Assigned(Element) then
            Break;
        end;

        if Assigned(Element) and (Element.ElementType in [etMainRecord, etSubRecordStruct]) then begin
          j := (Element as IwbContainer).AdditionalElementCount;
          if i >= j then
            with (Element.Def as IwbRecordDef).Members[i - j] do
              if (wbTranslationMode and (ConflictPriority[nil] <> cpTranslate)) or
                (wbTranslationMode and (ConflictPriority[nil] = cpIgnore)) then
                ConflictThis := ctIgnored;
        end;

        for j := Low(aNodeDatas) to High(aNodeDatas) do
          if ConflictThis > aNodeDatas[j].ConflictThis then
            aNodeDatas[j].ConflictThis := ConflictThis;
      end;
    end;
end;

function ConflictLevelForNodeDatas(const aNodeDatas: PViewNodeDatas;
  aNodeCount: Integer; aSiblingCompare, aInjected: Boolean): TConflictAll;
var
  Element                : IwbElement;
  CompareElement         : IwbElement;
  i, j                   : Integer;
  UniqueValues           : TnxFastStringListCS;

  MasterPosition         : Integer;
  FirstElement           : IwbElement;
  FirstElementNotIgnored : IwbElement;
  LastElement            : IwbElement;
  SameAsLast             : Boolean;
  SameAsFirst            : Boolean;
  OverallConflictThis    : TConflictThis;
  Priority               : TwbConflictPriority;
  ThisPriority           : TwbConflictPriority;
  FoundAny               : Boolean;
begin
//  if aSiblingCompare then
//    Priority := cpBenign
//  else
//    Priority := cpNormal;
//  IgnoreConflicts := False;
  FoundAny := False;
  MasterPosition := 0;
  OverallConflictThis := ctUnknown;
  case aNodeCount of
    0: Result := caUnknown;
    1: begin
        Element := aNodeDatas[0].Element;
        if Assigned(Element) then begin
          if Element.ConflictPriority = cpIgnore then
            aNodeDatas[0].ConflictThis := ctIgnored
          else
            aNodeDatas[0].ConflictThis := ctOnlyOne;
        end else
          aNodeDatas[0].ConflictThis := ctNotDefined;
        Result := caOnlyOne;
      end
  else
    LastElement := aNodeDatas[Pred(aNodeCount)].Element;
    FirstElement := aNodeDatas[0].Element;

    UniqueValues := TnxFastStringListCS.Create;
    UniqueValues.Sorted := True;
    UniqueValues.Duplicates := dupIgnore;
    Priority := cpNormal;
    try
      for i := 0 to Pred(aNodeCount) do begin
        Element := aNodeDatas[i].Element;
        if Assigned(Element) then begin
          FoundAny := True;
          Priority := Element.ConflictPriority;
          if Priority = cpNormalIgnoreEmpty then begin
            FirstElement := Element;
            MasterPosition := i;
            for j := Pred(aNodeCount) downto i do begin
              LastElement := aNodeDatas[j].Element;
              if Assigned(LastElement) then
                Break;
            end;
          end;
          if Element.ConflictPriorityCanChange then begin
            for j := Succ(i) to Pred(aNodeCount) do begin
              Element := aNodeDatas[j].Element;
              if Assigned(Element) then begin
                ThisPriority := Element.ConflictPriority;
                if ThisPriority > Priority then
                  Priority := ThisPriority;
              end;
            end;
          end;
          Break;
        end;
      end;

      if aSiblingCompare then
        if Priority > cpBenign then
          Priority := cpBenign;
      if aInjected and (Priority >= cpNormal) then
        Priority := cpCritical;

      if (Priority > cpIgnore) and (not Assigned(FirstElement) or (FirstElement.ConflictPriority = cpIgnore)) then
        FirstElementNotIgnored := nil
      else
        FirstElementNotIgnored := FirstElement;

      for i := 0 to Pred(aNodeCount) do begin
        Element := aNodeDatas[i].Element;
        if Assigned(Element) then begin
          ThisPriority := Element.ConflictPriority;
          if ThisPriority <> cpIgnore then
            UniqueValues.Add(Element.SortKey[True]);
        end else begin
          ThisPriority := Priority;
          if not (vnfIgnore in aNodeDatas[i].ViewNodeFlags) then
            if Priority <> cpNormalIgnoreEmpty then
              UniqueValues.Add('');
        end;

        if (ThisPriority = cpNormalIgnoreEmpty) and not Assigned(Element) then
          aNodeDatas[i].ConflictThis := ctIgnored
        else if ThisPriority = cpIgnore then
          aNodeDatas[i].ConflictThis := ctIgnored
        else if aSiblingCompare then
          aNodeDatas[i].ConflictThis := ctOnlyOne
        else if i = MasterPosition then begin

          if Assigned(Element) then
            aNodeDatas[i].ConflictThis := ctMaster
          else
            aNodeDatas[i].ConflictThis := ctUnknown;

        end else begin
          SameAsLast := (i = Pred(aNodeCount)) or not (
            (Assigned(Element) <> Assigned(LastElement)) or
            (Assigned(Element) and not SameStr(Element.SortKey[True], LastElement.SortKey[True]))
            );

          SameAsFirst := not (
            (Assigned(Element) <> Assigned(FirstElementNotIgnored)) or
            (Assigned(Element) and not SameStr(Element.SortKey[True], FirstElementNotIgnored.SortKey[True]))
            );

          if not SameAsFirst and
             (ThisPriority = cpBenignIfAdded) and
             SameAsLast and  // We are not overriden later
             not Assigned(FirstElementNotIgnored) then begin // The master did not have that element
            ThisPriority := cpBenign;
            Priority := cpBenign;
            SameAsFirst := True;
          end;

          if SameAsFirst then
            aNodeDatas[i].ConflictThis := ctIdenticalToMaster
          else if SameAsLast then
            aNodeDatas[i].ConflictThis := ctConflictWins
          else
            aNodeDatas[i].ConflictThis := ctConflictLoses;
        end;

        if (ThisPriority = cpBenign) and (aNodeDatas[i].ConflictThis > ctConflictBenign) then
          aNodeDatas[i].ConflictThis := ctConflictBenign;

        if aNodeDatas[i].ConflictThis > OverallConflictThis then
          OverallConflictThis := aNodeDatas[i].ConflictThis;
      end;

      case UniqueValues.Count of
        0: Result := caNoConflict;
        1: Result := caNoConflict;
        2: begin
            Element := aNodeDatas[0].Element;
            CompareElement := aNodeDatas[Pred(aNodeCount)].Element;
            if (Assigned(Element) <> Assigned(CompareElement)) or
              (Assigned(Element) and not SameStr(Element.SortKey[True], CompareElement.SortKey[True])) then
              Result := caOverride
            else if (UniqueValues.IndexOf('') >= 0) and Assigned(CompareElement) and (CompareElement.SortKey[True] <> '') then
              Result := caOverride
            else
              Result := caConflict;
          end
      else
        Result := caConflict;
      end;

      if aSiblingCompare and (Result > caConflictBenign) then
        Result := caConflictBenign;

      if not FoundAny then
        for i := 0 to Pred(aNodeCount) do
          aNodeDatas[i].ConflictThis := ctNotDefined;

      if Result > caNoConflict then
        case Priority of
          cpBenign: Result := caConflictBenign;
          cpCritical: begin
              if UniqueValues.Find('', i) then
                UniqueValues.Delete(i);
              if UniqueValues.Count > 1 then
                Result := caConflictCritical;
            end;
        end;

      if Priority > cpBenign then
        if OverallConflictThis > ctOverride then
          with aNodeDatas[Pred(aNodeCount)] do
            if ConflictThis < ctOverride then
              if ConflictThis = ctIdenticalToMaster then
                ConflictThis := ctIdenticalToMasterWinsConflict
              else
                ConflictThis := ctConflictWins;

      if Result in [caNoConflict, caOverride, caConflict] then
        for i := 0 to Pred(aNodeCount) do begin
          case aNodeDatas[i].ConflictThis of
            ctIdenticalToMaster: case Result of
                caNoConflict: ;
                caOverride, caConflict: if i = Pred(aNodeCount) then
                  aNodeDatas[i].ConflictThis := ctIdenticalToMasterWinsConflict
              end;
            ctConflictWins: case Result of
              caNoConflict: aNodeDatas[i].ConflictThis := ctIdenticalToMaster;
              caOverride: aNodeDatas[i].ConflictThis := ctOverride;
              caConflict: ;
            end;
          end;
        end;

      if Result < caConflict then
        for i := 0 to Pred(aNodeCount) do
          if aNodeDatas[i].ConflictThis >= ctIdenticalToMasterWinsConflict then begin
            Result := caConflict;
            Break;
          end;

    finally
      FreeAndNil(UniqueValues);
    end;
  end;
end;

function ConflictThisForMainRecord(aMainRecord: IwbMainRecord): TConflictThis;
var
  ct: TConflictThis;
  ca: TConflictAll;
begin
  ConflictLevelForMainRecord(aMainRecord, ca, ct);
  Result := ct;
end;

function ConflictAllForMainRecord(aMainRecord: IwbMainRecord): TConflictAll;
var
  ct: TConflictThis;
  ca: TConflictAll;
begin
  ConflictLevelForMainRecord(aMainRecord, ca, ct);
  Result := ca;
end;

function IsITPO(rec: IwbMainRecord): Boolean;
var
  mRec, prevOvr, ovr: IwbMainRecord;
  i: Integer;
begin
  // get previous override
  mRec := rec.MasterOrSelf;
  prevovr := mRec;
  for i := 0 to Pred(mRec.OverrideCount) do begin
    ovr := mRec.Overrides[i];
    if ovr.Equals(rec) then
      Break;
    prevovr := ovr;
  end;

  Result := ConflictAllForElements(prevovr, rec, False, False) <= caNoConflict;
end;

function IsITM(rec: IwbMainRecord): Boolean;
const
  ITMConflictArray: set of TConflictThis = [
    ctIdenticalToMaster,
    ctIdenticalToMasterWinsConflict
  ];
begin
  Result := ConflictThisForMainRecord(rec) in ITMConflictArray;
end;

end.
