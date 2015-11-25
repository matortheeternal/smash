unit msSettingsManager;

interface

uses
  Windows, Types, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, ValEdit, CommCtrl, Menus,
  ImgList,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteProgressForm, RttiTranslation,
  // ms units
  msFrontend, msPluginSelectionForm, msConflictForm, msThreads;

type
  TSettingsManager = class(TForm)
    [FormPrefix('msSet')]
      Splitter: TSplitter;
      [FormSection('Settings')]
        pnlEntries: TPanel;
        lvSettings: TListView;
        [FormSection('Settings Popup Menu')]
          SettingsPopupMenu: TPopupMenu;
          NewSettingItem: TMenuItem;
          DeleteSettingItem: TMenuItem;
          CloneSettingItem: TMenuItem;
          CombineSettingsItem: TMenuItem;
      [FormSection('Details')]
        pnlDetails: TPanel;
        lblName: TLabel;
        edName: TEdit;
        lblColor: TLabel;
        cbColor: TColorBox;
        lblDescription: TLabel;
        meDescription: TMemo;
        btnSave: TButton;
        btnDiscard: TButton;
        [FormSection('Tree')]
          lblTree: TLabel;
          tvRecords: TTreeView;
          TreePopupMenu: TPopupMenu;
          AddItem: TMenuItem;
          BuildFromPluginsItem: TMenuItem;
          AutosetItem: TMenuItem;
          SelectSimilarNodesItem: TMenuItem;
          ToggleNodesItem: TMenuItem;
          PreserveDeletionsItem: TMenuItem;
          SingleEntityItem: TMenuItem;
          ChainNodesItem: TMenuItem;
          LinkNodeToItem: TMenuItem;
          UnlinkNodeItem: TMenuItem;
          AutoPruneItem: TMenuItem;
          PruneNodesItem: TMenuItem;
          StateImages: TImageList;
          FlagIcons: TImageList;

    // TREE METHODS
    procedure DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
    procedure tvRecordsCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvRecordsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvRecordsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvRecordsKeyPress(Sender: TObject; var Key: Char);
    procedure tvRecordsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LinkNodeItemClick(Sender: TObject);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure AddItemClick(Sender: TObject);
    procedure BuildFromPluginsItemClick(Sender: TObject);
    procedure SelectSimilarNodesItemClick(Sender: TObject);
    procedure ToggleNodesItemClick(Sender: TObject);
    procedure PreserveDeletionsItemClick(Sender: TObject);
    procedure SingleEntityItemClick(Sender: TObject);
    procedure PruneNodesItemClick(Sender: TObject);
    procedure UnlinkNodeItemClick(Sender: TObject);
    procedure LinkNodes(node1, node2: TTreeNode);
    procedure ChainNodesItemClick(Sender: TObject);
    function DumpElement(node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure DeleteNodes(var aList: TList);
    procedure DeleteChildren(node: TTreeNode);
    function CanPruneRecords: boolean;
    procedure AutoPrune;
    procedure AutoPruneItemClick(Sender: TObject);
    procedure Autoset(parentNode: TTreeNode);
    procedure AutosetItemClick(Sender: TObject);
    // SETTINGS MANAGER EVENTS
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvSettingsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvSettingsData(Sender: TObject; Item: TListItem);
    procedure lvSettingsDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure SplitterMoved(Sender: TObject);
    procedure NewSettingItemClick(Sender: TObject);
    procedure DeleteSettingItemClick(Sender: TObject);
    procedure CloneSettingItemClick(Sender: TObject);
    procedure SettingsPopupMenuPopup(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure CombineSettings(var sl: TStringList);
    procedure CombineSettingsItemClick(Sender: TObject);
  private
    { Private declarations }
    lastHint: string;
  public
    { Public declarations }
    FilterFilename: string;
  end;

const
  collapseHitTestDelay = 0.1 * seconds;

var
  SettingsManager: TSettingsManager;
  NewSettings: TList;
  currentSetting: TSmashSetting;
  pForm: TProgressForm;
  LastCollapseTime: TDateTime;

implementation

{$R *.dfm}

{ Tree methods }
procedure TSettingsManager.DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
var
  icon: TIcon;
begin
  icon := TIcon.Create;
  FlagIcons.GetIcon(id, icon);
  Canvas.Draw(x, y, icon);
  Inc(x, 18);
  icon.Free;
end;

procedure TSettingsManager.tvRecordsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  LastCollapseTime := Now;
end;

procedure TSettingsManager.tvRecordsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  e: TElementData;
  R: TRect;
  x, y: Integer;
begin
  if Assigned(node.Data) then begin
    e := TElementData(node.Data);
    R := Node.DisplayRect(true);
    x := R.Right + 6;
    y := R.Top;

    // this fixes a bug with drawing flags when expanding a node
    if x < 20 then
      exit;

    // draw flags
    if e.preserveDeletions then
      DrawFlag(Sender.Canvas, x, y, 0);
    if e.singleEntity then
      DrawFlag(Sender.Canvas, x, y, 1);
    if (e.linkTo <> '') or (e.linkFrom <> '') then
      DrawFlag(Sender.Canvas, x, y, 2);
  end;
end;

procedure TSettingsManager.tvRecordsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if (Key = VK_SPACE) then begin
    for i := 0 to Pred(tvRecords.SelectionCount) do
      CheckboxManager(tvRecords.Selections[i]);
    // repaint tree view in case a single entity flag was unset
    tvRecords.Repaint;
  end;
end;

procedure TSettingsManager.tvRecordsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then
    Key := #0;
end;

procedure TSettingsManager.tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HT: THitTests;
  node: TTreeNode;
begin
  // this allows right clicking to be used to select nodes
  if Button = mbRight then begin
    node := tvRecords.GetNodeAt(X, Y);
    if Assigned(node) and not node.Selected then begin
      tvRecords.ClearSelection(false);
      tvRecords.Select(node);
    end;
  end;

  // this prevents a bug that happens when collapsing a node
  // would cause the control to scroll up, putting the user's mouse
  // above a checkbox
  // the tree view collapse is triggered prior to this mouse down
  // event, so the hacky solutin I came up with is to use a delay
  // to exit this event if we collapsed a node within the last 0.1
  // seconds
  if Now - LastCollapseTime < collapseHitTestDelay then
    exit;
  HT := tvRecords.GetHitTestInfoAt(X, Y);
  if (HT - [htOnStateIcon] <> HT) then
    CheckBoxManager(tvRecords.Selected);
  // repaint tree view in case a single entity flag was unset
  tvRecords.Repaint;
end;

procedure TSettingsManager.tvRecordsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  node: TTreeNode;
  e: TElementData;
  sHint: string;
begin
  // hide hint and exit if shift is down
  if (ssShift in Shift) then begin
    Application.HideHint;
    exit;
  end;

  // draw hint if on a node with the link parameter
  node := tvRecords.GetNodeAt(X, Y);
  if not Assigned(node) then
    exit;
  e := TElementData(node.Data);
  if not Assigned(e) then
    exit;

  // get hint
  sHint := node.Text + #13#10'Type: '+stToString(e.smashType);
  if e.linkTo <> '' then
    sHint := sHint + #13#10'Linked to: '+e.linkTo;
  if e.linkFrom <> '' then
    sHint := sHint + #13#10'Linked from: '+e.linkFrom;

  // display hint if it isn't the last hint we displayed
  if sHint <> lastHint then begin
    tvRecords.Hint := sHint;
    Application.ActivateHint(Mouse.CursorPos);
    lastHint := sHint;
  end;
end;

function GetSiblingNode(node: TTreeNode; text: string): TTreeNode;
var
  aNode: TTreeNode;
begin
  Result := nil;
  aNode := node.Parent.getFirstChild;
  while Assigned(aNode) do begin
    if aNode.Text = text then begin
      Result := aNode;
      exit;
    end;
    aNode := aNode.getNextSibling;
  end;
end;

procedure UnlinkNode(node: TTreeNode; bTo, bFrom: boolean);
var
  linkedNode: TTreeNode;
  e, le: TElementData;
begin
  e := TElementData(node.Data);
  if bTo and (e.linkTo <> '') then begin
    linkedNode := GetSiblingNode(node, e.linkTo);
    if Assigned(linkedNode) then begin
      le := TElementData(linkedNode.Data);
      le.linkFrom := '';
    end;
    e.linkTo := '';
  end;
  if bFrom and (e.linkFrom <> '') then begin
    linkedNode := GetSiblingNode(node, e.linkFrom);
    if Assigned(linkedNode) then begin
      le := TElementData(linkedNode.Data);
      le.linkTo := '';
    end;
    e.linkFrom := '';
  end;
end;

procedure TSettingsManager.UnlinkNodeItemClick(Sender: TObject);
var
  i: Integer;
  node: TTreeNode;
begin
  // unset link element data attribute for each selected node
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    node := tvRecords.Selections[i];
    UnlinkNode(node, true, true);
  end;

  // update gui
  tvRecords.Repaint;
end;

procedure TSettingsManager.LinkNodes(node1, node2: TTreeNode);
var
  e: TElementData;
begin
  // exit if nodes have different level
  if node1.Level <> node2.Level then
    exit;

  // unlink nodes as necessary
  UnlinkNode(node1, true, false);
  UnlinkNode(node2, false, true);

  // link nodes
  e := TElementData(node1.Data);
  e.linkTo := node2.Text;
  e := TElementData(node2.Data);
  e.linkFrom := node1.Text;
end;

procedure TSettingsManager.ChainNodesItemClick(Sender: TObject);
var
  i: Integer;
  prevNode, node: TTreeNode;
begin
  node := tvRecords.Selections[0];
  for i := 1 to Pred(tvRecords.SelectionCount) do begin
    prevNode := node;
    node := tvRecords.Selections[i];
    LinkNodes(prevNode, node);
  end;
  // link last node to first node
  prevNode := node;
  node := tvRecords.Selections[0];
  LinkNodes(prevNode, node);

  // repaint
  tvRecords.Repaint;
end;

procedure TSettingsManager.LinkNodeItemClick(Sender: TObject);
var
  item: TMenuItem;
  targetNodeText: string;
  node, targetNode: TTreeNode;
begin
  // get the target node to link to from the menu item clicked
  node := tvRecords.Selections[0];
  item := TMenuItem(Sender);
  targetNodeText := StringReplace(item.Caption, '&', '', [rfReplaceAll]);
  targetNode := GetSiblingNode(node, targetNodeText);
  if not Assigned(targetNode) then
    exit;

  // link the nodes
  LinkNodes(node, targetNode);

  // update gui
  tvRecords.Repaint;
end;

procedure TSettingsManager.TreePopupMenuPopup(Sender: TObject);
var
  bHasSelection, bTreeSelected, bHasMultiSelection, bSubrecordSelected,
  bHasChildren, bRecordsSelected, bSomeUnChecked: boolean;
  i: Integer;
  node: TTreeNode;
  MenuItem: TMenuItem;
begin
  // clear link node submenu
  LinkNodeToItem.Clear;

  // get selection booleans
  bHasSelection := tvRecords.SelectionCount > 0;
  bTreeSelected := (tvRecords.SelectionCount = 1)
    and (tvRecords.Selections[0].Level = 0);
  bHasMultiSelection := tvRecords.SelectionCount > 1;
  bSubrecordSelected := (tvRecords.SelectionCount = 1)
    and (tvRecords.Selections[0].Level > 1);

  // get multiselection booleans
  bHasChildren := false;
  bRecordsSelected := true;
  bSomeUnChecked := false;
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    bHasChildren := bHasChildren or tvRecords.Selections[i].HasChildren;
    bRecordsSelected := bRecordsSelected and (tvRecords.Selections[i].Level = 1);
    bSomeUnChecked := bSomeUnChecked or (tvRecords.Selections[i].StateIndex = csUnChecked);
  end;

  // enable/disable menu items
  AddItem.Visible := bTreeSelected;
  ToggleNodesItem.Enabled := bHasSelection;
  PreserveDeletionsItem.Enabled := bHasSelection and bHasChildren;
  SingleEntityItem.Enabled := bHasSelection and bHasChildren;
  AutoPruneItem.Enabled := CanPruneRecords;
  PruneNodesItem.Enabled := bHasSelection and bRecordsSelected and bSomeUnChecked;
  UnlinkNodeItem.Enabled := bHasSelection;
  ChainNodesItem.Enabled := bHasMultiSelection;
  LinkNodeToItem.Enabled := bSubrecordSelected;

  // build LinkNodeToItem submenu
  if bSubrecordSelected then begin
    node := tvRecords.Selected.Parent.getFirstChild;
    while Assigned(node) do begin
      if node = tvRecords.Selections[0] then begin
        node := node.getNextSibling;
        continue;
      end;
      MenuItem := TMenuItem.Create(LinkNodeToItem);
      MenuItem.Caption := node.Text;
      MenuItem.OnClick := LinkNodeItemClick;
      LinkNodeToItem.Add(MenuItem);
      node := node.getNextSibling;
    end;
  end;
end;

procedure TSettingsManager.AddItemClick(Sender: TObject);
var
  item: TMenuItem;
  groupName: string;
  recObj: ISuperObject;
begin
  item := (Sender as TMenuItem);
  groupName := StringReplace(item.Caption, '&', '', [rfReplaceAll]);
  recObj := GetRecordObj(currentSetting.tree, groupName);

  // build record def if it isn't already present
  if not Assigned(recObj) then begin
    if not BuildRecordDef(groupName, recObj) then
      exit;
    currentSetting.tree.A['records'].Add(recObj);
    LoadElement(tvRecords, tvRecords.Items[0], recObj, false);
  end;

  // update gui
  tvRecords.Repaint;
end;

procedure TSettingsManager.BuildFromPluginsItemClick(Sender: TObject);
var
  slPlugins, slSelection: TStringList;
  i, mr: Integer;
  plugin: TPlugin;
  selectionForm: TPluginSelectionForm;
begin
  // build list of plugin filenames
  slPlugins := TStringList.Create;
  slSelection := TStringList.Create;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    slPlugins.Add(plugin.filename);
  end;

  // prompt user for plugin selectcion
  selectionForm := TPluginSelectionForm.Create(self);
  selectionForm.pluginsList := slPlugins;
  selectionForm.selectionList := slSelection;
  mr := selectionForm.ShowModal;
  if mr = mrOK then
    BuildTreeFromPlugins(tvRecords, slSelection, currentSetting.tree);

  // free memory
  selectionForm.Free;
  slPlugins.Free;
  slSelection.Free;
end;

procedure TSettingsManager.SelectSimilarNodesItemClick(Sender: TObject);
var
  i, index: Integer;
  node: TTreeNode;
  reqSmashType, currentSmashType: TSmashtype;
  slSelection: TStringList;
begin
  // Create StringList
  slSelection := TStringList.Create;
  try
    // Add "Text" and "SmashType" of selected nodes to StringList
    // Note: "SmashType" has to be converted to be used in a StringList
    for i := 0 to Pred(tvRecords.SelectionCount) do begin
      node := tvRecords.Selections[i];
      reqSmashType := TElementData(node.Data).smashType;
      // "SmashType" converted into TObject
      slSelection.AddObject(node.Text, TObject(reqSmashType));
    end;
    // Go through all nodes and check if their "Text" is in StringList
    for i := 0 to Pred(tvRecords.Items.Count) do begin
      node := tvRecords.Items[i];
      currentSmashType := TElementData(node.Data).smashType;
      index := slSelection.IndexOf(node.Text);
      // If a "Text" is found, get associated "SmashType" from StringList
      if (index > -1) then begin
        // Convert "SmashType" back to TSmashType
        reqSmashType := TSmashType(slSelection.Objects[index]);
          // Check if node's and saved "SmashType" match and if node is selected
          if (currentSmashType = reqSmashType) and not node.Selected then
            tvRecords.Select(node, [ssCtrl]);
      end;
    end;
  finally
    // Finally free the StringList
    slSelection.Free;
  end;
end;

procedure TSettingsManager.ToggleNodesItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do
    CheckboxManager(tvRecords.Selections[i]);
end;

procedure TSettingsManager.PreserveDeletionsItemClick(Sender: TObject);
var
  i: Integer;
  node: TTreeNode;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    node := tvRecords.Selections[i];
    if not node.hasChildren then
      continue;
    e := TElementData(node.Data);
    e.preserveDeletions := not e.preserveDeletions;
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.SingleEntityItemClick(Sender: TObject);
var
  i, expectedLevel: Integer;
  node: TTreeNode;
  e: TElementData;
begin
  expectedLevel := 0;
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    node := tvRecords.Selections[i];
    if expectedLevel = 0 then
      expectedLevel := node.Level;
    if not node.hasChildren then
      continue;
    if expectedLevel <> node.Level then
      continue;
    e := TElementData(node.Data);
    e.singleEntity := not e.singleEntity;
    if e.singleEntity then
      node.StateIndex := csPartiallyChecked
    else begin
      SetChildren(node, csChecked);
      node.StateIndex := csChecked;
    end;
    UpdateParent(node.Parent);
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.AutoPruneItemClick(Sender: TObject);
begin
  AutoPrune;
end;

procedure TSettingsManager.Autoset(parentNode: TTreeNode);
const
  disabledElements: array[0..2] of string = (
    'Record Header',
    'Unused',
    'Unknown'
  );
  disabledRecords: array[0..4] of string = (
    'NAVM',
    'NAVI',
    'LCTN',
    'DOBJ',
    'RACE'
  );
var
  i: Integer;
  node, nextNode: TTreeNode;
  e: TElementData;
  bParentIsRoot, bParentIsRecord: boolean;
begin
  // get parent booleans
  bParentIsRoot := parentNode.Level = 0;
  bParentIsRecord := parentNode.Level = 1;

  // loop through children
  node := parentNode.getFirstChild;
  while Assigned(node) do begin
    node.StateIndex := csChecked;
    e := TElementData(node.Data);

    if Assigned(e) then begin
      // if parent is root, preserve deletions
      if bParentIsRoot then
        e.preserveDeletions := true;

      // if parent is record, perform case statement on type
      if bParentIsRecord then begin
        case Ord(e.smashType) of
          Ord(stStruct),
          Ord(stUnsortedStructArray): begin
            e.singleEntity := true;
            node.StateIndex := csPartiallyChecked;
          end;
          Ord(stSortedArray),
          Ord(stSortedStructArray):
            e.preserveDeletions := true;
          Ord(stByteArray):
            node.StateIndex := csUnChecked;
        end;
      end;
    end;

    // if we're not on the root, disable elements that match
    // a string in the disableElements array
    if not bParentIsRoot then begin
      for i := Low(disabledElements) to High(disabledElements) do
        if Pos(disabledElements[i], node.Text) > 0 then begin
          node.StateIndex := csUnChecked;
        end;
    end
    // else disable records that match a string in the
    // disabledRecords array
    else begin
      for i := Low(disabledRecords) to High(disabledRecords) do
        if Pos(disabledRecords[i], node.Text) = 1 then begin
          node.StateIndex := csUnChecked;
        end;
    end;

    // disable count elements
    if bParentIsRecord then begin
      nextNode := node.getNextSibling;
      if Assigned(nextNode)
      and (TElementData(nextNode.Data).smashType in stArrays)
      and (Pos('Count', node.Text) > 0) then
        node.StateIndex := csUnChecked;
    end;
    
    // recurse
    if (node.HasChildren) and (node.StateIndex <> csUnChecked) then
      Autoset(node);

    // go to next sibling
    node := node.getNextSibling;
  end;

  // update parent node
  UpdateParent(parentNode);

  // repaint
  tvRecords.Repaint;
end;

procedure TSettingsManager.AutosetItemClick(Sender: TObject);
const
  msg = 'This will change all nodes in this setting, are you sure you want to continue?';
var
  mr: Integer;
begin
  // confirm with the user that this is what they
  // really want to do
  mr := MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0);
  if mr = mrYes then begin
    Enabled := false;
    Autoset(tvRecords.Items[0]);
    Enabled := true;
  end;
end;

procedure TSettingsManager.PruneNodesItemClick(Sender: TObject);
var
  i: Integer;
  node: TTreeNode;
  nodesToPrune: TList;
begin
  nodesToPrune := TList.Create;
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    node := tvRecords.Selections[i];
    if (node.Level = 1) and (node.StateIndex = csUnChecked) then
      nodesToPrune.Add(node);
  end;
  DeleteNodes(nodesToPrune);
  nodesToPrune.Free;
  UpdateParent(tvRecords.Items[0].getFirstChild);
end;

function TSettingsManager.DumpElement(node: TTreeNode): ISuperObject;
var
  obj: ISuperObject;
  child: TTreeNode;
  e: TElementData;
  i: Integer;
begin
  obj := SO;
  // get name
  obj.S['n'] := node.Text;
  // get data properties
  e := TElementData(node.Data);
  if (e.priority > 0) then
    obj.I['r'] := e.priority;
  if (node.StateIndex <> csUnChecked) then
    obj.I['p'] := 1;
  if (e.preserveDeletions) then
    obj.I['d'] := 1;
  if (e.singleEntity) then
    obj.I['s'] := 1;
  if (e.smashType <> stUnknown) then
    obj.I['t'] := Ord(e.smashType);
  if (e.linkTo <> '') then
    obj.S['lt'] := e.linkTo;
  if (e.linkFrom <> '') then
    obj.S['lf'] := e.linkFrom;

  // exit if no children to dump
  if node.hasChildren then begin
    // dump subrecords (children)
    obj.O['c'] := SA([]);
    child := node.getFirstChild;
    i := 0;
    while Assigned(child) do begin
      obj.A['c'].O[i] := DumpElement(child);
      child := child.getNextSibling;
      Inc(i);
    end;
  end;

  Result := obj;
end;

procedure TSettingsManager.DumpTree;
var
  i: Integer;
  obj: ISuperObject;
  node, rootNode: TTreeNode;
  sl: TStringList;
begin
  obj := SO;
  obj.O['records'] := SA([]);
  rootNode := tvRecords.Items[0];

  // loop through records
  node := rootNode.getFirstChild;
  i := 0;
  sl := TStringList.Create;
  while Assigned(node) do begin
    obj.A['records'].O[i] := DumpElement(node);
    if node.StateIndex <> csUnChecked then
      sl.Add(Copy(node.Text, 1, 4));
    Inc(i);
    node := node.getNextSibling;
  end;

  currentSetting.tree := obj;
  currentSetting.records := sl.CommaText;
  sl.Free;
end;

procedure TSettingsManager.edNameChange(Sender: TObject);
begin
  if Assigned(currentSetting) then
    btnSave.Enabled := (currentSetting.name = edName.Text) or
      not Assigned(SettingByName(edName.Text));
end;

procedure TSettingsManager.DeleteNodes(var aList: TList);
var
  i: Integer;
  node: TTreeNode;
begin
  // delete nodes in reverse order
   for i := Pred(aList.Count) downto 0 do begin
    node := TTreeNode(aList[i]);
    if node.HasChildren then
      DeleteChildren(node);
    tvRecords.Items.Delete(node);
  end;
end;

procedure TSettingsManager.DeleteChildren(node: TTreeNode);
var
  nodesToDelete: TList;
  child: TTreeNode;
begin
  nodesToDelete := TList.Create;
  child := node.getFirstChild;
  // get nodes to prune
  while Assigned(child) do begin
    nodesToDelete.Add(child);
    child := child.getNextSibling;
  end;
  // delete nodes
  DeleteNodes(nodesToDelete);
  nodesToDelete.Free;
end;

procedure TSettingsManager.AutoPrune;
var
  mr: Integer;
  nodesToPrune: TList;
  node: TTreeNode;
begin
  mr := MessageDlg('Your setting tree has records that can be pruned.  '+
    'Would you like to prune them?', mtConfirmation, [mbYes, mbNo], 0);
  if mr = mrYes then begin
    nodesToPrune := TList.Create;
    node := tvRecords.Items[0].getFirstChild;
    // get nodes to prune
    while Assigned(node) do begin
      if node.StateIndex = csUnChecked then
        nodesToPrune.Add(node);
      node := node.getNextSibling;
    end;
    // prune nodes
    DeleteNodes(nodesToPrune);
    nodesToPrune.Free;
  end;
end;

function TSettingsManager.CanPruneRecords: boolean;
var
  node: TTreeNode;
begin
  Result := false;
  node := tvRecords.Items[0].getFirstChild;
  while Assigned(node) do begin
    if node.StateIndex = csUnChecked then begin
      Result := true;
      break;
    end;
    node := node.getNextSibling;
  end;
end;


{******************************************************************************}
{ Settings Manager Events }
{******************************************************************************}

procedure TSettingsManager.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);

  // initialize list view, SmashSettings list
  NewSettings := TList.Create;
  lvSettings.OwnerDraw := not settings.simpleDictionaryView;
  lvSettings.Items.Count := SmashSettings.Count;
end;

procedure TSettingsManager.FormShow(Sender: TObject);
const
  TVS_NOTOOLTIPS = $0080;
begin
  // disable tool tips on tree view
  SetWindowLong(tvRecords.Handle, GWL_STYLE,
    GetWindowLong(tvRecords.Handle, GWL_STYLE) or TVS_NOTOOLTIPS);

  // build AddItem submenu
  PopulateAddList(AddItem, AddItemClick);

  // force lvSettings to autosize columns
  lvSettings.Width := lvSettings.Width - 1;
  lvSettings.Width := lvSettings.Width + 1;
end;

// update meNotes when user changes entry
procedure TSettingsManager.lvSettingsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  tvRecords.Items.Clear;
  if (lvSettings.ItemIndex = -1) then begin
    currentSetting := nil;
    edName.Text := '';
    edName.Enabled := false;
    cbColor.Selected := clBlack;
    cbColor.Enabled := false;
    meDescription.Text := '';
    meDescription.Enabled := false;
    tvRecords.Enabled := false;
    btnSave.Enabled := false;
    btnDiscard.Enabled := false;
    exit;
  end;

  // set current setting values
  edName.Enabled := true;
  cbColor.Enabled := true;
  meDescription.Enabled := true;
  tvRecords.Enabled := true;
  btnSave.Enabled := true;
  btnDiscard.Enabled := true;
  currentSetting := TSmashSetting(SmashSettings[lvSettings.ItemIndex]);
  edName.Text := currentSetting.name;
  cbColor.Selected := TColor(currentSetting.color);
  meDescription.Lines.Text := currentSetting.description;

  // load tree if assigned
  if Assigned(currentSetting.tree) then
    LoadTree(tvRecords, currentSetting);
end;

procedure TSettingsManager.btnDiscardClick(Sender: TObject);
begin
  // reload the setting
  lvSettingsChange(nil, nil, TItemChange(nil));
end;

procedure TSettingsManager.btnSaveClick(Sender: TObject);
var
  index: Integer;
begin
  // adjust the current setting's attributes
  currentSetting.Rename(edName.Text);
  currentSetting.color := cbColor.Selected;
  currentSetting.description := meDescription.Lines.Text;

  // prompt user to auto-prune the tree if it's a new setting and
  // we can prune records,
  // then dump the records tree
  index := NewSettings.IndexOf(currentSetting);
  if (index > -1) then begin
    if CanPruneRecords then
      AutoPrune;
    NewSettings.Delete(index);
  end;
  DumpTree;

  // save the setting to disk
  currentSetting.Save;

  // repaint list view
  lvSettings.Repaint;
end;

procedure TSettingsManager.lvSettingsData(Sender: TObject; Item: TListItem);
var
  aSetting: TSmashSetting;
begin
  aSetting := TSmashSetting(SmashSettings[Item.Index]);
  Item.Caption := aSetting.name;
  Item.SubItems.Add(aSetting.records);
  lvSettings.Canvas.Font.Color := aSetting.color;
  lvSettings.Canvas.Font.Style := [fsBold];
end;

procedure TSettingsManager.lvSettingsDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  lv: TListView;
begin
  lv := TListView(Sender);
  if Item.Selected then begin
    lv.Canvas.Brush.Color := $FFEEDD;
    lv.Canvas.FillRect(Rect);
  end;
  x := Rect.Left;
  y := (Rect.Bottom - Rect.Top - lv.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  lv.Canvas.TextOut(x, y, ' '+Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    Inc(x, ListView_GetColumnWidth(lv.Handle, lv.Columns[i].Index));
    lv.Canvas.TextOut(x, y, ' '+Item.SubItems[i]);
  end;
end;

{******************************************************************************}
{ SettingPopupMenu methods
  Methods for dealing with the popup menu for the settings list view.
  - NewSettingItemClick
  - EditSettingItemClick
  - DeleteSettingItemClick
}
{******************************************************************************}

procedure TSettingsManager.SettingsPopupMenuPopup(Sender: TObject);
var
  bHasSelection, bHasMultiSelection: boolean;
begin
  bHasSelection := Assigned(lvSettings.Selected);
  bHasMultiSelection := lvSettings.SelCount > 1;
  DeleteSettingItem.Enabled := bHasSelection;
  CloneSettingItem.Enabled := bHasSelection;
  CombineSettingsItem.Enabled := bHasMultiSelection;
end;

procedure TSettingsManager.NewSettingItemClick(Sender: TObject);
var
  newSetting: TSmashSetting;
begin
  newSetting := TSmashSetting.Create;
  NewSettings.Add(newSetting);
  SmashSettings.Add(newSetting);
  lvSettings.Items.Count := lvSettings.Items.Count + 1;
end;

procedure TSettingsManager.DeleteSettingItemClick(Sender: TObject);
var
  i, index: Integer;
  setting: TSmashSetting;
begin
  for i := Pred(lvSettings.Items.Count) downto 0 do begin
    if not lvSettings.Items[i].Selected then
      continue;
    lvSettings.Items.Count := lvSettings.Items.Count - 1;
    setting := TSmashSetting(SmashSettings[i]);
    RemoveSettingFromPlugins(setting);
    SmashSettings.Delete(i);
    index := NewSettings.IndexOf(setting);
    if index > -1 then
      NewSettings.Delete(index);
    setting.Delete;
    setting.Free;
  end;
  lvSettings.Repaint;
  lvSettingsChange(nil, nil, TItemChange(nil));
end;

procedure TSettingsManager.CloneSettingItemClick(Sender: TObject);
var
  setting, clonedSetting: TSmashSetting;
begin
  clonedSetting := TSmashSetting.Create;
  setting := TSmashSetting(SmashSettings[lvSettings.Selected.Index]);
  clonedSetting.Clone(setting);
  SmashSettings.Add(clonedSetting);
  lvSettings.Items.Count := lvSettings.Items.Count + 1;
end;

function GetRecordObject(tree: ISuperObject; sig: string): ISuperObject;
var
  item: ISuperObject;
begin
  Result := nil;
  for item in tree['records'] do begin
    if item.S['n'] = sig then begin
      Result := item;
      break;
    end;
  end;
end;

procedure TSettingsManager.CombineSettings(var sl: TStringList);
var
  i: Integer;
  newSetting, aSetting: TSmashSetting;
  recordObj: ISuperObject;
begin
  newSetting := TSmashSetting.Create;
  newSetting.tree := SO;
  newSetting.tree.O['records'] := SA([]);

  for i := 0 to Pred(sl.Count) do begin
    aSetting := TSmashSetting(sl.Objects[i]);
    recordObj := GetRecordObject(aSetting.tree, sl[i]);
    newSetting.tree.A['records'].Add(recordObj);
  end;
  newSetting.records := sl.CommaText;

  // add new setting to display
  SmashSettings.Add(newSetting);
  lvSettings.Items.Count := lvSettings.Items.Count + 1;
end;

procedure TSettingsManager.CombineSettingsItemClick(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
  settingsToCombine: TList;
  setting: TSmashSetting;
  slRecords, sl: TStringList;
  j: Integer;
  bConflicting: boolean;
  cForm: TConflictForm;
begin
  settingsToCombine := TList.Create;
  slRecords := TStringList.Create;
  sl := TStringList.Create;
  bConflicting := false;
  for i := 0 to Pred(lvSettings.Items.Count) do begin
    ListItem := lvSettings.Items[i];
    if not ListItem.Selected then
      continue;
    setting := TSmashSetting(SmashSettings[i]);
    settingsToCombine.Add(setting);
    sl.CommaText := setting.records;
    for j := 0 to Pred(sl.Count) do begin
      if slRecords.IndexOf(sl[j]) > -1 then
        bConflicting := true;
      slRecords.AddObject(sl[j], TObject(setting));
    end;
  end;

  if bConflicting then begin
    cForm := TConflictForm.Create(self);
    cForm.slConflicts := slRecords;
    if cForm.ShowModal = mrOK then
      CombineSettings(slRecords);
  end
  else begin
    CombineSettings(slRecords);
  end;

  // free memory
  settingsToCombine.Free;
  slRecords.Free;
  sl.Free;
end;

// repaint when splitter is moved
procedure TSettingsManager.SplitterMoved(Sender: TObject);
begin
  Repaint;
end;

end.
