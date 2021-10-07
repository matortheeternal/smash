unit msSettingsManager;

interface

uses
  Windows, Types, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, ValEdit, CommCtrl, Menus,
  ImgList, StrUtils,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteLogger, mteProgressForm, mteBase, RttiTranslation,
  // ms units
  msCore, msConfiguration, msPluginSelectionForm, msConflictForm, msThreads,
  System.ImageList;

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
    lblHash: TLabel;
    edHash: TEdit;
    lblDescription: TLabel;
    meDescription: TMemo;
    btnSave: TButton;
    btnDiscard: TButton;
    [FormSection('Tree')]
    lblTree: TLabel;
    edSearch: TEdit;
    tvRecords: TTreeView;
    StateImages: TImageList;
    FlagIcons: TImageList;
    [FormSection('TreePopupMenu')]
    TreePopupMenu: TPopupMenu;
    LinkItem: TMenuItem;
    PruneItem: TMenuItem;
    BuildItem: TMenuItem;
    ToggleItem: TMenuItem;
    AddItem: TMenuItem;
    AddAllRecordsItem: TMenuItem;
    BuildFromPluginsItem: TMenuItem;
    AutosetItem: TMenuItem;
    SelectSimilarNodesItem: TMenuItem;
    ToggleNodesItem: TMenuItem;
    PreserveDeletionsItem: TMenuItem;
    OverrideDeletionsItem: TMenuItem;
    SingleEntityItem: TMenuItem;
    ForceValueItem: TMenuItem;
    ChainNodesItem: TMenuItem;
    LinkNodeToItem: TMenuItem;
    UnlinkNodeItem: TMenuItem;
    AutoPruneItem: TMenuItem;
    PruneNodesItem: TMenuItem;

    // TREE METHODS
    procedure DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
    procedure tvRecordsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvRecordsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure tvRecordsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvRecordsKeyPress(Sender: TObject; var Key: Char);
    procedure tvRecordsMouseMove(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure LinkNodeItemClick(Sender: TObject);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure AddItemClick(Sender: TObject);
    procedure AddAllRecordsItemClick(Sender: TObject);
    procedure BuildFromPluginsItemClick(Sender: TObject);
    procedure SelectSimilarNodesItemClick(Sender: TObject);
    procedure ToggleNodesItemClick(Sender: TObject);
    procedure PreserveDeletionsItemClick(Sender: TObject);
    procedure OverrideDeletionsItemClick(Sender: TObject);
    procedure SingleEntityItemClick(Sender: TObject);
    procedure ForceValueItemClick(Sender: TObject);
    procedure PruneNodesItemClick(Sender: TObject);
    procedure UnlinkNodeItemClick(Sender: TObject);
    procedure LinkNodes(node1, node2: TTreeNode);
    procedure ChainNodesItemClick(Sender: TObject);
    function DumpElement(Node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure DeleteNodes(var aList: TList);
    procedure DeleteChildren(Node: TTreeNode);
    function CanPruneRecords: Boolean;
    procedure AutoPrune;
    procedure AutoPruneItemClick(Sender: TObject);
    procedure Autoset(parentNode: TTreeNode);
    procedure AutosetItemClick(Sender: TObject);
    // SETTINGS MANAGER EVENTS
    function GetGroup(name: string; var group: TListGroup): Boolean;
    procedure AddSettingItem(aSetting: TSmashSetting; bSelect: Boolean = true);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectSetting(index: Integer);
    procedure lvSettingsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvSettingsDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure SplitterMoved(Sender: TObject);
    procedure NewSettingItemClick(Sender: TObject);
    procedure DeleteSettingItemClick(Sender: TObject);
    procedure CloneSettingItemClick(Sender: TObject);
    procedure CombineSettingsItemClick(Sender: TObject);
    procedure SettingsPopupMenuPopup(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edSearchClick(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: Char);
    procedure NextSearchResult();
    procedure ResetSearch();
    procedure FormDestroy(Sender: TObject);
    procedure lvSettingsClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    lastHint: string;
    bSearchActive: Boolean;
    slSearchResults: TStringList;
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
  currentSettingItem: TListItem;
  pForm: TProgressForm;
  LastCollapseTime: TDateTime;
  SearchIndex: Integer = -1;

implementation

{$R *.dfm}

procedure TSettingsManager.edSearchClick(Sender: TObject);
begin
  // On search field click set bSearchActive to false
  if (bSearchActive) and (MessageDlg('Start new search?', mtConfirmation,
    [mbyes, mbno], 0) = mrYes) then
    bSearchActive := False;
  // Empties the search field when clicked and a search is active
  if not bSearchActive then
    edSearch.Text := '';
end;

procedure TSettingsManager.edSearchKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  Node: TTreeNode;
begin
  // Exit if no records are available
  if tvRecords.Items.Count = 0 then
    exit;
  // Exit function if input is not "Enter"
  if Key <> #13 then
    exit;
  // Tell the user to enter search term
  if (edSearch.Text = 'Search...') or (edSearch.Text = '') then
  begin
    ShowMessage('Please enter a search term!');
    exit;
  end;
  // Do a search when bSearchActive is false
  if not bSearchActive then
  begin
    // Clear old results
    slSearchResults.Clear;
    for i := 0 to Pred(tvRecords.Items.Count) do
    begin
      Node := tvRecords.Items[i];
      // Check if search-string is contained in node-text
      if ContainsText(Node.Text, edSearch.Text) then
        slSearchResults.AddObject(Node.Text, TObject(Node));
    end;
    bSearchActive := true;
    // Show result
    NextSearchResult;
    Key := #0;
  end;
end;

procedure TSettingsManager.NextSearchResult();
var
  Node: TTreeNode;
begin
  // Notify the user of an unsuccessful search and reset search
  if slSearchResults.Count = 0 then
  begin
    ShowMessage('No results could be found!');
    ResetSearch;
    exit;
  end;

  // Exit the function if there is no search active
  if not bSearchActive then
    exit;

  // Increase the SearchIndex
  SearchIndex := SearchIndex + 1;
  // Display the amount of results and the current position in the search field
  edSearch.Text := ('Result: ' + InttoStr(SearchIndex + 1) + ' / ' +
    InttoStr(slSearchResults.Count + 1));
  // Go back to the beginning if end is reached
  if SearchIndex > Pred(slSearchResults.Count) then
  begin
    SearchIndex := 0;
  end;
  // select the node and set focus
  Node := TTreeNode(slSearchResults.Objects[SearchIndex]);
  tvRecords.SetFocus;
  Node.Selected := true;
  Node.Focused := true;
end;

procedure TSettingsManager.ResetSearch();
begin
  bSearchActive := False;
  edSearch.Text := 'Search...';
end;

{ Tree methods }
procedure TSettingsManager.DrawFlag(Canvas: TCanvas; var x, y: Integer;
  id: Integer);
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
  if Assigned(Node.Data) then
  begin
    e := TElementData(Node.Data);
    R := Node.DisplayRect(true);
    x := R.Right + 6;
    y := R.Top;

    // this fixes a bug with drawing flags when expanding a node
    if x < 20 then
      exit;

    // draw flags
    if e.preserveDeletions then
      DrawFlag(Sender.Canvas, x, y, 0);
    if e.overrideDeletions then
      DrawFlag(Sender.Canvas, x, y, 1);
    if e.forceValue then
      DrawFlag(Sender.Canvas, x, y, 2);
    if e.singleEntity then
      DrawFlag(Sender.Canvas, x, y, 3);
    if (e.linkTo <> '') or (e.linkFrom <> '') then
      DrawFlag(Sender.Canvas, x, y, 4);
  end;
end;

procedure TSettingsManager.tvRecordsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if (Key = VK_SPACE) then
  begin
    for i := 0 to Pred(tvRecords.SelectionCount) do
      CheckboxManager(tvRecords.Selections[i]);
    // repaint tree view in case a single entity flag was unset
    tvRecords.Repaint;
  end;
end;

procedure TSettingsManager.tvRecordsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    NextSearchResult;
  Key := #0;

  if Key = ' ' then
    Key := #0;
end;

procedure TSettingsManager.tvRecordsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  HT: THitTests;
  Node: TTreeNode;
begin
  // this allows right clicking to be used to select nodes
  if Button = mbRight then
  begin
    Node := tvRecords.GetNodeAt(x, y);
    if Assigned(Node) and not Node.Selected then
    begin
      tvRecords.ClearSelection(False);
      tvRecords.Select(Node);
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
  HT := tvRecords.GetHitTestInfoAt(x, y);
  if (HT - [htOnStateIcon] <> HT) then
    CheckboxManager(tvRecords.Selected);
  // repaint tree view in case a single entity flag was unset
  tvRecords.Repaint;
end;

procedure TSettingsManager.tvRecordsMouseMove(Sender: TObject;
  Shift: TShiftState; x, y: Integer);
var
  Node: TTreeNode;
  e: TElementData;
  sHint: string;
begin
  // hide hint and exit if shift is down
  if (ssShift in Shift) then
  begin
    Application.HideHint;
    exit;
  end;

  // draw hint if on a node
  Node := tvRecords.GetNodeAt(x, y);
  if not Assigned(Node) then
    exit;
  e := TElementData(Node.Data);
  if not Assigned(e) then
    exit;

  // get hint
  sHint := Node.Text + #13#10'Type: ' + stToString(e.smashType);
  if e.singleEntity then
    sHint := sHint + #13#10'Treated as a single entity';
  if e.forceValue then
    sHint := sHint + #13#10'Forcing values';
  if e.preserveDeletions then
    sHint := sHint + #13#10'Preserving deletions';
  if e.linkTo <> '' then
    sHint := sHint + #13#10'Linked to: ' + e.linkTo;
  if e.linkFrom <> '' then
    sHint := sHint + #13#10'Linked from: ' + e.linkFrom;

  // display hint if it isn't the last hint we displayed
  if sHint <> lastHint then
  begin
    tvRecords.Hint := sHint;
    Application.ActivateHint(Mouse.CursorPos);
    lastHint := sHint;
  end;
end;

function GetSiblingNode(Node: TTreeNode; Text: string): TTreeNode;
var
  aNode: TTreeNode;
begin
  Result := nil;
  aNode := Node.Parent.getFirstChild;
  while Assigned(aNode) do
  begin
    if aNode.Text = Text then
    begin
      Result := aNode;
      exit;
    end;
    aNode := aNode.getNextSibling;
  end;
end;

procedure UnlinkNode(Node: TTreeNode; bTo, bFrom: Boolean);
var
  linkedNode: TTreeNode;
  e, le: TElementData;
begin
  e := TElementData(Node.Data);
  if bTo and (e.linkTo <> '') then
  begin
    linkedNode := GetSiblingNode(Node, e.linkTo);
    if Assigned(linkedNode) then
    begin
      le := TElementData(linkedNode.Data);
      le.linkFrom := '';
    end;
    e.linkTo := '';
  end;
  if bFrom and (e.linkFrom <> '') then
  begin
    linkedNode := GetSiblingNode(Node, e.linkFrom);
    if Assigned(linkedNode) then
    begin
      le := TElementData(linkedNode.Data);
      le.linkTo := '';
    end;
    e.linkFrom := '';
  end;
end;

procedure TSettingsManager.UnlinkNodeItemClick(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
begin
  // unset link element data attribute for each selected node
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    UnlinkNode(Node, true, true);
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
  UnlinkNode(node1, true, False);
  UnlinkNode(node2, False, true);

  // link nodes
  e := TElementData(node1.Data);
  e.linkTo := node2.Text;
  e := TElementData(node2.Data);
  e.linkFrom := node1.Text;
end;

procedure TSettingsManager.ChainNodesItemClick(Sender: TObject);
var
  i: Integer;
  prevNode, Node: TTreeNode;
begin
  Node := tvRecords.Selections[0];
  for i := 1 to Pred(tvRecords.SelectionCount) do
  begin
    prevNode := Node;
    Node := tvRecords.Selections[i];
    LinkNodes(prevNode, Node);
  end;
  // link last node to first node
  prevNode := Node;
  Node := tvRecords.Selections[0];
  LinkNodes(prevNode, Node);

  // repaint
  tvRecords.Repaint;
end;

procedure TSettingsManager.LinkNodeItemClick(Sender: TObject);
var
  Item: TMenuItem;
  targetNodeText: string;
  Node, targetNode: TTreeNode;
begin
  // get the target node to link to from the menu item clicked
  Node := tvRecords.Selections[0];
  Item := TMenuItem(Sender);
  targetNodeText := StringReplace(Item.Caption, '&', '', [rfReplaceAll]);
  targetNode := GetSiblingNode(Node, targetNodeText);
  if not Assigned(targetNode) then
    exit;

  // link the nodes
  LinkNodes(Node, targetNode);

  // update gui
  tvRecords.Repaint;
end;

procedure TSettingsManager.TreePopupMenuPopup(Sender: TObject);
var
  bHasSelection, bTreeSelected, bHasMultiSelection, bSubrecordSelected,
    bHasChildren, bRecordsSelected, bSomeUnChecked, bIsContainer: Boolean;
  i: Integer;
  Node: TTreeNode;
  MenuItem: TMenuItem;
begin
  // clear link node submenu
  LinkNodeToItem.Clear;

  // get selection booleans
  bHasSelection := tvRecords.SelectionCount > 0;
  bTreeSelected := (tvRecords.SelectionCount = 1) and
    (tvRecords.Selections[0].Level = 0);
  bHasMultiSelection := tvRecords.SelectionCount > 1;
  bSubrecordSelected := (tvRecords.SelectionCount = 1) and
    (tvRecords.Selections[0].Level > 1);

  // get multiselection booleans
  bHasChildren := False;
  bRecordsSelected := true;
  bSomeUnChecked := False;
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    bHasChildren := bHasChildren or tvRecords.Selections[i].HasChildren;
    bRecordsSelected := bRecordsSelected and
      (tvRecords.Selections[i].Level = 1);
    bSomeUnChecked := bSomeUnChecked or
      (tvRecords.Selections[i].StateIndex = csUnChecked);
  end;

  // enable/disable menu items
  bIsContainer := bHasSelection and bHasChildren and not bTreeSelected;
  AddItem.Visible := bTreeSelected;
  ToggleNodesItem.Enabled := bHasSelection;
  PreserveDeletionsItem.Enabled := bIsContainer;
  OverrideDeletionsItem.Enabled := bIsContainer;
  SingleEntityItem.Enabled := bIsContainer and not bRecordsSelected;
  ForceValueItem.Enabled := bHasSelection and bRecordsSelected;
  AutoPruneItem.Enabled := CanPruneRecords;
  PruneNodesItem.Enabled := bHasSelection and bRecordsSelected and
    bSomeUnChecked;
  UnlinkNodeItem.Enabled := bHasSelection and not bTreeSelected;
  ChainNodesItem.Enabled := bHasMultiSelection and not bTreeSelected;
  LinkNodeToItem.Enabled := bSubrecordSelected and not bTreeSelected;

  // build LinkNodeToItem submenu
  if bSubrecordSelected then
  begin
    Node := tvRecords.Selected.Parent.getFirstChild;
    while Assigned(Node) do
    begin
      if Node = tvRecords.Selections[0] then
      begin
        Node := Node.getNextSibling;
        continue;
      end;
      MenuItem := TMenuItem.Create(LinkNodeToItem);
      MenuItem.Caption := Node.Text;
      MenuItem.OnClick := LinkNodeItemClick;
      LinkNodeToItem.Add(MenuItem);
      Node := Node.getNextSibling;
    end;
  end;
end;

procedure TSettingsManager.AddItemClick(Sender: TObject);
var
  Item: TMenuItem;
  groupName: string;
  recObj: ISuperObject;
begin
  Item := (Sender as TMenuItem);
  groupName := StringReplace(Item.Caption, '&', '', [rfReplaceAll]);
  recObj := GetRecordObj(currentSetting.tree, groupName);

  // build record def if it isn't already present
  if not Assigned(recObj) then
  begin
    if not BuildRecordDef(groupName, recObj) then
      exit;
    currentSetting.tree.A['records'].Add(recObj);
    LoadElement(tvRecords, tvRecords.Items[0], recObj, False);
  end;

  // update gui
  tvRecords.Repaint;
end;

procedure TSettingsManager.AddAllRecordsItemClick(Sender: TObject);
begin
  AddAllRecords(currentSetting, tvRecords);
  tvRecords.Repaint;
end;

procedure TSettingsManager.BuildFromPluginsItemClick(Sender: TObject);
var
  slPlugins, slSelection: TStringList;
  i, mr: Integer;
  plugin: TPlugin;
  selectionForm: TMiniPluginSelectionForm;
begin
  // build list of plugin filenames
  slPlugins := TStringList.Create;
  slSelection := TStringList.Create;
  for i := 0 to Pred(PluginsList.Count) do
  begin
    plugin := TPlugin(PluginsList[i]);
    slPlugins.Add(plugin.filename);
  end;

  // prompt user for plugin selectcion
  selectionForm := TMiniPluginSelectionForm.Create(self);
  selectionForm.PluginsList := slPlugins;
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
  Node: TTreeNode;
  reqSmashType, currentSmashType: TSmashtype;
  slSelection: TStringList;
begin
  // Create StringList
  slSelection := TStringList.Create;
  try
    // Add "Text" and "SmashType" of selected nodes to StringList
    // Note: "SmashType" has to be converted to be used in a StringList
    for i := 0 to Pred(tvRecords.SelectionCount) do
    begin
      Node := tvRecords.Selections[i];
      reqSmashType := TElementData(Node.Data).smashType;
      // "SmashType" converted into TObject
      slSelection.AddObject(Node.Text, TObject(reqSmashType));
    end;
    // Go through all nodes and check if their "Text" is in StringList
    for i := 0 to Pred(tvRecords.Items.Count) do
    begin
      Node := tvRecords.Items[i];
      currentSmashType := TElementData(Node.Data).smashType;
      index := slSelection.IndexOf(Node.Text);
      // If a "Text" is found, get associated "SmashType" from StringList
      if (index > -1) then
      begin
        // Convert "SmashType" back to TSmashType
        reqSmashType := TSmashtype(slSelection.Objects[index]);
        // Check if node's and saved "SmashType" match and if node is selected
        if (currentSmashType = reqSmashType) and not Node.Selected then
          tvRecords.Select(Node, [ssCtrl]);
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
  Node: TTreeNode;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    if not Node.HasChildren then
      continue;
    e := TElementData(Node.Data);
    e.preserveDeletions := not e.preserveDeletions;
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.OverrideDeletionsItemClick(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    if not Node.HasChildren then
      continue;
    e := TElementData(Node.Data);
    e.overrideDeletions := not e.overrideDeletions;
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.SingleEntityItemClick(Sender: TObject);
var
  i, expectedLevel: Integer;
  Node: TTreeNode;
  e: TElementData;
begin
  expectedLevel := 0;
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    if expectedLevel = 0 then
      expectedLevel := Node.Level;
    if not Node.HasChildren then
      continue;
    if expectedLevel <> Node.Level then
      continue;
    e := TElementData(Node.Data);
    e.singleEntity := not e.singleEntity;
    if e.singleEntity then
      Node.StateIndex := csPartiallyChecked
    else
    begin
      SetChildren(Node, csChecked);
      Node.StateIndex := csChecked;
    end;
    UpdateParent(Node.Parent);
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.ForceValueItemClick(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    e := TElementData(Node.Data);
    e.forceValue := not e.forceValue;
  end;
  tvRecords.Repaint;
end;

procedure TSettingsManager.AutoPruneItemClick(Sender: TObject);
begin
  AutoPrune;
end;

procedure TSettingsManager.Autoset(parentNode: TTreeNode);
const
  disabledElements: array [0 .. 2] of string = ('Record Header', 'Unused',
    'Unknown');
  disabledRecords: array [0 .. 4] of string = ('DOBJ', 'LCTN', 'IDLE', 'NAVM',
    'NAVI');
var
  i: Integer;
  Node, nextNode: TTreeNode;
  e: TElementData;
  bParentIsRoot, bParentIsRecord: Boolean;
begin
  // get parent booleans
  bParentIsRoot := parentNode.Level = 0;
  bParentIsRecord := parentNode.Level = 1;

  // loop through children
  Node := parentNode.getFirstChild;
  while Assigned(Node) do
  begin
    Node.StateIndex := csChecked;
    e := TElementData(Node.Data);

    if Assigned(e) then
    begin
      // if parent is root, preserve deletions
      if bParentIsRoot then
        e.preserveDeletions := true;

      // if parent is record, perform case statement on type
      if bParentIsRecord then
      begin
        case Ord(e.smashType) of
          Ord(stStruct):
            begin
              e.singleEntity := true;
              Node.StateIndex := csPartiallyChecked;
            end;
          Ord(stUnsortedStructArray):
            begin
              e.singleEntity := true;
              e.preserveDeletions := true;
              Node.StateIndex := csPartiallyChecked;
            end;
          Ord(stSortedArray), Ord(stSortedStructArray):
            e.preserveDeletions := true;
          Ord(stByteArray):
            Node.StateIndex := csUnChecked;
        end;
      end;
    end;

    // if we're not on the root, disable elements that match
    // a string in the disableElements array
    if not bParentIsRoot then
    begin
      for i := Low(disabledElements) to High(disabledElements) do
        if Pos(disabledElements[i], Node.Text) > 0 then
        begin
          Node.StateIndex := csUnChecked;
          e.singleEntity := False;
          e.preserveDeletions := False;
        end;
    end
    // else disable records that match a string in the
    // disabledRecords array
    else
    begin
      for i := Low(disabledRecords) to High(disabledRecords) do
        if Pos(disabledRecords[i], Node.Text) = 1 then
        begin
          Node.StateIndex := csUnChecked;
          e.singleEntity := False;
          e.preserveDeletions := False;
        end;
    end;

    // disable count elements
    if bParentIsRecord then
    begin
      nextNode := Node.getNextSibling;
      if Assigned(nextNode) and (TElementData(nextNode.Data).smashType
        in stArrays) and (Pos('Count', Node.Text) > 0) then
        Node.StateIndex := csUnChecked;
    end;

    // recurse
    if (Node.HasChildren) and (Node.StateIndex <> csUnChecked) then
      Autoset(Node);

    // go to next sibling
    Node := Node.getNextSibling;
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
  mr := MessageDlg(msg, mtConfirmation, [mbyes, mbno], 0);
  if mr = mrYes then
  begin
    Enabled := False;
    Autoset(tvRecords.Items[0]);
    Enabled := true;
  end;
end;

procedure TSettingsManager.PruneNodesItemClick(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
  nodesToPrune: TList;
begin
  nodesToPrune := TList.Create;
  for i := 0 to Pred(tvRecords.SelectionCount) do
  begin
    Node := tvRecords.Selections[i];
    if (Node.Level = 1) and (Node.StateIndex = csUnChecked) then
      nodesToPrune.Add(Node);
  end;
  DeleteNodes(nodesToPrune);
  nodesToPrune.Free;
  UpdateParent(tvRecords.Items[0].getFirstChild);
end;

function TSettingsManager.DumpElement(Node: TTreeNode): ISuperObject;
var
  obj: ISuperObject;
  child: TTreeNode;
  e: TElementData;
  i: Integer;
begin
  obj := SO;
  // get name
  obj.S['n'] := Node.Text;
  // get data properties
  e := TElementData(Node.Data);
  if (e.priority > 0) then
    obj.i['r'] := e.priority;
  if (Node.StateIndex <> csUnChecked) then
    obj.i['p'] := 1;
  if (e.preserveDeletions) then
    obj.i['d'] := 1;
  if (e.overrideDeletions) then
    obj.i['o'] := 1;
  if (e.singleEntity) then
    obj.i['s'] := 1;
  if (e.forceValue) then
    obj.i['f'] := 1;
  if (e.smashType <> stUnknown) then
    obj.i['t'] := Ord(e.smashType);
  if (e.linkTo <> '') then
    obj.S['lt'] := e.linkTo;
  if (e.linkFrom <> '') then
    obj.S['lf'] := e.linkFrom;

  // exit if no children to dump
  if Node.HasChildren then
  begin
    // dump subrecords (children)
    obj.O['c'] := SA([]);
    child := Node.getFirstChild;
    i := 0;
    while Assigned(child) do
    begin
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
  Node, rootNode: TTreeNode;
begin
  obj := SO;
  obj.O['records'] := SA([]);
  rootNode := tvRecords.Items[0];

  // loop through records
  Node := rootNode.getFirstChild;
  i := 0;
  while Assigned(Node) do
  begin
    obj.A['records'].O[i] := DumpElement(Node);
    Inc(i);
    Node := Node.getNextSibling;
  end;

  currentSetting.tree := obj;
  currentSetting.UpdateRecords;
end;

procedure TSettingsManager.edNameChange(Sender: TObject);
begin
  if Assigned(currentSetting) then
    btnSave.Enabled := (currentSetting.name = edName.Text) or
      not Assigned(TSettingHelpers.SettingByName(edName.Text));
end;

procedure TSettingsManager.DeleteNodes(var aList: TList);
var
  i: Integer;
  Node: TTreeNode;
begin
  // delete nodes in reverse order
  for i := Pred(aList.Count) downto 0 do
  begin
    Node := TTreeNode(aList[i]);
    if Node.HasChildren then
      DeleteChildren(Node);
    tvRecords.Items.Delete(Node);
  end;
end;

procedure TSettingsManager.DeleteChildren(Node: TTreeNode);
var
  nodesToDelete: TList;
  child: TTreeNode;
begin
  nodesToDelete := TList.Create;
  child := Node.getFirstChild;
  // get nodes to prune
  while Assigned(child) do
  begin
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
  Node: TTreeNode;
begin
  mr := MessageDlg('Your setting tree has records that can be pruned.  ' +
    'Would you like to prune them?', mtConfirmation, [mbyes, mbno], 0);
  if mr = mrYes then
  begin
    nodesToPrune := TList.Create;
    Node := tvRecords.Items[0].getFirstChild;
    // get nodes to prune
    while Assigned(Node) do
    begin
      if Node.StateIndex = csUnChecked then
        nodesToPrune.Add(Node);
      Node := Node.getNextSibling;
    end;
    // prune nodes
    DeleteNodes(nodesToPrune);
    nodesToPrune.Free;
  end;
end;

function TSettingsManager.CanPruneRecords: Boolean;
var
  Node: TTreeNode;
begin
  Result := False;
  Node := tvRecords.Items[0].getFirstChild;
  while Assigned(Node) do
  begin
    if Node.StateIndex = csUnChecked then
    begin
      Result := true;
      break;
    end;
    Node := Node.getNextSibling;
  end;
end;

{ ****************************************************************************** }
{ Settings Manager Events }
{ ****************************************************************************** }

function TSettingsManager.GetGroup(name: string; var group: TListGroup)
  : Boolean;
var
  i: Integer;
begin
  Result := False;

  // get the group
  for i := 0 to Pred(lvSettings.Groups.Count) do
  begin
    group := lvSettings.Groups[i];
    if SameText(group.Header, name) then
    begin
      Result := true;
      break;
    end;
  end;
end;

procedure TSettingsManager.AddSettingItem(aSetting: TSmashSetting;
  bSelect: Boolean = true);
var
  index: Integer;
  Item: TListItem;
  sGroupName: string;
  group: TListGroup;
begin
  // add item to list view
  Item := lvSettings.Items.Add;
  Item.Caption := aSetting.name;
  Item.SubItems.Add(aSetting.records);

  // get group
  sGroupName := 'Ungrouped';
  index := Pos('.', aSetting.name);
  if index > 0 then
    sGroupName := Copy(aSetting.name, 1, index - 1);
  if not GetGroup(sGroupName, group) then
  begin
    group := lvSettings.Groups.Add;
    group.Header := sGroupName;
    group.State := [lgsCollapsible];
  end;

  // assign group
  Item.GroupID := group.id;

  // set selected item to the new setting
  if bSelect then
    SelectSetting(Item.index);
end;

procedure TSettingsManager.FormCreate(Sender: TObject);
var
  i: Integer;
  aSetting: TSmashSetting;
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

  // prepare groups and load items
  for i := 0 to Pred(SmashSettings.Count) do
  begin
    aSetting := TSmashSetting(SmashSettings[i]);
    AddSettingItem(aSetting, False);
  end;

  // Create the StringList for the Search
  slSearchResults := TStringList.Create;
end;

procedure TSettingsManager.FormDestroy(Sender: TObject);
begin
  // Free the Search StringList
  slSearchResults.Free;
end;

procedure TSettingsManager.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 70) and (Shift = [ssCtrl]) then
    edSearch.SetFocus;
end;

procedure TSettingsManager.FormShow(Sender: TObject);
const
  TVS_NOTOOLTIPS = $0080;
begin
  // disable tool tips on tree view
  SetWindowLong(tvRecords.Handle, GWL_STYLE, GetWindowLong(tvRecords.Handle,
    GWL_STYLE) or TVS_NOTOOLTIPS);

  // build AddItem submenu
  PopulateAddList(AddItem, AddItemClick);

  // force lvSettings to autosize columns
  lvSettings.Width := lvSettings.Width - 1;
  lvSettings.Width := lvSettings.Width + 1;
end;

// selects the setting specified by @index
procedure TSettingsManager.SelectSetting(index: Integer);
begin
  lvSettings.ClearSelection;
  lvSettings.Selected := lvSettings.Items[index];
  lvSettingsChange(nil, nil, TItemChange(nil));
end;

// update setting data whenever user changes their selection
procedure TSettingsManager.lvSettingsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  tvRecords.Items.Clear;
  if (lvSettings.ItemIndex = -1) then
  begin
    currentSetting := nil;
    edName.Text := '';
    edName.Enabled := False;
    cbColor.Selected := clBlack;
    cbColor.Enabled := False;
    edHash.Text := '$00000000';
    meDescription.Text := '';
    meDescription.Enabled := False;
    tvRecords.Enabled := False;
    btnSave.Enabled := False;
    btnDiscard.Enabled := False;
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
  currentSettingItem := lvSettings.Items[lvSettings.ItemIndex];
  edName.Text := currentSetting.name;
  edHash.Text := '$' + currentSetting.hash;
  cbColor.Selected := TColor(currentSetting.color);
  meDescription.Lines.Text := currentSetting.description;

  // load tree if assigned
  if Assigned(currentSetting.tree) then
    LoadTree(tvRecords, currentSetting);
end;

procedure TSettingsManager.lvSettingsClick(Sender: TObject);
begin
  if bSearchActive then
    ResetSearch;
end;

procedure TSettingsManager.btnDiscardClick(Sender: TObject);
begin
  // reload the setting
  lvSettingsChange(nil, nil, TItemChange(nil));
end;

procedure TSettingsManager.btnSaveClick(Sender: TObject);
var
  index: Integer;
  group: TListGroup;
begin
  // adjust the current setting's attributes
  currentSetting.Rename(edName.Text);
  currentSetting.color := cbColor.Selected;
  currentSetting.description := meDescription.Lines.Text;

  // prompt user to auto-prune the tree if it's a new setting and
  // we can prune records,
  // then dump the records tree
  index := NewSettings.IndexOf(currentSetting);
  if (index > -1) then
  begin
    if CanPruneRecords then
      AutoPrune;
    NewSettings.Delete(index);
  end;
  DumpTree;

  // save the setting to disk
  currentSetting.Save;

  // update edHash
  edHash.Text := currentSetting.hash;

  // adjust the current setting in the list view
  GetGroup(currentSetting.name, group);
  currentSettingItem.GroupID := group.id;
  currentSettingItem.Caption := currentSetting.name;
  currentSettingItem.SubItems[0] := currentSetting.records;

  // repaint list view
  lvSettings.Repaint;
end;

procedure TSettingsManager.lvSettingsDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: Integer;
  lv: TListView;
  aSetting: TSmashSetting;
begin
  lv := TListView(Sender);

  // get color
  aSetting := TSmashSetting(SmashSettings[Item.index]);
  lv.Canvas.Font.color := aSetting.color;
  lv.Canvas.Font.Style := [fsBold];
  lv.Canvas.Refresh;

  // draw selected rect
  if Item.Selected then
  begin
    lv.Canvas.Brush.color := $FFEEDD;
    lv.Canvas.FillRect(Rect);
  end;

  // draw item
  x := Rect.Left;
  y := (Rect.Bottom - Rect.Top - lv.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  lv.Canvas.TextOut(x, y, ' ' + Item.Caption);

  // draw subitems
  for i := 0 to Item.SubItems.Count - 1 do
  begin
    Inc(x, ListView_GetColumnWidth(lv.Handle, lv.Columns[i].index));
    lv.Canvas.TextOut(x, y, ' ' + Item.SubItems[i]);
  end;
end;

{ ****************************************************************************** }
{ SettingPopupMenu methods
  Methods for dealing with the popup menu for the settings list view.
  - NewSettingItemClick
  - EditSettingItemClick
  - DeleteSettingItemClick
}
{ ****************************************************************************** }

procedure TSettingsManager.SettingsPopupMenuPopup(Sender: TObject);
var
  bHasSelection, bHasMultiSelection: Boolean;
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
  // create new setting
  newSetting := TSmashSetting.Create;
  NewSettings.Add(newSetting);
  SmashSettings.Add(newSetting);
  AddSettingItem(newSetting);
end;

procedure TSettingsManager.DeleteSettingItemClick(Sender: TObject);
var
  i, index: Integer;
  setting: TSmashSetting;
begin
  for i := Pred(lvSettings.Items.Count) downto 0 do
  begin
    if not lvSettings.Items[i].Selected then
      continue;
    lvSettings.Items.Delete(i);
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
  setting := TSmashSetting(SmashSettings[lvSettings.Selected.index]);
  clonedSetting.Clone(setting);
  SmashSettings.Add(clonedSetting);
  // add setting to list view
  AddSettingItem(clonedSetting);
end;

procedure TSettingsManager.CombineSettingsItemClick(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
  settingsToCombine: TList;
  setting: TSmashSetting;
  sl, slRecords: TStringList;
  cForm: TConflictForm;
begin
  // create lists
  settingsToCombine := TList.Create;
  slRecords := TStringList.Create;
  sl := TStringList.Create;
  sl.StrictDelimiter := true;
  sl.Delimiter := ',';

  try
    // add selected settings to the settingsToCombine list
    for i := 0 to Pred(lvSettings.Items.Count) do
    begin
      ListItem := lvSettings.Items[i];
      if not ListItem.Selected then
        continue;
      setting := TSmashSetting(SmashSettings[i]);
      settingsToCombine.Add(setting);
      sl.Add(setting.name);
    end;

    // build a list of the record objects in the settings
    // if conflicts found, have user resolve them
    if CombineSettingTrees(settingsToCombine, slRecords) then
    begin
      cForm := TConflictForm.Create(self);
      cForm.slConflicts := slRecords;
      if cForm.ShowModal = mrOK then
        CreateCombinedSetting(slRecords, sl.DelimitedText);
    end
    // else just create the combined setting
    else
    begin
      CreateCombinedSetting(slRecords, sl.DelimitedText);
    end;
  finally
    // update lvSettings if we created a new setting
    if SmashSettings.Count > lvSettings.Items.Count then
    begin
      setting := TSmashSetting(SmashSettings[Pred(SmashSettings.Count)]);
      AddSettingItem(setting);
    end;

    // free memory
    settingsToCombine.Free;
    slRecords.Free;
    sl.Free;
  end;
end;

// repaint when splitter is moved
procedure TSettingsManager.SplitterMoved(Sender: TObject);
begin
  Repaint;
end;

end.
