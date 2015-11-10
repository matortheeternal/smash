unit msSettingsManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, ValEdit, CommCtrl, Menus,
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
          ToggleNodesItem: TMenuItem;
          PreserveDeletionsItem: TMenuItem;
          SingleEntityItem: TMenuItem;
          PruneNodesItem: TMenuItem;
          LinkNodeToItem: TMenuItem;
          UnlinkNodeItem: TMenuItem;
          ChainNodesItem: TMenuItem;
          StateImages: TImageList;
          FlagIcons: TImageList;

    // TREE METHODS
    procedure TreeDone;
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
    procedure ToggleNodesItemClick(Sender: TObject);
    procedure PreserveDeletionsItemClick(Sender: TObject);
    procedure SingleEntityItemClick(Sender: TObject);
    procedure PruneNodesItemClick(Sender: TObject);
    procedure UnlinkNodeItemClick(Sender: TObject);
    procedure LinkNodes(node1, node2: TTreeNode);
    procedure ChainNodesItemClick(Sender: TObject);
    procedure BuildTreeFromPlugins(var sl: TStringList);
    procedure BuildTree;
    function DumpElement(node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure DeleteNodes(var aList: TList);
    procedure DeleteChildren(node: TTreeNode);
    procedure AutoPrune;
    function CanPruneRecords: boolean;
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
procedure TSettingsManager.TreeDone;
begin
  xEditLogGroup := 'GENERAL';
  pForm.SaveLog;
  pForm.Visible := false;
  FlashWindow(Application.Handle, True);
  pForm.ShowModal;
  pForm.Free;
  Enabled := true;
  ShowWindow(Handle, SW_RESTORE);
  SetForegroundWindow(Handle);

  // save setting if it's a new setting
  if NewSettings.IndexOf(currentSetting) > -1 then
    DumpTree;

  // free lists
  if Assigned(timeCosts) then timeCosts.Free;
  if Assigned(pluginsToHandle) then pluginsToHandle.Free;
end;

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
begin
  if (Key = VK_SPACE) and Assigned(tvRecords.Selected) then begin
    CheckBoxManager(tvRecords.Selected);
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
    if not node.Selected then begin
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
  // draw hint if on a node with the link parameter
  node := tvRecords.GetNodeAt(X, Y);
  if not Assigned(node) then
    exit;
  e := TElementData(node.Data);
  if not Assigned(e) then
    exit;
  sHint := '';
  if e.linkTo <> '' then
    sHint := 'Linked to: '+e.linkTo;
  if e.linkFrom <> '' then begin
    if sHint <> '' then
      sHint := sHint + #13#10'Linked from: '+e.linkFrom
    else
      sHint := 'Linked from: '+e.linkFrom;
  end;
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
    le := TElementData(linkedNode.Data);
    le.linkFrom := '';
    e.linkTo := '';
  end;
  if bFrom and (e.linkFrom <> '') then begin
    linkedNode := GetSiblingNode(node, e.linkFrom);
    le := TElementData(linkedNode.Data);
    le.linkTo := '';
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
  LinkNodes(node, tvRecords.Selections[0]);
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
  bHasSelection, bHasMultiSelection, bSubrecordSelected, bHasChildren: boolean;
  i: Integer;
  node: TTreeNode;
  MenuItem: TMenuItem;
begin
  // clear link node submenu
  LinkNodeToItem.Clear;

  // get selection booleans
  bHasSelection := tvRecords.SelectionCount > 0;
  bHasMultiSelection := tvRecords.SelectionCount > 1;
  bSubrecordSelected := (tvRecords.SelectionCount = 1)
    and (tvRecords.Selections[0].Level > 1);
  bHasChildren := false;
  for i := 0 to Pred(tvRecords.SelectionCount) do
    bHasChildren := bHasChildren or tvRecords.Selections[i].HasChildren;

  // enable/disable menu items
  ToggleNodesItem.Enabled := bHasSelection;
  PreserveDeletionsItem.Enabled := bHasSelection and bHasChildren;
  SingleEntityItem.Enabled := bHasSelection and bHasChildren;
  PruneNodesItem.Enabled := bHasSelection;
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
    tvRecords.Selections[i].Data := e;
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
    if e.singleEntity then begin
      SetChildren(node, cUnChecked);
      node.StateIndex := cPartiallyChecked;
    end
    else begin
      SetChildren(node, cChecked);
      node.StateIndex := cChecked;
    end;
    UpdateParent(node.Parent);
    tvRecords.Selections[i].Data := e;
  end;
  tvRecords.Repaint;
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
    if (node.Level = 1) and (node.StateIndex = cUnChecked) then
      nodesToPrune.Add(node);
  end;
  DeleteNodes(nodesToPrune);
  nodesToPrune.Free;
  UpdateParent(tvRecords.Items[0].getFirstChild);
end;

procedure TSettingsManager.BuildTreeFromPlugins(var sl: TStringList);
var
  i: Integer;
  plugin: TPlugin;
begin
  // create lists
  pluginsToHandle := TList.Create;
  timeCosts := TStringList.Create;

  // populate lists
  for i := 0 to Pred(sl.Count) do begin
    plugin := PluginByFilename(sl[i]);
    if not Assigned(plugin) then continue;
    pluginsToHandle.Add(plugin);
    timeCosts.Add(plugin.numRecords);
  end;

  // free and exit if no patches to check for errors
  if pluginsToHandle.Count = 0 then begin
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // prepare to build tree in separate thread
  TreeView := tvRecords;
  Enabled := false;

  // show progress form
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := GetString('msProg_BuildingTree');
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start save thread
  TreeCallback := TreeDone;
  TTreeThread.Create;
end;

procedure TSettingsManager.BuildTree;
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
  bOverridesOnly := selectionForm.overridesOnly;
  bTarget := selectionForm.targetRecords;
  sRecords := selectionForm.records;
  if mr = mrOK then
    BuildTreeFromPlugins(slSelection);

  // free memory
  selectionForm.Free;
  slPlugins.Free;
  slSelection.Free;

  // delete setting if mr is cancel
  if mr = mrCancel then begin
    lvSettings.Items.Count := lvSettings.Items.Count - 1;
    SmashSettings.Delete(SmashSettings.IndexOf(currentSetting));
    NewSettings.Delete(NewSettings.IndexOf(currentSetting));
    currentSetting.Free;
    lvSettings.Repaint;
    lvSettingsChange(nil, nil, TItemChange(nil));
  end;
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
  if (e.priority > 0) then obj.I['r'] := e.priority;
  if (node.StateIndex <> cUnChecked) then
    obj.I['p'] := 1;
  if (e.preserveDeletions) then obj.I['d'] := 1;
  if (e.singleEntity) then obj.I['s'] := 1;
  if (e.linkTo <> '') then obj.S['lt'] := e.linkTo;
  if (e.linkFrom <> '') then obj.S['lf'] := e.linkFrom;

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
    if node.StateIndex <> cUnChecked then
      sl.Add(node.Text);
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
      if node.StateIndex = cUnChecked then
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
    if node.StateIndex = cUnChecked then begin
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
  if Assigned(currentSetting.tree) then
    LoadTree(tvRecords, currentSetting)
  else begin
    BuildTree;
  end;
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
  lvSettingsChange(nil, nil, TItemChange(nil));
  lvSettings.Repaint;
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
