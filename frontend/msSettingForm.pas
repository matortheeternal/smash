unit msSettingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ExtCtrls, StrUtils, ImgList,
  // superobject
  superobject,
  // xEdit
  wbInterface,
  // mte units
  mteHelpers, mteProgressForm,
  // ms units
  msFrontend, msThreads, msPluginSelectionForm, msPriorityForm;

type
  TSettingMode = ( smNew, smEdit, smClone, smCombine );
  TSettingForm = class(TForm)
    PageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    TreePopupMenu: TPopupMenu;
    btnCancel: TButton;
    btnOK: TButton;
    lblName: TLabel;
    edName: TEdit;
    meDescription: TMemo;
    tvRecords: TTreeView;
    RightPanel: TPanel;
    lblDescription: TLabel;
    ToggleNodesItem: TMenuItem;
    ChangePriorityItem: TMenuItem;
    IgnoreDeletionsItem: TMenuItem;
    SingleEntityItem: TMenuItem;
    StateImages: TImageList;
    PruneNodesItem: TMenuItem;
    FlagIcons: TImageList;
    procedure TreeDone;
    procedure BuildTreeFromPlugins(var sl: TStringList);
    procedure BuildTree;
    procedure LoadElement(node: TTreeNode; obj: ISuperObject; bWithinSingle: boolean);
    procedure LoadTree;
    function DumpElement(node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure DeleteNodes(var aList: TList);
    procedure DeleteChildren(node: TTreeNode);
    procedure AutoPrune;
    function CanPruneRecords: boolean;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToggleNodesItemClick(Sender: TObject);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure ChangePriorityItemClick(Sender: TObject);
    procedure IgnoreDeletionsItemClick(Sender: TObject);
    procedure SingleEntityItemClick(Sender: TObject);
    procedure tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvRecordsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edNameChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure PruneNodesItemClick(Sender: TObject);
    procedure DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
    procedure tvRecordsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    setting: TSmashSetting;
    mode: TSettingMode;
  end;

var
  SettingForm: TSettingForm;
  pForm: TProgressForm;

implementation

{$R *.dfm}

procedure TSettingForm.TreeDone;
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

  // free lists
  if Assigned(timeCosts) then timeCosts.Free;
  if Assigned(pluginsToHandle) then pluginsToHandle.Free;
end;

{
  SetChildren
  Sets the StateIndex attribute of all the children of @node
  to @state.  Uses recursion.
}
procedure SetChildren(node: TTreeNode; state: Integer);
var
  e: TElementData;
  tmp: TTreeNode;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // loop through children setting StateIndex to state
  // if child has children, recurse into that child
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    tmp.StateIndex := state;
    e := TElementData(tmp.Data);
    e.process := state <> cUnChecked;
    tmp.Data := Pointer(e);
    if tmp.HasChildren then
      SetChildren(tmp, state);
    tmp := tmp.getNextSibling;
  end;
end;

{
  UpdateParent
  Calculates and sets the StateIndex attribute for @node based
  on the StateIndex values of its children.  Uses recursion to
  update parents of the parent that was updated.
}
procedure UpdateParent(node: TTreeNode);
var
  tmp: TTreeNode;
  state: Integer;
  e: TElementData;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // parent state is checked if all siblings are checked
  state := cChecked;
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    if tmp.StateIndex <> cChecked then begin
      state := cPartiallyChecked;
      break;
    end;
    tmp := tmp.getNextSibling;
  end;

  // parent state is unchecked if all siblings are unchecked
  if state = cPartiallyChecked then begin
    state := cUnChecked;
    tmp := node.getFirstChild;
    while Assigned(tmp) do begin
      if tmp.StateIndex <> cUnChecked then begin
        state := cPartiallyChecked;
        break;
      end;
      tmp := tmp.getNextSibling;
    end;
  end;

  // set state, recurse to next parent
  node.StateIndex := state;
  e := TElementData(node.Data);
  e.process := state <> cUnChecked;
  e.singleEntity := false;
  node.Data := Pointer(e);
  tmp := node.Parent;
  UpdateParent(tmp);
end;

{
  CheckBoxManager
  Manages checkboxes in the TTreeView.  Changes the StateIndex
  of the checkbox associated with @node.  Uses SetChildren and
  UpdateParent.  Called by tvClick and tvKeyDown.
}
procedure CheckBoxManager(node: TTreeNode);
var
  e: TElementData;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // if unchecked or partially checked, set to checked and
  // set all children to checked, update parents
  if (node.StateIndex = cUnChecked)
  or (node.StateIndex = cPartiallyChecked) then begin
    node.StateIndex := cChecked;
    e := TElementData(node.Data);
    e.process := true;
    e.singleEntity := false;
    node.Data := Pointer(e);
    UpdateParent(node.Parent);
    SetChildren(node, cChecked);
  end
  // if checked, set to unchecked and set all children to
  // unchecked, update parents
  else if node.StateIndex = cChecked then begin
    node.StateIndex := cUnChecked;
    e := TElementData(node.Data);
    node.Data := Pointer(e);
    e.process := false;
    e.singleEntity := false;
    UpdateParent(node.Parent);
    SetChildren(node, cUnChecked);
  end;
end;

procedure TSettingForm.DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
var
  icon: TIcon;
begin
  icon := TIcon.Create;
  FlagIcons.GetIcon(id, icon);
  Canvas.Draw(x, y, icon);
  Inc(x, 18);
  icon.Free;
end;

procedure TSettingForm.tvRecordsCustomDrawItem(Sender: TCustomTreeView;
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

    if e.ignoreDeletions then
      DrawFlag(Sender.Canvas, x, y, 0);
    if e.singleEntity then
      DrawFlag(Sender.Canvas, x, y, 1);
  end;
end;

procedure TSettingForm.tvRecordsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(tvRecords.Selected) then
    CheckBoxManager(tvRecords.Selected);
  // repaint tree view in case a single entity flag was unset
  tvRecords.Repaint;
end;

procedure TSettingForm.tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HT: THitTests;
begin
  HT := tvRecords.GetHitTestInfoAt(X, Y);
  if (HT - [htOnStateIcon] <> HT) then
    CheckBoxManager(tvRecords.Selected);
  // repaint tree view in case a single entity flag was unset
  tvRecords.Repaint;
end;

procedure TSettingForm.TreePopupMenuPopup(Sender: TObject);
var
  bHasSelection, bHasChildren: boolean;
  i: Integer;
begin
  bHasSelection := tvRecords.SelectionCount > 0;
  bHasChildren := false;
  for i := 0 to Pred(tvRecords.SelectionCount) do
    bHasChildren := bHasChildren or tvRecords.Selections[i].HasChildren;
  ToggleNodesItem.Enabled := bHasSelection;
  IgnoreDeletionsItem.Enabled := bHasSelection and bHasChildren;
  SingleEntityItem.Enabled := bHasSelection and bHasChildren;
  PruneNodesItem.Enabled := bHasSelection;
end;

procedure TSettingForm.ToggleNodesItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do
    CheckboxManager(tvRecords.Selections[i]);
end;

procedure TSettingForm.ChangePriorityItemClick(Sender: TObject);
var
  cForm: TPriorityForm;
  i: Integer;
  e: TElementData;
  node: TTreeNode;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    node := tvRecords.Selections[i];
    e := TElementData(node.Data);
    cForm := TPriorityForm.Create(self);
    cForm.Caption := Format('Change node priority (%s)', [node.Text]);
    cForm.Priority := e.priority;
    if cForm.ShowModal = mrOK then begin
      e.priority := cForm.Priority;
      tvRecords.Selections[i].Data := Pointer(e);
    end;
    cForm.Free;
  end;
  tvRecords.Repaint;
end;

procedure TSettingForm.IgnoreDeletionsItemClick(Sender: TObject);
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
    e.ignoreDeletions := not e.ignoreDeletions;
    tvRecords.Selections[i].Data := Pointer(e);
  end;
  tvRecords.Repaint;
end;

procedure TSettingForm.SingleEntityItemClick(Sender: TObject);
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
    tvRecords.Selections[i].Data := Pointer(e);
  end;
  tvRecords.Repaint;
end;

procedure TSettingForm.PruneNodesItemClick(Sender: TObject);
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

procedure TSettingForm.BuildTreeFromPlugins(var sl: TStringList);
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
  pForm.Caption := GetString('mpProg_BuildingTree');
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start save thread
  TreeCallback := TreeDone;
  TTreeThread.Create;
end;

procedure TSettingForm.BuildTree;
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
  if mr = mrOK then
    BuildTreeFromPlugins(slSelection);

  // free memory
  selectionForm.Free;
  slPlugins.Free;
  slSelection.Free;

  // close if mr is cancel
  if mr = mrCancel then
    ModalResult := mrCancel;
end;

procedure TSettingForm.LoadElement(node: TTreeNode; obj: ISuperObject;
  bWithinSingle: boolean);
var
  item: ISuperObject;
  child: TTreeNode;
  bProcess, bIgnoreDeletions, bIsSingle: boolean;
begin
  if not Assigned(obj) then
    exit;
  child := tvRecords.Items.AddChild(node, obj.S['n']);
  bProcess := obj.I['p'] = 1;
  bIgnoreDeletions := obj.I['i'] = 1;
  bIsSingle := obj.I['s'] = 1;
  bWithinSingle := bWithinSingle or bIsSingle;
  if bIsSingle then
    child.StateIndex := cPartiallyChecked
  else if bProcess then
    child.StateIndex := cChecked
  else
    child.StateIndex := cUnChecked;
  child.Data := Pointer(TElementData.Create( obj.I['r'], bProcess,
      bIgnoreDeletions, bIsSingle ));
  if Assigned(obj.O['c']) then try
    for item in obj['c'] do
      LoadElement(child, item, bWithinSingle);
    if not bWithinSingle then
      UpdateParent(child);
  except
    on x : Exception do
      // nothing
  end;
end;

procedure TSettingForm.LoadTree;
var
  obj, item: ISuperObject;
  rootNode: TTreeNode;
begin
  rootNode := tvRecords.Items.Add(nil, 'Records');
  obj := setting.tree;
  for item in obj['records'] do
    LoadElement(rootNode, item, false);
end;

function TSettingForm.DumpElement(node: TTreeNode): ISuperObject;
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
  obj.I['r'] := e.priority;
  obj.I['p'] := IfThenInt(e.process);
  obj.I['i'] := IfThenInt(e.ignoreDeletions);
  obj.I['s'] := IfThenInt(e.singleEntity);

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

procedure TSettingForm.DumpTree;
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

  setting.tree := obj;
  setting.records := sl.CommaText;
  sl.Free;
end;

procedure TSettingForm.edNameChange(Sender: TObject);
begin
  btnOK.Enabled := (setting.name = edName.Text) or
    not Assigned(SettingByName(edName.Text));
end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  edName.Text := setting.name;
  meDescription.Lines.Text := setting.description;
  if Assigned(setting.tree) then
    LoadTree
  else begin
    BuildTree;
  end;
end;

procedure TSettingForm.btnCancelClick(Sender: TObject);
begin
  if (mode = smNew) or (mode = smClone) then
    setting.Free;
  ModalResult := mrCancel;
end;

procedure TSettingForm.DeleteNodes(var aList: TList);
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

procedure TSettingForm.DeleteChildren(node: TTreeNode);
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

procedure TSettingForm.AutoPrune;
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

function TSettingForm.CanPruneRecords: boolean;
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

procedure TSettingForm.btnOKClick(Sender: TObject);
begin
  setting.Rename(edName.Text);
  setting.description := meDescription.Lines.Text;
  if (mode <> smEdit) and CanPruneRecords then
    AutoPrune;
  DumpTree;
  ModalResult := mrOK;
end;

end.
