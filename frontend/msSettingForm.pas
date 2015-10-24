unit msSettingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ExtCtrls, StrUtils,
  // superobject
  superobject,
  // xEdit
  wbInterface,
  // mte units
  mteHelpers, mteProgressForm,
  // ms units
  msFrontend, msThreads, msPluginSelectionForm, msPriorityForm, ImgList;

type
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
    ToggleItem: TMenuItem;
    ChangePriorityItem: TMenuItem;
    IgnoreDeletionsItem: TMenuItem;
    SingleEntityItem: TMenuItem;
    StateImages: TImageList;
    procedure TreeDone;
    procedure BuildTreeFromPlugins(var sl: TStringList);
    procedure BuildTree;
    procedure LoadElement(node: TTreeNode; obj: ISuperObject);
    procedure LoadTree;
    function DumpElement(node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToggleItemClick(Sender: TObject);
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure ChangePriorityItemClick(Sender: TObject);
    procedure IgnoreDeletionsItemClick(Sender: TObject);
    procedure SingleEntityItemClick(Sender: TObject);
    procedure tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvRecordsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    setting: TSmashSetting;
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

procedure TSettingForm.TreePopupMenuPopup(Sender: TObject);
var
  bHasSelection: boolean;
  //i: Integer;
  //e: TElementData;
begin
  bHasSelection := tvRecords.SelectionCount > 0;
  ToggleItem.Enabled := bHasSelection;
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
    UpdateParent(node.Parent);
    SetChildren(node, cUnChecked);
  end;
end;

procedure TSettingForm.tvRecordsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(tvRecords.Selected) then
    CheckBoxManager(tvRecords.Selected);
end;

procedure TSettingForm.tvRecordsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HT: THitTests;
begin
  HT := tvRecords.GetHitTestInfoAt(X, Y);
  if (HT - [htOnStateIcon] <> HT) then
    CheckBoxManager(tvRecords.Selected);
end;

procedure TSettingForm.ToggleItemClick(Sender: TObject);
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
end;

procedure TSettingForm.IgnoreDeletionsItemClick(Sender: TObject);
var
  i: Integer;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    e := TElementData(tvRecords.Selections[i].Data);
    e.ignoreDeletions := not e.ignoreDeletions;
    tvRecords.Selections[i].Data := Pointer(e);
  end;
end;

procedure TSettingForm.SingleEntityItemClick(Sender: TObject);
var
  i: Integer;
  e: TElementData;
begin
  for i := 0 to Pred(tvRecords.SelectionCount) do begin
    e := TElementData(tvRecords.Selections[i].Data);
    e.singleEntity := not e.singleEntity;
    tvRecords.Selections[i].Data := Pointer(e);
  end;
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

procedure TSettingForm.LoadElement(node: TTreeNode; obj: ISuperObject);
var
  item: ISuperObject;
  child: TTreeNode;
  bProcess: boolean;
begin
  if not Assigned(obj) then
    exit;
  child := tvRecords.Items.AddChild(node, obj.S['n']);
  bProcess := obj.I['p'] = 1;
  if bProcess then
    child.StateIndex := cChecked
  else
    child.StateIndex := cUnChecked;
  child.Data := Pointer(TElementData.Create( obj.I['r'], obj.I['p'] = 1,
      obj.I['i'] = 1, obj.I['s'] = 1 ));
  if Assigned(obj.O['c']) then try
    for item in obj['c'] do
      LoadElement(child, item);
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
    LoadElement(rootNode, item);
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
  obj.I['p'] := IfThenInt(e.process, 1, 0);
  obj.I['i'] := IfThenInt(e.ignoreDeletions, 1, 0);
  obj.I['s'] := IfThenInt(e.singleEntity, 1, 0);

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
begin
  obj := SO;
  obj.O['records'] := SA([]);
  rootNode := tvRecords.Items[0];

  // loop through records
  node := rootNode.getFirstChild;
  i := 0;
  while Assigned(node) do begin
    obj.A['records'].O[i] := DumpElement(node);
    node := node.getNextSibling;
    Inc(i);
  end;

  setting.tree := obj;
end;

procedure TSettingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //tvRecords.Free;
  //if Assigned(TreeView) then
    //TreeView.Free;
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

procedure TSettingForm.btnOKClick(Sender: TObject);
begin
  setting.name := edName.Text;
  setting.description := meDescription.Lines.Text;
  DumpTree;
  ModalResult := mrOK;
end;

end.
