unit msSettingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ExtCtrls,
  // superobject
  superobject,
  // xEdit
  wbInterface,
  // mte units
  mteHelpers, mteProgressForm,
  // ms units
  msFrontend, msThreads, msPluginSelectionForm;

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
    procedure TreeDone;
    procedure BuildTreeFromPlugins(var sl: TStringList);
    procedure BuildTree;
    procedure LoadElement(node: TTreeNode; obj: ISuperObject);
    procedure LoadTree;
    function DumpElement(node: TTreeNode): ISuperObject;
    procedure DumpTree;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TSettingForm.BuildTreeFromPlugins(var sl: TStringList);
var
  i: Integer;
  plugin: TPlugin;
  rootNode: TTreeNode;
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
begin
  child := TreeView.Items.AddChild(node, obj.S['name']);
  child.Data := Pointer(TElementData.Create( obj.I['priority'], obj.B['process'],
      obj.B['ignoreDeletions'], obj.B['singleEntity'] ));
  try
    for item in obj.O['children'] do
      LoadElement(child, item);
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
  rootNode := TreeView.Items.Add(nil, 'Records');
  obj := setting.tree;
  for item in obj['Records'] do
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
  obj.S['name'] := node.Text;
  // get data properties
  e := TElementData(node.Data);
  obj.I['priority'] := e.priority;
  obj.B['process'] := e.process;
  obj.B['ignoreDeletions'] := e.ignoreDeletions;
  obj.B['singleEntity'] := e.singleEntity;

  // exit if no children to dump
  if not node.hasChildren then
    exit;
  // dump subrecords (children)
  obj.O['children'] := SA([]);
  child := node.getFirstChild;
  i := 0;
  while Assigned(child) do begin
    obj.A['children'].O[i] := DumpElement(child);
    child := child.getNextSibling;
    Inc(i);
  end;
end;

procedure TSettingForm.DumpTree;
var
  i: Integer;
  obj: ISuperObject;
  node, rootNode: TTreeNode;
begin
  obj := SO;
  obj.O['Records'] := SA([]);
  rootNode := TreeView.Items[0];

  // loop through records
  node := rootNode.getFirstChild;
  i := 0;
  while Assigned(node) do begin
    obj.A['Records'].O[i] := DumpElement(node);
    node := node.getNextSibling;
    Inc(i);
  end;

  setting.tree := obj;
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
end;

end.
