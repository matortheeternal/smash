unit msConflictForm;

interface

uses
  Windows, Types, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, ImgList,
  // superobject
  superobject,
  // mte units
  mteHelpers, mteBase,
  // ms units
  msCore, msChoicePanel;

type
  TConflictForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    tvRecords: TTreeView;
    sbChoices: TScrollBox;
    FlagIcons: TImageList;
    StateImages: TImageList;
    procedure DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
    procedure tvRecordsCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PopulateTree(sig: string; aSetting: TSmashSetting);
    procedure ChoiceSelected(Sender: TObject);
    procedure CreateChoicePanel(sig: string; var lst: TList);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvRecordsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    lastHint: string;
  public
    // slConflicts is a stringlist of record signatures
    // each signature has a TSmashSetting associated with it
    // in slConflicts.Objects
    // slConflicts may have duplicate signatures with different
    // TSmashSetting objects
    slConflicts: TStringList;
  end;

var
  ConflictForm: TConflictForm;
  ChoicePanels, SelectedPanels: TList;

implementation

{$R *.dfm}

procedure TConflictForm.DrawFlag(Canvas: TCanvas; var x, y: Integer; id: Integer);
var
  icon: TIcon;
begin
  if x < 30 then
    exit;
  icon := TIcon.Create;
  FlagIcons.GetIcon(id, icon);
  Canvas.Draw(x, y, icon);
  Inc(x, 18);
  icon.Free;
end;

procedure TConflictForm.tvRecordsCustomDrawItem(Sender: TCustomTreeView;
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

procedure TConflictForm.tvRecordsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
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

procedure TConflictForm.PopulateTree(sig: string; aSetting: TSmashSetting);
var
  rootNode: TTreeNode;
  item: ISuperObject;
begin
  rootNode := tvRecords.Items[0];
  for item in aSetting.tree['records'] do
    if Copy(item.S['n'], 1, 4) = sig then begin
      LoadElement(tvRecords, rootNode, item, false);
      break;
    end;
end;

procedure TConflictForm.btnOKClick(Sender: TObject);
var
  i: Integer;
  aChoicePanel: TChoicePanel;
  aSetting: TSmashSetting;
begin
  // get user selection from choice panels, add it to slConflicts
  // to be handled later
  for i := 0 to Pred(ChoicePanels.Count) do begin
    aChoicePanel := TChoicePanel(ChoicePanels[i]);
    aSetting := aChoicePanel.GetSetting;
    if Assigned(aSetting) then begin
      DeleteMatchingItems(aChoicePanel.GetLabel, slConflicts);
      slConflicts.AddObject(aChoicePanel.GetLabel, TObject(aSetting));
    end;
  end;

  // close form with ModalResult = mrOK
  ModalResult := mrOK;
end;

procedure TConflictForm.ChoiceSelected(Sender: TObject);
var
  i: Integer;
  sig: string;
  aChoicePanel: TChoicePanel;
  aSetting: TSmashSetting;
begin
  tvRecords.Items.Clear;
  tvRecords.Items.Add(nil, 'Records');

  // populate tree view with selected record nodes
  for i := 0 to Pred(ChoicePanels.Count) do begin
    aChoicePanel := TChoicePanel(ChoicePanels[i]);
    if not aChoicePanel.Selected then
      continue;
    sig := aChoicePanel.GetLabel;
    aSetting := aChoicePanel.GetSetting;
    if Assigned(aSetting) then
      PopulateTree(sig, aSetting);
  end;
end;

procedure TConflictForm.CreateChoicePanel(sig: string; var lst: TList);
var
  aChoicePanel: TChoicePanel;
  i: Integer;
begin
  aChoicePanel := TChoicePanel.Create(sbChoices);
  aChoicePanel.SetLabel(sig);
  aChoicePanel.SetSelectCallback(ChoiceSelected);
  aChoicePanel.SetTop(36 * ChoicePanels.Count);
  for i := 0 to Pred(lst.Count) do
    aChoicePanel.AddChoice(lst[i]);
  ChoicePanels.Add(aChoicePanel);
end;

procedure TConflictForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeList(ChoicePanels);
end;

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  ChoicePanels := TList.Create;
end;

procedure TConflictForm.FormShow(Sender: TObject);
var
  i, index: Integer;
  aSetting: TSmashSetting;
  sl: TStringList;
  lst: TList;
begin
  // construct a new stringlist that has each unique record signature
  // paired with a list of all the settings that have that record signature
  // (retrieved from slConflicts)
  sl := TStringList.Create;
  try
    for i := 0 to Pred(slConflicts.Count) do begin
      aSetting := TSmashSetting(slConflicts.Objects[i]);
      index := sl.IndexOf(slConflicts[i]);
      if index = -1 then
        index := sl.AddObject(slConflicts[i], TObject(TList.Create));
      lst := TList(sl.Objects[index]);
      lst.Add(aSetting);
    end;

    // create choice panels
    for i := 0 to Pred(sl.Count) do begin
      lst := TList(sl.Objects[i]);
      if lst.Count > 1 then
        CreateChoicePanel(sl[i], lst);
    end;
  finally
    // free memory
    sl.Free;
  end;
end;

end.
