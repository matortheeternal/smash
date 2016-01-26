unit msPluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Menus, ComCtrls;

type
  TMiniPluginSelectionForm = class(TForm)
    CheckListBox: TCheckListBox;
    btnOK: TButton;
    lblPrompt: TLabel;
    btnCancel: TButton;
    CheckListPopupMenu: TPopupMenu;
    SelectAllItem: TMenuItem;
    SelectNoneItem: TMenuItem;
    InvertSelectionItem: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure CheckListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectAllItemClick(Sender: TObject);
    procedure SelectNoneItemClick(Sender: TObject);
    procedure InvertSelectionItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pluginsList: TStringList;
    selectionList: TStringList;
  end;

var
  MiniPluginSelectionForm: TMiniPluginSelectionForm;

implementation

{$R *.dfm}

procedure TMiniPluginSelectionForm.btnOKClick(Sender: TObject);
var
  i: Integer;
begin
  selectionList.Clear;
  for i := 0 to Pred(CheckListBox.Items.Count) do begin
    if CheckListBox.Checked[i] then
      selectionList.Add(CheckListBox.Items[i]);
  end;
  if selectionList.Count > 0 then
    ModalResult := mrOK;
end;

procedure TMiniPluginSelectionForm.CheckListBoxClick(Sender: TObject);
var
  i: Integer;
begin
  btnOK.Enabled := false;
  for i := 0 to Pred(CheckListBox.Items.Count) do begin
    if CheckListBox.Checked[i] then begin
      btnOK.Enabled := true;
      break;
    end;
  end;
end;

procedure TMiniPluginSelectionForm.CheckListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CheckListBoxClick(nil);
end;

procedure TMiniPluginSelectionForm.FormShow(Sender: TObject);
var
  i, index: Integer;
begin
  CheckListBox.Items.Text := pluginsList.Text;
  for i := 0 to Pred(selectionList.Count) do begin
    index := pluginsList.IndexOf(selectionList[i]);
    if index > -1 then
      CheckListBox.Checked[index] := true;
  end;

  // reset OK button enabled state
  CheckListBoxClick(nil);
end;

procedure TMiniPluginSelectionForm.InvertSelectionItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(CheckListBox.GetCount) do
    CheckListBox.Checked[i] := not CheckListBox.Checked[i];

  // reset OK button enabled state
  CheckListBoxClick(nil);
end;

procedure TMiniPluginSelectionForm.SelectAllItemClick(Sender: TObject);
begin
  CheckListBox.CheckAll(cbChecked);

  // reset OK button enabled state
  CheckListBoxClick(nil);
end;

procedure TMiniPluginSelectionForm.SelectNoneItemClick(Sender: TObject);
begin
  CheckListBox.CheckAll(cbUnChecked);

  // reset OK button enabled state
  CheckListBoxClick(nil);
end;

end.
