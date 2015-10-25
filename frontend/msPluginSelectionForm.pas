unit msPluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst;

type
  TPluginSelectionForm = class(TForm)
    CheckListBox: TCheckListBox;
    btnOK: TButton;
    lblPrompt: TLabel;
    btnCancel: TButton;
    kbOverridesOnly: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure CheckListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    pluginsList: TStringList;
    selectionList: TStringList;
    overridesOnly: boolean;
  end;

var
  PluginSelectionForm: TPluginSelectionForm;

implementation

{$R *.dfm}

procedure TPluginSelectionForm.btnOKClick(Sender: TObject);
var
  i: Integer;
begin
  selectionList.Clear;
  for i := 0 to Pred(CheckListBox.Items.Count) do begin
    if CheckListBox.Checked[i] then
      selectionList.Add(CheckListBox.Items[i]);
  end;
  overridesOnly := kbOverridesOnly.Checked;
  if selectionList.Count > 0 then
    ModalResult := mrOK;
end;

procedure TPluginSelectionForm.CheckListBoxClick(Sender: TObject);
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

procedure TPluginSelectionForm.CheckListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  CheckListBoxClick(nil);
end;

procedure TPluginSelectionForm.FormShow(Sender: TObject);
var
  i, index: Integer;
begin
  CheckListBox.Items.Text := pluginsList.Text;
  for i := 0 to Pred(selectionList.Count) do begin
    index := pluginsList.IndexOf(selectionList[i]);
    if index > -1 then
      CheckListBox.Checked[index] := true;
  end;
end;

end.
