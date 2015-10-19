unit mpEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  mpBackend, mteLogger;

type
  TEditForm = class(TForm)
    lblUsername: TLabel;
    lblFilename: TLabel;
    edUsername: TEdit;
    edFilename: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lblRecordcount: TLabel;
    edRecordCount: TEdit;
    edHash: TEdit;
    lblHash: TLabel;
    lblRating: TLabel;
    lblMergeVersion: TLabel;
    lblNotes: TLabel;
    cbRating: TComboBox;
    edMergeVersion: TEdit;
    meNotes: TMemo;
    procedure FormShow(Sender: TObject);
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    report: TReport;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.dfm}

{ Save report by pressing enter in any TEdit }
procedure TEditForm.btnOkClick(Sender: TObject);
begin
  report.username := edUsername.Text;
  report.filename := edFilename.Text;
  report.hash := edHash.Text;
  report.recordCount := StrToInt(edRecordCount.Text);
  report.rating := cbRating.ItemIndex - 1;
  report.mergeVersion := edMergeVersion.Text;
  report.notes := meNotes.Lines.Text;
end;

procedure TEditForm.edKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (HiWord(GetKeyState(vk_Return)) <> 0) and btnOk.Enabled then begin
    btnOkClick(nil);
    ModalResult := mrOk;
  end;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  if Assigned(report) then begin
    edUsername.Text := report.username;
    edFilename.Text := report.filename;
    edHash.Text := report.hash;
    edRecordCount.Text := IntToStr(report.recordCount);
    cbRating.ItemIndex := report.rating + 1;
    edMergeVersion.Text := report.mergeVersion;
    meNotes.Lines.Text := report.notes;
  end;
end;

end.
