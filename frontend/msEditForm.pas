unit msEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // Mator Smash components
  msFrontend;

type
  TEditForm = class(TForm)
    PageControl: TPageControl;
    EditTabSheet: TTabSheet;
    btnOK: TButton;
    lblName: TLabel;
    lblFilename: TLabel;
    edName: TEdit;
    edFilename: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    patch: TPatch;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.dfm}

procedure TEditForm.btnOKClick(Sender: TObject);
begin
  patch.name := edName.Text;
  patch.filename := edFilename.Text;
  ModalResult := mrOK;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  edName.Text := patch.name;
  edFilename.Text := patch.filename;
end;

end.
