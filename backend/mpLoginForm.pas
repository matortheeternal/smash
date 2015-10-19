unit mpLoginForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, pngimage, StdCtrls, Buttons, FileCtrl,
  mpBackend;

type
  TLoginForm = class(TForm)
    btnLogin: TButton;
    btnCancel: TButton;
    lblUser: TLabel;
    gbLogin: TGroupBox;
    edUser: TEdit;
    lblPassword: TLabel;
    edPassword: TEdit;
    lblDatabase: TLabel;
    edDatabase: TEdit;
    lblHost: TLabel;
    edHost: TEdit;
    lblPort: TLabel;
    edPort: TEdit;
    procedure btnLoginClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.dfm}

const
  SelectedColor = $f0e3d8;
  DefaultColor = clBtnFace;

procedure TLoginForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TLoginForm.btnLoginClick(Sender: TObject);
begin
  // attempt to login to the mysql database
  DBLogin(edUser.Text, edPassword.text, edDatabase.Text, edHost.Text,
    edPort.Text);

  // if login successful, close form with ModalResult = mrOk
  if bLoginSuccess then begin
    settings.sqlUser := edUser.Text;
    settings.sqlPassword := edPassword.Text;
    settings.sqlDatabase := edDatabase.Text;
    settings.sqlHost := edHost.Text;
    settings.sqlPort := edPort.Text;
    SaveSettings;
    ModalResult := mrOk;
  end;
end;
procedure TLoginForm.FormCreate(Sender: TObject);
begin
  // load login from settings for fields that are not empty
  if not (settings.sqlUser = '') then
    edUser.Text := settings.sqlUser;
  if not (settings.sqlPassword = '') then
    edPassword.Text := settings.sqlPassword;
  if not (settings.sqlDatabase = '') then
    edDatabase.Text := settings.sqlDatabase;
  if not (settings.sqlHost = '') then
    edHost.Text := settings.sqlHost;
  if not (settings.sqlPort = '') then
    edPort.Text := settings.sqlPort;
end;

end.
