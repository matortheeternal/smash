unit mpUserForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  // mp components
  mpBackend;

type
  TUserForm = class(TForm)
    PageControl: TPageControl;
    TabSheet: TTabSheet;
    btnOK: TButton;
    lblIP: TLabel;
    lblUsername: TLabel;
    lblAuth: TLabel;
    lblFirstSeen: TLabel;
    lblLastSeen: TLabel;
    lblTimesSeen: TLabel;
    lblDownload: TLabel;
    lblUpload: TLabel;
    lblTimesRun: TLabel;
    lblMergesBuilt: TLabel;
    lblPluginsChecked: TLabel;
    lblPluginsMerged: TLabel;
    lblReportsSubmitted: TLabel;
    edIP: TEdit;
    edUsername: TEdit;
    edAuth: TEdit;
    edFirstSeen: TEdit;
    edLastSeen: TEdit;
    edTimesSeen: TEdit;
    edDownload: TEdit;
    edUpload: TEdit;
    edTimesRun: TEdit;
    edMergesBuilt: TEdit;
    edPluginsChecked: TEdit;
    edPluginsMerged: TEdit;
    edReportsSubmitted: TEdit;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    User: TUser;
  end;

var
  UserForm: TUserForm;

implementation

{$R *.dfm}

procedure TUserForm.btnOKClick(Sender: TObject);
var
  WhereClause, SetClause: string;
begin
  WhereClause := UserWhereClause(user);
  user.ip := edIP.Text;
  user.username := edUsername.Text;
  user.auth := edAuth.Text;
  user.firstSeen := StrToDateTime(edFirstSeen.Text);
  user.lastSeen := StrToDateTime(edLastSeen.Text);
  user.timesSeen := StrToInt(edTimesSeen.Text);
  user.download := StrToInt(edDownload.Text);
  user.upload := StrToInt(edUpload.Text);
  user.timesRun := StrToInt(edTimesRun.Text);
  user.mergesBuilt := StrToInt(edMergesBuilt.Text);
  user.pluginsChecked := StrToInt(edPluginsChecked.Text);
  user.pluginsMerged := StrToInt(edPluginsMerged.Text);
  user.reportsSubmitted := StrToInt(edReportsSubmitted.Text);
  SetClause := UserSetClause(user);
  DBUpdateUser(SetClause, WhereClause);
  ModalResult := mrOk;
end;

procedure TUserForm.FormShow(Sender: TObject);
begin
  caption := UserString(user);
  edIP.Text := user.ip;
  edUsername.Text := user.username;
  edAuth.Text := user.auth;
  edFirstSeen.Text := DateTimeToStr(user.firstSeen);
  edLastSeen.Text := DateTimeToStr(user.lastSeen);
  edTimesSeen.Text := IntToStr(user.timesSeen);
  edDownload.Text := IntToStr(user.download);
  edUpload.Text := IntToStr(user.upload);
  edTimesRun.Text := IntToStr(user.timesRun);
  edMergesBuilt.Text := IntToStr(user.mergesBuilt);
  edPluginsChecked.Text := IntToStr(user.pluginsChecked);
  edPluginsMerged.Text := IntToStr(user.pluginsMerged);
  edReportsSubmitted.Text := IntToStr(user.reportsSubmitted);
end;

end.
