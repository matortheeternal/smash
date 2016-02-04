unit msTagHelper;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst,
  // mte units
  RttiTranslation,
  // smash units
  msConfiguration;

type
  TTagHelper = class(TForm)
    CheckList: TCheckListBox;
    lblPrompt: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slTags: TStringList;
    iMode: Integer;
  end;

var
  TagHelper: TTagHelper;

implementation

{$R *.dfm}

procedure TTagHelper.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  slTags.Clear;
  if ModalResult = mrOK then
    for i := 0 to Pred(CheckList.Items.Count) do begin
      if CheckList.Checked[i] then
        slTags.Add(CheckList.Items[i]);
    end;
end;

procedure TTagHelper.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);
end;

procedure TTagHelper.FormShow(Sender: TObject);
begin
  // update lblPrompt
  case iMode of
    -1: begin
      Caption := GetLanguageString('msTagH_RemoveTags');
      lblPrompt.Caption := GetLanguageString('msTagH_PromptRemove');
    end;
    1: begin
      Caption := GetLanguageString('msTagH_AddTags');
      lblPrompt.Caption := GetLanguageString('msTagH_PromptAdd');
    end;
  end;

  // populate checklist
  CheckList.Items.Text := slTags.Text;
end;

end.
