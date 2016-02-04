unit msEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mte units
  mteHelpers,
  // mator smash units
  msCore, msConfiguration;

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
    function NameValid: boolean;
    function ESPFilenameValid: boolean;
    procedure edFilenameChange(Sender: TObject);
    procedure edFilenameEnter(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

function TEditForm.NameValid: boolean;
var
  aPatch: TPatch;
  i: integer;
begin
  Result := false;

  // return false if illegal characters present
  if not FileNameValid(edName.Text) then
    exit;

  // return false if edName is blank
  if Trim(edName.Text) = '' then
    exit;

  // return false if patch with specified name already exists
  for i := 0 to Pred(PatchesList.Count) do begin
    aPatch := TPatch(PatchesList[i]);
    if (aPatch.name = edName.Text) and (aPatch <> patch) then
      exit;
  end;

  // all tests passed, return true
  Result := true;
end;

function TEditForm.ESPFilenameValid: boolean;
var
  aPatch: TPatch;
  plugin: TPlugin;
  loadOrder, highLoadOrder, i: integer;
  sFilename: string;
begin
  Result := false;

  // return false if illegal characters present
  if not FileNameValid(edFilename.Text) then
    exit;

  // return false if filename doesn't end in .esp
  if not StrEndsWith(edFilename.Text, '.esp') then
    exit;

  // return false if specified filename corresponds to a
  // plugin that is in patch
  if patch.plugins.IndexOf(edFilename.Text) > -1 then
    exit;

  // check if there's a load order error patching into the specified file
  plugin := PluginByFilename(edFilename.Text);
  loadOrder := PluginLoadOrder(edFilename.Text);
  highLoadOrder := MaxInt;
  if patch.plugins.Count > 0 then begin
    sFilename := patch.plugins[patch.plugins.Count -1];
    highLoadOrder := PluginLoadOrder(sFilename);
  end;

  // return false if there's a load order error
  if Assigned(plugin) and (loadOrder > highLoadOrder) then
    exit;

  // return false if patch exists
  for i := 0 to Pred(PatchesList.Count) do begin
    aPatch := TPatch(PatchesList[i]);
    if (aPatch.filename = edFileName.Text) and (aPatch <> patch) then
      exit;
  end;

  // all tests passed, return true
  Result := true;
end;

procedure TEditForm.edFilenameChange(Sender: TObject);
var
  valid: boolean;
begin
  // if invalid disable btnOk, show hint, and make font color red
  valid := ESPFilenameValid;
  btnOk.Enabled := valid and NameValid;
  edFilename.ShowHint := valid;
  if valid then
    edFilename.Font.Color := clWindowText
  else
    edFilename.Font.Color := $0000ff;
end;

procedure TEditForm.edFilenameEnter(Sender: TObject);
begin
  // change selection to not include the .esp
  if (edFilename.SelLength = Length(edFilename.Text))
  and StrEndsWith(edFilename.Text, '.esp') then
    edFilename.SelLength := edFilename.SelLength - 4;
end;

procedure TEditForm.edNameChange(Sender: TObject);
var
  valid, exists: boolean;
begin
  valid := NameValid;
  exists := DirectoryExists(settings.patchDirectory + edName.Text)
    and (edName.Text <> patch.name);

  // if invalid show hint and make font color red
  btnOk.Enabled := valid and ESPFilenameValid;
  edName.ShowHint := (not valid) or exists;
  if (not valid) or exists then
    edName.Font.Color := $0000ff
  else
    edName.Font.Color := clWindowText;
end;

{ Save patch by pressing enter in edName or edFilename }
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
  edName.Text := patch.name;
  edFilename.Text := patch.filename;
end;

end.
