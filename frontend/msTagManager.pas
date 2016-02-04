unit msTagManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, RegularExpressions,
  // mte units
  RttiTranslation,
  // smash units
  msCore, msConfiguration, msTagHelper;

type
  TTagManager = class(TForm)
    Panel: TPanel;
    lblDescription: TLabel;
    meDescription: TMemo;
    btnClear: TButton;
    btnRemove: TButton;
    btnAdd: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnReset: TButton;
    kbCombine: TCheckBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ReadTags;
    procedure WriteTags;
    procedure AddTags(var slTagsToAdd: TStringList);
    procedure RemoveTags(var slTagsToRemove: TStringList);
  private
    { Private declarations }
  public
    { Public declarations }
    plugin: TPlugin;
  end;

var
  TagManager: TTagManager;

implementation

var
  slTags: TStringList;

{$R *.dfm}

procedure TTagManager.btnAddClick(Sender: TObject);
var
  thForm: TTagHelper;
begin
  thForm := TTagHelper.Create(self);
  try
    thForm.iMode := 1;
    thForm.slTags := TStringList.Create;
    GetMissingTags(slTags, thForm.slTags);
    if thForm.ShowModal = mrOK then
      AddTags(thForm.slTags);
  finally
    ReadTags;
    thForm.Free;
  end;
end;

procedure TTagManager.btnRemoveClick(Sender: TObject);
var
  thForm: TTagHelper;
begin
  thForm := TTagHelper.Create(self);
  try
    thForm.iMode := -1;
    thForm.slTags := TStringList.Create;
    thForm.slTags.Text := slTags.Text;
    if thForm.ShowModal = mrOK then
      RemoveTags(thForm.slTags);
  finally
    ReadTags;
    thForm.Free;
  end;
end;

procedure TTagManager.btnClearClick(Sender: TObject);
begin
  meDescription.Lines.Text := ClearTags(meDescription.Lines.Text);
  ReadTags;
end;

procedure TTagManager.btnResetClick(Sender: TObject);
begin
  meDescription.Lines.Text := plugin.description.Text;
  ReadTags;
end;

procedure TTagManager.FormShow(Sender: TObject);
begin
  // create tags stringlist
  slTags := TStringList.Create;

  // update description and tags
  meDescription.Lines.Text := plugin.description.Text;
  ReadTags;

  // update the form's caption
  Caption := Format(GetLanguageString('msTagM_Caption'), [plugin.filename]);
end;

procedure TTagManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOK then begin
    plugin.description.Text := meDescription.Lines.Text;
    if kbCombine.Checked then
      plugin.GetSettingTag;
    plugin.WriteDescription;
    plugin.Save;
  end;

  // free memory
  slTags.Free;
end;

procedure TTagManager.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);
end;

procedure TTagManager.ReadTags;
var
  bHasTags: Boolean;
begin
  slTags.Clear;
  ParseTags(meDescription.Lines.Text, slTags);
  bHasTags := slTags.Count > 0;
  btnClear.Enabled := bHasTags;
  btnRemove.Enabled := bHasTags;
end;

procedure TTagManager.WriteTags;
var
  sTags: String;
begin
  // clear tags
  meDescription.Lines.Text := ClearTags(meDescription.Lines.Text);

  // if no tags to write, exit
  if slTags.Count = 0 then
    exit;

  // see if all of the tags belong to the same group
  sTags := GetTagString(slTags);

  // write the tags to the description
  meDescription.Lines.Add(' ');
  meDescription.Lines.Add(sTags);
  meDescription.Lines.Text := Trim(meDescription.Lines.Text);
end;

procedure TTagManager.AddTags(var slTagsToAdd: TStringList);
var
  tag: string;
begin
  // add the tags to slTags
  for tag in slTagsToAdd do
    slTags.Add(tag);

  // write tags to the description
  WriteTags;
end;

procedure TTagManager.RemoveTags(var slTagsToRemove: TStringList);
var
  tag: string;
begin
  // remove the tags from slTags
  for tag in slTagsToRemove do
    slTags.Delete(slTags.IndexOf(tag));

  // write tags to the description
  WriteTags;
end;

end.
