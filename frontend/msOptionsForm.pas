unit msOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ImgList, FileCtrl, ExtCtrls, Types,
  // mte units
  mteHelpers, RttiTranslation,
  // mp units
  msConfiguration;

type
  TOptionsForm = class(TForm)
    [FormPrefix('mpOpt')]
      SettingsPageControl: TPageControl;
      btnCancel: TButton;
      btnOK: TButton;
      IconList: TImageList;
      [FormSection('General Tab')]
        GeneralTabSheet: TTabSheet;
        gbLanguage: TGroupBox;
        lblLanguage: TLabel;
        gbStyle: TGroupBox;
        kbSimpleDictionary: TCheckBox;
        kbSimplePlugins: TCheckBox;
        [FormSection('DontTranslate')]
          cbLanguage: TComboBox;
      [FormSection('Patching Tab')]
        PatchingTabSheet: TTabSheet;
        gbGeneral: TGroupBox;
        edPatchDirectory: TEdit;
        lblDestinationDirectory: TLabel;
        btnBrowsePatchDirectory: TSpeedButton;
        gbDebug: TGroupBox;
        kbDebugPatchStatus: TCheckBox;
        kbDebugMasters: TCheckBox;
        kbDebugArrays: TCheckBox;
        kbDebugSkips: TCheckBox;
        kbDebugTraversal: TCheckBox;
        kbDebugTypes: TCheckBox;
        kbDebugChanges: TCheckBox;
        kbDebugSingle: TCheckBox;
        kbDebugLinks: TCheckBox;
        gbOther: TGroupBox;
        kbBuildRefs: TCheckBox;
        kbPreserveITPOs: TCheckBox;
      [FormSection('Advanced Tab')]
        AdvancedTabSheet: TTabSheet;
        lblCurrentProfile: TLabel;
        gbLogging: TGroupBox;
        lblClientColor: TLabel;
        cbClientColor: TColorBox;
        lblGeneralColor: TLabel;
        cbGeneralColor: TColorBox;
        lblLoadColor: TLabel;
        cbLoadColor: TColorBox;
        lblPluginColor: TLabel;
        cbPluginColor: TColorBox;
        lblErrorColor: TLabel;
        cbErrorColor: TColorBox;
        lblTemplate: TLabel;
        meTemplate: TMemo;
        lblSample: TLabel;
        [FormSection('DontTranslate')]
          lblCurrentProfileName: TLabel;
          lblSampleValue: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure LoadLanguageOptions;
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowsePatchDirectoryClick(Sender: TObject);
    procedure meTemplateChange(Sender: TObject);
    procedure appendBackslashOnExit(Sender: TObject);
    procedure btnChangeSmashProfileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;
  slSampleLogMessage: TStringList;

implementation

{$R *.dfm}

procedure TOptionsForm.btnBrowsePatchDirectoryClick(Sender: TObject);
begin
  BrowseForFolder(edPatchDirectory, PathList.Values['ProgramPath']);
end;

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  // check if we need to update patch status afterwards
  ProgramStatus.bUpdatePatchStatus := settings.patchDirectory <> edPatchDirectory.Text;

  // General > Language
  settings.language := cbLanguage.Text;
  // General > Style
  settings.simpleDictionaryView := kbSimpleDictionary.Checked;
  settings.simplePluginsView := kbSimplePlugins.Checked;

  // Patching > General
  settings.patchDirectory := edPatchDirectory.Text;
  // Patching > Debug
  settings.debugPatchStatus := kbDebugPatchStatus.Checked;
  settings.debugMasters := kbDebugMasters.Checked;
  settings.debugArrays := kbDebugArrays.Checked;
  settings.debugSkips := kbDebugSkips.Checked;
  settings.debugTraversal := kbDebugTraversal.Checked;
  settings.debugTypes := kbDebugTypes.Checked;
  settings.debugChanges := kbDebugChanges.Checked;
  settings.debugSingle := kbDebugSingle.Checked;
  settings.debugLinks := kbDebugLinks.Checked;
  settings.buildRefs := kbBuildRefs.Checked;
  settings.preserveITPOs := kbPreserveITPOs.Checked;

  // Advanced > Logging
  settings.clientMessageColor := cbClientColor.Selected ;
  settings.generalMessageColor := cbGeneralColor.Selected;
  settings.loadMessageColor := cbLoadColor.Selected;
  //settings.patchMessageColor := cbPatchColor.Selected;
  settings.pluginMessageColor := cbPluginColor.Selected;
  settings.errorMessageColor := cbErrorColor.Selected;
  settings.logMessageTemplate := meTemplate.Lines.Text;

  SaveSettings;
end;

procedure TOptionsForm.btnChangeSmashProfileClick(Sender: TObject);
begin
  ProgramStatus.bChangeProfile := true;
  btnOKClick(nil);
  Close;
end;

procedure TOptionsForm.appendBackslashOnExit(Sender: TObject);
var
  ed: TEdit;
begin
  // add trailing backslash if length > 0 and it is missing
  ed := TEdit(Sender);
  if Length(ed.Text) > 0 then
    ed.Text := AppendIfMissing(ed.Text, '\');
end;

procedure TOptionsForm.LoadLanguageOptions;
var
  info: TSearchRec;
  sLang: string;
begin
  cbLanguage.Items.Add('English');
  cbLanguage.ItemIndex := 0;
  if not DirectoryExists('lang') then
    exit;
  if FindFirst('lang\*.lang', faAnyFile, info) <> 0 then
    exit;
  repeat
    sLang := TitleCase(ChangeFileExt(info.Name, ''));
    if sLang <> 'English' then
      cbLanguage.Items.Add(sLang);
  until FindNext(info) <> 0;
  FindClose(info);
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  index: Integer;
begin
  // do translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);

  // prepare sample log message
  slSampleLogMessage := TStringList.Create;
  slSampleLogMessage.Values['Time'] := '12:34:56';
  slSampleLogMessage.Values['AppTime'] := '00:01:52';
  slSampleLogMessage.Values['Group'] := 'GENERAL';
  slSampleLogMessage.Values['Label'] := 'Test';
  slSampleLogMessage.Values['Text'] := 'This is a test message.';

  // General > Language
  LoadLanguageOptions;
  index := cbLanguage.Items.IndexOf(settings.language);
  if index > -1 then
    cbLanguage.ItemIndex := index;
  // General > Style
  kbSimpleDictionary.Checked := settings.simpleDictionaryView;
  kbSimplePlugins.Checked := settings.simplePluginsView;

  // Patching > General
  edPatchDirectory.Text := settings.patchDirectory;
  // Patching > Debug
  kbDebugPatchStatus.Checked := settings.debugPatchStatus;
  kbDebugMasters.Checked := settings.debugMasters;
  kbDebugArrays.Checked := settings.debugArrays;
  kbDebugSkips.Checked := settings.debugSkips;
  kbDebugTraversal.Checked := settings.debugTraversal;
  kbDebugTypes.Checked := settings.debugTypes;
  kbDebugChanges.Checked := settings.debugChanges;
  kbDebugSingle.Checked := settings.debugSingle;
  kbDebugLinks.Checked := settings.debugLinks;
  kbBuildRefs.Checked := settings.buildRefs;
  kbPreserveITPOs.Checked := settings.preserveITPOs;

  // Advanced > Profile
  lblCurrentProfileName.Caption := settings.profile;
  // Advanced > Logging
  cbClientColor.Selected := TColor(settings.clientMessageColor);
  cbGeneralColor.Selected := TColor(settings.generalMessageColor);
  cbLoadColor.Selected := TColor(settings.loadMessageColor);
  //cbPatchColor.Selected := TColor(settings.patchMessageColor);
  cbPluginColor.Selected := TColor(settings.pluginMessageColor);
  cbErrorColor.Selected := TColor(settings.errorMessageColor);
  meTemplate.Lines.Text := settings.logMessageTemplate;

  // set up browse buttons
  btnBrowsePatchDirectory.Flat := true;
  IconList.GetBitmap(0, btnBrowsePatchDirectory.Glyph);
end;

procedure TOptionsForm.meTemplateChange(Sender: TObject);
var
  template: string;
begin
  template := meTemplate.Lines.Text;
  lblSampleValue.Caption := ApplyTemplate(template, slSampleLogMessage);
end;

end.
