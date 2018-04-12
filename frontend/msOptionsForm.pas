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
        kbBuildRefs: TCheckBox;
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
      [FormSection('Integrations Tab')]
        IntegrationsTabSheet: TTabSheet;
        btnDetect: TButton;
        gbModOrganizer: TGroupBox;
        kbUsingMO: TCheckBox;
        lblModOrganizerPath: TLabel;
        edModOrganizerPath: TEdit;
        lblModOrganizerModsPath: TLabel;
        edModOrganizerModsPath: TEdit;
        lblModOrganizerInstanceName: TLabel;
        edModOrganizerInstanceName: TEdit;
        [FormSection('DontTranslate')]
          btnBrowseMO: TSpeedButton;
          btnBrowseMOMods: TSpeedButton;

    procedure FormCreate(Sender: TObject);
    procedure LoadLanguageOptions;
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowsePatchDirectoryClick(Sender: TObject);
    procedure btnBrowseMOClick(Sender: TObject);
    procedure kbUsingMOClick(Sender: TObject);
    procedure btnChangePatchProfileClick(Sender: TObject);
    procedure edUsernameChange(Sender: TObject);
    procedure searchForModOrganizer;
    procedure btnDetectClick(Sender: TObject);
    procedure meTemplateChange(Sender: TObject);
    procedure btnBrowseMOModsClick(Sender: TObject);
    procedure appendBackslashOnExit(Sender: TObject);
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

procedure TOptionsForm.btnBrowseMOClick(Sender: TObject);
begin
  BrowseForFolder(edModOrganizerPath, PathList.Values['ProgramPath']);
  if DirectoryExists(edModOrganizerPath.Text + 'mods\') then begin
    edModOrganizerModsPath.Text := edModOrganizerPath.Text + 'mods\';
    edPatchDirectory.Text := edModOrganizerPath.Text + 'mods\';
  end;
end;

procedure TOptionsForm.btnBrowseMOModsClick(Sender: TObject);
begin
  BrowseForFolder(edModOrganizerModsPath, PathList.Values['ProgramPath']);
end;

procedure TOptionsForm.btnDetectClick(Sender: TObject);
begin
  // search for mod organizer
  if kbUsingMo.Checked then
    searchForModOrganizer;
end;

procedure TOptionsForm.searchForModOrganizer;
const
  validModOrganizerFilenames: array[1..1] of string = ('ModOrganizer.exe');
  ignore: array[1..1] of string = ('data');
var
  DrivesArray: TStringDynArray;
  modOrganizerPath, sPaths, sDrive: string;
begin
  // search for installations in GamePath
  modOrganizerPath := RecursiveFileSearch(PathList.Values['GamePath'],
    validModOrganizerFilenames, ignore, 2);

  // search for installations in ?:\Program Files and ?:\Program Files (x86)
  DrivesArray := GetDriveList;
  for sDrive in DrivesArray do begin
    if not DriveReady(sDrive) then
      continue;
    if DirectoryExists(sDrive + 'Program Files') then
      sPaths := sPaths + sDrive + 'Program Files;';
    if DirectoryExists(sDrive + 'Program Files (x86)') then
      sPaths := sPaths + sDrive + 'Program Files (x86);';
  end;

  if (modOrganizerPath = '') then
    modOrganizerPath := FileSearch('Mod Organizer\ModOrganizer.exe', sPaths);

  // search each folder in each valid Program Files directory for ModOrganizer.exe
  if (modOrganizerPath = '') then
    modOrganizerPath := SearchPathsForFile(sPaths, 'ModOrganizer.exe');

  // if found, set TEdit captions, else alert user
  if (modOrganizerPath <> '') then begin
    edModOrganizerPath.Text := Copy(modOrganizerPath, 1, length(modOrganizerPath) - 16);
    if DirectoryExists(edModOrganizerPath.Text + 'mods\') then begin
      edModOrganizerModsPath.Text := edModOrganizerPath.Text + 'mods\';
      edPatchDirectory.Text := edModOrganizerPath.Text + 'mods\';
    end;
  end
  else begin
    MessageDlg(GetLanguageString('mpOpt_ModOrganizerNotFound'), mtConfirmation, [mbOk], 0);
    edModOrganizerPath.Text := '';
  end;
end;

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  // check if we need to update patch status afterwards
  ProgramStatus.bUpdatePatchStatus := (settings.usingMO <> kbUsingMO.Checked)
    or (settings.ManagerPath <> edModOrganizerPath.Text)
    or (settings.patchDirectory <> edPatchDirectory.Text);

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

  // Advanced > Logging
  settings.clientMessageColor := cbClientColor.Selected ;
  settings.generalMessageColor := cbGeneralColor.Selected;
  settings.loadMessageColor := cbLoadColor.Selected;
  //settings.patchMessageColor := cbPatchColor.Selected;
  settings.pluginMessageColor := cbPluginColor.Selected;
  settings.errorMessageColor := cbErrorColor.Selected;
  settings.logMessageTemplate := meTemplate.Lines.Text;

  // Integrations > Mod Organizer
  settings.usingMO := kbUsingMO.Checked;
  settings.ManagerPath := edModOrganizerPath.Text;
  settings.ModsPath := edModOrganizerModsPath.Text;
  settings.InstanceName := edModOrganizerInstanceName.Text;

  SaveSettings;
end;

procedure TOptionsForm.btnChangePatchProfileClick(Sender: TObject);
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

procedure TOptionsForm.edUsernameChange(Sender: TObject);
begin
  {if not TCPClient.Connected then begin
    lblStatusValue.Caption := GetString('mpOpt_ServerUnavailable');
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := GetString('mpOpt_ServerUnavailable');
    btnRegister.Enabled := false;
    exit;
  end;

  btnRegister.Caption := GetString('mpOpt_Check');
  if Length(edUsername.Text) < 4 then begin
    lblStatusValue.Caption := GetString('mpOpt_InvalidUsername');
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := GetString('mpOpt_UsernameTooShort');
    btnRegister.Enabled := false;
  end
  else if Length(edUsername.Text) > 24 then begin
    lblStatusValue.Caption := GetString('mpOpt_InvalidUsername');
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := GetString('mpOpt_UsernameTooLong');
    btnRegister.Enabled := false;
  end
  else begin
    lblStatusValue.Caption := GetString('mpOpt_ValidUsername');
    lblStatusValue.Font.Color := clBlack;
    lblStatusValue.Hint := GetString('mpOpt_ValidUsername');
    btnRegister.Enabled := true;
  end;  }
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

  // get status update if we can
  {if GetStatus then
    CompareStatuses; }

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

  // Integrations > Mod Organizer
  kbUsingMO.Checked := settings.usingMO;
  edModOrganizerPath.Text := settings.ManagerPath;
  edModOrganizerModsPath.Text := settings.ModsPath;
  edModOrganizerInstanceName.Text := settings.InstanceName;

  // disable controls if not using mod organizer
  kbUsingMOClick(nil);

  // set up browse buttons
  btnBrowsePatchDirectory.Flat := true;
  btnBrowseMO.Flat := true;
  btnBrowseMOMods.Flat := true;
  IconList.GetBitmap(0, btnBrowsePatchDirectory.Glyph);
  IconList.GetBitmap(0, btnBrowseMO.Glyph);
  IconList.GetBitmap(0, btnBrowseMOMods.Glyph);
end;

procedure TOptionsForm.kbUsingMOClick(Sender: TObject);
var
  b: boolean;
begin
  b := kbUsingMO.Checked;
  edModOrganizerPath.Enabled := b;
  edModOrganizerModsPath.Enabled := b;
  edModOrganizerInstanceName.Enabled := b;
  btnBrowseMO.Enabled := b;
  btnBrowseMOMods.Enabled := b;
end;

procedure TOptionsForm.meTemplateChange(Sender: TObject);
var
  template: string;
begin
  template := meTemplate.Lines.Text;
  lblSampleValue.Caption := ApplyTemplate(template, slSampleLogMessage);
end;

end.
