unit msOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ImgList, FileCtrl, ExtCtrls, Types,
  // mte units
  mteHelpers, RttiTranslation,
  // mp units
  msConfiguration, msClient;

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
        gbReports: TGroupBox;
        lblUsername: TLabel;
        edUsername: TEdit;
        btnRegister: TButton;
        lblStatus: TLabel;
        lblStatusValue: TLabel;
        btnReset: TButton;
        gbUpdating: TGroupBox;
        kbUpdateDictionary: TCheckBox;
        lblDictionaryStatus: TLabel;
        btnUpdateDictionary: TButton;
        kbUpdateProgram: TCheckBox;
        lblProgramStatus: TLabel;
        btnUpdateProgram: TButton;
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
        gbBackend: TGroupBox;
        lblHost: TLabel;
        lblPort: TLabel;
        kbNoStatistics: TCheckBox;
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
          edHost: TEdit;
          edPort: TEdit;
      [FormSection('Integrations Tab')]
        IntegrationsTabSheet: TTabSheet;
        btnDetect: TButton;
        gbModOrganizer: TGroupBox;
        kbUsingMO: TCheckBox;
        lblModOrganizerPath: TLabel;
        edModOrganizerPath: TEdit;
        lblModOrganizerModsPath: TLabel;
        edModOrganizerModsPath: TEdit;
        [FormSection('DontTranslate')]
          btnBrowseMO: TSpeedButton;
          btnBrowseMOMods: TSpeedButton;
    kbMergeRedundant: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure LoadLanguageOptions;
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowsePatchDirectoryClick(Sender: TObject);
    procedure btnBrowseMOClick(Sender: TObject);
    procedure kbUsingMOClick(Sender: TObject);
    procedure btnChangePatchProfileClick(Sender: TObject);
    procedure edUsernameChange(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUpdateDictionaryClick(Sender: TObject);
    procedure btnUpdateProgramClick(Sender: TObject);
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
  // General > Reports
  settings.username := edUsername.Text;
  // General > Style
  settings.simpleDictionaryView := kbSimpleDictionary.Checked;
  settings.simplePluginsView := kbSimplePlugins.Checked;
  // General > Updating
  settings.updateDictionary := kbUpdateDictionary.Checked;
  settings.updateProgram := kbUpdateProgram.Checked;

  // Patching > General
  settings.patchDirectory := edPatchDirectory.Text;
  settings.mergeRedundantPlugins := kbMergeRedundant.Checked;
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

  // Advanced > Backend
  settings.serverHost := edHost.Text;
  settings.serverPort := StrToInt(edPort.Text);
  settings.dontSendStatistics := kbNoStatistics.Checked;
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

  SaveSettings;
end;

procedure TOptionsForm.btnRegisterClick(Sender: TObject);
begin
  if not TCPClient.Connected then begin
    lblStatusValue.Caption := GetLanguageString('mpOpt_ServerUnavailable');
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := GetLanguageString('mpOpt_ServerUnavailable');
    btnRegister.Enabled := false;
    exit;
  end;

  if (btnRegister.Caption = GetLanguageString('mpOpt_Register')) then begin
    if RegisterUser(edUsername.Text) then begin
      settings.registered := true;
      settings.username := edUsername.Text;
      SaveSettings;
      lblStatusValue.Caption := GetLanguageString('mpOpt_Registered');
      lblStatusValue.Font.Color := clGreen;
      lblStatusValue.Hint := '';
      edUsername.Enabled := false;
      btnRegister.Enabled := false;
    end
    else begin
      lblStatusValue.Caption := GetLanguageString('mpOpt_FailedToRegister');
      lblStatusValue.Font.Color := clRed;
      lblStatusValue.Hint := GetLanguageString('mpOpt_FailedToRegister');
    end;
  end
  else begin
    if UsernameAvailable(edUsername.Text) then begin
      lblStatusValue.Caption := GetLanguageString('mpOpt_UsernameAvailable');
      lblStatusValue.Font.Color := clBlue;
      lblStatusValue.Hint := GetLanguageString('mpOpt_UsernameAvailable');
      btnRegister.Caption := GetLanguageString('mpOpt_Register');
    end
    else begin
      lblStatusValue.Caption := GetLanguageString('mpOpt_UsernameUnavailable');
      lblStatusValue.Font.Color := clRed;
      lblStatusValue.Hint := GetLanguageString('mpOpt_UsernameUnavailable');
    end;
  end;
end;

procedure TOptionsForm.btnResetClick(Sender: TObject);
begin
  if settings.registered and not ProgramStatus.bAuthorized then begin
    ResetAuth;
    CheckAuthorization;
    if ProgramStatus.bAuthorized then begin
      btnReset.Enabled := false;
      lblStatusValue.Caption := GetLanguageString('mpOpt_Registered');
      lblStatusValue.Font.Color := clGreen;
      lblStatusValue.Hint := '';
    end;
  end;
end;

procedure TOptionsForm.btnUpdateDictionaryClick(Sender: TObject);
begin
  if TCPClient.Connected then begin
    if UpdateDictionary then begin
      LocalStatus := TmsStatus.Create;
      CompareStatuses;
      btnUpdateDictionary.Enabled := false;
      lblDictionaryStatus.Caption := GetLanguageString('mpOpt_UpToDate');
      lblDictionaryStatus.Font.Color := clGreen;
    end;
  end;
end;

procedure TOptionsForm.btnUpdateProgramClick(Sender: TObject);
begin
  {if TCPClient.Connected then begin
    if ChangeLogPrompt(self) and DownloadProgram then begin
      bInstallUpdate := true;
      btnOKClick(nil);
      Close;
    end;
  end;}
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
  // General > reports
  edUsername.Text := settings.username;
  // General > Style
  kbSimpleDictionary.Checked := settings.simpleDictionaryView;
  kbSimplePlugins.Checked := settings.simplePluginsView;
  // General > Updating
  kbUpdateDictionary.Checked := settings.updateDictionary;
  kbUpdateProgram.Checked := settings.updateProgram;

  // Patching > General
  edPatchDirectory.Text := settings.patchDirectory;
  kbMergeRedundant.Checked := settings.mergeRedundantPlugins;
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

  // Advanced > Backend
  edHost.Text := settings.serverHost;
  edPort.Text := IntToStr(settings.serverPort);
  kbNoStatistics.Checked := settings.dontSendStatistics;
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

  // disable controls if not using mod organizer
  kbUsingMOClick(nil);

  // if already registered, lock registering controls
  edUsernameChange(nil);
  if settings.registered then begin
    edUsername.Enabled := false;
    btnRegister.Enabled := false;
    // if not authorized then enable reset button
    if TCPClient.Connected then begin
      if not ProgramStatus.bAuthorized then begin
        btnReset.Enabled := true;
        lblStatusValue.Caption := GetLanguageString('mpOpt_AuthFailed');
        lblStatusValue.Font.Color := clRed;
        lblStatusValue.Hint := GetLanguageString('mpOpt_AuthFailed');
      end
      else begin
        lblStatusValue.Caption := GetLanguageString('mpOpt_Registered');
        lblStatusValue.Font.Color := clGreen;
        lblStatusValue.Hint := '';
        ProgramStatus.bAuthorized := true;
      end;
    end;
  end;

  // dictionary update
  if ProgramStatus.bDictionaryUpdate then begin
    btnUpdateDictionary.Enabled := ProgramStatus.bDictionaryUpdate;
    lblDictionaryStatus.Caption := GetLanguageString('mpOpt_UpdateAvailable');
    lblDictionaryStatus.Font.Color := $000080FF;
  end;

  // program update
  if ProgramStatus.bProgramUpdate then begin
    btnUpdateProgram.Enabled := ProgramStatus.bProgramUpdate;
    lblProgramStatus.Caption := GetLanguageString('mpOpt_UpdateAvailable');
    lblProgramStatus.Hint := Format(GetLanguageString('mpOpt_VersionCompare'),
      [LocalStatus.ProgramVersion, RemoteStatus.ProgramVersion]);
    lblProgramStatus.Font.Color := $000080FF;
  end;

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
