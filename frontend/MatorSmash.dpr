{*******************************************************************************

     The contents of this file are subject to the Mozilla Public License
     Version 1.1 (the "License"); you may not use this file except in
     compliance with the License. You may obtain a copy of the License at
     http://www.mozilla.org/MPL/

     Software distributed under the License is distributed on an "AS IS"
     basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
     License for the specific language governing rights and limitations
     under the License.

*******************************************************************************}

program MatorSmash;

uses
  Forms,
  Dialogs,
  Controls,
  SysUtils,
  mteHelpers,
  msProfileForm in 'msProfileForm.pas' {ProfileForm},
  msProfilePanel in 'msProfilePanel.pas',
  msSmashForm in 'msSmashForm.pas' {SmashForm},
  msFrontend in 'msFrontend.pas',
  msThreads in 'msThreads.pas',
  msOptionsForm in 'msOptionsForm.pas' {OptionsForm},
  msSplashForm in 'msSplashForm.pas' {SplashForm},
  msEditForm in 'msEditForm.pas' {EditForm},
  msSettingsManager in 'msSettingsManager.pas' {SettingsManager},
  msPluginSelectionForm in 'msPluginSelectionForm.pas' {PluginSelectionForm},
  msConflictForm in 'msConflictForm.pas' {ConflictForm},
  msChoicePanel in 'msChoicePanel.pas',
  msSmash in 'msSmash.pas',
  msAlgorithm in 'msAlgorithm.pas';

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;


{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  bProfileProvided: boolean;
  ProgramPath: string;
begin
  // set important vars
  SysUtils.FormatSettings.DecimalSeparator := '.';
  Application.HintHidePause := 8000;
  //ReportMemoryLeaksOnShutdown := true;
  ProgramPath := ExtractFilePath(ParamStr(0));

  // get command line arguments
  bProfileProvided := FindCmdLineSwitch('profile');

  // initialize application, load settings
  Application.Initialize;
  ForceDirectories(ProgramPath + 'profiles');

  // have user select game mode
  if not bProfileProvided then begin
    ProfileForm := TProfileForm.Create(nil);
    if not (ProfileForm.ShowModal = mrOk) then
      exit;
    ProfileForm.Free;
  end;

  // run main application
  Application.Title := 'Mator Smash';
  Application.CreateForm(TSmashForm, SmashForm);
  Application.CreateForm(TProfileForm, ProfileForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TSettingsManager, SettingsManager);
  Application.CreateForm(TPluginSelectionForm, PluginSelectionForm);
  Application.CreateForm(TConflictForm, ConflictForm);
  Application.Run;
end.
