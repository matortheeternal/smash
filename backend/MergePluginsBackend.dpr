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

program MergePluginsBackend;

uses
  Forms,
  Dialogs,
  Controls,
  SysUtils,
  mpBackendForm in 'mpBackendForm.pas' {BackendForm},
  mpBackend in 'mpBackend.pas',
  mpDictionaryForm in 'mpDictionaryForm.pas' {DictionaryForm},
  mpOptionsForm in 'mpOptionsForm.pas' {OptionsForm},
  mpEditForm in 'mpEditForm.pas' {EditForm},
  mpLoginForm in 'mpLoginForm.pas' {LoginForm},
  mpUserForm in 'mpUserForm.pas' {UserForm};

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;


{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  ProgramPath: string;
begin
  // set important vars
  SysUtils.FormatSettings.DecimalSeparator := '.';
  Application.HintHidePause := 8000;
  ProgramPath := ExtractFilePath(ParamStr(0));

  // initialize application, load settings
  Application.Initialize;
  LoadSettings;
  LoadStatistics;

  // have user log in to database
  LoginForm := TLoginForm.Create(nil);
  if not (LoginForm.ShowModal = mrOk) then begin
    LoginForm.Free;
    exit;
  end;
  LoginForm.Free;

  // run main application
  Inc(statistics.timesRun);
  Application.Title := 'Merge Plugins Backend';
  Application.CreateForm(TBackendForm, BackendForm);
  Application.CreateForm(TDictionaryForm, DictionaryForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TUserForm, UserForm);
  Application.Run;
end.
