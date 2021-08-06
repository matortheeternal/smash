unit mteProgressForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mte components
  W7Taskbar, mteTracker,
  // third party
  LoggerPro;

type
  TProgressForm = class(TForm)
    DetailsMemo: TMemo;
    DetailsButton: TButton;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    CancelButton: TButton;
    procedure UpdateProgress(const i: Integer);
    procedure StatusMessage(const s: string);
    procedure Write(const s: string);
    procedure SetProgress(const i: Integer);
    procedure SetMaxProgress(const i: Integer);
    function GetProgress: Integer;
    function GetMaxProgress: Integer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToggleDetails(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FLog: ILogWriter;
  public
    bDetailsVisible: Boolean;
  end;

implementation

uses
  {$IFDEF DEBUG}
  // Only useful for us in debug builds?
  LoggerPro.OutputDebugStringAppender,
  {$ENDIF}
  LoggerPro.VCLMemoAppender,
  LoggerPro.FileAppender;

var
  lastHeight: Integer;

{$R *.dfm}

procedure TProgressForm.ToggleDetails(Sender: TObject);
begin
  bDetailsVisible := not bDetailsVisible;
  if bDetailsVisible then
  begin
    self.Height := lastHeight;
    DetailsMemo.Visible := true;
    DetailsButton.Caption := 'Hide details';
    DetailsMemo.Height := self.Height - 135;
  end
  else
  begin
    DetailsMemo.Visible := false;
    DetailsButton.Caption := 'Show details';
    lastHeight := self.Height;
    self.Height := 129;
  end;
end;

procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetTaskbarProgressState(tbpsNone);
  Tracker.OnSetMaxEvent := nil;
  Tracker.OnUpdateEvent := nil;
  Tracker.OnLogEvent := nil;
  Tracker.OnSetEvent := nil;
  Tracker.OnGetEvent := nil;
  Tracker.OnGetMaxEvent := nil;
  Tracker.OnStatusEvent := nil;
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (fsModal in FormState);
  // Tracker.Write('CanClose = '+BoolToStr(CanClose, true));
  if not(CanClose or Tracker.Cancel) then
  begin
    Tracker.Write('Cancelling...');
    SetTaskbarProgressState(tbpsError);
    Tracker.Cancel := true;
  end;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  lastHeight := 380;
  SetTaskbarProgressState(tbpsNormal);
  bDetailsVisible := true;
  DetailsMemo.ReadOnly := true;
  Tracker.OnSetMaxEvent := SetMaxProgress;
  Tracker.OnUpdateEvent := UpdateProgress;
  Tracker.OnLogEvent := Write;
  Tracker.OnSetEvent := SetProgress;
  Tracker.OnStatusEvent := StatusMessage;
  Tracker.OnGetEvent := GetProgress;
  Tracker.OnGetMaxEvent := GetMaxProgress;
  FLog := BuildLogWriter([
    {$IFDEF DEBUG}
    // Only useful for us in debug builds?
    TLoggerProOutputDebugStringAppender.Create(),
    {$ENDIF}
    TLoggerProFileAppender.Create(),
    TVCLMemoLogAppender.Create(DetailsMemo)
  ]);
end;

procedure TProgressForm.FormShow(Sender: TObject);
begin
  if (fsModal in FormState) then
  begin
    CancelButton.Caption := 'Close';
    if not bDetailsVisible then
      ToggleDetails(nil);
  end
  else if not bDetailsVisible then
  begin
    bDetailsVisible := false;
    DetailsMemo.Visible := false;
    DetailsButton.Caption := 'Show details';
    lastHeight := self.Height;
    self.Height := 129;
  end;
end;

procedure TProgressForm.SetProgress(const i: Integer);
begin
  ProgressBar.Position := i;
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
end;

procedure TProgressForm.SetMaxProgress(const i: Integer);
begin
  ProgressBar.Max := i;
end;

function TProgressForm.GetProgress: Integer;
begin
  Result := ProgressBar.Position;
end;

function TProgressForm.GetMaxProgress: Integer;
begin
  Result := ProgressBar.Max;
end;

procedure TProgressForm.UpdateProgress(const i: Integer);
begin
  ProgressBar.StepBy(i);
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
end;

procedure TProgressForm.StatusMessage(const s: string);
begin
  ProgressLabel.Caption := s;
end;

procedure TProgressForm.Write(const s: string);
begin
  if Pos(' ', s) <> 1 then
    StatusMessage(s);
  FLog.Info(s, Tracker.Tag);
end;

end.
