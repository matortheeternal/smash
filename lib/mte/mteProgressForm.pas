unit mteProgressForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mte components
  W7Taskbar, mteTracker;

type
  TProgressForm = class(TForm)
    DetailsMemo: TMemo;
    DetailsButton: TButton;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    CancelButton: TButton;
    procedure UpdateProgress(const i: Integer);
    procedure Write(const s: string);
    procedure SaveLog;
    procedure MaxProgress(const i: Integer);
    procedure SetProgress(const i: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToggleDetails(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    bDetailsVisible: boolean;
    LogPath: string;
  end;

implementation

var
  lastHeight: integer;

{$R *.dfm}

procedure TProgressForm.ToggleDetails(Sender: TObject);
begin
  bDetailsVisible := not bDetailsVisible;
  if bDetailsVisible then begin
    self.Height := lastHeight;
    DetailsMemo.Visible := true;
    DetailsButton.Caption := 'Hide details';
    DetailsMemo.Height := self.Height - 135;
  end
  else begin
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
  Tracker.OnMaxEvent := nil;
  Tracker.OnProgressEvent := nil;
  Tracker.OnLogEvent := nil;
  Tracker.OnSetEvent := nil;
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (fsModal in FormState);
  //Tracker.Write('CanClose = '+BoolToStr(CanClose, true));
  if not (CanClose or Tracker.Cancel) then begin
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
  Tracker.OnMaxEvent := MaxProgress;
  Tracker.OnProgressEvent := UpdateProgress;
  Tracker.OnLogEvent := Write;
  Tracker.OnSetEvent := SetProgress;
end;

procedure TProgressForm.FormShow(Sender: TObject);
begin
  if (fsModal in FormState) then
    CancelButton.Caption := 'Close';
end;

procedure TProgressForm.MaxProgress(const i: Integer);
begin
  ProgressBar.Max := i;
end;

procedure TProgressForm.SetProgress(const i: Integer);
begin
  ProgressBar.Position := i;
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
end;

procedure TProgressForm.UpdateProgress(const i: Integer);
begin
  ProgressBar.StepBy(i);
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
end;

procedure TProgressForm.SaveLog;
var
  fdt: string;
begin
  try
    ForceDirectories(LogPath);
    fdt := FormatDateTime('mmddyy_hhnnss', TDateTime(Now));
    DetailsMemo.Lines.SaveToFile(LogPath + 'log_'+fdt+'.txt');
  except on Exception do
    // nothing to do
  end;
end;

procedure TProgressForm.Write(const s: string);
begin
  if Pos(' ', s) <> 1 then
    ProgressLabel.Caption := s;
  DetailsMemo.SelLength := 0;
  DetailsMemo.Lines.Add(s);
end;

end.
