unit mteTracker;

interface

uses Classes, SysUtils;

type
  TUpdateEvent = procedure(const i: integer) of object;
  TLogEvent = procedure(const s: string) of object;

  TProgressTracker = class
  private
    FUpdateEvent : TUpdateEvent;
    FSetEvent : TUpdateEvent;
    FMaxEvent : TUpdateEvent;
    FLogEvent : TLogEvent;
  public
    Cancel: boolean;
    procedure SetMax(const i: integer);
    property OnMaxEvent: TUpdateEvent read FMaxEvent write FMaxEvent;
    procedure SetProgress(const i: integer);
    property OnSetEvent: TUpdateEvent read FSetEvent write FSetEvent;
    procedure UpdateProgress(const i: integer);
    property OnProgressEvent: TUpdateEvent read FUpdateEvent write FUpdateEvent;
    procedure Write(const s: string);
    property OnLogEvent: TLogEvent read FLogEvent write FLogEvent;
  end;

var
  Tracker: TProgressTracker;

implementation


procedure TProgressTracker.SetMax(const i: Integer);
begin
  if Assigned(FMaxEvent) then
    FMaxEvent(i);
end;

procedure TProgressTracker.SetProgress(const i: integer);
begin
  if Assigned(FSetEvent) then
    FSetEvent(i);
end;

procedure TProgressTracker.UpdateProgress(const i: integer);
begin
  if Assigned(FUpdateEvent) then
    FUpdateEvent(i);
end;

procedure TProgressTracker.Write(const s: string);
begin
  if s = '' then
    exit;
  if Assigned(FLogEvent) then
    FLogEvent(s);
end;

initialization
  Tracker := TProgressTracker.Create;

finalization
  FreeAndNil(Tracker);

end.
