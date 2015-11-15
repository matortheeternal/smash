unit mteTracker;

interface

uses Classes, SysUtils;

type
  TUpdateEvent = procedure(const i: integer) of object;
  TReadEvent = function: Integer of object;
  TLogEvent = procedure(const s: string) of object;

  TProgressTracker = class
  private
    FUpdateEvent : TUpdateEvent;
    FSetEvent : TUpdateEvent;
    FGetEvent : TReadEvent;
    FMaxEvent : TUpdateEvent;
    FGetMaxEvent : TReadEvent;
    FLogEvent : TLogEvent;
    FStatusEvent : TLogEvent;
  public
    Cancel: boolean;
    procedure SetMaxProgress(const i: integer);
    property OnSetMaxEvent: TUpdateEvent read FMaxEvent write FMaxEvent;
    procedure SetProgress(const i: integer);
    property OnSetEvent: TUpdateEvent read FSetEvent write FSetEvent;
    function GetProgress: Integer;
    property OnGetEvent: TReadEvent read FGetEvent write FGetEvent;
    function GetMaxProgress: Integer;
    property OnGetMaxEvent: TReadEvent read FGetMaxEvent write FGetMaxEvent;
    procedure UpdateProgress(const i: integer);
    property OnUpdateEvent: TUpdateEvent read FUpdateEvent write FUpdateEvent;
    procedure StatusMessage(const s: string);
    property OnStatusEvent: TLogEvent read FStatusEvent write FStatusEvent;
    procedure Write(const s: string);
    property OnLogEvent: TLogEvent read FLogEvent write FLogEvent;
  end;

var
  Tracker: TProgressTracker;

implementation


procedure TProgressTracker.SetMaxProgress(const i: Integer);
begin
  if Assigned(FMaxEvent) then
    FMaxEvent(i);
end;

procedure TProgressTracker.StatusMessage(const s: string);
begin
  if Assigned(FStatusEvent) then
    FStatusEvent(s);
end;

procedure TProgressTracker.SetProgress(const i: integer);
begin
  if Assigned(FSetEvent) then
    FSetEvent(i);
end;

function TProgressTracker.GetProgress: Integer;
begin
  Result := FGetEvent();
end;

function TProgressTracker.GetMaxProgress: Integer;
begin
  Result := FGetMaxEvent();
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
