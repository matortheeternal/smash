unit mteLogger;

interface

uses Classes, SysUtils;

type
  TLogEvent = procedure(const group, &label, text: string) of object;

  TLogger = class
  private
    FLogEvent : TLogEvent;
  public
    procedure Write(const group, &label, text: string);
    property OnLogEvent: TLogEvent read FLogEvent write FLogEvent;
  end;

var Logger : TLogger;

implementation

procedure TLogger.Write(const group, &label, text: string);
begin
 if Assigned(FLogEvent) then
   FLogEvent(group, &label, text);
end;

initialization
 Logger := TLogger.Create;

finalization
 FreeAndNil(Logger);

end.
