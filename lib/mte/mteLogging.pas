{
  mteLogging
  created by matortheeternal

  This unit contains the TFilter and TLogMessage types which offer filterable
  logging for Delphi VCL applications.  Log messages have a group and a label,
  and filters can apply to groups or labels.
}

unit mteLogging;

interface

uses
  Classes, SysUtils, Forms,
  // mte units
  mteProgressForm, mteHelpers;

type
  TFilter = class(TObject)
  public
    group: string;
    &label: string;
    enabled: boolean;
    constructor Create(group: string; enabled: boolean); Overload;
    constructor Create(group, &label: string; enabled: boolean); Overload;
  end;
  TLogMessage = class (TObject)
  public
    time: string;
    appTime: string;
    group: string;
    &label: string;
    text: string;
    constructor Create(time, appTime, group, &label, text: string); Overload;
  end;

  { Log methods }
  procedure RebuildLog;
  procedure SaveLog(var Log: TList);
  function MessageEnabled(msg: TLogMessage): boolean;
  procedure ShowProgressForm(parent: TForm; var pf: TProgressForm;
    sCaption, sLogSubPath: string);

var
  BaseLog, Log, LabelFilters, GroupFilters: TList;
  LogPath: string;
  TimeCosts: TStringList;
  AppStartTime: TDateTime;

implementation

{ TFilter }
constructor TFilter.Create(group: string; enabled: boolean);
begin
  self.group := group;
  self.enabled := enabled;
end;

constructor TFilter.Create(group, &label: string; enabled: boolean);
begin
  self.group := group;
  self.&label := &label;
  self.enabled := enabled;
end;

{ TLogMessage }
constructor TLogMessage.Create(time, appTime, group, &label, text: string);
begin
  self.time := time;
  self.appTime := appTime;
  self.group := group;
  self.&label := &label;
  self.text := text;
end;

{******************************************************************************}
{ Log methods
  Set of methods for logging

  List of methods:
  - InitLog
  - RebuildLog
  - SaveLog
  - MessageGroupEnabled
}
{******************************************************************************}

procedure RebuildLog;
var
  i: Integer;
  msg: TLogMessage;
begin
  Log.Clear;
  for i := 0 to Pred(BaseLog.Count) do begin
    msg := TLogMessage(BaseLog[i]);
    if MessageEnabled(msg) then
      Log.Add(msg);
  end;
end;

procedure SaveLog(var Log: TList);
var
  sl: TStringList;
  i: Integer;
  msg: TLogMessage;
  fdt: string;
begin
  sl := TStringList.Create;
  for i := 0 to Pred(Log.Count) do begin
    msg := TLogMessage(Log[i]);
    sl.Add(Format('[%s] (%s) %s: %s', [msg.time, msg.group, msg.&label, msg.text]));
  end;
  fdt := FormatDateTime('mmddyy_hhnnss', TDateTime(Now));
  ForceDirectories(LogPath+'main\');
  sl.SaveToFile(LogPath+'main\log_'+fdt+'.txt');
  sl.Free;
end;

function GetGroupFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(GroupFilters.Count) do begin
    filter := TFilter(GroupFilters[i]);
    if filter.group = msg.group then begin
      Result := filter;
      exit;
    end;
  end;
end;

function GetLabelFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(LabelFilters.Count) do begin
    filter := TFilter(LabelFilters[i]);
    if (filter.&label = msg.&label) and (filter.group = msg.group) then begin
      Result := filter;
      exit;
    end;
  end;
end;

function MessageEnabled(msg: TLogMessage): boolean;
var
  GroupFilter, LabelFilter: TFilter;
begin
  Result := true;
  GroupFilter := GetGroupFilter(msg);
  LabelFilter := GetLabelFilter(msg);
  if GroupFilter <> nil then
    Result := Result and GroupFilter.enabled;
  if LabelFilter <> nil then
    Result := Result and LabelFilter.enabled;
end;

procedure ShowProgressForm(parent: TForm; var pf: TProgressForm;
  sCaption, sLogSubPath: string);
begin
  pf := TProgressForm.Create(parent);
  pf.pfLogPath := LogPath + sLogSubPath + '\';
  pf.PopupParent := parent;
  pf.Caption := sCaption;
  pf.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pf.Show;
end;

initialization
begin
  BaseLog := TList.Create;
  Log := TList.Create;
  LabelFilters := TList.Create;
  GroupFilters := TList.Create;
end;

finalization
begin
  FreeList(BaseLog);
  Log.Free;
end;

end.
