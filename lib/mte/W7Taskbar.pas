unit W7Taskbar;

interface

uses
  Forms, Types, Windows, SysUtils, ComObj, Controls, Graphics;

type
  TTaskBarProgressState = (tbpsNone, tbpsIndeterminate, tbpsNormal, tbpsError,
    tbpsPaused);

  function InitializeTaskbarAPI: boolean;
  function SetTaskbarProgressState(const AState: TTaskBarProgressState): boolean;
  function SetTaskbarProgressValue(const ACurrent: UInt64; const AMax: UInt64): boolean;
  function SetTaskbarOverlayIcon(const AIcon: THandle; const ADescription: String): boolean;

implementation

const
  TASKBAR_CID: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';
  TBPF_NOPROGRESS = 0;
  TBPF_INDETERMINATE = 1;
  TBPF_NORMAL = 2;
  TBPF_ERROR = 4;
  TBPF_PAUSED = 8;

type
  ITaskBarList3 = interface(IUnknown)
  ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function HrInit(): HRESULT; stdcall;
    function AddTab(hwnd: THandle): HRESULT; stdcall;
    function DeleteTab(hwnd: THandle): HRESULT; stdcall;
    function ActivateTab(hwnd: THandle): HRESULT; stdcall;
    function SetActiveAlt(hwnd: THandle): HRESULT; stdcall;
    function MarkFullscreenWindow(hwnd: THandle; fFullscreen: Boolean): HRESULT; stdcall;
    function SetProgressValue(hwnd: THandle; ullCompleted: UInt64; ullTotal: UInt64): HRESULT; stdcall;
    function SetProgressState(hwnd: THandle; tbpFlags: Cardinal): HRESULT; stdcall;
    function RegisterTab(hwnd: THandle; hwndMDI: THandle): HRESULT; stdcall;
    function UnregisterTab(hwndTab: THandle): HRESULT; stdcall;
    function SetTabOrder(hwndTab: THandle; hwndInsertBefore: THandle): HRESULT; stdcall;
    function SetTabActive(hwndTab: THandle; hwndMDI: THandle; tbatFlags: Cardinal): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: THandle; himl: THandle): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: THandle; hIcon: THandle; pszDescription: PChar): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: THandle; pszDescription: PChar): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: THandle; var prcClip: TRect): HRESULT; stdcall;
  end;

var
  TaskBarInterface: ITaskBarList3;

function InitializeTaskbarAPI: Boolean;
var
  Unknown: IInterface;
  Temp: ITaskBarList3;
begin
  // return true and exit if already initialized
  if Assigned(TaskBarInterface) then begin
    Result := True;
    Exit;
  end;

  // create COM object for taskbar CID
  try
    Unknown := CreateComObject(TASKBAR_CID);
    if Assigned(Unknown) then begin
      Temp := Unknown as ITaskBarList3;
      if Temp.HrInit() = S_OK then
        TaskBarInterface := Temp;
    end;
  except
    TaskBarInterface := nil;
  end;

  // if we got the interface, return true
  Result := Assigned(TaskBarInterface);
end;

{ Check to see if the API has been initialized }
function CheckAPI: boolean;
begin
  Result := Assigned(TaskBarInterface);
end;

function SetTaskbarProgressState(const AState: TTaskBarProgressState): boolean;
var
  Flag: Cardinal;
begin
  Result := False;

  // exit if api not initialized
  if not CheckAPI then
    exit;

  // check if state is valid, else use no progress
  case AState of
    tbpsIndeterminate: Flag := TBPF_INDETERMINATE;
    tbpsNormal: Flag := TBPF_NORMAL;
    tbpsError: Flag := TBPF_ERROR;
    tbpsPaused: Flag := TBPF_PAUSED;
  else
    Flag := TBPF_NOPROGRESS;
  end;

  // set progress state
  Result := TaskBarInterface.SetProgressState(Application.Handle, Flag) = S_OK;
end;

function SetTaskbarProgressValue(const ACurrent:UInt64; const AMax: UInt64): boolean;
begin
  Result := False;

  // exit if api not initialized
  if not CheckAPI then
    exit;

  // set progress value
  Result := TaskBarInterface.SetProgressValue(Application.Handle, ACurrent, AMax) = S_OK;
end;

function SetTaskbarOverlayIcon(const AIcon: THandle; const ADescription: String): boolean;
begin
  Result := False;

  // exit if api not initialized
  if not CheckAPI then
    exit;

  // set icon
  Result := TaskBarInterface.SetOverlayIcon(Application.Handle, AIcon, PWideChar(ADescription)) = S_OK;
end;

initialization
  TaskBarInterface := nil;

finalization
  TaskBarInterface := nil;

end.
