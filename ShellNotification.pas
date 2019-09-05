unit ShellNotification;

interface

{$Mode objfpc}{$H+}

uses Windows, Classes, Sysutils{$IfDef Logging}, Logging{$EndIf};

type
  TOnWindowCreation = procedure(Window: HWND) of object;
type
  TShellNotification = class
  private
    FOnWindowCreation: TOnWindowCreation;
  public
    property OnWindowCreation: TOnWindowCreation read FOnWindowCreation write FOnWindowCreation;

    constructor Create(OnWndCreation: TOnWindowCreation = nil);
    destructor Destroy; override;
  end;

function RegisterShellHookWindow(hWnd: HWND): Boolean; stdcall; external 'user32.dll';
function DeregisterShellHookWindow(hWnd: HWND): Boolean; stdcall; external 'user32.dll';

implementation

const
  ShellHookMsgName = 'SHELLHOOK'; // dieser Name muss so sein, sonst werden keine Msg's empfangen.
  ShellHookWndClass = 'FT-SHELLHOOK-MSG-RECEIVER-CLASS';
var
  ShellHookWindow: HWND;
  ShellHookMsg: Cardinal;
  InstanceList: TList;
  {$IfDef Debug}LastThread: Cardinal = 0;{$EndIf}

function WndProc(Window: HWND; Msg: UINT; WParam: WParam; LParam: LParam): LResult; stdcall;
var
  i: Integer;
  sn: TShellNotification;
begin
  if Msg = ShellHookMsg then
  begin
    Result := 1;
    if WParam = HSHELL_WINDOWCREATED then
    begin
      for i := 0 to InstanceList.Count-1 do
      begin
        sn := TShellNotification(InstanceList.Items[i]);
        if Assigned(sn.OnWindowCreation) then sn.OnWindowCreation(LParam);
      end;
    end;
  end else Result := DefWindowProc(Window, Msg, WParam, LParam);
end;

function AllocateHWnd_: HWND;
var
  utilWindowClass  : TWndClass;
begin
  FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
  utilWindowClass.lpszClassName := ShellHookWndClass;
  utilWindowClass.hInstance := HInstance;
  utilWindowClass.lpfnWndProc := @WndProc;
  Windows.RegisterClass(utilWindowClass);
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, ShellHookWndClass, '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure DeallocateHWnd_(wnd: HWND);
begin
  DestroyWindow(wnd);
  Windows.UnregisterClass(ShellHookWndClass, HInstance);
end;

constructor TShellNotification.Create(OnWndCreation: TOnWindowCreation = nil);
begin
  {$IfDef Debug}
  if (LastThread <> 0) and (GetCurrentThreadId <> LastThread) then raise Exception.Create('TShellNotification may only be used in one thred (called ->Create).');
  LastThread := GetCurrentThreadId;
  {$EndIf}
  if (InstanceList = nil) or (InstanceList.Count <= 0) then
  begin
    InstanceList := TList.Create;
    ShellHookMsg := RegisterWindowMessage(ShellHookMsgName);
    ShellHookWindow := AllocateHWnd_;
    RegisterShellHookWindow(ShellHookWindow);
  end;
  FOnWindowCreation := OnWndCreation;
  InstanceList.Add(Self);
  {$IfDef Logging}Logger.Add('ShellNotification-Object created.');{$EndIf}
end;

destructor TShellNotification.Destroy;
begin
  {$IfDef Debug}
  if (LastThread <> 0) and (GetCurrentThreadId <> LastThread) then raise Exception.Create('TShellNotification may only be used in one thred (called ->Destroy).');
  LastThread := GetCurrentThreadId;
  {$EndIf}
  InstanceList.Delete(InstanceList.IndexOf(Self));
  if InstanceList.Count <= 0 then
  begin
    DeregisterShellHookWindow(ShellHookWindow);
    DeallocateHWnd_(ShellHookWindow);
    FreeAndNil(InstanceList);
  end;
  {$IfDef Logging}Logger.Add('ShellNotification-Object destroyed.');{$EndIf}
end;

end.
