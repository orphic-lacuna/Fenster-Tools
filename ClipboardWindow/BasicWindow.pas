unit BasicWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtendedThreads, windows, ExtendedWinAPI, DwmApi;

type

  { TBasicWindow }

  TBasicWindow = class(TThreadEx)
  private
    fHandle: HWND;

    fWndClass: TWNDCLASSW;

    procedure RegisterWndClass;
    function EnableGlassEffect: Boolean;
    function WindowProc(var Msg: TMsg): Integer;
  protected
    function CreateWindow: Boolean; virtual;
    procedure DestroyWindow; virtual;
    procedure ThreadInternalCreate; override;
    procedure ThreadInternalDestroy; override;
  public
    property Handle: HWND read fHandle;

    procedure SetBounds(aLeft, aTop, aWidth, aHeight: Integer); virtual;

    procedure Show; virtual;
    procedure Hide; virtual;
  end;

implementation

uses ToolBox;

const
  WND_CLASS_NAME = 'FensterTools WndBaseClass'#0#0;

{ TBasicWindow }

procedure TBasicWindow.ThreadInternalCreate;
begin
  inherited ThreadInternalCreate;
  RegisterWndClass;
  CreateWindow;
end;

procedure TBasicWindow.RegisterWndClass;
var
  tmpClass: TWNDCLASSW;
  alreadyRegistered: Boolean;
begin
  alreadyRegistered := GetClassInfoW(HInstance, WND_CLASS_NAME, @tmpClass{%H-});
  if not alreadyRegistered then
  begin
    fWndClass.lpszClassName := PWideChar(UTF8Decode(WND_CLASS_NAME));
    fWndClass.lpfnWndProc   := @Toolbox.ThreadSafeWindowProc;
    fWndClass.Style         := CS_VREDRAW or CS_HREDRAW;
    fWndClass.hInstance     := hInstance;
    fWndClass.hIcon         := LoadIcon(0, IDI_APPLICATION);
    fWndClass.hCursor       := LoadCursor(0, IDC_ARROW);
    fWndClass.hbrBackground := (COLOR_WINDOW + 1);
    fWndClass.lpszMenuName  := nil;
    fWndClass.cbClsExtra    := 0;
    fWndClass.cbWndExtra    := SizeOf(TMethod);
    RegisterClassW(fWndClass);
  end;
end;

function TBasicWindow.CreateWindow: Boolean;
begin
  Result := false;

  // Layered und Klicktransparenz (WS_EX_TRANSPARENT) und kein Rahmen (WS_POPUP)
  fHandle := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST, WND_CLASS_NAME, '', WS_POPUP, 0, 0, 0, 0, 0, 0, hInstance, nil);
  if fHandle <> 0 then
  begin
    SetObjectWndProc(fHandle, TMethod(@Self.WindowProc));
    // Topmost
    SetWindowPos(fHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
    Result := true;

    EnableGlassEffect;
  end;
end;

procedure TBasicWindow.DestroyWindow;
begin
  if fHandle <> 0 then
  begin
    Windows.DestroyWindow(fHandle);
    fHandle := 0;
  end;
  // WndClass should not be unregistered because there could be other windows of this classtype !
  // windows automatically unregisters a window class on application termination
  if WindowsVersion.IsWinVistaOrGreater and not WindowsVersion.IsWin8OrGreater then FreeDwmLibrary;
end;

function TBasicWindow.EnableGlassEffect: Boolean;
var
  a: AccentPolicy;
  wcdata: WindowCompositionAttributeData;
  bb: DWM_BLURBEHIND;
  r: TRect;
begin
  Result := false;
  if WindowsVersion.IsWin10OrGreater then
  begin
    ZeroMemory(@a, SizeOf(a));
    a.AccentState := ACCENT_ENABLE_BLURBEHIND; a.AccentFlags := ACCENT_FLAG_DRAWALLBORDERS;
    wcdata.Data := PtrInt(@a); wcdata.SizeOfData := SizeOf(a); wcdata.Attribute := WCA_ACCENT_POLICY;
    Result := SetWindowCompositionAttribute(fHandle, @wcdata);
  end else if WindowsVersion.IsWinVistaOrGreater and not WindowsVersion.IsWin8OrGreater then begin
    if InitDwmLibrary then
    begin
      bb.dwFlags := DWM_BB_ENABLE or DWM_BB_TRANSITIONONMAXIMIZED or DWM_BB_BLURREGION;
      GetWindowRect(fHandle, r);
      bb.fEnable := true; bb.fTransitionOnMaximized := false; bb.hRgnBlur := CreateRoundRectRgn(0, 0, r.Right-r.Left, r.Bottom-r.Top, 10, 10);
      DwmEnableBlurBehindWindow(fHandle, @bb);
      DeleteObject(bb.hRgnBlur);
      Result := true;
    end;
  end;
end;

function TBasicWindow.WindowProc(var Msg: TMsg): Integer;
begin
  case Msg.message of
    WM_TIMER:;
  //WM_ERASEBKGND: ;//Result := 1;
    else Result := DefWindowProc(Msg.hwnd, Msg.Message, Msg.wParam, Msg.LParam);
  end;
end;

procedure TBasicWindow.Show;
begin
  ShowWindow(fHandle, SW_SHOW);
end;

procedure TBasicWindow.Hide;
begin
  ShowWindow(fHandle, SW_HIDE);
end;

procedure TBasicWindow.ThreadInternalDestroy;
begin
  DestroyWindow;
  inherited ThreadInternalDestroy;
end;

procedure TBasicWindow.SetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  if fHandle <> 0 then
  begin
    SetWindowPos(fHandle, 0, aLeft, aTop, aWidth, aHeight, SWP_NOZORDER);
    // if we have something older than windows 10, we need to reenable glass effect, since glass effect region is window size dependent
    if not WindowsVersion.IsWin10OrGreater then EnableGlassEffect;
  end;
end;

end.

