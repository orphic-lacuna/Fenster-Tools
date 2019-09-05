unit LivePatching;

interface

{$mode objfpc}{$H+}
{$IfDef Debug}{$IfDef Logging}
  {$Define PatcherLogging}
{$EndIf}{$EndIf}

uses Windows, Classes, Sysutils, Messages, ShellNotification, FensterToolsCommon, ToolBox, ExtendedWinAPI{$IfDef Logging}, Logging, TypInfo{$EndIf}, HelperSvcDefinitions;

  {$packset 1}
  {$packenum 1}
type
  // Don't change the order, this is a bitmask !!!
  TPatchTypes = set of (ptGetClipboardData, ptOpenClipboardARFIX, ptSetWindowsHookEx, ptUnused3, ptUnused4, ptUnused5, ptUnused6, ptUnused7);

  {$packset DEFAULT}
  {$packenum DEFAULT}

const
  WM_SETWINDOWSHOOKEX = WM_USER + 354;

type
  TPatcher = class // this class is not thread-safe
  private
    FCallbackMsg_PasteNotification, FCallbackMsg_SetWindowsHookEx: Cardinal;
    FCallbackWnd_PasteNotifcation, FCallbackWnd_SetWindowsHookEx: HWND;

    FActivePatches: TPatchTypes;
    //PatchHelper64: TPatchHelper64;

    FActive: Boolean;

    shell_notification: TShellNotification;
    procedure PatchWindow(Window: HWND);
    procedure UnpatchWindow(Window: HWND);
    //procedure SetPatchEnabledARFIX(Value: Boolean);
    procedure SetPatchEnabledGetClipboardData(Value: Boolean);
    procedure SetPatchEnabledSetWindowsHookEx(Value: Boolean);
    //function GetPatchEnabledARFIX: boolean;
    function GetPatchEnabledGetClipboardData: boolean;
    function GetPatchEnabledSetWindowsHookEx: boolean;

    //procedure SetCallbackWnd_PasteNotifcation(Value: HWND);

    procedure SetActive(Value: boolean);
  public
    //property PatchEnabledARFIX: boolean read GetPatchEnabledARFIX write SetPatchEnabledARFIX;//ActivePatches: TPatchTypes read FActivePatches write SetActivePatches;
    property PatchEnabledGetClipboardData: boolean read GetPatchEnabledGetClipboardData write SetPatchEnabledGetClipboardData;
    property PatchEnabledSetWindowsHookEx: boolean read GetPatchEnabledSetWindowsHookEx write SetPatchEnabledSetWindowsHookEx;
    property Active: boolean read FActive write SetActive;

    property PasteNotificationMsg: Cardinal read FCallbackMsg_PasteNotification;// write FCallbackMsg_PasteNotification;
    property SetWindowsHookExMsg: Cardinal read FCallbackMsg_SetWindowsHookEx;// write FCallbackMsg_SetWindowsHookEx;
    property CallbackWnd_PasteNotifcation: HWND read FCallbackWnd_PasteNotifcation write FCallbackWnd_PasteNotifcation;
    property CallbackWnd_SetWindowsHookEx: HWND read FCallbackWnd_SetWindowsHookEx write FCallbackWnd_SetWindowsHookEx;

    procedure PatchAllProcesses;
    procedure UnpatchAllProcesses;
    constructor Create();
    destructor Destroy; override;
  end;

var
  Patcher: TPatcher;

implementation

uses ServiceConnection;

const
  PasteNotificationMsgName = 'FT-PASTE-NOTIFICATION-MSG';

{$Region 'TPatcher'}
constructor TPatcher.Create();
begin
  {$IfDef Debug}if GetCurrentThreadID <> MainThreadID then raise Exception.Create('TPatcher wurde nicht im MainThread erzeugt.');{$EndIf}
  FCallbackWnd_PasteNotifcation := 0; FCallbackWnd_SetWindowsHookEx := 0;
  FCallbackMsg_SetWindowsHookEx := WM_SETWINDOWSHOOKEX;
//  if IsVista_7 then ChangeWindowMessageFilterEx(FOwner, FCallbackMsg, MSGFLT_ALLOW);

  FActivePatches := [];
  FActive := false;
end;

procedure TPatcher.SetActive(Value: boolean);
begin
  {$IfDef Debug}if GetCurrentThreadID <> MainThreadID then raise Exception.Create('Patcher.SetActive wurde auﬂerhalb des MainThread aufgerufen.');{$EndIf}
  FActive := Value;
  if FActive then
  begin
    PatchAllProcesses;
    {$IfDef Logging}Logger.Add('Patcher is active');{$EndIf}
  end;
end;
        {
procedure TPatcher.SetPatchEnabledARFIX(Value: Boolean);
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('Zugriff auf Patcher darf nur im MainThread erfolgen.');{$EndIf}
  //if Value = PatchEnabledARFIX then exit; // Warum das raus
  if Value then
  begin
    FActivePatches := FActivePatches + [ptOpenClipboardARFIX];
    if FActive then
    begin
      PatchAllProcesses;
      {$IfDef Logging}Logger.Add('Patched/Verified OpenClipboard');{$EndIf}
    end else
    begin
      {$IfDef Logging}Logger.Add('Added OpenClipboard to patch list');{$EndIf}
    end;
  end else
  begin
    FActivePatches := FActivePatches - [ptOpenClipboardARFIX];
    if FActive then UnpatchAllProcesses;
    {$IfDef Logging}Logger.Add('Unpatched OpenClipboard');{$EndIf}
  end;
end;  }

procedure TPatcher.SetPatchEnabledGetClipboardData(Value: Boolean);
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('Zugriff auf Patcher darf nur im MainThread erfolgen.');{$EndIf}
  //if Value = PatchEnabledGetClipboardData then exit; // commented because if FThreadWindow (receiver Window for WM_PASTE from patched process) has changed, repatching is necessary
  if Value then
  begin
    FCallbackMsg_PasteNotification := RegisterWindowMessage(PasteNotificationMsgName);
    // falls FensterTools mit Admin-Rechten l‰uft, muss man die Fenster-Nachrichten-Firewall so einstellen, dass diese Nachrichten von nicht-Admin-Prozessen empfangen wird
    ChangeWindowMessageFilterEx(FCallbackWnd_PasteNotifcation, FCallbackMsg_PasteNotification, MSGFLT_ALLOW, nil);
    FActivePatches := FActivePatches + [ptGetClipboardData];
    if FActive then
    begin
      PatchAllProcesses;
      {$IfDef Logging}Logger.Add('Patched/Verified GetClipboardData');{$EndIf}
    end else
    begin
      {$IfDef Logging}Logger.Add('Added GetClipboardData to patch list');{$EndIf}
    end;
  end else
  begin
    FActivePatches := FActivePatches - [ptGetClipboardData];
    if FActive then UnpatchAllProcesses;
    {$IfDef Logging}Logger.Add('Unpatched GetClipboardData');{$EndIf}
  end;
end;

procedure TPatcher.SetPatchEnabledSetWindowsHookEx(Value: Boolean);
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('Zugriff auf Patcher darf nur im MainThread erfolgen.');{$EndIf}
  //if Value = PatchEnabledSetWindowsHookEx then exit; // Warum das raus
  if Value then
  begin
    FActivePatches := FActivePatches + [ptSetWindowsHookEx];
    if FActive then
    begin
      PatchAllProcesses;
      {$IfDef Logging}Logger.Add('Patched/Verified SetWindowsHookEx');{$EndIf}
    end else
    begin
      {$IfDef Logging}Logger.Add('Added SetWindowsHookEx to patch list');{$EndIf}
    end;
  end else
  begin
    FActivePatches := FActivePatches - [ptSetWindowsHookEx];
    if FActive then UnpatchAllProcesses;
    {$IfDef Logging}Logger.Add('Unpatched SetWindowsHookEx');{$EndIf}
  end;
end;

{function TPatcher.GetPatchEnabledARFIX: boolean;
begin
  Result := ptOpenClipboardARFIX in FActivePatches;
end;}

function TPatcher.GetPatchEnabledGetClipboardData: boolean;
begin
  Result := ptGetClipboardData in FActivePatches;
end;

function TPatcher.GetPatchEnabledSetWindowsHookEx: boolean;
begin
  Result := ptSetWindowsHookEx in FActivePatches;
end;

procedure TPatcher.PatchWindow(Window: HWND);
var
  pid, parent_pid: Cardinal;
  mod_name: String;
begin
  if not IsWindowVisible(Window) then exit;

  GetWindowThreadProcessId(Window, pid{%H-});
  if (pid = 0) or (pid = GetCurrentProcessId) then exit;

  mod_name := GetModuleFileName(pid, @parent_pid);
  if (mod_name = 'sidebar.exe') or (mod_name = 'SndVol.exe') or (lowercase(mod_name) = 'rocketdock.exe') then exit;

  // der AdobeReader Prozess wird ¸ber sein Fenster gefunden
  // -> das ist der Protected-Mode-Prozess mit Integrity-Level Low
  // deshalb den normalen Prozess (das ist der ParentProzess) patchen
  if (mod_name = 'AcroRd32.exe') then pid := parent_pid;

  if (ptGetClipboardData in FActivePatches) then
  begin
    HelperSvc.Patch(ctPatch, THookFuncType.hfGetClipboardData, PID, FCallbackWnd_PasteNotifcation, FCallbackMsg_PasteNotification);
  end;
  {if (ptOpenClipboardARFIX in FActivePatches) then
  begin
    if (StrPos(PChar(GetWindowText(Window)), 'Adobe Reader') <> nil) or (StrPos(PChar(GetWindowText(Window)), 'Excel') <> nil) then
    begin
      PatchOpenClipboard(pid); 
    end;
  end;}
  if (ptSetWindowsHookEx in FActivePatches) and (GetModuleFileName(pid) = 'mstsc.exe') then
  begin
    HelperSvc.Patch(ctPatch, THookFuncType.hfSetWindowsHookEx, PID, FCallbackWnd_SetWindowsHookEx, FCallbackMsg_SetWindowsHookEx);
  end;
end;

procedure TPatcher.UnpatchWindow(Window: HWND);
var
  pid, parent_pid: Cardinal;
begin
  if not IsWindowVisible(Window) then exit;

  GetWindowThreadProcessId(Window, pid{%H-});
  if (pid = 0) or (pid = GetCurrentProcessId) then exit;
  // der AdobeReader Prozess wird ¸ber sein Fenster gefunden
  // -> das ist der Protected-Mode-Prozess mit Integrity-Level Low
  // deshalb den normalen Prozess (das ist der ParentProzess) patchen
  if (GetModuleFileName(pid, @parent_pid) = 'AcroRd32.exe') then pid := parent_pid;

  if (ptGetClipboardData in FActivePatches) then
  begin
    HelperSvc.Patch(ctUnpatch, hfGetClipboardData, PID);
  end;
  {if (ptOpenClipboardARFIX in FActivePatches) then
  begin
    if (StrPos(PChar(GetWindowText(Window)), 'Adobe Reader') <> nil) or (StrPos(PChar(GetWindowText(Window)), 'Excel') <> nil) then
    begin
      UnpatchOpenClipboard(pid);
    end;
  end;}
  if (ptSetWindowsHookEx in FActivePatches) and (GetModuleFileName(pid) = 'mstsc.exe') then
  begin
    HelperSvc.Patch(ctUnpatch, hfSetWindowsHookEx, PID);
  end;
end;

function EnumWindowsProcPatch(hWnd: HWND; lParam: Integer): LongBool; stdcall;
var
  Patcher: TPatcher;
begin
  Result := true;
  Patcher := TPatcher(lParam);
  Patcher.PatchWindow(hWnd);
end;

function EnumWindowsProcUnpatch(hWnd: HWND; lParam: Integer): LongBool; stdcall;
var
  Patcher: TPatcher;
begin
  Result := true;
  Patcher := TPatcher(lParam);
  Patcher.UnpatchWindow(hWnd);
end;

procedure TPatcher.PatchAllProcesses;
begin
  if FActivePatches = [] then exit;

  EnumWindows(@EnumWindowsProcPatch, Integer(Self));
  if shell_notification = nil then shell_notification := TShellNotification.Create(@PatchWindow);
end;

procedure TPatcher.UnpatchAllProcesses();
begin
  //if (FActivePatches = []) or (FActivePatches = [ptGetClipboardData, ptOpenClipboardARFIX, ptSetWindowsHookEx]) then exit;
  if Byte(FActivePatches) xor $07 = 0 then exit; // the above statement, only shorter and faster but less readable

  EnumWindows(@EnumWindowsProcUnpatch, Integer(Self));

{  if (FActivePatches * [ptGetClipboardData, ptSetWindowsHookEx] = []) then
  begin
    TryFreeAndNil(PatchHelper64);
  end;}
  if (FActivePatches = []) then
  begin
    TryFreeAndNil(shell_notification);
  end;
end;

{procedure TPatcher.SetCallbackWnd_PasteNotifcation(Value: HWND);
begin
  if FCallbackWnd_PasteNotifcation <> Value then
  begin
    FCallbackWnd_PasteNotifcation := Value;
  end;
end;  }

destructor TPatcher.Destroy();
begin
  {$IfDef Debug}if GetCurrentThreadID <> MainThreadID then raise Exception.Create('TPacher.Destroy wurde auﬂerhalb des MainThread aufgerufen.');{$EndIf}
  FActivePatches := [];
//  UnpatchAllProcesses; // ist theoretisch schon geschehen durch einzelnes Abschalten der Patches
  //TryFreeAndNil(PatchHelper64);
end;

end.
