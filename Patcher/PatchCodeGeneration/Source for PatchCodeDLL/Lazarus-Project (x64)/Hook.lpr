library Hook;

{$mode objfpc}{$H+}

uses
  Classes, Windows
  { you can add units after this };

type
  TPostMessage = function(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; stdcall;
  TPostThreadMessage = function(ThreadID: DWord; Msg: Cardinal; wParam, lParam: Integer): LongBool; stdcall;
  TGetCurrentProcessID = function(): DWord; stdcall;
  TVirtualProtect = function(lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect: PDWord):WINBOOL; stdcall;

type
  TGetClipboardData = function(uFormat:UINT):HANDLE; stdcall;
  TWindowsHookProc = function(Code, wParam, lParam: Integer): Integer;
  TSetWindowsHookEx = function(idHook: longint; HookProc: TWindowsHookProc; hMod: HINST; dwThreadID: DWORD): HHOOK; stdcall;

const
  WH_KEYBOARD_LL = 13;
  WH_MOUSE_LL = 14;

type
  THookInfo = record
    FuncAddr: Pointer;
    OriginalCode: Pointer;
    PatchedCode: Pointer;
    PatchLen: Integer;
    CallbackWnd: HWND;
    CallbackMsg: Integer;
    pPostMessage: TPostMessage;
    pPostThreadMessage: TPostThreadMessage;
    pGetCurrentProcessID: TGetCurrentProcessID;
    pVirtualProtect: TVirtualProtect;
  end;

function NewGetClipboardData(Format: Cardinal): HANDLE; stdcall;
var
  oldProtection: Cardinal;
  HookInfo: ^THookInfo;
  i: NativeInt;
begin
  {these are invalid opcodes -> easy to find and replace
  $0F7A0F04 = Address of HookInfo
  }

  HookInfo := Pointer($0F7A0F050F7A0F04);

  //if (Format = CF_TEXT) or (Format = CF_OEMTEXT) or (FORMAT = CF_UNICODETEXT) or (FORMAT = CF_LOCALE) then
    HookInfo^.pPostMessage(HookInfo^.CallbackWnd, HookInfo^.CallbackMsg, HookInfo^.pGetCurrentProcessID(), Format);

  // DE-PATCH
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(HookInfo^.FuncAddr+i)^ := PByte(HookInfo^.OriginalCode+i)^;
  end;
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
  // DE-PATCH ENDE

  // original-Fkt. aufrufen
  Result := TGetClipboardData(HookInfo^.FuncAddr)(Format);

  // PATCH
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(HookInfo^.FuncAddr+i)^ := PByte(HookInfo^.PatchedCode+i)^;
  end;
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
end;

function NewSetWindowsHookEx(idHook: longint; HookProc: TWindowsHookProc; hMod: HINST; dwThreadID: DWORD): HHOOK; stdcall;
var
  oldProtection: Cardinal;
  HookInfo: ^THookInfo;
  i: NativeInt;
begin
  {these are invalid opcodes -> easy to find and replace
  $0F7A0F04 = Address of HookInfo
  }

  // mal damit versuchen?
  HookInfo := Pointer($0F7A0F050F7A0F04);

  // DE-PATCH
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(HookInfo^.FuncAddr+i)^ := PByte(HookInfo^.OriginalCode+i)^;
  end;

  // original-Fkt. aufrufen
  Result := TSetWindowsHookEx(HookInfo^.FuncAddr)(idHook, HookProc, hMod, dwThreadID);

    // PATCH
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(HookInfo^.FuncAddr+i)^ := PByte(HookInfo^.PatchedCode+i)^;
  end;
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);

  if (dwThreadID = 0) and ((idHook = WH_KEYBOARD_LL) or (idHook = WH_MOUSE_LL)) then
  begin
    HookInfo^.pPostMessage(HookInfo^.CallbackWnd, HookInfo^.CallbackMsg, HookInfo^.pGetCurrentProcessID(), idHook);
  end;
end;

exports
  NewGetClipboardData, NewSetWindowsHookEx;

begin
end.

