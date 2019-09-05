library PasteHook;

uses
  Windows,
  Messages,
  SysUtils,
  Classes;

{$R *.res}

{
type TGetProcAddress = function(hModule: Cardinal; lpProcName: PAnsiChar): Pointer; stdcall;
type TGetModuleHandle = function(lpModuleName: PAnsiChar): Cardinal; stdcall;

type TRemoteGetProcAddrInfo = record
  ResultAddr: ^Pointer;
  FunctionName: PChar;
  ModuleName: PChar;
  pGetProcAddr: TGetProcAddress;
  pGetModuleHandle: TGetModuleHandle;
end;
PRemoteGetProcAddrInfo = ^TRemoteGetProcAddrInfo;


function RemoteGetProcAddr(RemoteGetProcAddrInfo: PRemoteGetProcAddrInfo): Cardinal; stdcall;
begin
  with RemoteGetProcAddrInfo^ do
  begin
    ResultAddr^ := pGetProcAddr(pGetModuleHandle(ModuleName), FunctionName);
  end;
  Result := 0;
end;
         }
type
  TVirtualProtect = function(lpAddress: Pointer; dwSize, flNewProtect: Cardinal; lpflOldProtect: Pointer): LongBool; stdcall;
//  TCopyMemory = procedure(Destination, Source: Pointer; Length: Cardinal); stdcall;
  TPostMessage = function(hWnd: HWND; Msg: Cardinal; wParam, lParam: Integer): LongBool; stdcall;
  TPostThreadMessage = function(ThreadID: DWord; Msg: Cardinal; wParam, lParam: Integer): LongBool; stdcall;
  TGetCurrentProcessID = function(): Cardinal; stdcall;
  TSleep = procedure(milliseconds: Cardinal); stdcall;

type
  THookInfo = record
    FuncAddr: Pointer;
    OriginalCode: Pointer;
    PatchedCode: Pointer;
    PatchLen: Integer;
    CallbackWnd: HWND;
    CallbackMsg: Cardinal;
    pVirtualProtect: TVirtualProtect;
    pGetCurrentProcessID: TGetCurrentProcessID;
    pPostMessage: TPostMessage;
    pPostThreadMessage: TPostThreadMessage;
    pSleep: TSleep;
  end;

type
  TGetClipboardData = function(Format: Cardinal): Cardinal; stdcall;
  TOpenClipboard = function(hwnd: HWND): Integer; stdcall;
  TWindowsHookProc = function(Code, wParam, lParam: Integer): Integer; stdcall;
  TSetWindowsHookEx = function(idHook: Integer; HookProc: TWindowsHookProc; hMod, dwThreadID: Cardinal): HHOOK; stdcall;

const
  WH_KEYBOARD_LL = 13;
  WH_MOUSE_LL = 14;

function NewGetClipboardData(Format: Cardinal): Cardinal; stdcall;
var
  oldProtection: Cardinal;
  HookInfo: ^THookInfo;
  i: Integer;
begin
  {these are invalid opcodes -> easy to find and replace
  $0F7A0F04 = Address of HookInfo
  }

  HookInfo := Pointer($0F7A0F04);

  //if (Format = CF_TEXT) or (Format = CF_OEMTEXT) or (FORMAT = CF_UNICODETEXT) or (FORMAT = CF_LOCALE) then
    HookInfo^.pPostMessage(HookInfo^.CallbackWnd, HookInfo^.CallbackMsg, HookInfo^.pGetCurrentProcessID, Format);

  // DE-PATCH
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);

  //  HookInfo^.pCopyMemory(HookInfo^.FuncAddr, HookInfo^.OriginalCode, HookInfo^.PatchLen);

  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.OriginalCode)+i)^;
  end;
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
  // DE-PATCH ENDE

  // original-Fkt. aufrufen
  Result := TGetClipboardData(HookInfo^.FuncAddr)(Format);

  // PATCH
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);

  //  HookInfo^.pCopyMemory(HookInfo^.FuncAddr, HookInfo^.PatchedCode, HookInfo^.PatchLen);

  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.PatchedCode)+i)^;
  end;
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
end;

function NewOpenClipboard(hwnd: HWND): Integer; stdcall;
var
  oldProtection: Cardinal;
  HookInfo: ^THookInfo;
  i: Integer;
begin
  {these are invalid opcodes -> easy to find and replace
  $0F7A0F04 = Address of HookInfo
  }

  // mal damit versuchen?

  HookInfo := Pointer($0F7A0F04);

  // DE-PATCH
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.OriginalCode)+i)^;
  end;
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
  // DE-PATCH ENDE

  // original-Fkt. aufrufen
  i := 0;
  repeat
    Result := TOpenClipboard(HookInfo^.FuncAddr)(hwnd);
    Inc(i);
    if (Result = 0) then
    begin
      HookInfo^.pSleep(15);
    end;
  until (i >= 25) or (Result <> 0);
//  Result := TOpenClipboard(HookInfo^.FuncAddr)(hwnd);

  // RE-PATCH
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.PatchedCode)+i)^;
  end;
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);

end;

function NewSetWindowsHookEx(idHook: Integer; HookProc: TWindowsHookProc; hMod, dwThreadID: Cardinal): HHOOK; stdcall;
var
  oldProtection: Cardinal;
  HookInfo: ^THookInfo;
  i: Integer;
begin
  {these are invalid opcodes -> easy to find and replace
  $0F7A0F04 = Address of HookInfo
  }

  // mal damit versuchen?
  HookInfo := Pointer($0F7A0F04);

  // DE-PATCH
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.OriginalCode)+i)^;
  end;
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);
  // DE-PATCH ENDE

  // original-Fkt. aufrufen
  Result := TSetWindowsHookEx(HookInfo^.FuncAddr)(idHook, HookProc, hMod, dwThreadID);

  // RE-PATCH
//  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, PAGE_EXECUTE_READWRITE, @oldProtection);
  // CopyMemory
  for i := 0 to HookInfo^.PatchLen-1 do
  begin
    PByte(Integer(HookInfo^.FuncAddr)+i)^ := PByte(Integer(HookInfo^.PatchedCode)+i)^;
  end;
  HookInfo^.pVirtualProtect(HookInfo^.FuncAddr, HookInfo^.PatchLen, oldProtection, nil);

  if (dwThreadID = 0) and ((idHook = WH_KEYBOARD_LL) or (idHook = WH_MOUSE_LL)) then
  begin
    HookInfo^.pPostThreadMessage(HookInfo^.CallbackWnd, HookInfo^.CallbackMsg, HookInfo^.pGetCurrentProcessID, idHook);
  end;
end;




exports
  NewGetClipboardData,{RemoteGetProcAddr,}NewOpenClipboard,NewSetWindowsHookEx;

begin

end.
