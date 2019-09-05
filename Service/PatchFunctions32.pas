unit PatchFunctions32;

{$Mode objfpc}{$H+}
{$HINTS OFF}

interface

uses Windows;

{$Region 'Generic Function Headers'}
function PatchFunction(PID: Cardinal; HookFuncAddr: Pointer; HookCode: Pointer; HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
function VerifyPatchedFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
procedure UnpatchFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer);
{$EndRegion}

{$Region 'Specific Function Headers'}
function PatchGetClipboardData(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): Boolean;
function VerifyGetClipboardData(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): boolean;
procedure UnpatchGetClipboardData(PID: Cardinal);

function PatchSetWindowsHookEx(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): Boolean;
function VerifySetWindowsHookEx(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): boolean;
procedure UnpatchSetWindowsHookEx(PID: Cardinal);

function PatchOpenClipboard(PID: Cardinal): Boolean;
function VerifyOpenClipboard(PID: Cardinal): boolean;
procedure UnpatchOpenClipboard(PID: Cardinal);
{$EndRegion}

implementation

{$Region 'Generic Patch Functions'}
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

var
  pPostMessage: Pointer = nil;
  pPostThreadMessage: Pointer = nil;
  pGetCurrentProcessID : Pointer= nil;
  pVirtualProtect: Pointer = nil;
  pSleep: Pointer = nil;

function GetFunctionPointers: Boolean;
begin
  if pPostMessage = nil then pPostMessage := GetProcAddress(GetModuleHandle(user32), 'PostMessageA');
  if pPostThreadMessage = nil then pPostThreadMessage := GetProcAddress(GetModuleHandle(user32), 'PostThreadMessageA');//RemoteGetProcAddress(ProcessID, user32, 'PostMessageA');
  if pGetCurrentProcessID = nil then pGetCurrentProcessID := GetProcAddress(GetModuleHandle(kernel32), 'GetCurrentProcessId');
  if pVirtualProtect = nil then pVirtualProtect := GetProcAddress(GetModuleHandle(kernel32), 'VirtualProtect');
  if pSleep = nil then pSleep := GetProcAddress(GetModuleHandle(kernel32), 'Sleep');

  Result := (pPostMessage <> nil) and (pPostThreadMessage <> nil) and (pGetCurrentProcessID <> nil) and (pVirtualProtect <> nil) and (pSleep <> nil);
end;

function PatchFunction(PID: Cardinal; HookFuncAddr: Pointer; HookCode: Pointer; HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
var
  process: Cardinal;
  BytesWritten: PtrUInt;
  patched_beginning: Array [0..6] of Byte;
  HookInfo: THookInfo;
  HookFuncCode, pHookInfo: Pointer;

  i: Cardinal;
  tmp: ^Cardinal;

  mem_size: Cardinal;
  mem_local, mem_remote: Pointer;
begin
  Result := false;

  if (HookFuncAddr = nil) then exit;
  if VerifyPatchedFunction(PID, HookFuncAddr, HookCodeLen, CallbackWnd, CallbackMsg) then
  begin
    Result := true;
    exit;
  end;

  HookInfo.FuncAddr := HookFuncAddr;

  HookInfo.PatchLen := Length(patched_beginning);
  HookInfo.CallbackWnd := CallbackWnd;
  HookInfo.CallbackMsg := CallbackMsg;

  mem_size := 2*HookInfo.PatchLen+SizeOf(THookInfo)+HookCodeLen;
  mem_local := GetMemory(mem_size);

  if not GetFunctionPointers then exit;

  HookInfo.pPostMessage := TPostMessage(pPostMessage);
  HookInfo.pPostThreadMessage := TPostThreadMessage(pPostThreadMessage);
  HookInfo.pGetCurrentProcessID := TGetCurrentProcessID(pGetCurrentProcessID);
  HookInfo.pVirtualProtect := TVirtualProtect(pVirtualProtect);
  HookInfo.pSleep := TSleep(pSleep);

  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);

  if process <> 0 then
  begin
    Result := true;

    mem_remote := VirtualAllocEx(process, nil, mem_size, MEM_RESERVE OR MEM_COMMIT, PAGE_EXECUTE_READWRITE);

    HookInfo.PatchedCode := mem_remote;
    HookInfo.OriginalCode := Pointer(Cardinal(HookInfo.PatchedCode)+Cardinal(HookInfo.PatchLen));
    pHookInfo := Pointer(Cardinal(HookInfo.OriginalCode)+Cardinal(HookInfo.PatchLen));
    HookFuncCode := Pointer(Cardinal(pHookInfo)+Cardinal(SizeOf(THookInfo)));

    //mov eax, our_address
    patched_beginning[0] := $b8;
    patched_beginning[1] := Cardinal(HookFuncCode) AND $FF;
    patched_beginning[2] := (Cardinal(HookFuncCode) shr 8) AND $FF;
    patched_beginning[3] := (Cardinal(HookFuncCode) shr 16) AND $FF;
    patched_beginning[4] := (Cardinal(HookFuncCode) shr 24) AND $FF;
    //jmp eax
    patched_beginning[5] := $ff;
    patched_beginning[6] := $e0;

    // Patched beginning
    CopyMemory(mem_local, @patched_beginning[0], Length(patched_beginning));

    // read out original beginning
    ReadProcessMemory(process, HookInfo.FuncAddr, Pointer(Integer(mem_local)+Length(patched_beginning)), HookInfo.PatchLen, BytesWritten);

    // fill in address of HookInfo in remote process
    // load alternative func code

    CopyMemory(Pointer(Cardinal(mem_local)+2*Length(patched_beginning)+SizeOf(THookInfo)), HookCode, HookCodeLen);

    // Addresse für HookInfo ersetzen
    for i := 0 to HookCodeLen - 1 - SizeOf(Cardinal) do
    begin
      tmp := Pointer(Cardinal(mem_local) + 2*Length(patched_beginning) + SizeOf(THookInfo) + i);
      if (tmp^ = $0F7A0F04) then
      begin
        tmp^ := Integer(pHookInfo);
        break;
      end;
    end;

    CopyMemory(Pointer(Cardinal(mem_local)+2*Length(patched_beginning)), @HookInfo, SizeOf(THookInfo));

    // write mem_local to mem_remote
    if not WriteProcessMemory(process, mem_remote, mem_local, mem_size, BytesWritten) then Result := false;
    // overwrite beginning of function which we want to hook
    if not WriteProcessMemory(process, HookInfo.FuncAddr, @patched_beginning[0], Length(patched_beginning), BytesWritten) then Result := false;

    CloseHandle(process);
  end;
  FreeMem(mem_local);
end;

function VerifyPatchedFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
var
  process, BytesRead: Cardinal;
  mbi: _MEMORY_BASIC_INFORMATION;
  patched_beginning: Array [0..6] of Byte;
  phookinfo: ^THookInfo;
  mem_remote, buf: Pointer;
  mem_size: Cardinal;
begin
  Result := false;
  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);
  if process <> 0 then
  begin
    ReadProcessMemory(process, HookFuncAddr, @patched_beginning[0], Length(patched_beginning), BytesRead);

    if (patched_beginning[0] = $b8) and (patched_beginning[5] = $ff) and (patched_beginning[6] = $e0) then
    begin
      mem_remote := Pointer(PCardinal(@patched_beginning[1])^ - 2*Length(patched_beginning) - SizeOf(THookInfo));
      mem_size := 2*Length(patched_beginning)+SizeOf(THookInfo)+HookCodeLen;
      VirtualQueryEx(process, mem_remote, mbi, SizeOf(_MEMORY_BASIC_INFORMATION));
      if (mbi.AllocationBase = mem_remote) and (mbi.RegionSize >= mem_size) then
      begin
        Result := true;
        buf := GetMemory(mem_size);
        ReadProcessMemory(process, mem_remote, buf, mem_size, BytesRead);
        phookinfo := Pointer(Cardinal(buf) + 2*Length(patched_beginning));
        if (phookinfo^.CallbackWnd <> CallbackWnd) or (phookinfo^.CallbackMsg <> CallbackMsg) then
        begin
          phookinfo^.CallbackWnd := CallbackWnd;
          phookinfo^.CallbackMsg := CallbackMsg;
          WriteProcessMemory(process, mem_remote, buf, mem_size, BytesRead);
        end;
        FreeMem(buf);
      end;
    end;
    CloseHandle(process);
  end;
end;

procedure UnpatchFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer);
var
  process, BytesRead: Cardinal;
  mbi: _MEMORY_BASIC_INFORMATION;
  patched_beginning: Array [0..6] of Byte;
   mem_remote, buf: Pointer;
  mem_size: Cardinal;
begin
  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);
  if process <> 0 then
  begin
    ReadProcessMemory(process, HookFuncAddr, @patched_beginning[0], Length(patched_beginning), BytesRead);
    if (patched_beginning[0] = $b8) and (patched_beginning[5] = $ff) and (patched_beginning[6] = $e0) then
    begin
      mem_remote := Pointer(patched_beginning[1] + patched_beginning[2] shl 8 + patched_beginning[3] shl 16 + patched_beginning[4] shl 24 - 2*Length(patched_beginning) - SizeOf(THookInfo));
      mem_size := 2*Length(patched_beginning)+SizeOf(THookInfo)+HookCodeLen;
      VirtualQueryEx(process, mem_remote, mbi, SizeOf(_MEMORY_BASIC_INFORMATION));
      if (mbi.AllocationBase = mem_remote) and (mbi.RegionSize >= mem_size) then
      begin
        buf := GetMemory(mem_size);
        ReadProcessMemory(process, mem_remote, buf, mem_size, BytesRead);
        WriteProcessMemory(process, HookFuncAddr, Pointer(Integer(buf)+Length(patched_beginning)), Length(patched_beginning), BytesRead);
        VirtualFreeEx(process, mem_remote, mem_size, MEM_RELEASE);
        FreeMem(buf);
      end;
    end;
    CloseHandle(process);
  end;
end;
{$EndRegion}

{$Region 'Specific Patch Functions'}

{$Region 'GetClipboardData'}
const
  HookCode_GetClipboardData: Array [0..135] of Byte = (
  $55, $8B, $EC, $51, $53, $56, $57, $8B, $75, $08, $BB, $04, $0F, $7A, $0F, $56, $FF, $53, $1C, $50, $8B, $43, $14, $50, $8B,
  $43, $10, $50, $FF, $53, $20, $8D, $45, $FC, $50, $6A, $40, $8B, $43, $0C, $50, $8B, $03, $50, $FF, $53, $18, $8B, $53, $0C,
  $4A, $85, $D2, $7C, $15, $42, $33, $C0, $8B, $4B, $04, $03, $C8, $0F, $B6, $09, $8B, $3B, $03, $F8, $88, $0F, $40, $4A, $75,
  $EE, $56, $FF, $13, $8B, $F0, $8B, $53, $0C, $4A, $85, $D2, $7C, $15, $42, $33, $C0, $8B, $4B, $08, $03, $C8, $0F, $B6, $09,
  $8B, $3B, $03, $F8, $88, $0F, $40, $4A, $75, $EE, $6A, $00, $8B, $45, $FC, $50, $8B, $43, $0C, $50, $8B, $03, $50, $FF, $53,
  $18, $8B, $C6, $5F, $5E, $5B, $59, $5D, $C2, $04, $00
  );

function PatchGetClipboardData(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): Boolean;
begin
  Result := PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), @HookCode_GetClipboardData[0], Length(HookCode_GetClipboardData), CallbackWnd, CallbackMsg);
  {$IfDef PatcherLogging}
  if Result then
    Logger.Add('Patched GetClipboardData in ' + GetModuleFileName(PID) + ' successfully.')
  else
    Logger.Add('Error while patching GetClipboardData in ' + GetModuleFileName(PID) + '.', LT_ERROR);
  {$EndIf}
end;

function VerifyGetClipboardData(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): boolean;
begin
  Result := VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), Length(HookCode_GetClipboardData), CallbackWnd, CallbackMsg);
end;

procedure UnpatchGetClipboardData(PID: Cardinal);
begin
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), Length(HookCode_GetClipboardData));
  {$IfDef PatcherLogging}Logger.Add('Unpatched GetClipboardData in ' + GetModuleFileName(PID) + '.');{$EndIf}
end;
{$EndRegion}
{$Region 'SetWindowsHookEx'}
var HookCode_SetWindowsHookEx: Array [0..168] of Byte = (
  $55, $8B, $EC, $83, $C4, $F8, $53, $56, $57, $8B, $75, $08, $BB, $04, $0F, $7A, $0F, $8D, $45, $F8, $50, $6A, $40, $8B, $43,
  $0C, $50, $8B, $03, $50, $FF, $53, $18, $8B, $53, $0C, $4A, $85, $D2, $7C, $15, $42, $33, $C0, $8B, $4B, $04, $03, $C8, $0F,
  $B6, $09, $8B, $3B, $03, $F8, $88, $0F, $40, $4A, $75, $EE, $8B, $45, $14, $50, $8B, $45, $10, $50, $8B, $45, $0C, $50, $56,
  $FF, $13, $89, $45, $FC, $8B, $53, $0C, $4A, $85, $D2, $7C, $15, $42, $33, $C0, $8B, $4B, $08, $03, $C8, $0F, $B6, $09, $8B,
  $3B, $03, $F8, $88, $0F, $40, $4A, $75, $EE, $6A, $00, $8B, $45, $F8, $50, $8B, $43, $0C, $50, $8B, $03, $50, $FF, $53, $18,
  $83, $7D, $14, $00, $75, $1A, $83, $FE, $0D, $74, $05, $83, $FE, $0E, $75, $10, $56, $FF, $53, $1C, $50, $8B, $43, $14, $50,
  $8B, $43, $10, $50, $FF, $53, $24, $8B, $45, $FC, $5F, $5E, $5B, $59, $59, $5D, $C2, $10, $00);

function PatchSetWindowsHookEx(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): Boolean;
begin
  Result := PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), @HookCode_SetWindowsHookEx[0], Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
  Result := Result and PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), @HookCode_SetWindowsHookEx[0], Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
  {$IfDef PatcherLogging}
  if Result then
    Logger.Add('Patched SetWindowsHookEx in ' + GetModuleFileName(PID) + ' successfully.')
  else
    Logger.Add('Error while patching SetWindowsHookEx in ' + GetModuleFileName(PID) + '.', LT_ERROR);
  {$EndIf}
end;

function VerifySetWindowsHookEx(PID: Cardinal; CallbackWnd, CallbackMsg: Cardinal): boolean;
begin
  Result := VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
  Result := Result and VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
end;

procedure UnpatchSetWindowsHookEx(PID: Cardinal);
begin
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), Length(HookCode_SetWindowsHookEx));
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), Length(HookCode_SetWindowsHookEx));
  {$IfDef PatcherLogging}Logger.Add('Unpatched SetWindowsHookEx in ' + GetModuleFileName(PID) + '.');{$EndIf}
end;


{$EndRegion}
{$Region 'OpenClipboard'}
var HookCode_OpenClipboard: Array [0..140] of Byte = (
  $55, $8B, $EC, $51, $53, $56, $57, $BE, $04, $0F, $7A, $0F, $8D, $45, $FC, $50, $6A, $40, $8B, $46, $0C, $50, $8B, $06, $50,
  $FF, $56, $18, $8B, $46, $0C, $48, $85, $C0, $7C, $15, $40, $33, $DB, $8B, $56, $04, $03, $D3, $0F, $B6, $12, $8B, $0E, $03,
  $CB, $88, $11, $43, $48, $75, $EE, $33, $DB, $8B, $45, $08, $50, $FF, $16, $8B, $F8, $43, $85, $FF, $75, $05, $6A, $0F, $FF,
  $56, $28, $83, $FB, $19, $7D, $04, $85, $FF, $74, $E5, $8B, $46, $0C, $48, $85, $C0, $7C, $15, $40, $33, $DB, $8B, $56, $08,
  $03, $D3, $0F, $B6, $12, $8B, $0E, $03, $CB, $88, $11, $43, $48, $75, $EE, $6A, $00, $8B, $45, $FC, $50, $8B, $46, $0C, $50,
  $8B, $06, $50, $FF, $56, $18, $8B, $C7, $5F, $5E, $5B, $59, $5D, $C2, $04, $00
);

function PatchOpenClipboard(PID: Cardinal): Boolean;
begin
  Result := PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'OpenClipboard'), @HookCode_OpenClipboard[0], Length(HookCode_OpenClipboard), 0, 0);
  {$IfDef PatcherLogging}
  if Result then
    Logger.Add('Patched OpenClipboard in ' + GetModuleFileName(PID) + ' successfully.')
  else
    Logger.Add('Error while patching OpenClipboard in ' + GetModuleFileName(PID) + '.', LT_ERROR);
  {$EndIf}
end;

function VerifyOpenClipboard(PID: Cardinal): boolean;
begin
  Result := VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'OpenClipboard'), Length(HookCode_OpenClipboard), 0, 0);
end;

procedure UnpatchOpenClipboard(PID: Cardinal);
begin
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'OpenClipboard'), Length(HookCode_OpenClipboard));
  {$IfDef PatcherLogging}Logger.Add('Unpatched OpenClipboard in ' + GetModuleFileName(PID) + '.');{$EndIf}
end;
{$EndRegion}

{$EndRegion}

end.
