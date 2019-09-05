unit PatchFunctions64;

{$mode objfpc}{$H+}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$HINTS OFF}

interface

uses
  Windows, logging, Sysutils;

{$Region 'Generic Function Headers'}
function PatchFunction(PID: Cardinal; HookFuncAddr: Pointer; HookCode: Pointer; HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
function VerifyPatchedFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
procedure UnpatchFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer);
{$EndRegion}

{$Region 'Specific Function Headers'}
function PatchGetClipboardData(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
function VerifyGetClipboardData(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
procedure UnpatchGetClipboardData(PID: Cardinal);

procedure UnpatchSetWindowsHookEx(PID: Cardinal);
function VerifySetWindowsHookEx(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
function PatchSetWindowsHookEx(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
{$EndRegion}

implementation

{$Region 'Generic Patch Functions'}
type
  TPostMessage = function(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; stdcall;
  TPostThreadMessage = function(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; stdcall;
//  TGetClipboardData = function(uFormat:UINT):HANDLE; stdcall;
  TGetCurrentProcessID = function(): DWord; stdcall;
  TVirtualProtect = function(lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect: PDWord):WINBOOL; stdcall;

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

var
  pPostMessage: Pointer = nil;
  pPostThreadMessage: Pointer = nil;
  pGetCurrentProcessID : Pointer= nil;
  pVirtualProtect: Pointer = nil;

function GetFunctionPointers: Boolean;
begin
  if pPostMessage = nil then pPostMessage := GetProcAddress(GetModuleHandle(user32), 'PostMessageA');
  if pPostThreadMessage = nil then pPostThreadMessage := GetProcAddress(GetModuleHandle(user32), 'PostThreadMessageA');//RemoteGetProcAddress(ProcessID, user32, 'PostMessageA');
  if pGetCurrentProcessID = nil then pGetCurrentProcessID := GetProcAddress(GetModuleHandle(kernel32), 'GetCurrentProcessId');
  if pVirtualProtect = nil then pVirtualProtect := GetProcAddress(GetModuleHandle(kernel32), 'VirtualProtect');

  Result := (pPostMessage <> nil) and (pPostThreadMessage <> nil) and (pGetCurrentProcessID <> nil) and (pVirtualProtect <> nil);
end;

function PatchFunction(PID: Cardinal; HookFuncAddr: Pointer; HookCode: Pointer; HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
var
  process: HANDLE;
  BytesWritten: PtrUInt;
  patched_beginning: Array [0..12] of Byte;
  HookInfo: THookInfo;
  HookFuncCode, pHookInfo: Pointer;

  i: UInt64;
  tmp: ^UInt;
  found: Byte;

  mem_size: UInt64;
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

  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);

  if process <> 0 then
  begin
    Result := true;

    mem_remote := VirtualAllocEx(process, nil, mem_size, MEM_RESERVE OR MEM_COMMIT, PAGE_EXECUTE_READWRITE);

    HookInfo.PatchedCode := mem_remote;
    HookInfo.OriginalCode := HookInfo.PatchedCode+HookInfo.PatchLen;
    pHookInfo := HookInfo.OriginalCode+HookInfo.PatchLen;
    HookFuncCode := pHookInfo+SizeOf(THookInfo);

    patched_beginning[0] := $48; // 64bit Operand
    patched_beginning[1] := $B8; // MOV RAX, our address
    patched_beginning[2] := NativeUint(HookFuncCode) AND $FF;
    patched_beginning[3] := (NativeUint(HookFuncCode) shr 8) AND $FF;
    patched_beginning[4] := (NativeUint(HookFuncCode) shr 16) AND $FF;
    patched_beginning[5] := (NativeUint(HookFuncCode) shr 24) AND $FF;
    patched_beginning[6] := (NativeUint(HookFuncCode) shr 32) AND $FF;
    patched_beginning[7] := (NativeUint(HookFuncCode) shr 40) AND $FF;
    patched_beginning[8] := (NativeUint(HookFuncCode) shr 48) AND $FF;
    patched_beginning[9] := (NativeUint(HookFuncCode) shr 56) AND $FF;
    //jmp eax
    patched_beginning[10] := $48; // 64bit Operand
    patched_beginning[11] := $ff; // JMP RAX
    patched_beginning[12] := $e0;

    // Patched beginning
    CopyMemory(mem_local, @patched_beginning[0], Length(patched_beginning));

    // read out original beginning
    ReadProcessMemory(process, HookInfo.FuncAddr, mem_local+Length(patched_beginning), HookInfo.PatchLen, BytesWritten{%H-});

    // fill in address of HookInfo in remote process
    // load alternative func code

    CopyMemory(mem_local+2*Length(patched_beginning)+SizeOf(THookInfo), HookCode, HookCodeLen);


    // Addresse für HookInfo ersetzen
    found := 0;
    for i := 0 to HookCodeLen - 1 - SizeOf(UInt64) do
    begin
      tmp := mem_local + 2*Length(patched_beginning) + SizeOf(THookInfo) + i;
      if (tmp^ = $0F7A0F05) then
      begin
        tmp^ := UInt(UInt64(pHookInfo) shr 32);
        Inc(found);
      end else if (tmp^ = $0F7A0F04) then
      begin
        tmp^ := UInt(UInt64(pHookInfo) and $FFFFFFFF);
        Inc(found);
      end;
      if found = 2 then break;
    end;

    CopyMemory(mem_local+2*Length(patched_beginning), @HookInfo, SizeOf(THookInfo));

    // write mem_local to mem_remote
    if not WriteProcessMemory(process, mem_remote, mem_local, mem_size, BytesWritten) then Result := false;
    // overwrite beginning of function which we want to hook
    if not WriteProcessMemory(process, HookInfo.FuncAddr, @patched_beginning[0], Length(patched_beginning), BytesWritten) then
    begin
      Log(IntToStr(GetLastError)+': '+SysErrorMessage(GetLastError));
      Result := false;
    end;

    CloseHandle(process);
  end;
  FreeMem(mem_local);
end;

function VerifyPatchedFunction(PID: Cardinal; HookFuncAddr: Pointer; {HookCode: Pointer;} HookCodeLen: Integer; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
var
  process: HANDLE;
  BytesRead: PtrUInt;
  mbi: _MEMORY_BASIC_INFORMATION;
  patched_beginning: Array [0..12] of Byte;
  phookinfo: ^THookInfo;
  mem_remote, buf: Pointer;
  mem_size: Cardinal;

  remoteHookCodeAddr: ^UInt64;
begin
  Result := false;
  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);
  if process <> 0 then
  begin
    ReadProcessMemory(process, HookFuncAddr, @patched_beginning[0], Length(patched_beginning), BytesRead);
    if (patched_beginning[0] = $48) and (patched_beginning[1] = $b8) and (patched_beginning[10] = $48) and (patched_beginning[11] = $FF) and (patched_beginning[12] = $E0) then
    begin
      remoteHookCodeAddr := @patched_beginning[2];
      mem_remote := Pointer(remoteHookCodeAddr^ - 2*Length(patched_beginning) - SizeOf(THookInfo));
      mem_size := 2*Length(patched_beginning)+SizeOf(THookInfo)+HookCodeLen;
      VirtualQueryEx(process, mem_remote, mbi{%H-}, SizeOf(_MEMORY_BASIC_INFORMATION));
      if (mbi.AllocationBase = mem_remote) and (mbi.RegionSize >= mem_size) then
      begin
        Result := true;
        buf := GetMemory(mem_size);
        ReadProcessMemory(process, mem_remote, buf, mem_size, BytesRead);
        phookinfo := Pointer(NativeUInt(buf) + 2*Length(patched_beginning));
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
  process: HANDLE;
  BytesRead: PtrUInt;
  mbi: _MEMORY_BASIC_INFORMATION;
  patched_beginning: Array [0..12] of Byte;
  mem_remote, buf: Pointer;
  mem_size: Cardinal;

  remoteHookCodeAddr: ^UInt64;
begin
  process := OpenProcess(PROCESS_ALL_ACCESS, false, PID);
  if process <> 0 then
  begin
    ReadProcessMemory(process, HookFuncAddr, @patched_beginning[0], Length(patched_beginning), BytesRead);
    patched_beginning[10] := $48; // 64bit Operand
    patched_beginning[11] := $ff; // JMP RAX
    patched_beginning[12] := $e0;
    if (patched_beginning[0] = $48) and (patched_beginning[1] = $b8) and (patched_beginning[10] = $48) and (patched_beginning[11] = $FF) and (patched_beginning[12] = $E0) then
    begin
      remoteHookCodeAddr := @patched_beginning[2];
      mem_remote := Pointer(remoteHookCodeAddr^ - 2*Length(patched_beginning) - SizeOf(THookInfo));
      mem_size := 2*Length(patched_beginning)+SizeOf(THookInfo)+HookCodeLen;
      VirtualQueryEx(process, mem_remote, mbi{%H-}, SizeOf(_MEMORY_BASIC_INFORMATION));
      if (mbi.AllocationBase = mem_remote) and (mbi.RegionSize >= mem_size) then
      begin
        buf := GetMemory(mem_size);
        ReadProcessMemory(process, mem_remote, buf, mem_size, BytesRead);
        WriteProcessMemory(process, HookFuncAddr, Pointer(NativeUInt(buf)+Length(patched_beginning)), Length(patched_beginning), BytesRead);
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
  HookCode_GetClipboardData: Array [0..328] of Byte = (
  $55,$48,$89,$E5,$48,$83,$EC,$50,$89,$4D,$F8,$C7,$45,$E0,$04,$0F,$7A,$0F,$C7,$45,
  $E4,$05,$0F,$7A,$0F,$48,$8B,$45,$E0,$48,$8B,$40,$40,$48,$FF,$D0,$41,$89,$C0,$41,
  $81,$E0,$FF,$FF,$FF,$FF,$48,$8B,$45,$E0,$8B,$50,$28,$48,$8B,$45,$E0,$48,$8B,$48,
  $20,$44,$8B,$4D,$F8,$48,$8B,$45,$E0,$48,$8B,$40,$30,$48,$FF,$D0,$4C,$8D,$4D,$E8,
  $48,$8B,$45,$E0,$8B,$50,$18,$48,$8B,$45,$E0,$48,$8B,$08,$41,$B8,$40,$00,$00,$00,
  $48,$8B,$45,$E0,$48,$8B,$40,$48,$48,$FF,$D0,$48,$8B,$45,$E0,$48,$63,$40,$18,$48,
  $FF,$C8,$48,$C7,$45,$D8,$00,$00,$00,$00,$48,$3B,$45,$D8,$7C,$34,$48,$FF,$4D,$D8,
  $66,$66,$66,$90,$48,$FF,$45,$D8,$48,$8B,$55,$E0,$4C,$8B,$02,$48,$8B,$55,$D8,$49,
  $01,$D0,$48,$8B,$55,$E0,$48,$8B,$4A,$08,$48,$8B,$55,$D8,$48,$01,$D1,$8A,$11,$41,
  $88,$10,$48,$3B,$45,$D8,$7F,$D4,$8B,$4D,$F8,$48,$8B,$45,$E0,$48,$8B,$00,$48,$FF,
  $D0,$48,$89,$45,$F0,$48,$8B,$45,$E0,$48,$63,$40,$18,$48,$FF,$C8,$48,$C7,$45,$D8,
  $00,$00,$00,$00,$48,$3B,$45,$D8,$7C,$36,$48,$FF,$4D,$D8,$66,$66,$66,$90,$66,$90,
  $48,$FF,$45,$D8,$48,$8B,$55,$E0,$4C,$8B,$02,$48,$8B,$55,$D8,$49,$01,$D0,$48,$8B,
  $55,$E0,$48,$8B,$4A,$10,$48,$8B,$55,$D8,$48,$01,$D1,$8A,$11,$41,$88,$10,$48,$3B,
  $45,$D8,$7F,$D4,$48,$8B,$45,$E0,$8B,$50,$18,$48,$8B,$45,$E0,$48,$8B,$08,$44,$8B,
  $45,$E8,$49,$B9,$00,$00,$00,$00,$00,$00,$00,$00,$48,$8B,$45,$E0,$48,$8B,$40,$48,
  $48,$FF,$D0,$48,$8B,$45,$F0,$C9,$C3
  );

function PatchGetClipboardData(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
begin
  Result := PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), @HookCode_GetClipboardData[0], Length(HookCode_GetClipboardData), CallbackWnd, CallbackMsg);
end;

function VerifyGetClipboardData(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
begin
  Result := VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), Length(HookCode_GetClipboardData), CallbackWnd, CallbackMsg);
end;

procedure UnpatchGetClipboardData(PID: Cardinal);
begin
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'GetClipboardData'), Length(HookCode_GetClipboardData));
end;
{$EndRegion}

{$Region 'SetWindowsHookEx'}
const
  HookCode_SetWindowsHookEx: Array [0..371] of Byte = (
  $55,$48,$89,$E5,$48,$83,$EC,$60,$89,$4D,$F8,$48,$89,$55,$F0,$4C,$89,$45,$E8,$44,
  $89,$4D,$E0,$C7,$45,$C8,$04,$0F,$7A,$0F,$C7,$45,$CC,$05,$0F,$7A,$0F,$4C,$8D,$4D,
  $D0,$48,$8B,$45,$C8,$8B,$50,$18,$48,$8B,$45,$C8,$48,$8B,$08,$41,$B8,$40,$00,$00,
  $00,$48,$8B,$45,$C8,$48,$8B,$40,$48,$48,$FF,$D0,$48,$8B,$45,$C8,$48,$63,$40,$18,
  $48,$FF,$C8,$48,$C7,$45,$C0,$00,$00,$00,$00,$48,$3B,$45,$C0,$7C,$33,$48,$FF,$4D,
  $C0,$66,$66,$90,$48,$FF,$45,$C0,$48,$8B,$55,$C8,$4C,$8B,$02,$48,$8B,$55,$C0,$49,
  $01,$D0,$48,$8B,$55,$C8,$48,$8B,$4A,$08,$48,$8B,$55,$C0,$48,$01,$D1,$8A,$11,$41,
  $88,$10,$48,$3B,$45,$C0,$7F,$D4,$44,$8B,$4D,$E0,$4C,$8B,$45,$E8,$48,$8B,$55,$F0,
  $8B,$4D,$F8,$48,$8B,$45,$C8,$48,$8B,$00,$48,$FF,$D0,$48,$89,$45,$D8,$48,$8B,$45,
  $C8,$48,$63,$40,$18,$48,$FF,$C8,$48,$C7,$45,$C0,$00,$00,$00,$00,$48,$3B,$45,$C0,
  $7C,$32,$48,$FF,$4D,$C0,$66,$90,$48,$FF,$45,$C0,$48,$8B,$55,$C8,$4C,$8B,$02,$48,
  $8B,$55,$C0,$49,$01,$D0,$48,$8B,$55,$C8,$48,$8B,$4A,$10,$48,$8B,$55,$C0,$48,$01,
  $D1,$8A,$11,$41,$88,$10,$48,$3B,$45,$C0,$7F,$D4,$48,$8B,$45,$C8,$8B,$50,$18,$48,
  $8B,$45,$C8,$48,$8B,$08,$44,$8B,$45,$D0,$49,$B9,$00,$00,$00,$00,$00,$00,$00,$00,
  $48,$8B,$45,$C8,$48,$8B,$40,$48,$48,$FF,$D0,$8B,$45,$E0,$48,$85,$C0,$75,$43,$8B,
  $45,$F8,$83,$F8,$0D,$74,$08,$8B,$45,$F8,$83,$F8,$0E,$75,$33,$48,$8B,$45,$C8,$48,
  $8B,$40,$40,$48,$FF,$D0,$41,$89,$C0,$41,$81,$E0,$FF,$FF,$FF,$FF,$48,$8B,$45,$C8,
  $8B,$50,$28,$48,$8B,$45,$C8,$48,$8B,$48,$20,$4C,$63,$4D,$F8,$48,$8B,$45,$C8,$48,
  $8B,$40,$30,$48,$FF,$D0,$48,$8B,$45,$D8,$C9,$C3);

function PatchSetWindowsHookEx(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): Boolean;
begin
  Result := PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), @HookCode_SetWindowsHookEx[0], Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
  Result := Result and PatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), @HookCode_SetWindowsHookEx[0], Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
end;

function VerifySetWindowsHookEx(PID: Cardinal; CallbackWnd: HWND; CallbackMsg: Cardinal): boolean;
begin
  Result := VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
  Result := Result and VerifyPatchedFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), Length(HookCode_SetWindowsHookEx), CallbackWnd, CallbackMsg);
end;

procedure UnpatchSetWindowsHookEx(PID: Cardinal);
begin
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExW'), Length(HookCode_SetWindowsHookEx));
  UnpatchFunction(PID, GetProcAddress(GetModuleHandle(user32), 'SetWindowsHookExA'), Length(HookCode_SetWindowsHookEx));
end;
{$EndRegion}



{$EndRegion}

end.

