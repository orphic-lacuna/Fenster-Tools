unit FensterToolsCommon;

{$mode objfpc}{$H+}

interface

uses Windows, Classes, Messages, JwaTlHelp32, Sysutils, Dialogs, ExtendedWinAPI, shfolder, FileUtil, fpJson;

{type
  TThreadMethodAddHotkey = procedure(Value: THotkeyNotificationInfo) of object;

  TAddHotkeyParameters = class
  public
    Value: THotkeyNotificationInfo;
    Method: TThreadMethodAddHotkey;
  end;    }

type
  //http://msdn2.microsoft.com/en-us/library/ms724833.aspx
  TOSVersionInfoEx = packed record
    dwOSVersionInfoSize : DWORD;
    dwMajorVersion      : DWORD;
    dwMinorVersion      : DWORD;
    dwBuildNumber       : DWORD;
    dwPlatformId        : DWORD;
    szCSDVersion        : array[0..127] of Char;
    wServicePackMajor   : WORD;
    wServicePackMinor   : WORD;
    wSuiteMask          : WORD;
    wProductType        : BYTE;
    wReserved           : BYTE;
  end;



const
  VER_SUITE_PERSONAL  = $00000200;
  VER_NT_WORKSTATION  = $00000001;
  VER_SUITE_WH_SERVER = $00008000;
  SM_SERVERR2         = 89;

  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;

  WH_KEYBOARD_LL = 13;
  WH_MOUSE_LL = 14;

  // Messages
  WM_SHOWClIPBOARDPOPUP = WM_USER + 1008;

//function GetOSVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall; external kernel32 name 'GetVersionExA';
procedure GetNativeSystemInfo(var lpSystemInfo: _SYSTEM_INFO); stdcall; external kernel32;
//function GetWinVersionString: String;
function GetWinVersionJSON: TJSONObject;
//function Is64BitOS: Boolean;
function IsProcess64BitHANDLE(hProcess: Cardinal): boolean;
function IsProcess64BitPID(PID: Cardinal): boolean;
function InputQueryInt(Headline, Subheader: String; x{, min, max}: Integer): Integer;
function InputQueryFloat(Headline, Subheader: String; x{, min, max}: Single; Format: String): Single;

function GetDesktopIconHandle: HWND;
function GetModuleFileName(const PID: Cardinal; parentPID: PCardinal = nil): string;
function GetWindowModuleFileName(const hSrcWnd: HWND): string;
function GetWindowText(hwnd: HWND): UnicodeString;
function GetStartButtonhwnd: HWND;

Function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
function GetFullExeNameFromWindowHandle(aHandle: HWND): UnicodeString;

var
  IsVista_7: boolean;
  //WindowsVersion: TWinVersionInfo;
  ConfigFileName, ClipboardIndexFilename, ClipboardDataFilename, AppDataDir, Filename_HiddenWindows, UpdateTmpFilename: UnicodeString;

resourcestring
  REL_PATH_ICON_REFRESH = 'Icons\view_refresh_7.ico';
  REL_PATH_ICON_DOWNLOAD_UPDATE = 'Icons\software-update-download.ico';
  REL_PATH_ICON_CHECK = 'Icons\mark.ico';
  REL_PATH_ICON_CROSS = 'Icons\dialog-close.png';

implementation

uses Logging, AdminCheck, ToolBox;

function QueryFullProcessImageName(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPCWSTR; lpdwSize: PDWORD): WINBOOL; stdcall; external kernel32 name 'QueryFullProcessImageNameW';

type _OSVERSIONINFOEXW = record
  dwOSVersionInfoSize, dwMajorVersion, dwMinorVersion, dwBuildNumber, dwPlatformId: Cardinal;
  szCSDVersion: Array [0..127] of WideChar;
  wServicePackMajor, wServicePackMinor, wSuiteMask: Word;
  wProductType, wReserved: Char;
end;
type RTL_OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
type PRTL_OSVERSIONINFOEXW = ^_OSVERSIONINFOEXW;

function RtlGetVersion(lpVersionInformation: PRTL_OSVERSIONINFOEXW): Cardinal; stdcall; external 'ntdll.dll';
{function Is64BitOS: Boolean;
var
  SystemInfo: TSystemInfo;
begin
  {$IfDef WIN64}
    Result := true;
  {$Else}
    GetNativeSystemInfo(SystemInfo{%H-});
    Result := (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64);
  {$EndIf}
end;
procedure QueryWindowsVersion;
var
  osver: RTL_OSVERSIONINFOEXW;
begin
  osver.dwOSVersionInfoSize := SizeOf(osver);
  RtlGetVersion(@osver);

  WindowsVersion.Is64Bit := Is64BitOS;
  WindowsVersion.IsWinXPOrGreater := (osver.dwMajorVersion >= 5) and (osver.dwMinorVersion >= 1);
  WindowsVersion.IsWinVistaOrGreater := (osver.dwMajorVersion >= 6);
  WindowsVersion.IsWin7OrGreater := (osver.dwMajorVersion >= 6) and (osver.dwMinorVersion >= 1);
  WindowsVersion.IsWin8OrGreater := (osver.dwMajorVersion >= 6) and (osver.dwMinorVersion >= 2);
  WindowsVersion.IsWin81OrGreater := (osver.dwMajorVersion >= 6) and (osver.dwMinorVersion >= 3);
  WindowsVersion.IsWin10OrGreater := (osver.dwMajorVersion >= 6) and (osver.dwMinorVersion >= 4);
end;
         }

Function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
var
  Srch,OldP,RemS: UnicodeString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
  begin
    Srch:=UnicodeUpperCase(Srch);
    OldP:=UnicodeUpperCase(OldP);
  end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

procedure OutputDebugString2(Str: String);
var
  Mutex, hMapFile, BufferReady, DataReady: Cardinal;
  pBuf: PAnsiChar;
  success: boolean;
begin
  Mutex := CreateMutex(nil, true, 'FT_DBG_MUTEX');
  DataReady := CreateEvent(nil, false, false, 'FT_DBG_DATA_READY');
  BufferReady := CreateEvent(nil, false, false, 'FT_DBG_BUFFER_READY');
  success := WaitForSingleObject(BufferReady, 10000) = WAIT_OBJECT_0;
  if success then
  begin
    hMapFile := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, 4096, 'FT_DBG_BUFFER');
    pBuf := MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, 4096);
    StrPCopy(pBuf, Str);
    UnmapViewOfFile(pBuf);
    CloseHandle(hMapFile);
    SetEvent(DataReady);
  end;
  ReleaseMutex(Mutex);
end;

procedure StripString(var x: String; DelChars: String);
var
  i, verzug, res: Integer;
begin
  verzug := 0;
  for i := 1 to Length(x)-verzug do
  begin
    res := Pos(x[i-verzug], DelChars);
    if res <> 0 then
    begin
      if i-verzug < Length(x) then
      begin
        x[i-verzug] := x[i-verzug+1];
        Inc(verzug);
      end;
    end;
  end;
  SetLength(x, Length(x)-verzug);
end;


function InputQueryInt(Headline, Subheader: String; x{, min, max}: Integer): Integer;
var
  str: String;
begin
  str := IntToStr(x);
  Result := x;
  if InputQuery(Headline, Subheader, str) then
  begin
    try
      Result := StrToInt(str);
    except
    end;
  end;
  //if Result < min then Result := min
  //else if Result > max then Result := max;
end;

function InputQueryFloat(Headline, Subheader: String; x{, min, max}: Single; Format: String): Single;
var
  str: String;
begin
  str := FormatFloat(Format, x);
  Result := x;
  if InputQuery(Headline, Subheader, str) then
  begin
    StripString(str, '  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz');
    try
      Result := StrToFloat(str);
    except

    end;
  end;
  //if Result < min then Result := min
  //else if Result > max then Result := max;
end;


{function GetOSVersionInfoEx : TOSVersionInfoEx;
var
  OSVersionInfo   : TOSVersionInfo absolute Result;
  Done : Boolean;
begin
  FillChar(Result{%H-}, SizeOf(Result), #0);
  Done := False;
  try
    Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
    Done := GetOSVersionEx(Result);
  except
  end;
  if not(Done) then
  begin
    try
      FillChar(Result, SizeOf(Result), #0);
      Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      GetVersionEx(OSVersionInfo);
    except
    end;
  end;
end; }

{function GetWinVersionString: String;
var
  OsVersionInfo: TOSVersionInfoEx;
  SystemInfo: TSystemInfo;
begin
  Result := 'Unbekannte Windows-Version';
  OsVersionInfo.dwOSVersionInfoSize := SizeOf(OsVersionInfo);
  RtlGetVersion(@OsVersionInfo);
  GetNativeSystemInfo(SystemInfo{%H-});

  case OsVersionInfo.dwPlatformId of
    VER_PLATFORM_WIN32s:
    begin
      Result := 'Win32s';
    end;

    VER_PLATFORM_WIN32_WINDOWS:
    begin
      if (OsVersionInfo.dwMajorVersion = 4) and (OsVersionInfo.dwMinorVersion =  0) then
        begin
             Result := 'Windows 95';
        end;
      if (OsVersionInfo.dwMajorVersion = 4) and (OsVersionInfo.dwMinorVersion = 10) then
        begin
             Result := 'Windows 98';
        end;
      if (OsVersionInfo.dwMajorVersion = 4) and (OsVersionInfo.dwMinorVersion = 90) then
        begin
             Result := 'Windows Millennium Edition';
        end;
    end;

    VER_PLATFORM_WIN32_NT:
    begin
      if (OsVersionInfo.dwMajorVersion = 4) and (OsVersionInfo.dwMinorVersion = 0) then
        begin
             Result := 'Windows NT';
        end;
      if (OsVersionInfo.dwMajorVersion = 5) and (OsVersionInfo.dwMinorVersion = 0) then
        begin
             Result := 'Windows 2000';
        end;
      if (OsVersionInfo.dwMajorVersion = 5) and (OsVersionInfo.dwMinorVersion = 1) then
        begin
             Result := 'Windows XP';
        end;

      if (OsVersionInfo.dwMajorVersion = 5) and (OsVersionInfo.dwMinorVersion = 2) then
      begin
        if GetSystemMetrics(SM_SERVERR2) <> 0 then
          begin
                Result := 'Windows Server 2003 "R2"';
          end
         else
           if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
             begin
                  Result := 'Windows XP x64';
             end
           else
             if OsVersionInfo.wSuiteMask = VER_SUITE_WH_SERVER then
               begin
                    Result := 'Windows Home Server';
               end
             else
               begin
                    Result := 'Windows Server 2003';
               end;
      end;
      if (OsVersionInfo.dwMajorVersion = 6) and (OsVersionInfo.dwMinorVersion = 0) then
      begin
        if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
          begin
               Result := 'Windows Vista';
          end
        else
          begin
               Result := 'Windows Server 2008';
          end;
      end;
      if (OsVersionInfo.dwMajorVersion = 6) and (OsVersionInfo.dwMinorVersion = 1) then
      begin
        if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
          begin
               Result := 'Windows 7';
          end
        else
          begin
               Result := 'Windows Server 2008 R2';
          end;
      end;

      if (OsVersionInfo.dwMajorVersion = 6) and (OsVersionInfo.dwMinorVersion = 2) then
      begin
        if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
          begin
               Result := 'Windows 8';
          end
        else
          begin
               Result := 'Windows Server 2012';
          end;
      end;

      if (OsVersionInfo.dwMajorVersion = 6) and (OsVersionInfo.dwMinorVersion = 3) then
      begin
        if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
          begin
               Result := 'Windows 8.1';
          end
        else
          begin
               Result := 'Windows Server 2012 R2';
          end;
      end;

      if (OsVersionInfo.dwMajorVersion = 6) and (OsVersionInfo.dwMinorVersion = 4) then
      begin
        if (OsVersionInfo.wProductType = VER_NT_WORKSTATION) then
          begin
               Result := 'Windows 10';
          end
        else
          begin
               Result := 'Windows Server (10)';
          end;
      end;

      if (OsVersionInfo.wSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then
        Result := Result + ' Home Edition'
      else
        Result := Result + ' Professional';
    end;
  end;
  if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
  begin
    Result := Result + ' 64 bit';
  end else if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL) then
  begin
    Result := Result + ' 32 bit';
  end else if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64) then
  begin
    Result := Result + ' Intel Itanium-based Processor';
  end else if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_UNKNOWN) then
  begin
    Result := Result + ' Unknown Processor Type';
  end;
  Result := Result + ' ' + OsVersionInfo.szCSDVersion;
end; }


function GetWinVersionJSON: TJSONObject;
var
  OsVersionInfo: TOSVersionInfoEx;
  SystemInfo: TSystemInfo;
begin
  OsVersionInfo.dwOSVersionInfoSize := SizeOf(OsVersionInfo);
  RtlGetVersion(@OsVersionInfo);
  GetNativeSystemInfo(SystemInfo{%H-});

  Result := TJSONObject.Create;
  Result.Add('MajorVersion', OsVersionInfo.dwMajorVersion);
  Result.Add('MinorVersion', OsVersionInfo.dwMinorVersion);
  if SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL then
    Result.Add('Bitness', 32)
  else if SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
    Result.Add('Bitness', 64);

  Result.Add('ProcessorCount', SystemInfo.dwNumberOfProcessors);
end;


function IsProcess64BitPID(PID: Cardinal): boolean;
var
  hProc: NativeUInt;
begin
  Result := WindowsVersion.Is64Bit;
  if not Result then exit;
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION, false, PID);
  Result := not IsWow64Process(hProc);
  CloseHandle(hProc);
end;

function IsProcess64BitHANDLE(hProcess: Cardinal): boolean;
begin
  Result := WindowsVersion.Is64Bit and not IsWow64Process(hProcess);
end;

function GetWindowText(hwnd: HWND): UnicodeString;
var
  x: Integer;
begin
  x := GetWindowTextLengthW(hwnd)+1;
  SetLength(Result, x); // GetWindowTextLength(hwnd)
  Windows.GetWindowTextW(hwnd, @Result[1], Length(Result));
end;

function GetDesktopIconHandle: HWND;
var
  wndMain, wndChild: HWND;
begin
  Result := 0;
  wndMain := FindWindow('Progman','Program Manager');
  if wndMain <> 0 then
  begin
    wndChild := FindWindowEx(wndMain, 0, 'SHELLDLL_DefView', nil);
    wndChild := FindWindowEx(wndChild, 0, 'SysListView32', nil);
    if wndChild <> 0 then
    begin
      Result := wndChild;
    end;
  end;
end;

function GetModuleFileName(const PID: Cardinal; parentPID: PCardinal = nil): string;
var
  Data :TProcessEntry32;
  Snap : Integer;
  Done : boolean;
  ExeName : string;
begin
  Result := '';
  try
    Snap:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
    Done:=false;
    ExeName := '';
    try
      Data.dwSize:=SizeOf(Data);
      if(Process32First(Snap,Data))then
      begin
        repeat
          if Data.th32ProcessID=PID then
          begin
            ExeName:= StrPas(Data.szExeFile);
            if parentPID <> nil then parentPID^ := Data.th32ParentProcessID;
            Done:=true;
          end;
        until Done or not(Process32Next(Snap,Data));
      end;
    finally
      Windows.CloseHandle(Snap);
    end;
    result := ExeName;
  except
  end;
end;

function GetWindowModuleFileName(const hSrcWnd: HWND): string;
var
  hID :DWord;
begin
  GetWindowThreadProcessId(hSrcWnd,@hID);
  Result := GetModuleFileName(hID);
end;

function GetFullExeNameFromWindowHandle(aHandle: HWND): UnicodeString;
var
  size, procID: DWOrd;
  h: HANDLE;
begin
  GetWindowThreadProcessId(aHandle, @procID);
  h := OpenProcess(PROCESS_QUERY_INFORMATION, false, procID);
  size := 64*1024;
  SetLength(Result, size);
  QueryFullProcessImageName(h, 0, @Result[1], @size);
  CloseHandle(h);
  SetLength(Result, size);
end;


function GetStartButtonhwnd: HWND;
begin
  if WindowsVersion.IsWinVistaOrGreater then
  begin
    // Win7
    Result := FindWindow('Button', 'Start');
  end else begin
    //XP
    Result := FindWindowEx(GetDesktopWindow, 0, 'Button', nil);
  end;
end;

var
  Pfad: UnicodeString;

initialization
  //QueryWindowsVersion;
  IsVista_7 := WindowsVersion.IsWinVistaOrGreater;

  SetLength(Pfad, MAX_PATH+1);
  SHGetFolderPathW(0, CSIDL_APPDATA, 0, 0, @Pfad[1]);
  //  ConfigFileName := UTF8Encode(WideCharToString(@Pfad[1])) + '\FensterTools\Config.cfg';
  AppDataDir := StrPas(PWideChar(@Pfad[1])) + UTF8Decode('\FensterTools');
  if not DirectoryExists(AppDataDir) then raise Exception.Create('Konfigurations-Verzeichnis ' + AppDataDir + ' existiert nicht. Ist die Installation fehlerhaft?');
  ConfigFileName := AppDataDir + UTF8Decode('\Config.json');
  ClipboardIndexFilename := AppDataDir + UTF8Decode('\ClipboardIndex.bin');
  ClipboardDataFilename := AppDataDir + UTF8Decode('\ClipboardData.bin');
  //ClipboardTmpFilename := AppDataDir + '\Clipboard.tmp';
  LogFileName := AppDataDir +  UTF8Decode('\Log.txt');
  //if not DirectoryExists(LogFileBaseName) then CreateDir(LogFileBaseName);
  //LogFileExtension :=  UTF8Decode('.txt');
  Filename_HiddenWindows := AppDataDir + UTF8Decode('\HiddenWindows.json');
  UpdateTmpFilename := AppDataDir + UTF8Decode('\Update.exe');
end.
