unit ToolBoxServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, JwaWindows, FileUtil{$IfDef ServiceMode}, logging{$EndIf};

type TCreateProcessUserType = (cpusActiveUser, cpusSystemAccount);
function CreateProcessUnderActiveSession(aExeFile, Parameters: UnicodeString; aUserType: TCreateProcessUserType; UseElevatedToken: Boolean = false): Boolean;
function GetCurrentUserToken(UseElevatedTokenIfAvailable: Boolean = false): HANDLE;
function GetActiveSessionID: DWORD;

implementation

function GetActiveSessionID: DWORD;
var
  sessInfo, sessInfo_ori: PWTS_SESSION_INFOW;
  count: DWORD;
  i: Integer;
begin
  Result := 0; count := 0;
  if not WTSEnumerateSessionsW(WTS_CURRENT_SERVER_HANDLE, 0, 1, sessInfo{%H-}, count) then exit;
  sessInfo_ori := sessInfo;
  for i := 0 to count-1 do
  begin
    if sessInfo^.State = WTSActive then
    begin
      Result := sessInfo^.SessionId;
      break;
    end;
    Inc(sessInfo);
  end;
  WTSFreeMemory(sessInfo_ori);
end;

function GetCurrentUserToken(UseElevatedTokenIfAvailable: Boolean): HANDLE;
var
  count: DWORD;
  token: HANDLE;
  linkedToken: Handle;
begin
  Result := INVALID_HANDLE_VALUE;
  if WTSQueryUserToken(GetActiveSessionID, token{%H-}) then
  begin
    //Log('DuplicateTokenEx');
    //if DuplicateTokenEx(token, TOKEN_ASSIGN_PRIMARY or TOKEN_ALL_ACCESS, nil, SecurityImpersonation, TokenPrimary, Result) then
      //Log('Weve got the token. YEAH')
    //else
      //Log('UPS. Weve got no token.');

    // get the linked token, if one exists (that is the elevated token, to create an elevated process)
    if UseElevatedTokenIfAvailable and GetTokenInformation(token, _TOKEN_INFORMATION_CLASS(TokenLinkedToken), @linkedToken, SizeOf(linkedToken), count{%H-}) then
    begin
      {$IfDef ServiceMode}Log('Got linked token.');{$EndIf}
      Result := linkedToken;
    end else
    begin
      {$IfDef ServiceMode}Log(IntToStr(GetLastError)+' did not get linked token, use normal one.');{$EndIf}
      Result := token;
    end;
  end{$IfDef ServiceMode}else Log('WTSQueryUserToken failed'){$EndIf};
end;

function GetSystemAccountToken: HANDLE;
var
  hSnap, hProcess, hToken: HANDLE;
  procEntry: PROCESSENTRY32W;
  active_session_id, session_id, winlogon_PID: DWORD;
  hPrimaryToken: HANDLE;
begin
  Result := INVALID_HANDLE_VALUE;
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnap = INVALID_HANDLE_VALUE then
  begin
    {$IfDef ServiceMode}Log('CreateToolhelp32Snapshot failed with ' + IntToStr(GetLastError));{$EndIf}
    exit;
  end;

  procEntry.dwSize := SizeOf(procEntry);
  if not Process32FirstW(hSnap, procEntry) then
  begin
    {$IfDef ServiceMode}Log('Process32FirstW failed with ' + IntToStr(GetLastError));{$EndIf}
    exit;
  end;

  active_session_id := GetActiveSessionID;
  winlogon_PID := 0;
  repeat
    if UnicodeString(procEntry.szExeFile) = UTF8Decode('winlogon.exe') then
    begin
      if ProcessIdToSessionId(procEntry.th32ProcessID, session_id{%H-}) then
      begin
        if active_session_id = session_id then
        begin
          winlogon_PID := procEntry.th32ProcessID;
          break;
        end;
      end;
    end;
  until not Process32NextW(hSnap, procEntry);

  if winlogon_PID = 0 then
  begin
    {$IfDef ServiceMode}Log('Could not find winlogon.exe instance in currently active session');{$EndIf}
    exit;
  end;

  hProcess := OpenProcess(MAXIMUM_ALLOWED, false, winlogon_PID);
  if not OpenProcessToken(hProcess, {TOKEN_ADJUST_PRIVILEGES or TOKEN_ADJUST_SESSIONID or TOKEN_WRITE or }TOKEN_QUERY or TOKEN_DUPLICATE or TOKEN_ASSIGN_PRIMARY or TOKEN_READ, hToken{%H-}) then
  begin
    {$IfDef ServiceMode}Log('OpenProcessToken failed with ' + IntToStr(GetLastError));{$EndIf}
    exit;
  end;

  {$IfDef ServiceMode}Log('Before DuplicateTokenEx');{$EndIf}
  if not DuplicateTokenEx(hToken, MAXIMUM_ALLOWED, nil, SecurityIdentification, TokenPrimary, hPrimaryToken{%H-}) then
  begin
    {$IfDef ServiceMode}Log('DuplicateTokenEx failed with ' + IntToStr(GetLastError));{$EndIf}
    CloseHandle(hToken);
    CloseHandle(hProcess);
    exit;
  end;

  CloseHandle(hToken);
  CloseHandle(hProcess);
  Result := hPrimaryToken;
end;

function CreateProcessUnderActiveSession(aExeFile, Parameters: UnicodeString; aUserType: TCreateProcessUserType; UseElevatedToken: Boolean): Boolean;
var
  token, user_token: HANDLE;
  startupinf: STARTUPINFOW;
  process_info: PROCESS_INFORMATION;
  environment: LPVOID;
  cmd_line: UnicodeString;
begin
  Result := false;
  if not FileExists(aExeFile) then exit;
  if aUserType = cpusActiveUser then
  begin
    token := GetCurrentUserToken(UseElevatedToken);
    user_token := token;
  end else //if aUserType = cpusSystemAccount then
  begin
    token := GetSystemAccountToken;
    user_token := GetCurrentUserToken(false);
  end;
  if (token = INVALID_HANDLE_VALUE) or (user_token = INVALID_HANDLE_VALUE) then
  begin
    {$IfDef ServiceMode}Log('Got an invalid token');{$EndIf}
    exit;
  end;
  ZeroMemory(@startupinf, sizeof(startupinf));
  startupinf.cb := sizeof(startupinf);
  startupinf.lpDesktop:='winsta0\default';

  if not CreateEnvironmentBlock(@environment, user_token, false) then
  begin
    {$IfDef ServiceMode}Log('CreateEnvironmentBlock failed with ' + IntToStr(GetLastError));{$EndIf}
    exit;
  end;

  if Parameters <> '' then cmd_line := UTF8Decode('"')+aExeFile+UTF8Decode('" ')+Parameters else cmd_line := '';
  //geht, aber Process crashes: if CreateProcessAsUserW(token, PWideChar(x), nil, nil, nil, false, CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS, nil, nil, startupinf, process_info) then
  //if CreateProcessWithLogonW('Adrian', nil, 'Laptop2010', LOGON_WITH_PROFILE, PWideChar(x), nil, CREATE_SUSPENDED or NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT, environment, nil, startupinf, process_info) then
  //if CreateProcessWithTokenW(token, LOGON_WITH_PROFILE, PWideChar(x), nil, CREATE_SUSPENDED or NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT or CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP, environment, nil, @startupinf, @process_info) then
  if CreateProcessAsUserW(token, PWideChar(aExeFile), PWideChar(cmd_line), nil, nil, false, CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT, environment, nil, startupinf, process_info{%H-}) then
  begin
    {$IfDef ServiceMode}Log('Successfully forked new Process with PID ' + IntToStr(process_info.dwProcessId));{$EndIf}
    Result := true;
  end{$IfDef ServiceMode}else Log('CreateProcessAsUserW failed with ' + IntToStr(GetLastError)){$EndIf};

  DestroyEnvironmentBlock(environment);
  CloseHandle(token);
  if user_token <> token then CloseHandle(user_token);
end;

end.

