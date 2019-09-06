program FensterTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Windows, Unit1, FensterToolsCommon,
  Classes, Sysutils, AdminCheck,
  ShellNotification, Replacer, KineticScrolling,
  LivePatching,
  Regeleditor,
  {$IfDef Logging}DebugConsole,{$EndIf}
  WindowManager, ApplicationRestart, SettingsBaseTypes,
  SettingsBoundaries, ServiceConnection, KeyPatternMatching,
  MouseBehaviour, ToolBoxServices, Clipboard.Notifications, Clipboard.Manager, Clipboard.Item.Header, Clipboard.Item.Body, Hotkey.Actions,
  Hotkey.Manager, Hotkey.List, ClipboardInspector, BasicWindow, AppFileInfo, BasicAnimatedWindow, LayeredWindowRenderer,
  ClipboardStatusWindow, ClipboardStatusWindow.Renderer, ClipboardWindow.Settings, SettingsDerivedBaseTypes, unit2;

{$R *.res}

var
  SingleInstanceMutex: Cardinal;
  WaitResultBefore: boolean = false;

function WaitForOtherInstance(DontWait: boolean = false; DoHalt: boolean = false): boolean;
var
  WaitResult: Cardinal;
  LastError: Cardinal;
begin
  if (WaitResultBefore) then
  begin
    Result := true;
    exit;
  end;

  SingleInstanceMutex := CreateMutex(nil, True, 'FensterTools');
  // WaitForSingleObject waits until the Mutex is SIGNALED that means
  // no thread owns this mutex
  // if the mutex already exists (another instance is running) we automatically don't own
  // the mutex despite of InitialOwner=True (second parameter)
  // but we recieve a valid handle to the mutex, we can use in WaitForSingleObject
  // the Mutex is unowned, when the other instance terminates
  // Problem: If we own the mutex, WaitForSingleObject will never return (deadlock),
  // so we must only WaitFor if we couldn't create AND OWN the mutex successfully
  // in this case GetLastError would be <> 0
  LastError := GetLastError;
  if (LastError <> 0) then
  begin
    // Mutex is owned by another running instance
    if DontWait then
      Result := false
    else begin
      WaitResult := WaitForSingleObject(SingleInstanceMutex, 60000);
      if (WaitResult = WAIT_TIMEOUT) or (WaitResult = WAIT_FAILED) then
        Result := false
      else
        Result := true;
    end;
  end else Result := true;
  WaitResultBefore := Result;
  if not Result and DoHalt then halt;
end;

function MustRestartAsNormalUser: Boolean;
begin
  Result := (lowercase(ParamStr(1)) = '/updated') and (GetCurrentUserToken <> INVALID_HANDLE_VALUE);
end;

function GetAllParameters: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Paramcount do
  begin
    Result := Result + ParamStr(i) + ' ';
  end;
end;

begin
  if MustRestartAsNormalUser then
  begin
    if CreateProcessUnderActiveSession(ParamStr(0), GetAllParameters, cpusActiveUser) then halt;
  end;
  // advises the RTL to treat all standard strings as UTF8 (necessary especially for streaming JSON in UTF8)
  SetMultiByteConversionCodePage(CP_UTF8);


  {$IfNDef ReleaseMode}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$EndIf}

  WaitForOtherInstance(false, true);

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(Tfrm_DebugConsole, frm_DebugConsole);
  Application.CreateForm(Tfrm_Settings, frm_Settings);
  Application.CreateForm(Tfrm_Regeleditor, frm_Regeleditor);
  Application.CreateForm(Tfrm_ClipboardInspector, frm_ClipboardInspector);
  {$IfDef Logging}{$EndIf}
  Application.Run;
end.



