unit ServiceConnection;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, Dialogs, ServiceManager, JwaWindows, JwaWinSvc, FensterToolsCommon, ToolBox, NamedPipes{$IfDef Logging}, Logging, typinfo{$EndIf}, ExtendedThreads, HelperSvcDefinitions;

type

  TOnInstallUpdateResultReceived = procedure(UpdateInstallationReady: Boolean) of object;

  { TServiceConnection }

  TServiceConnection = class(TBasicThreadWithCallbackWnd)
  private
    fOnConnectionEstablished: TNotifyEvent;
    fOnInstallUpdateResultReceived: TOnInstallUpdateResultReceived;

    // Thread-specific
    fSvcMan: TServiceManager;
    pipe32, pipe64: TNamedPipeClient;
    cmd: THelperSvcCommand;
    //stopEvent: HANDLE;
    overlappedIOList: TOverlappedIOList;

    procedure StartHelperServices;
    //function Connect32: Boolean;
    //function Connect64: Boolean;
    //function Transact(pipe: TNamedPipeClient): Boolean;
    procedure WriteToPipe(pipe: TNamedPipeClient; Data: Pointer = nil);
    procedure OnPipeRead(Sender: TOverlappedIO);

    // callback procs
    procedure OnRestartElevatedSucceeded;
    procedure DoOnInstallUpdateResultReceived(UpdateInstallationReady: Boolean);
    procedure DoOnConnectionEstablished;
  protected
    procedure ThreadInternalCreate; override;
    procedure ThreadInternalMain; override;
    procedure ThreadInternalDestroy; override;

    procedure BeforeDestruction; override;
  public
    property OnConnectionEstablished: TNotifyEvent read fOnConnectionEstablished write fOnConnectionEstablished;
    property OnInstallUpdateResultReceived: TOnInstallUpdateResultReceived read fOnInstallUpdateResultReceived write fOnInstallUpdateResultReceived;
    procedure InstallUpdate(VerySilent: Boolean);
    procedure RestartApplicationElevated;
    procedure Patch(Mode: THelperSvcCommandType; Func: THookFuncType; PID: DWORD; CallbackWnd: HWND = 0; CallbackMsg: Cardinal = 0);

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  HelperSvc: TServiceConnection;

implementation

{ TServiceConnection }

constructor TServiceConnection.Create;
begin
  overlappedIOList := TOverlappedIOList.Create;
  inherited Create;
end;

procedure TServiceConnection.ThreadInternalCreate;
begin
  inherited ThreadInternalCreate;

  fTerminated := false;
  cmd.CommandID := 0;

  {$IfDef Logging}Logger.RegisterThread('ServiceConnection');{$EndIf}

  //stopEvent := CreateEvent(nil, true, false, nil);
  //overlappedIOList.ControlEvent := stopEvent;

  fSvcMan := TServiceManager.Create(nil);
  try
    fSvcMan.Access := SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE;
    fSvcMan.Connect;
  except
    {$IfDef Logging}Logger.Add('Could not open service manager', LT_ERROR);{$EndIf}
  end;

  if fSvcMan.Connected then StartHelperServices;

  if pipe32 <> nil then overlappedIOList.Add(pipe32.Read(SizeOf(THelperSvcCommandResponse), @OnPipeRead));
  if WindowsVersion.Is64Bit and (pipe64 <> nil) then overlappedIOList.Add(pipe64.Read(SizeOf(THelperSvcCommandResponse), @OnPipeRead));
end;

procedure TServiceConnection.ThreadInternalMain;
begin
  while not fTerminated do
  begin
    overlappedIOList.WaitForEvents;
  end;
end;

procedure TServiceConnection.StartHelperServices;
var
  ss: TServiceStatus;
  timestamp: UInt64;
begin
  {$IfDef Logging}Logger.Add('Starting 32-bit Helper Service ...');{$EndIf}
  try
    fSvcMan.GetServiceStatus(HelperSvcName32, ss{%H-});
    if (ss.dwCurrentState = SERVICE_STOPPED) or (ss.dwCurrentState = SERVICE_STOP_PENDING) then fSvcMan.StartService(HelperSvcName32, nil);
  except on E: Exception do
    begin
      {$IfDef Logging}Logger.Add('Error querying status of ' + HelperSvcName32 + ': ' + E.Message, LT_ERROR);{$EndIf}
      {$Message Warn 'Notify user about corrupted installation'}
      exit;
    end;
  end;

  {$IfDef Logging}try{$EndIf}
    pipe32 := TNamedPipeClient.Create(HelperSvcPipename32);
  {$IfDef Logging}except
    Logger.Add('Could not create pipe32', LT_ERROR);
  end;{$EndIf}

  if WindowsVersion.Is64Bit then
  begin
    {$IfDef Logging}Logger.Add('Starting 64-bit Helper Service ...');{$EndIf}
    try
      fSvcMan.GetServiceStatus(HelperSvcName64, ss);
      if (ss.dwCurrentState = SERVICE_STOPPED) or (ss.dwCurrentState = SERVICE_STOP_PENDING) then fSvcMan.StartService(HelperSvcName64, nil);
    except on E: Exception do
      begin
        {$IfDef Logging}Logger.Add('Error querying status of ' + HelperSvcName64 + ': ' + E.Message, LT_ERROR);{$EndIf}
        {$Message Warn 'Notify user about corrupted installation'}
        exit;
      end;
    end;

    {$IfDef Logging}try{$EndIf}
      pipe64 := TNamedPipeClient.Create(HelperSvcPipename64);
    {$IfDef Logging}except
      Logger.Add('Could not create pipe64', LT_ERROR);
    end;{$EndIf}

    timestamp := GetTickCount64;
    repeat
      sleep(1000);
      fSvcMan.GetServiceStatus(HelperSvcName64, ss);
      {$IfDef Logging}Logger.Add('Waiting for start of 64-bit Helper service ['+IntToStr((GetTickCount64-timestamp)div 1000)+']');{$EndIf}
    until (GetTickCount64-timestamp > 60000) or (ss.dwCurrentState = SERVICE_RUNNING);
    sleep(1000);

    {$IfDef Logging}if {$EndIf}pipe64.Open {$IfDef Logging}then Logger.Add('Pipe (64 bit) successfully opened.') else Logger.Add('Could not open pipe (64 bit).', LT_ERROR);{$EndIf}
  end;

  timestamp := GetTickCount64;
  repeat
    sleep(1000);
    fSvcMan.GetServiceStatus(HelperSvcName32, ss);
    {$IfDef Logging}Logger.Add('Waiting for start of 32-bit Helper service ['+IntToStr((GetTickCount64-timestamp)div 1000)+']');{$EndIf}
  until (GetTickCount64-timestamp > 60000) or (ss.dwCurrentState = SERVICE_RUNNING);
  sleep(1000);

  if pipe32.Open then
  begin
    {$IfDef Logging}Logger.Add('Pipe (32 bit) successfully opened.');{$EndIf}
    DoCallbackProc(@DoOnConnectionEstablished);
  end {$IfDef Logging}else Logger.Add('Could not open pipe (32 bit).', LT_ERROR){$EndIf};
end;

procedure TServiceConnection.WriteToPipe(pipe: TNamedPipeClient; Data: Pointer);
begin
  {$IfDef Debug}
  if pipe = nil then raise Exception.Create('WriteToPipe was called with invalid pipe instance');
  {$EndIf}

  overlappedIOList.Add(pipe.Write(cmd, SizeOf(cmd), nil), true);
  if Data <> nil then
    overlappedIOList.Add(pipe.Write(Data^, cmd.DataLength, nil), true);

  Inc(cmd.CommandID);
end;

procedure TServiceConnection.OnPipeRead(Sender: TOverlappedIO);
var
  cmdResponse: ^THelperSvcCommandResponse;
  invalidResponse: Boolean;
begin
  {$IfDef Debug}if GetCurrentThreadID <> FThreadID then raise Exception.Create('TServiceConnection.OnPipeRead was called outside the service connection thread.');{$EndIf}

  if Sender.IOResult = iorCompleted then
  begin
    cmdResponse := Sender.Buf;
    invalidResponse := false;
    case cmdResponse^.Command of
      ctPatch: begin
          {$IfDef Logging}
          if cmdResponse^.Result then
            Logger.Add('Patching of "' + GetModuleFileName(cmdResponse^.PID) + '" (' + IntToStr(cmdResponse^.PID) + ') succeeded. (CommandID: ' + IntToStr(cmdResponse^.CommandID) + ')')
          else
            Logger.Add('Patching of "' + GetModuleFileName(cmdResponse^.PID) + '" (' + IntToStr(cmdResponse^.PID) + ') failed. (CommandID: ' + IntToStr(cmdResponse^.CommandID) + ')', LT_WARNING);
          {$EndIf};
        end;
      ctUnpatch: begin
        {$IfDef Logging}
        if cmdResponse^.Result then
          Logger.Add('Unpatching of "' + GetModuleFileName(cmdResponse^.PID) + '" (' + IntToStr(cmdResponse^.PID) + ') succeeded. (CommandID: ' + IntToStr(cmdResponse^.CommandID) + ')')
        else
          Logger.Add('Unpatching of "' + GetModuleFileName(cmdResponse^.PID) + '" (' + IntToStr(cmdResponse^.PID) + ') failed. (CommandID: ' + IntToStr(cmdResponse^.CommandID) + ')', LT_WARNING);
        {$EndIf};
        end;
      ctRestartElevated: begin
        {$IfDef Logging}Logger.Add('ctRestartElevated result received: ' + BoolToStr(cmdResponse^.Result, true));{$EndIf}
        DoCallbackProc(@OnRestartElevatedSucceeded);
      end;
      ctInstallUpdate: begin
        {$IfDef Logging}Logger.Add('ctInstallUpdate: ' + BoolToStr(cmdResponse^.Result, true));{$EndIf}
        //if cmdResponse^.Result and WindowsVersion.Is64Bit then fSvcMan.StopService(HelperSvcName64, false);
        DoCallbackProc(@DoOnInstallUpdateResultReceived, Boolean(cmdResponse^.Result));
      end;
      else begin
        {$IfDef Logging}Logger.Add('Got invalid response from helper service.', LT_ERROR);{$EndIf}
        invalidResponse := true;
      end;
    end;
  end else if Sender.IOResult = iorBrokenPipe then
  begin
    invalidResponse := true;
    {$IfDef Logging}Logger.Add('Service pipe is broken', LT_ERROR);{$EndIf}
  end else if Sender.IOResult = iorCancelled then
  begin
    invalidResponse := true;
    {$IfDef Logging}Logger.Add('Service pipe read operation cancelled', LT_ERROR);{$EndIf}
  end else if Sender.IOResult = iorUnknownError then
  begin
    invalidResponse := true;
    {$IfDef Logging}Logger.Add('Unknown error in service pipe', LT_ERROR);{$EndIf}
  end;

  // if response was valid, start next pipe read operation
  if not invalidResponse then
    overlappedIOList.Add(TNamedPipeClient(Sender.Owner).Read(SizeOf(THelperSvcCommandResponse), @OnPipeRead));
end;

procedure TServiceConnection.OnRestartElevatedSucceeded;
begin
  {$IfDef Logging}Logger.Add('Application restarted elevated. This instance will quit now.');{$EndIf}
  Application.Terminate;
end;

procedure TServiceConnection.DoOnInstallUpdateResultReceived(UpdateInstallationReady: Boolean);
begin
  if Assigned(fOnInstallUpdateResultReceived) then fOnInstallUpdateResultReceived(UpdateInstallationReady);
  if UpdateInstallationReady then Application.Terminate;
end;

procedure TServiceConnection.DoOnConnectionEstablished;
begin
  if Assigned(fOnConnectionEstablished) then fOnConnectionEstablished(Self);
end;

procedure TServiceConnection.InstallUpdate(VerySilent: Boolean);
begin
  cmd.Command := ctInstallUpdate;
  cmd.VerySilent := VerySilent;
  cmd.DataLength := SizeOf(UnicodeChar)*Length(UpdateTmpFilename);
  {$IfDef Logging}Logger.Add('Sending install update request to 32 bit Helper Service, CommandID=' + IntToStr(cmd.CommandID));{$EndIf}
  WriteToPipe(pipe32, @UpdateTmpFilename[1]);
end;

procedure TServiceConnection.RestartApplicationElevated;
begin
  cmd.Command := ctRestartElevated;
  {$IfDef Logging}Logger.Add('Sending restart request to 32 bit Helper Service, CommandID=' + IntToStr(cmd.CommandID));{$EndIf}
  WriteToPipe(pipe32);
end;

procedure TServiceConnection.Patch(Mode: THelperSvcCommandType;
  Func: THookFuncType; PID: DWORD; CallbackWnd: HWND; CallbackMsg: Cardinal);
begin
  {$IfDef Debug}if (Mode <> ctPatch) and (Mode <> ctUnpatch) then raise Exception.Create('TServiceConnection.Patch was called with invalid Mode');{$EndIf}
  cmd.Command := Mode;
  cmd.HookFunc := Func;
  cmd.PID := PID;
  cmd.CallbackMsg := CallbackMsg;
  cmd.CallbackWnd := CallbackWnd;
  if not IsProcess64BitPID(PID) then
  begin
    {$IfDef Logging}Logger.Add('Sending patch request to 32 bit Helper Service: ' + GetEnumName(TypeInfo(THelperSvcCommandType), Ord(Mode)) + ', ' + GetModuleFileName(PID) + ' (' + IntToStr(PID) + '), CommandID=' + IntToStr(cmd.CommandID));{$EndIf}
    WriteToPipe(pipe32);
  end else begin
    {$IfDef Logging}Logger.Add('Sending patch request to 64 bit Helper Service: ' + GetEnumName(TypeInfo(THelperSvcCommandType), Ord(Mode)) + ', ' + GetModuleFileName(PID) + ' (' + IntToStr(PID) + '), CommandID=' + IntToStr(cmd.CommandID));{$EndIf}
    WriteToPipe(pipe64);
  end;
end;

procedure TServiceConnection.ThreadInternalDestroy;
begin
  //for o in overlappedIOList do o.Free;
  //CloseHandle(stopEvent);

  {$IfDef Logging}Logger.EndThread;{$EndIf}

  TryFreeAndNil(pipe32);
  if WindowsVersion.Is64Bit then TryFreeAndNil(pipe64);

  fSvcMan.Disconnect;
  fSvcMan.Free;

  inherited ThreadInternalDestroy;
end;

procedure TServiceConnection.BeforeDestruction;
begin
  fTerminated := true; // ThreadMain - Schleife soll enden
  overlappedIOList.CancelWait; // laufendes Warten mittels Event abbrechen
  inherited BeforeDestruction; // ruft StopThread auf und funktioniert, da overlappedIOList.CancelWait das laufende Warten abgebrochen hat;
end;

destructor TServiceConnection.Destroy;
begin
  overlappedIOList.Free; // aufräumen
  inherited Destroy;
end;

end.

