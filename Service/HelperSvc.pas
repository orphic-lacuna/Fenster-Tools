program HelperSvc;

{$mode objfpc}{$H+}


uses
  Classes, Windows, Sysutils, logging, FileUtil, NamedPipes,
  WinService, md5, {$IfDef Win64}PatchFunctions64{$Else}PatchFunctions32{$EndIf}, ToolBox, Updater, HelperSvcDefinitions,
  Typ, typinfo, ToolBoxServices;

{$IfNDef Debug}
  {$I MainExecutableHashes.inc}
{$EndIf}

type
  { TMyApp }

  TMyApp = class(TInterfacedObject, IServiceApp)
  private
    pipe: TNamedPipeServer;
    //stopEvent: HANDLE;
    LastCmd: THelperSvcCommand;

    fTerminated: Boolean;
    overlappedIOList: TOverlappedIOList;
    procedure ExecuteCmd(const cmd: THelperSvcCommand; Data: Pointer = nil{; DataLen: DWord = 0});
    function VerifyPipeClient(aFile: UnicodeString): Boolean;
    procedure ClientConnected(Sender: TNamedPipeServer);
    procedure PipeCommandReceived(Sender: TOverlappedIO);
    procedure PipeDataReceived(Sender: TOverlappedIO);
    procedure PipeDataWritten(Sender: TOverlappedIO);

  public
    procedure Stop;
    procedure Main(Sender: TWinService);
  end;

var
  MyApp: TMyApp;

{ TMyApp }

procedure TMyApp.ClientConnected(Sender: TNamedPipeServer);
begin
  if VerifyPipeClient(Sender.GetClientExePath) then
  begin
    {$IfDef Debug}
    Log('Client connected, verification skipped (DebugMode)');
    {$Else}
    Log('Client connected and verified');
    {$EndIf}
    overlappedIOList.Add(pipe.Read(SizeOf(THelperSvcCommand), @PipeCommandReceived));
  end else
  begin
    Log('Client connected, could not be verified');
    Sender.Close;
    Stop;
  end;
end;

procedure TMyApp.PipeCommandReceived(Sender: TOverlappedIO);
begin
  if Sender.IOResult = iorCompleted then
  begin
    LastCmd := PHelperSvcCommand(Sender.Buf)^;
    if LastCmd.DataLength = 0 then
    begin
      Log('PipeCommandReceived, completed.');
      ExecuteCmd(LastCmd);
      overlappedIOList.Add(pipe.Read(SizeOf(THelperSvcCommand), @PipeCommandReceived));
    end else begin
      Log('PipeCommandReceived, waiting for data ('+IntToStr(LastCmd.DataLength)+' Bytes)...');
      overlappedIOList.Add(pipe.Read(LastCmd.DataLength, @PipeDataReceived));
    end;
  end else begin
    Log('PipeCommandReceived, failed. This instance will quit now.');
    pipe.Close;
    //overlappedIOList.Add(pipe.Open);
    Stop;
  end;
end;

procedure TMyApp.PipeDataReceived(Sender: TOverlappedIO);
begin
  if Sender.IOResult = iorCompleted then
  begin
    Log('PipeDataReceived, completed.');

    ExecuteCmd(LastCmd, Sender.Buf{, Sender.BufSize});
    overlappedIOList.Add(pipe.Read(SizeOf(THelperSvcCommand), @PipeCommandReceived));
  end else begin
    Log('PipeDataReceived, failed. This instance will quit now.');
    pipe.Close;
    //overlappedIOList.Add(pipe.Open);
    Stop;
  end;
end;

procedure TMyApp.PipeDataWritten(Sender: TOverlappedIO);
begin
  if Sender.IOResult = iorCompleted then
  begin
    Log('PipeDataWritten, completed.');
  end else Log('PipeDataWritten, failed.');
end;

procedure TMyApp.Stop;
begin
  fTerminated := true;
  overlappedIOList.CancelWait;
end;

         {
procedure TMyApp.FetchCmd;
var
  oio: TOverlappedIO;
begin
  Log('FetchCmd');
  if pipe.Connected then
  begin
    Log('--> Pipe is connected, read it');
    overlappedIOList.WaitForEvents;
    if oio.WaitFor([stopEvent], 1) then
    begin
      if oio.IOResult = iorCompleted then ExecuteCmd
      else if oio.IOResult = iorBrokenPipe then
      begin
        Log('--> Client closed the connection');
        pipe.Close; // client closed the pipe
      end;
    end else Log('--> Caught stop event');
    oio.Free;
  end else
  begin
    Log('Connecting the pipe');
    overlappedIOList.Add(pipe.Open);
    Log('--> Waiting for inbound connection');
    if overlappedIOList.WaitForEvents then
    begin
      Log('--> Got connection');
      //if not VerifyPipeClient(pipe.GetClientExePath) then pipe.Close;
    end else Log('--> Caught stop event');
  end;
end;   }

procedure TMyApp.ExecuteCmd(const cmd: THelperSvcCommand; Data: Pointer);
var
  r: THelperSvcCommandResponse;
  str: UnicodeString;
begin
  {$IfDef Debug}
  Log('Got a COMMAND: ' + GetEnumName(TypeInfo(THelperSvcCommandType), Ord(cmd.Command)));
  {$EndIf}
  r.CommandID := cmd.CommandID;
  r.Command := cmd.Command;
  case cmd.Command of
    ctRestartElevated: begin
      r.Result := CreateProcessUnderActiveSession(pipe.GetClientExePath, '/restarted', cpusActiveUser, true);
    end;
    ctInstallUpdate: begin
      Log('ctInstallUpdate');
      SetLength(str, cmd.DataLength div 2);
      CopyMemory(@str[1], Data, cmd.DataLength);
      Log('str restored as follows: ', false);
      Log(str);
      r.Result := TUpdater.InstallUpdate(str, cmd.VerySilent);
      Log('TUpdater.InstallUpdate result is: ' + BoolToStr(r.Result, true));
      if r.Result then
      begin
        Log('Updater process forked. This instance will quit now.');
        Stop; // service must be stopped, if update is going to be installed
      end;
    end;
    ctPatch: begin
      r.PID := cmd.PID;
      case cmd.HookFunc of
        hfSetWindowsHookEx: r.Result := PatchSetWindowsHookEx(cmd.PID, cmd.CallbackWnd, cmd.CallbackMsg);
        hfGetClipboardData: r.Result := PatchGetClipboardData(cmd.PID, cmd.CallbackWnd, cmd.CallbackMsg);
      end;
    end;
    ctUnpatch: begin
      r.Result := true;
      r.PID := cmd.PID;
      case cmd.HookFunc of
        hfSetWindowsHookEx: UnpatchSetWindowsHookEx(cmd.PID);
        hfGetClipboardData: UnpatchGetClipboardData(cmd.PID);
      end;
    end;
    {ctVerifyPatch: begin
      case cmd.HookFunc of
        hfSetWindowsHookEx: r.Result := VerifySetWindowsHookEx(cmd.PID, cmd.CallbackWnd, cmd.CallbackMsg);
        hfGetClipboardData: r.Result := VerifyGetClipboardData(cmd.PID, cmd.CallbackWnd, cmd.CallbackMsg);
      end;
    end;}
  end;
  overlappedIOList.Add(pipe.Write(r, SizeOf(r), @PipeDataWritten));
end;

function TMyApp.VerifyPipeClient(aFile: UnicodeString): Boolean;
var
  i: Integer;
  file_md5: TMD5Digest;
begin
  {$IfDef Debug}
    Result := true;
  {$Else}
    Result := false;
    file_md5 := MD5File(UTF8Encode(aFile));
    for i := Low(FILE_HASHES) to High(FILE_HASHES) do
    begin
      if MD5Match(file_md5, FILE_HASHES[i]) then
      begin
        Result := true;
        break;
      end;
    end;
  {$EndIf}
end;

procedure TMyApp.Main(Sender: TWinService);
begin
  //stopEvent := TWinService(Sender).StopEvent;
  //overlappedIOList.ControlEvent := stopEvent;
  fTerminated := false;
  overlappedIOList := TOverlappedIOList.Create;

  Sender.SetStatus;
  log('Set status to SERVICE_START_PENDING, Checkpoint 2');

  Log('Creating NamedPipeServer "'+ HelperSvcPipename + '"');
  pipe := TNamedPipeServer.Create(HelperSvcPipename);
  pipe.OnClientConnected := @ClientConnected;

  Sender.SetStatus;
  log('Set status to SERVICE_START_PENDING, Checkpoint 3');

  //Updater := TUpdater.Create;

  Sender.SetStatus;
  log('Set status to SERVICE_START_PENDING, Checkpoint 4');

  overlappedIOList.Add(pipe.Open);

  log('Set status to SERVICE_RUNNING');
  Sender.SetStatus(SERVICE_RUNNING);

  Log('Entering main loop: ' + IntToHex(PtrUInt(Sender), 16));
  while not fTerminated do
  begin
    overlappedIOList.WaitForEvents;
  end;
  Log('SvcMain ende');
  //Log('SvcMain');

  //Updater.Free;
  pipe.Free;
  overlappedIOList.Free;
end;

begin
  ExitCode := 0;
  MyApp := TMyApp.Create;

  if ParamCount > 0 then Write('Initializing ... ');
  Service.Initialize(UTF8Decode(HelperSvcName), UTF8Decode(HelperSvcDisplayName), UTF8Decode(HelperSvcDescription), MyApp);
  if ParamCount > 0 then WriteLn('+OK');


  if (lowercase(ParamStr(1)) = '-i') or (lowercase(ParamStr(1)) = '--install') then
  begin
    Write('Registering service ... ');
    if not Service.Install then
    begin
      WriteLn('-FAILED');
      ExitCode := 1;
    end else WriteLn('+OK');
  end else if (lowercase(ParamStr(1)) = '-u') or (lowercase(ParamStr(1)) = '--uninstall') then
  begin
    Write('Unregistering service ... ');
    if not Service.Uninstall then
    begin
      WriteLn('-FAILED');
      ExitCode := 1;
    end else WriteLn('+OK');
  end else begin
    if not Service.Run then WriteLn('You must start this application as a service, not as a console application');
  end;

  //if ParamCount > 0 then ReadLn;

  // since interfaced objects are reference counted, Free would result in an access violation
  // cause IMainApp in ServiceObject would try to call ._Release on the freed object
  //MyApp.Free;
end.

