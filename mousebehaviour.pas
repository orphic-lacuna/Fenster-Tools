unit MouseBehaviour;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UserInputProvider, SettingsManager, SettingsBaseTypes, Windows, KineticScrolling, fpjson, ExtendedThreads{$IfDef Logging}, Logging{$EndIf}, typinfo;

type

  { TMouseBehaviourManager }

  TMiddleMouseButtonMode = (MMBM_NONE, MMBM_OPEN_CLIPBOARD_MENU, MMBM_DOUBLE_CLICK);

  TMouseBehaviourManager = class
  private
    fMiddleMouseButtonMode: TMiddleMouseButtonMode;
    fKineticScroller: TKineticScroller;
    procedure SettingsChanged(Sender: TObject);
    procedure MiddleMouseButton(const aUserInput: TUserInput; var PassToSystem: Boolean);
    procedure SetKineticScrollingOptions(ScrollingOptions: TJSONObject);
    //procedure SetKineticMode(Value: Boolean);

    // OS-specific scroll message functions
    procedure SendScrollMsg(const aUserInput: TUserInput; MessageIsDirectlyFromHook: Boolean; var PassToSystem: Boolean);
    procedure HandleScrollMsg(const aUserInput: TUserInput; var PassToSystem: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MouseBehaviourManager: TMouseBehaviourManager;

implementation

uses Unit1, FensterToolsCommon, ToolBox, ExtendedWinAPI;

{ TMouseBehaviourManager }

constructor TMouseBehaviourManager.Create;
begin
  Settings.Mouse.ChangeHandler.Add(@SettingsChanged);
end;

procedure TMouseBehaviourManager.SettingsChanged(Sender: TObject);
var
  filter: TUserInputFilter;
begin
  if {(Sender = Settings.Mouse.MiddleMouseBtnClipboardMenu) or }(Sender = Settings.Mouse.MiddleMouseBtnDoubleClick) then
  begin
    if Settings.Mouse.MiddleMouseBtnDoubleClick then fMiddleMouseButtonMode := MMBM_DOUBLE_CLICK
    //else if Settings.Mouse.MiddleMouseBtnClipboardMenu then fMiddleMouseButtonMode := MMBM_OPEN_CLIPBOARD_MENU
    else fMiddleMouseButtonMode := MMBM_NONE;

    if fMiddleMouseButtonMode <> MMBM_NONE then
    begin
      filter := TUserInputFilter.Create;
      filter.InputTypes := [itKey]; filter.SensitiveKeyList[VK_MBUTTON] := true;
      {$IfDef Logging}Logger.Add('Registering UserInputCallback for MouseBehaviourManager (MiddleMouseButton)');{$EndIf}
      UserInput.RegisterCallback(@MiddleMouseButton, filter);
    end else
    begin
      {$IfDef Logging}Logger.Add('Unregistering UserInputCallback for MouseBehaviourManager (MiddleMouseButton)');{$EndIf}
      UserInput.UnregisterCallback(@MiddleMouseButton);
    end;
  end else if (Sender = Settings.Mouse.ScrollWindowUnderMouse) or (Sender = Settings.Mouse.KineticScrollingEnabled) then
  begin
    if (not Settings.Mouse.KineticScrollingEnabled.Value and (not Settings.Mouse.ScrollWindowUnderMouse.Value or WindowsVersion.IsWin10OrGreater)) then
    begin
      {$IfDef Logging}Logger.Add('Unregistering UserInputCallback for MouseBehaviourManager (Scrolling)');{$EndIf}
      UserInput.UnregisterCallback(@HandleScrollMsg);
    end else
    begin
      {$IfDef Logging}Logger.Add('Registering UserInputCallback for MouseBehaviourManager (Scrolling)');{$EndIf}
      filter := TUserInputFilter.Create(true); filter.InputTypes := [itScroll, itKey];
      // prevent receiving of scrolling as itKey-event (would immediately stop scrolling)
      filter.SensitiveKeyList[VK_MOUSEWHEEL_DOWN] := false;
      filter.SensitiveKeyList[VK_MOUSEWHEEL_UP] := false;
      filter.SensitiveKeyList[VK_HMOUSEWHEEL_DOWN] := false;
      filter.SensitiveKeyList[VK_HMOUSEWHEEL_UP] := false;
      UserInput.RegisterCallback(@HandleScrollMsg, filter);
    end;

    if Settings.Mouse.KineticScrollingEnabled.Value then
    begin
      if fKineticScroller = nil then
      begin
        fKineticScroller := TKineticScroller.Create;
        fKineticScroller.OnSendScrollMsg := @SendScrollMsg;
        SettingsChanged(Settings.Mouse.KineticScrollingOptions); // newly created KineticScroller instance needs update of scrolling options
      end;
    end else begin
      TryFreeAndNil(fKineticScroller);
    end;
  end else if (Sender = Settings.Mouse.KineticScrollingOptions) then
  begin
    if fKineticScroller <> nil then
    begin
      UserInput.DoThreadProc(TThreadMethodParamObjectContainer.MethodType(@SetKineticScrollingOptions), Settings.Mouse.KineticScrollingOptions.Value.Clone);
    end;
  end;
end;

{
Windows 7				Verbessertes Scrollen		Kinetisches Scrollen		Method for scroll event propagation
-------------------------------------------------------------------------------------------------------------------
						              	nein						nein					=> no KineticScroller necessary
						              	ja							nein					=> PostMessage
						                nein						ja						=> SendInput / (Passthrough if not KineticMode)
						                ja							ja						=> PostMessage
standard behaviour: mouse wheel scrolls focus window

Windows 8		IsImmersiveProcess		Verbessertes Scrollen		Kinetisches Scrollen		Method for scroll event propagation
-----------------------------------------------------------------------------------------------------------------------------------
				                    X						nein					  	  nein					        => no KineticScroller necessary
				                    nein					ja						  	nein				        	=> PostMessage
				                    ja						ja							  nein				        	=> SendInput / Passthrough
				                    X						nein					  	  ja					        	=> SendInput / (Passthrough if not KineticMode)
				                    nein					ja						  	ja					        	=> PostMessage
				                    ja						ja						  	ja					        	=> SendInput / (Passthrough if not KineticMode)
standard behaviour: mouse wheel scrolls focus window

Windows 8.1		same as windows 8
standard behaviour: mouse wheel scrolls hovered window if it belongs to an WinRT-App, otherwise it scrolls the focus window
see: http://msdn.microsoft.com/en-us/library/windows/desktop/dn457652.aspx

Windows 10		IsImmersiveProcess		Verbessertes Scrollen		Kinetisches Scrollen		Method for scroll event propagation
-----------------------------------------------------------------------------------------------------------------------------------
					X						X							nein						=> no KineticScroller necessary
					X						X							ja							=> SendInput / (Passthrough if not KineticMode)
the option "Verbessertes Scrollen" switches the HKCU\Control Panel\Desktop\MouseWheelRouting between 2 and 0
standard behaviour: mouse wheel scrolls hovered window (if option HKEY_CURRENT_USER\Control Panel\Desktop\MouseWheelRouting = 2)
}

{See https://msdn.microsoft.com/de-de/library/windows/desktop/ms646310%28v=vs.85%29.aspx
Never use SendInput from within a hook

I experienced ma-ssive problems when using SendInput from within a global lowlevel mouse hook. If you want to test this behaviour on your own:

1. Create a global low Level mouse hook, that suppresses all WM_MOUSEWHEEL Events.
2. Within the hook procedure use SendInput to re-inject the previously suppressed WM_MOUSEWHEEL event (AND: make sure, that you only call SendInput for WM_MOUSEWHEEL events that are not injected, otherwise you get an infinite loop)

-> It will work fine (Scrolling feels normal, just smooth) under Windows 7
-> Windows 8 not tested
-> It will work fine under Windows 10, if no WinRT-Apps are open.

But if you open for e.g. Windows Store App, every scrolled line has a long delay, scrolling lags. The problem is SendInput, which takes up to 300 ms for execution if any Windows App is opened. And if you use SendInput repeatedly, at some point SendInput will start to hang completely and wait infinitely for something. You even cannot terminate your program via TaskManager. You have to reboot in this case.

Workaround: Wrap the call to SendInput into another thread. Example: Mouse-Hook (Thread A) uses PostMessage to inform Thread B, Thread B then calls SendInput. Surprisingly using this approach there is no delay, scrolling feels just smooth as always.
}

procedure TMouseBehaviourManager.SendScrollMsg(const aUserInput: TUserInput; MessageIsDirectlyFromHook: Boolean; var PassToSystem: Boolean);
  procedure SendScrollMsg_via_SendInput;
  var
    x: INPUT;
  begin
    if MessageIsDirectlyFromHook then
    begin
      {$IfDef Logging}Logger.Add('Passthrough Scrollmessage (directly from hook)');{$EndIf}
      PassToSystem := true
    end else begin
      x.typ := INPUT_MOUSE;
      x.mi.dwExtraInfo := 0;
      x.mi.time := aUserInput.Timestamp; x.mi.dwFlags := MOUSEEVENTF_WHEEL;
      {$IFDEF DEBUG}
        {$OVERFLOWCHECKS OFF}
        {$RANGECHECKS OFF}
      {$ENDIF}
      x.mi.mouseData := SmallInt(aUserInput.WheelDelta);
      {$IfDef Logging}Logger.Add('Before SendInput');{$EndIf}
      SendInput(1, @x, SizeOf(x));
      {$IfDef Logging}Logger.Add('Scrollmsg delivered via SendInput: X: ' + IntToStr(aUserInput.Point.X) + '; Y: ' + IntToStr(aUserInput.Point.Y));{$EndIf}
      PassToSystem := false;
    end;
  end;
  procedure SendScrollMsg_via_PostMessage;
  begin
    PostMessage(aUserInput.Window, WM_MOUSEWHEEL, SmallInt(aUserInput.WheelDelta) shl 16, aUserInput.Point.x + (aUserInput.Point.y shl 16));
    {$IfDef Logging}Logger.Add('Scrollmsg delivered via PostMessage: X: ' + IntToStr(aUserInput.Point.X) + '; Y: ' + IntToStr(aUserInput.Point.Y) + '; Wnd: ' + IntToStr(aUserInput.Window));{$EndIf}// + '; WndName: ' + GetWindowText(aUserInput.Window));
    PassToSystem := false;
  end;
var
  PID: DWORD;
  hProc: HANDLE;
begin
  if WindowsVersion.IsWin10OrGreater then
  begin
    SendScrollMsg_via_SendInput();
  end else if WindowsVersion.IsWin8OrGreater then
  begin
    if not Settings.Mouse.ScrollWindowUnderMouse.Value then
      SendScrollMsg_via_SendInput
    else begin
      GetWindowThreadProcessId(aUserInput.Window, PID);
      hProc := OpenProcess(PROCESS_QUERY_INFORMATION, false, PID);
      if not IsImmersiveProcess(hProc) then
      begin
        SendScrollMsg_via_PostMessage;
        {$IfDef Logging}Logger.Add('PID ' + IntToStr(PID) + ' (' + GetModuleFileName(PID) + ') is not immersive (->PostMessage)');{$EndIf}
      end else begin
        if UpperCase(GetFullProcessName(hProc)) = UTF8Decode('C:\WINDOWS\EXPLORER.EXE') then
        begin
          SendScrollMsg_via_PostMessage;
          {$IfDef Logging}Logger.Add('PID ' + IntToStr(PID) + ' (' + GetModuleFileName(PID) + ') is explorer.exe (->PostMessage)');{$EndIf}
        end else begin
          SendScrollMsg_via_SendInput;
          {$IfDef Logging}Logger.Add('PID ' + IntToStr(PID) + ' (' + GetModuleFileName(PID) + ') is immersive (->SendInput)');{$EndIf}
        end;
      end;
      CloseHandle(hProc);
    end;
  end else if WindowsVersion.IsWin7OrGreater then
  begin
    if Settings.Mouse.ScrollWindowUnderMouse.Value then
      SendScrollMsg_via_PostMessage
    else
      SendScrollMsg_via_SendInput;
  end;
end;

procedure TMouseBehaviourManager.HandleScrollMsg(const aUserInput: TUserInput; var PassToSystem: Boolean);
begin
  //{$IfDef Logging}Logger.Add('Scrollmsg: ' + GetEnumName(TypeInfo(TInputType), Ord(aUserInput.InputType)) + 'X: ' + IntToStr(aUserInput.Point.X) + '; Y: ' + IntToStr(aUserInput.Point.Y) + '; Wnd: ' + IntToStr(aUserInput.Window) + '; WndName: ');// + GetWindowText(aUserInput.Window));{$EndIf}
  if fKineticScroller = nil then
  begin
    //{$IfDef Logging}Logger.Add('KineticScroller=nil');{$EndIf}
    if aUserInput.InputType = itScroll then
    begin
      //{$IfDef Logging}Logger.Add('InputType=itScroll');{$EndIf}
      SendScrollMsg(aUserInput, true, PassToSystem);
    end;
  end else
  begin
    //{$IfDef Logging}Logger.Add('KineticScroller<>nil');{$EndIf}
    if not fKineticScroller.IsNotScrolling and (aUserInput.InputType = itKey) then
      fKineticScroller.StopScrolling
    else if aUserInput.InputType = itScroll then begin
      //{$IfDef Logging}Logger.Add('PrecessScrollMsgKinetic');{$EndIf}
      fKineticScroller.ProcessScrollMsgKinetic(aUserInput, PassToSystem);
    end;
  end;
end;

procedure TMouseBehaviourManager.MiddleMouseButton(const aUserInput: TUserInput; var PassToSystem: Boolean);
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.MiddleMouseButton was called outside the hook thread. That is not expected.');{$EndIf}
  if fMiddleMouseButtonMode = MMBM_DOUBLE_CLICK then
  begin
    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);

    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    PassToSystem := false;
  end else if fMiddleMouseButtonMode = MMBM_OPEN_CLIPBOARD_MENU then
  begin
    { TODO: Maybe build a simple callback, with just a window? }
    PostMessage(frm_Settings.Handle, WM_SHOWClIPBOARDPOPUP, aUserInput.Point.X, aUserInput.Point.Y);
    PassToSystem := false;
  end;
end;

procedure TMouseBehaviourManager.SetKineticScrollingOptions(ScrollingOptions: TJSONObject);
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TMouseBehaviourManager.SetKineticScrollingOptions was called outside the UserInputHook-thread.');{$EndIf}
  fKineticScroller.ScrollingOptions := ScrollingOptions;
end;
{
procedure TMouseBehaviourManager.SetKineticMode(Value: Boolean);
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TMouseBehaviourManager.SetKineticScrollingOptions was called outside the UserInputHook-thread.');{$EndIf}
  fKineticScroller.KineticMode := Value;
end;
}
destructor TMouseBehaviourManager.Destroy;
begin
  Settings.Mouse.ChangeHandler.Remove(@SettingsChanged); // erst das (damit Settingsänderung das nicht wieder reaktivieren kann)
  {$IfDef Logging}Logger.Add('Unregistering UserInputCallback for MouseBehaviourManager (MiddleMouseButton)');{$EndIf}
  UserInput.UnregisterCallback(@MiddleMouseButton);
  {$IfDef Logging}Logger.Add('Unregistering UserInputCallback for MouseBehaviourManager (Scrolling)');{$EndIf}
  UserInput.UnregisterCallback(@HandleScrollMsg);
  TryFreeAndNil(fKineticScroller);
  inherited Destroy;
end;

end.

