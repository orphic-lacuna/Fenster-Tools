unit Clipboard.Notifications;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, ExtendedThreads, WindowedThreadEx, ExtendedWinAPI, FensterToolsCommon;

type

  { TClipboardBase }

  TClipboardBase = class(TWindowedThreadEx)
  private
    fTickCountLastCopy, fTickCountLastPaste: QWord;

    function GetClipboardPasteNotificationEnabled: Boolean;
    procedure SetClipboardPasteNotificationEnabled(aValue: Boolean);

    procedure DoClipboardChanged;
    procedure DoClipboardPasted(FormatType: Cardinal; PID: Cardinal);
  protected
    procedure ThreadInternalCreate; override;
    procedure ThreadInternalDestroy; override;
    procedure HandleThreadWindowMessage(var msg: TMessage); override;

    procedure OnSettingChanged(aSetting: TObject); virtual;

    procedure ClipboardChanged; virtual; abstract;
    procedure ClipboardPasted(FormatType: Cardinal; PID: Cardinal); virtual; abstract;

    //property ClipboardChangeNotificationEnabled: Boolean read fClipboardChangeNotificationEnabled write SetClipboardChangeNotificationEnabled;
    property ClipboardPasteNotificationEnabled: Boolean read GetClipboardPasteNotificationEnabled write SetClipboardPasteNotificationEnabled;
  public
    //procedure StartThread; override;
    //procedure StopThread; override;

    constructor Create; reintroduce;

    procedure ClearClipboard; virtual;

    destructor Destroy; override;
  end;

implementation

uses LivePatching, SettingsManager{$IfDef Logging}, Logging{$EndIf};

{ TClipboardBase }

constructor TClipboardBase.Create;
begin
  inherited Create(false);
  Settings.Clipboard.ChangeHandler.Add(@OnSettingChanged);
end;

procedure TClipboardBase.ClearClipboard;
begin
  if IsRunInThread then
  begin
    if TryOpenClipboard(FThreadWindow) then
    begin
      EmptyClipboard;
      CloseClipboard;
    end {$IfDef Logging}else Logger.Add('TClipboardManager.ClearClipboard: Could not open clipboard within time'){$EndIf};

    DoClipboardChanged;
  end else DoThreadProc(@ClearClipboard);
end;

procedure TClipboardBase.OnSettingChanged(aSetting: TObject);
begin
  if aSetting = Settings.Clipboard.EnableClipboardExtension then
  begin
    if Settings.Clipboard.EnableClipboardExtension.Value then
    begin
      StartThread;
      {$IfDef Logging}Logger.Add('TClipboardNotifications.OnSettingChanged: Clipboard extension enabled');{$EndIf}
    end else begin
      StopThread;
      {$IfDef Logging}Logger.Add('TClipboardNotifications.OnSettingChanged: Clipboard extension disabled');{$EndIf}
    end;
  end;
end;

function TClipboardBase.GetClipboardPasteNotificationEnabled: Boolean;
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('TClipboardNotifications.GetClipboardPasteNotificationEnabled must be entered from main thread');{$EndIf}
  Result := Patcher.PatchEnabledGetClipboardData;
end;

procedure TClipboardBase.SetClipboardPasteNotificationEnabled(aValue: Boolean);
begin
  if GetCurrentThreadId <> MainThreadID then
  begin
    DoCallbackProc(@SetClipboardPasteNotificationEnabled, aValue);
  end else begin
    //the following line is commented, because if FThreadWindow has changed, a repatching is necessary
    //if Value = FListeningToCBChanges then exit;
    if aValue then
    begin
      Patcher.CallbackWnd_PasteNotifcation := FThreadWindow;
      Patcher.PatchEnabledGetClipboardData := true;
      {$IfDef Logging}Logger.Add('TClipboardNotifications.SetClipboardPasteNotificationEnabled: PasteNotification patch enabled, CallbackWnd=%', [FThreadWindow]);{$EndIf}
    end else
    begin
      Patcher.PatchEnabledGetClipboardData := false;
      {$IfDef Logging}Logger.Add('TClipboardNotifications.SetClipboardPasteNotificationEnabled: PasteNotification patch disabled');{$EndIf}
    end;
  end;
end;

procedure TClipboardBase.DoClipboardChanged;
begin
  {$IfDef Debug}if not IsRunInThread then raise Exception.Create('TClipboardNotifications.DoClipboardChanged must be entered from clipboard thread');{$EndIf}

  fTickCountLastCopy := GetTickCount64;

  if GetClipboardOwner <> FThreadWindow then
  begin
    {$IfDef Logging}Logger.Add('TClipboardNotifications.DoClipboardChanged: Calling ClipboardChanged in % ms', [Settings.Clipboard.ReadDelay]);{$EndIf}
    DoThreadProcDelayed(@ClipboardChanged, Settings.Clipboard.ReadDelay, false); // dont recall function if last call is pending
  end else begin
    {$IfDef Logging}Logger.Add('TClipboardNotifications.DoClipboardChanged: Change was self-made, nothing to do');{$EndIf}
  end;
end;

procedure TClipboardBase.DoClipboardPasted(FormatType: Cardinal; PID: Cardinal);
begin
  {$IfDef Debug}if not IsRunInThread then raise Exception.Create('TClipboardNotifications.DoClipboardPasted must be entered from clipboard thread');{$EndIf}
  {$IfDef Debug}if not Patcher.PatchEnabledGetClipboardData then raise Exception.Create('TClipboardNotifications.DoClipboardPasted: GetClipboardData patch is not active, although a clipboard pasted message was received');{$EndIf}
  {$IfDef Logging}Logger.Add('Clipboard pasted by "' + GetModuleFileName(PID) + '" (' + IntToStr(PID) + '), requested format is ' + DebugLookupClipboardFormat(FormatType));{$Endif}

  fTickCountLastPaste := GetTickCount64;

  // Due to this filter mechanism, it is ok, if ReadDelay > PasteEventDelay
  // every paste event occured PasteEventDeadTimeAfterCopy [ms] after a copy event is ignored, since we assume that all applications
  // read the clipboard because they got notified about the new clipboard content
  if fTickCountLastPaste - fTickCountLastCopy < Settings.Clipboard.PasteEventDeadTimeAfterCopy.Value then
  begin
    {$IfDef Logging}Logger.Add('  -> ignore it (PasteEventDeadTimeAfterCopy)');{$EndIf}
    exit;
  end;

  if (FormatType <> CF_LOCALE) and (FormatType <> CF_DIB) and (FormatType <> CF_DIF) and (FormatType <> CF_TIFF) and (FormatType <> CF_HDROP) and (FormatType <> CF_BITMAP) then
  begin
    if (GetModuleFileName(PID) <> 'TeamViewer.exe') then
    begin
      DoThreadProcDelayed(@ClipboardPasted, FormatType, PID, Settings.Clipboard.PasteEventDelay, false);
    end else begin
      {$IfDef Logging}Logger.Add('  -> ignore it (TeamViewer)');{$EndIf}
    end;
  end;
end;

procedure TClipboardBase.ThreadInternalCreate;
begin
  inherited ThreadInternalCreate;
  {$IfDef Logging}Logger.RegisterThread('Clipboard');{$EndIf}
  {$IfDef Logging}if not {$EndIf}AddClipboardFormatListener(FThreadWindow){$IfNDef Logging};{$Else} then Logger.Add('TClipboardNotifications.ThreadInternalCreate: AddClipboardFormatListener returned false', LT_ERROR);{$EndIf}
end;

procedure TClipboardBase.ThreadInternalDestroy;
begin
  {$IfDef Logging}if not {$EndIf}RemoveClipboardFormatListener(FThreadWindow){$IfNDef Logging};{$Else} then Logger.Add('TClipboardNotifications.ThreadInternalCreate: RemoveClipboardFormatListener returned false', LT_ERROR);{$EndIf}

  {$IfDef Logging}Logger.EndThread;{$EndIf}
  inherited ThreadInternalDestroy;
end;

procedure TClipboardBase.HandleThreadWindowMessage(var msg: TMessage);
begin
  {$IfDef Debug}if not IsRunInThread then raise Exception.Create('TClipboardNotifications.HandleThreadWindowMessage must be entered from clipboard thread');{$EndIf}
  case msg.msg of
    WM_CLIPBOARDUPDATE: begin
      DoClipboardChanged();
    end;
    else begin
      if (Patcher <> nil) and (msg.msg = Patcher.PasteNotificationMsg) then
      begin
        DoClipboardPasted(msg.lParam, msg.wParam);
      end else inherited HandleThreadWindowMessage(msg);
    end;
  end;
end;

destructor TClipboardBase.Destroy;
begin
  {$IfDef Logging}Logger.EndThread;{$EndIf}
  Patcher.PatchEnabledGetClipboardData := false;
  inherited Destroy;
end;

end.

