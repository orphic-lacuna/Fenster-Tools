unit Hotkey.Actions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Messages, ExtendedThreads, SlimReaderWriterLock, ToolBox, fgl;

type
  // important: the order of hotkeys must be the same as the order in TSettingsBaseHotkeys -> Published-properties
  // numbering must start with 0
  // Hotkey wird mittels ID dem jeweiligen Published-Property zugeordnet
  THotkeyActionID = (haNavigateUp = 0, haNavigateDown, haCloseAllWindows, haMakeWindowTopmost, haHideWindow, haBlockCopy);
const
  HotkeyNames: Array [THotkeyActionID] of String = ('Blättern durch die Zwischenablage (Vorwärts)', 'Blättern durch die Zwischenablage (Abwärts)', 'Schließen aller offenen Fenster', 'Anpinnen eines Fensters', 'Verstecken eines Fensters', 'Wechseln des Kopiermodus');
type
  TOnHotkeyDetected = procedure of object;

type

  { THotkeyAction }

  { This class holds one hotkey action and also the information in which thread the hotkey action has to be executed.
    A hotkey action can be of several types:
      - an own HotkeyReceiverWnd
      - a Method (with optionally a specific ThreadContext)
  }

  THotkeyActionManager = class;
  THotkeyAction = class
  private
    fID: THotkeyActionID;
    fLock: SRWLock; // prevents change of fHotkeyReceiverWnd, fHotkeyNotificationMethod or fThreadContext during running execution of an HotkeyAction
    fHotkeyReceiverWnd: HWND;
    fHotkeyNotificationMethod: TOnHotkeyDetected;
    fThreadContext: TThreadEx; // the Thread which has to execute the fHotkeyNotificationMethod
    fParent: THotkeyActionManager;
    function GetCallbackWnd: HWND;
    procedure ExecuteFromMainThread;
  public
    // property IsUsed: Boolean read GetIsUsed; ?
    property ID: THotkeyActionID read fID;
    property CallbackWnd: HWND read GetCallbackWnd;

    constructor Create(aID: THotkeyActionID; aParent: THotkeyActionManager);
    procedure RegisterActionHandler(NotificationMethod: TOnHotkeyDetected = nil; ThreadContext: TThreadEx = nil); overload;
    procedure RegisterActionHandler(aReceiverWnd: HWND = 0; NotificationMethod: TOnHotkeyDetected = nil; ThreadContext: TThreadEx = nil); overload;
    procedure UnregisterActionHandler;
    procedure Execute;
    destructor Destroy; override;
  end;

  { THotkeyActionManager }

  { This class holds a predefined set of hotkey actions.

    }

  THotkeyActionManager = class
  private type THotkeyActionList = specialize TFPGList<THotkeyAction>;
  procedure CallbackWndProc(var msg: TMessage);
  private
    fActions: Array [Low(THotkeyActionID)..High(THotkeyActionID)] of THotkeyAction;
    fCallbackWnd: HANDLE;
    function GetHotkeyAction(Index: THotkeyActionID): THotkeyAction;
  public
    property Actions[Index: THotkeyActionID]: THotkeyAction read GetHotkeyAction; default;
    constructor Create;
    destructor Destroy; override;
  end;

var
  HotkeyActionManager: THotkeyActionManager;

implementation

const
  WM_EXECUTE_HOTKEY_METHOD_IN_MAINTHREAD = WM_USER+1;

{ THotkeyActionManager }

procedure THotkeyActionManager.CallbackWndProc(var msg: TMessage);
begin
  case msg.msg of
    WM_HOTKEY: begin
      if (msg.wParam >= Ord(Low(fActions))) and (msg.wParam <= Ord(High(fActions))) then fActions[THotkeyActionID(msg.wParam)].Execute;
    end;
    WM_EXECUTE_HOTKEY_METHOD_IN_MAINTHREAD: begin
      if (msg.wParam >= Ord(Low(fActions))) and (msg.wParam <= Ord(High(fActions))) then fActions[THotkeyActionID(msg.wParam)].ExecuteFromMainThread;
    end;
    else msg.Result := DefWindowProc(fCallbackWnd, msg.msg, msg.wParam, msg.lParam);
  end;
end;

function THotkeyActionManager.GetHotkeyAction(Index: THotkeyActionID): THotkeyAction;
begin
  Result := fActions[Index];
end;

constructor THotkeyActionManager.Create;
var
  i: THotkeyActionID;
begin
  for i := Low(fActions) to High(fActions) do
  begin
    fActions[i] := THotkeyAction.Create(i, Self);
  end;
  fCallbackWnd := AllocateHWndThreadSafe(@CallbackWndProc);
end;

destructor THotkeyActionManager.Destroy;
var
  ha: THotkeyAction;
begin
  DeallocateHWNDThreadSafe(fCallbackWnd);
  for ha in fActions do ha.Free;
  inherited Destroy;
end;

{ THotkeyAction }

function THotkeyAction.GetCallbackWnd: HWND;
begin
  Result := fParent.fCallbackWnd;
end;

constructor THotkeyAction.Create(aID: THotkeyActionID; aParent: THotkeyActionManager);
begin
  fLock.Init;
  fID := aID;
  fParent := aParent;
end;

procedure THotkeyAction.RegisterActionHandler(NotificationMethod: TOnHotkeyDetected; ThreadContext: TThreadEx);
begin
  // RegisterActionHandler is called by actor classes from within their threads
  // so we have to use a lock
  RegisterActionHandler(0, NotificationMethod, ThreadContext);
end;

procedure THotkeyAction.RegisterActionHandler(aReceiverWnd: HWND; NotificationMethod: TOnHotkeyDetected; ThreadContext: TThreadEx);
begin
  // RegisterActionHandler is called by actor classes from within their threads
  // so we have to use a lock
  fLock.AcquireExclusive;
  fHotkeyReceiverWnd := aReceiverWnd;
  fHotkeyNotificationMethod := NotificationMethod;
  fThreadContext := ThreadContext;
  fLock.ReleaseExclusive;
end;

procedure THotkeyAction.UnregisterActionHandler;
begin
  // RegisterActionHandler is called by actor classes from within their threads
  // so we have to use a lock
  fLock.AcquireExclusive;
  fHotkeyReceiverWnd := 0;
  fHotkeyNotificationMethod := nil;
  fThreadContext := nil;
  fLock.ReleaseExclusive;
end;

procedure THotkeyAction.Execute;
begin
  // we can be within MainThread (if received a WM_HOTKEY from a standard windows hotkey) or within UserInputHook-Thread

  // remember: we are possibly within the keyboard-hook -> we must be lightning fast
  fLock.AcquireShared;
  // if we have a method to execute and a thread context, then execute it immediately (it's another thread, we are just sending a message from this thread), that's fast enough
  if Assigned(fHotkeyNotificationMethod) and (fThreadContext <> nil) then fThreadContext.DoThreadProc(fHotkeyNotificationMethod);
  // if we have a have method, that needs to be executed in main thread, we must inform our callback window
  if Assigned(fHotkeyNotificationMethod) and (fThreadContext = nil) then
  begin
    if GetCurrentThreadId = MainThreadID then
      fHotkeyNotificationMethod()
    else
      PostMessage(CallbackWnd, WM_EXECUTE_HOTKEY_METHOD_IN_MAINTHREAD, Ord(fID), 0);
  end;

  // if we have a fHotkeyReceiverWnd then just send the message immediately (it's fast enough)
  if fHotkeyReceiverWnd <> 0 then PostMessage(fHotkeyReceiverWnd, WM_HOTKEY, Ord(fID), 0);
  fLock.ReleaseShared;
end;

procedure THotkeyAction.ExecuteFromMainThread;
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('THotkeyAction.ExecuteFromMainThread called outside of main thread');{$EndIf}
  fHotkeyNotificationMethod();
end;

destructor THotkeyAction.Destroy;
begin
  inherited Destroy;
end;

end.

