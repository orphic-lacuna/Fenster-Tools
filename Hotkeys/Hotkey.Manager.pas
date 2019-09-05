unit Hotkey.Manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Hotkey.Actions, Hotkey.List, fgl, Hotkey, Windows, UserInputProvider, Toolbox{$IfDef Logging}, Logging{$EndIf};

type

  { THotkeyManager }

  { This class manages all hotkeys.
    Main functions:
      - holding a list of shortcuts
      - registers / unregisters hotkeys (either via RegisterHotkey(WINAPI) or via a keyboard hook)
      - gets notified by keyboard hook and checks if a hotkey is pressed
      - executes the corresponding hotkey action
    }
  THotkeyManager = class
  private

    type

      { TManagedHotkey }

      TManagedHotkey = class
      private
        fShortcut: TShortcut;
        fAction: THotkeyAction;
        function GetNeedsHook: Boolean;
        procedure RegisterHotkey(aSettingsObject: THotkeyListItem; var aFilter: TUserInputFilter);
        procedure UnregisterHotkey;
      public
        property NeedsHook: Boolean read GetNeedsHook;

        function Check(Key: Byte; Modifiers: TShiftStateEx; AutoExecuteHotkey: Boolean): Boolean;
      end;

  private type TManagedHotkeyList = specialize TFPGList<THotkeyManager.TManagedHotkey>;
  private
    fActive: Boolean;
    fList: THotkeyManager.TManagedHotkeyList;

    procedure RegisterHotkeys;
    procedure UnregisterHotkeys;
    procedure SetActive(aValue: Boolean);
    function CheckHotkey(Key: Byte; Modifiers: TShiftStateEx; AutoExecute: Boolean): Boolean;
    procedure OnHotkeyKeyDown(const aUserInput: TUserInput; var PassToSystem: Boolean);
  public
    property Active: Boolean read fActive write SetActive;
    //constructor Create;
    //destructor Destroy; override;
  end;

var
  HotkeyManager: THotkeyManager;

implementation

uses SettingsManager;

{ THotkeyManager.TManagedHotkey }

function THotkeyManager.TManagedHotkey.GetNeedsHook: Boolean;
begin
  Result := (fShortcut.Key = VK_MOUSEWHEEL_UP) or
    (fShortcut.Key = VK_MOUSEWHEEL_DOWN) or
    (fShortcut.Key = VK_HMOUSEWHEEL_DOWN) or
    (fShortcut.Key = VK_HMOUSEWHEEL_UP) or
    (fShortcut.Key = VK_LBUTTON) or
    (fShortcut.Key = VK_MBUTTON) or
    (fShortcut.Key = VK_RBUTTON) or
    (fShortcut.Key = VK_XBUTTON1) or
    (fShortcut.Key = VK_XBUTTON2) or
    fShortcut.Force;
  {$IfDef Logging}Logger.Add('TManagedHotkey.GetNeedsHook returned %', [Result]);{$EndIf}
end;

procedure THotkeyManager.TManagedHotkey.RegisterHotkey(aSettingsObject: THotkeyListItem; var aFilter: TUserInputFilter);
begin
  {$IfDef Logging}Logger.Add('TManagedHotkey.RegisterHotkey');{$EndIf}

  // make a copy of the shortcut so that we don't have to access the SettingsObject from within the UserInputHook-Thread
  fShortcut := aSettingsObject.Value;
  fAction := HotkeyActionManager.Actions[aSettingsObject.ActionID];

  if not NeedsHook then
  begin
    if not Windows.RegisterHotKey(fAction.CallbackWnd, Ord(fAction.ID), GetModifiersFromShortcut(fShortcut), fShortcut.Key) then
      aSettingsObject.NotifyAlreadyInUse('Tastenkombination wird bereits von einem anderen Programm verwendet.', false);
  end else
  begin
    if aFilter = nil then
    begin
      aFilter := TUserInputFilter.Create;
      aFilter.InputTypes := [itKey];
    end;
    {$IfDef Debug}if (fShortcut.Key <= 0) or (fShortcut.Key > 255) then raise Exception.Create('TManagedHotkey.Shortcut.Key is invalid: ' + IntToStr(fShortcut.Key));{$EndIf}
    aFilter.SensitiveKeyList[fShortcut.Key] := true;
  end;
end;

procedure THotkeyManager.TManagedHotkey.UnregisterHotkey;
begin
  if not NeedsHook then
  begin
    Windows.UnregisterHotkey(fAction.CallbackWnd, Ord(fAction.ID));
    {$IfDef Logging}Logger.Add('TManagedHotkey.UnregisterHotkey (windows-standard) for HotkeyID %', [Ord(fAction.ID)]);{$EndIf}
  end;
end;

function THotkeyManager.TManagedHotkey.Check(Key: Byte; Modifiers: TShiftStateEx; AutoExecuteHotkey: Boolean): Boolean;
begin
  // we have to be fast (we are within a GLOBAL HOOK)

  Result := (fShortcut.Key = Key) and (fShortcut.Modifiers = Modifiers);
  if Result and AutoExecuteHotkey then fAction.Execute;
end;

{ THotkeyManager }
      {
constructor THotkeyManager.Create;
begin
  fActive := false;
end;
     }
procedure THotkeyManager.OnHotkeyKeyDown(const aUserInput: TUserInput; var PassToSystem: Boolean);
begin
  PassToSystem := not CheckHotkey(aUserInput.Key, UserInput.Modifiers, aUserInput.Pressed);
end;

// checks if one of the hotkeys was pressed by user, executes the assigned action if necessary
function THotkeyManager.CheckHotkey(Key: Byte; Modifiers: TShiftStateEx; AutoExecute: Boolean): Boolean;
var
  mh: TManagedHotkey;
begin
  // remember: we are in the hook-thread, and we must be FAST !!!
  Result := false;
  if Key = 0 then exit;

  for mh in fList do
  begin
    if mh.Check(Key, Modifiers, AutoExecute) then
    begin
      Result := true;
      break;
    end;
  end;
end;

procedure THotkeyManager.SetActive(aValue: Boolean);
begin
  {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('THotkeyManager.SetActive was called outside the main thread');{$EndIf}

  if fActive = aValue then Exit;
  fActive := aValue;

  if fActive then RegisterHotkeys else UnregisterHotkeys;
end;

procedure THotkeyManager.RegisterHotkeys;
var
  item: THotkeyListItem;
  mh: TManagedHotkey;
  filter: TUserInputFilter;
begin
  {$IfDef Debug}if fList <> nil then raise Exception.Create('THotkeyManager.RegisterHotkeys: Tried to re-register hotkeys');{$EndIf}
  fList := TManagedHotkeyList.Create;

  filter := nil; // local variables must be intialized, they may contain bullshit without initialization

  for item in Settings.Hotkeys.Items do
  begin
    if (item.Value.Key <> 0) then
    begin
      mh := TManagedHotkey.Create;
      mh.RegisterHotkey(item, filter); // fReceiverWnd gets allocated (if necessary within mh.RegisterHotkey)
      fList.Add(mh);
    end;
  end;

  if filter <> nil then
  begin
    {$IfDef Logging}Logger.Add('Registering UserInputCallback for THotkeyManager');{$EndIf}
    UserInput.RegisterCallback(@OnHotkeyKeyDown, filter, 100);
  end;
end;

procedure THotkeyManager.UnregisterHotkeys;
var
  item: TManagedHotkey;
begin
  {$IfDef Debug}if fList = nil then raise Exception.Create('THotkeyManager.UnregisterHotkeys: Tried to re-unregister hotkeys');{$EndIf}

  for item in fList do
  begin
    item.UnregisterHotkey;
    item.Free;
  end;
  UserInput.UnregisterCallback(@OnHotkeyKeyDown);
  FreeAndNil(fList);
end;
         {
destructor THotkeyManager.Destroy;
begin
  inherited Destroy;
end;
        }
end.

