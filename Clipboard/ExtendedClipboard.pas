unit ExtendedClipboard;

interface

{$mode objfpc}{$H+}

uses Clipboard, Sysutils, Windows, Messages, Replacer, Classes, FensterToolsCommon, ToolBox, Clipboard.List, Clipboard.Formats, Textfenster, SettingsBase, SettingsBaseTypes, KeyPatternMatching;

{
  Timer:
    // 0. XP: Clipboard Chain Renewing Timer
    1. Select Next Entry Timer
      --> is used only in Smart Copy Reversed Mode to delay the Copy-Back of the first item after a new item has been copied to the CB
    2. Clipboard read delay Timer (after a CB-change was detected)
    3. PasteEvent delay Timer (after the CB has been pasted until the event is handled)
}

type
  TCopyMode = (CM_NORMAL=0, CM_SMARTCOPY=1, CM_SMARTCOPY_REVERSED=2, CM_NORMAL_INSERTED_TO_TOP=3);

operator := (a: TObject) b: TCopyMode;

type
  TThreadMethodDoClipboardChanged = procedure(Action: TClipboardChangedAction; ID: Integer; Str: UnicodeString; Check: boolean) of object;
  TThreadMethodSetCopyMode = procedure(Value: TCopyMode) of object;

  TDoClipboardChangedParameters = class
  public
    Action: TClipboardChangedAction;
    ID: Integer;
    Str: UnicodeString;
    Check: Boolean;
    Method: TThreadMethodDoClipboardChanged;
  end;
  TSetCopyModeParameters = class
  public
    Value: TCopyMode;
    Method: TThreadMethodSetCopyMode;
  end;

  TCopyModeChangeEvent = procedure(Copymode: TCopyMode) of object;

type

	{ TExtendedClipboard }

  TExtendedClipboard = class(TClipboard)
  private
    FHistory: TClipboardStack;
    FTextFenster: TTextFenster;

    ActiveEntry: TClipboardItem;

    // Settings
    FMaxHistoryLen, FMaxEntryLength: Integer;
    fAutoReplace: boolean;
    FCopyBlockTimeLimit, FInsertCopyBlockTimeLimit: Integer;
    fTimeLimitForDoubleCopy, fTimeLimitCopyBlock, fTimeLimitInsertCopyBlock: Integer;

    //fReplaceList: TReplaceList;
    fReplacer: TReplacer;

    fStrgCCKeyPattern: TKeyPattern_StrgCC;

    FCopyMode, FStandardCopyMode: TCopyMode;
    SelectNextEntryTimer: NativeInt;

    FRestoreCB: boolean;

    FOnClipboardChanged: TClipboardChangedEvent;
    FCopyModeChangeEvent: TCopyModeChangeEvent;

		procedure HandleHotkey(HotkeyID: Integer);
    procedure SetAutoReplace(AValue: Boolean);
    procedure SetMaxHistoryLen(Value: Integer);
    procedure SetMaxEntryLength(Value: Integer);// hier die Einträge neu bauen

    //procedure SetReplaceList(Value: TReplaceList);

    procedure SaveToFile; // UseTemporaryFile: Boolean = false
    //procedure SetRestoreCB(Value: boolean);
    procedure LoadFromFile(); // UseTemporaryFile: Boolean = false
    function MakeMenuString(Value: UnicodeString): UnicodeString;

    function GetActiveEntryID: Integer;
    procedure SetActiveEntryID(Value: Integer);

    procedure SetCopyMode(Value: TCopyMode);
    procedure SetStandardCopyMode(Value: TCopyMode);
    procedure RebuildClipboardMenu;

    procedure DoubleCopyingDetected;

    procedure SelectCurrentItemAccordingToCopyMode(DoItImmediately: boolean);
  protected
    procedure OnSettingChanged(Sender: TObject); override;

    procedure ClipboardChanged(); override;
    procedure ClipboardPasted(PID: Cardinal); override;

    procedure SetListeningToCBChanges(Value: boolean); override;

    procedure HandleThreadWindowMessage(var Msg: TMessage); override;

    procedure ThreadInternalCreate; override;
    procedure ThreadInternalDestroy; override;

    procedure DoClipboardChanged(Action: TClipboardChangedAction; ID: Integer = 0; Str: UnicodeString = ''; Check: boolean = false); virtual;


    (* überladen von TThreadEx *)
    //procedure DoThreadProc(ThreadMethodSetHotkey: TThreadMethodSetHotkey; HotkeyID: HotkeyIDs; Value: TBettershortcut); overload;
    procedure DoCallbackProc(ThreadMethodDoClipboardChanged: TThreadMethodDoClipboardChanged; Action: TClipboardChangedAction; ID: Integer = 0; Str: UnicodeString = ''; Check: boolean = false); overload;
    procedure DoCallbackProc(ThreadMethodSetCopyMode: TThreadMethodSetCopyMode; Value: TCopyMode); overload;
    procedure ParametrizedMethodInvoker(Params: TObject);
    (* ----------------------- *)
  public
    property AutoReplace: Boolean read FAutoReplace write SetAutoReplace;
    property RestoreCB: Boolean read FRestoreCB write FRestoreCB;

    property CopyMode: TCopyMode read FCopyMode write SetCopyMode;
    property StandardCopyMode: TCopyMode read FStandardCopyMode write SetStandardCopyMode;
    //property CopyBlockTimeLimit: Integer read FCopyBlockTimeLimit write FCopyBlockTimeLimit;
    //property InsertCopyBlockTimeLimit: Integer read FInsertCopyBlockTimeLimit write FInsertCopyBlockTimeLimit;

    property MaxEntryLength: Integer read FMaxEntryLength write SetMaxEntryLength;

    property ActiveEntryID: Integer read GetActiveEntryID write SetActiveEntryID;
    property Textfenster: TTextFenster read FTextfenster;

    property OnClipboardChanged: TClipboardChangedEvent read FOnClipboardChanged write FOnClipboardChanged;
    property OnCopyModeChange: TCopyModeChangeEvent read FCopyModeChangeEvent write FCopyModeChangeEvent;

    property MaxHistoryLen: Integer read FMaxHistoryLen write SetMaxHistoryLen;
    //property ReplaceList: TReplaceList read fReplaceList write SetReplaceList;

    procedure Clear({_OBSOLETE_ ClearCurrentContent: boolean = true}); override; {_OBSOLETE_ reintroduce;}

    constructor Create; reintroduce;
    destructor Destroy(); override;

  end;

implementation

uses SettingsManager, AdvancedHotkeyHandler{$IfDef Logging}, Logging{$EndIf};

const
  //TIME_LIMIT_TWICE_COPY: Cardinal = 1750;
//  TIME_LIMIT_CONTINUE_ENTRY: Cardinal = 1000;
  CBFS_Header = 'FTCB';
  CBFS_Version = 2;
  SELECT_NEXT_ENTRY_TIMER_ID = 14529701;
  SELECT_NEXT_ENTRY_DELAY: Cardinal = 150;

type TClipboardFileSignature = record
    Header: Array [0..3] of Char;
    Version: Cardinal;
  end;

operator := (a: TObject) b: TCopyMode;
begin
  {$IfDef Debug}if a is TSettingsBaseInteger then{$EndIf}
  b := TCopyMode((a as TSettingsBaseInteger).Value)
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TCopyMode implicitly although it was not a integer-type.');{$EndIf}
end;

constructor TExtendedClipboard.Create();
begin
  FTextFenster := TTextFenster.Create;
  FCopyMode := CM_SMARTCOPY; FCopyBlockTimeLimit := MaxInt; FInsertCopyBlockTimeLimit := MaxInt;
  //FRestoreCB := false;

  inherited Create;
end;

procedure TExtendedClipboard.ThreadInternalCreate;
begin
  inherited;

  FHistory := TClipboardStack.Create(@DoClipboardChanged{$IfDef Debug}, FThreadID{$EndIf});

  if FMaxHistoryLen = 0 then FMaxHistoryLen := 1;
  FAutoReplace := false;

  fReplacer := TReplacer.Create;

  LoadFromFile;
end;

procedure TExtendedClipboard.OnSettingChanged(Sender: TObject);
begin
  //if Sender = TObject(Settings.Clipboard.ReplaceList) then
    //ReplaceList := TReplaceList(TObject(Sender))
  //else begin
    if not Sender.InheritsFrom(TSettingsBase) then exit;

    if Sender = Settings.Clipboard.MaxEntryCount then MaxHistoryLen := Sender
	  else if Sender = Settings.Clipboard.EnableClipboardExtension then ListeningToCBChanges := Sender
	  else if Sender = Settings.Clipboard.MaxMenuEntryLength then MaxEntryLength := Sender
	  else if Sender = Settings.Clipboard.RestoreAfterReboot then RestoreCB := Sender
    else if Sender = Settings.Clipboard.StandardCopyMode then StandardCopyMode := Sender
    else if Sender = Settings.Clipboard.AutoReplace then AutoReplace := Sender
    else if Sender = Settings.Clipboard.TimeLimitForDoubleCopy then fTimeLimitForDoubleCopy := Sender
    else if Sender = Settings.Clipboard.TimeLimitCopyBlock then fTimeLimitCopyBlock := Sender
    else if Sender = Settings.Clipboard.TimeLimitInsertCopyBlock then fTimeLimitInsertCopyBlock := Sender
    else if TSettingsBase(Sender).Parent = Settings.Clipboard.Window then
    begin
      if (Sender = Settings.Clipboard.Window.FontName) then FTextFenster.FontName := Sender
      else if (Sender = Settings.Clipboard.Window.FontSize) then FTextFenster.FontSize := Sender
      else if (Sender = Settings.Clipboard.Window.ForeColor) then FTextFenster.FontColor := Sender
      else if (Sender = Settings.Clipboard.Window.BackColor) then FTextFenster.BackgroundColor := Sender
      else if (Sender = Settings.Clipboard.Window.DisplayDuration) then FTextFenster.WindowTimeout := Sender;
    end else begin
      inherited OnSettingChanged(Sender);
    end;
  //end;
end;

procedure TExtendedClipboard.SetListeningToCBChanges(Value: boolean);
begin
  if Value and (FTextFenster = nil) then FTextFenster := TTextFenster.Create()
  else if not Value then TryFreeAndNil(FTextFenster);

  inherited SetListeningToCBChanges(Value);
  // now Clipboard thread is started, FThreadHandle and FThreadID are valid
  // no thread-safe hotkey-handler is needed, we are in MainThread here
  if Value then
  begin
    HotkeyHandler.InitializeHotkey(HID_NAVIGATE_UP, FThreadWindow);
    HotkeyHandler.InitializeHotkey(HID_NAVIGATE_DOWN, FThreadWindow);
    HotkeyHandler.InitializeHotkey(HID_NAVIGATE_UP2, FThreadWindow);
    HotkeyHandler.InitializeHotkey(HID_NAVIGATE_DOWN2, FThreadWindow);
  end else
  begin
    HotkeyHandler.FreeHotkey(HID_NAVIGATE_UP);
    HotkeyHandler.FreeHotkey(HID_NAVIGATE_DOWN);
    HotkeyHandler.FreeHotkey(HID_NAVIGATE_UP2);
    HotkeyHandler.FreeHotkey(HID_NAVIGATE_DOWN2);
  end;
end;

{procedure TExtendedClipboard.SetRestoreCB(Value: boolean);
begin
  if FRestoreCB = Value then exit;
  FRestoreCB := Value;
  if FRestoreCB then LoadFromFile;
end;        }

procedure TExtendedClipboard.SetMaxHistoryLen(Value: Integer);
begin
  {$IfDef Debug}
  if (Value < 1) or (Value > 1000) then
  begin
    raise Exception.Create('MaxHistoryLen außerhalb des zulässigen Bereiches.');
    exit;
  end;
  {$EndIf}
  if not ListeningToCBChanges or (GetCurrentThreadID = FThreadID) then
  begin
    FMaxHistoryLen := Value;
    if (FHistory <> nil) then FHistory.CutOff(FMaxHistoryLen);
  end else DoThreadProc(@SetMaxHistoryLen, Value);
end;

{$Region 'Helpers'}
procedure TExtendedClipboard.ParametrizedMethodInvoker(Params: TObject);
begin
  if Params is TSetCopyModeParameters then
  begin
    (Params as TSetCopyModeParameters).Method((Params as TSetCopyModeParameters).Value);
  end else if Params is TDoClipboardChangedParameters then
  begin
    (Params as TDoClipboardChangedParameters).Method((Params as TDoClipboardChangedParameters).Action, (Params as TDoClipboardChangedParameters).ID, (Params as TDoClipboardChangedParameters).Str, (Params as TDoClipboardChangedParameters).Check);
  end;
end;

procedure TExtendedClipboard.DoCallbackProc(ThreadMethodDoClipboardChanged: TThreadMethodDoClipboardChanged; Action: TClipboardChangedAction; ID: Integer; Str: UnicodeString; Check: boolean);
var
  p: TDoClipboardChangedParameters;
begin
  p := TDoClipboardChangedParameters.Create;
  p.Action := Action; p.ID := ID; p.Str := Str; p.Check := Check;
  p.Method := ThreadMethodDoClipboardChanged;
  DoCallbackProc(@ParametrizedMethodInvoker, p);
end;

{$Message Hint 'Lässt sich das hier nicht besser lösen?'}
procedure TExtendedClipboard.DoCallbackProc(ThreadMethodSetCopyMode: TThreadMethodSetCopyMode; Value: TCopyMode);
var
  p: TSetCopyModeParameters;
begin
  p := TSetCopyModeParameters.Create;
  p.Value := Value;
  p.Method := ThreadMethodSetCopyMode;
  DoCallbackProc(@ParametrizedMethodInvoker, p);
end;
{$EndRegion}

procedure TExtendedClipboard.Clear({_OBSOLETE_ ClearCurrentContent: boolean = true});
begin
  if GetCurrentThreadId = FThreadID then
  begin
    FHistory.Clear;
    inherited Clear();
    //DoClipboardChanged(CCA_CLEAR); 
    {_OBSOLETE_ if ClearCurrentContent then
    begin
      // alles löschen, auch den aktuellen Inhalt
      inherited Clear();
      DoCallbackProc(DoClipboardChanged, CCA_CLEAR, -1, '', false);
    end;}
  end else DoThreadProc(@Clear{_OBSOLETE_, ClearCurrentContent});
end;

procedure TExtendedClipboard.HandleThreadWindowMessage(var Msg: TMessage);
begin
  {$IfDef Debug}if GetCurrentThreadId <> FThreadID then raise Exception.Create('HandleCBThreadMsg was called in the MainThread');{$EndIf}

  case Msg.Msg of
    WM_TIMER:
      begin
        if Msg.WParam = SelectNextEntryTimer then
        begin
          KillTimer(FThreadWindow, SELECT_NEXT_ENTRY_TIMER_ID);
          SelectCurrentItemAccordingToCopyMode(true); 
        end else inherited HandleThreadWindowMessage(Msg);
      end;
    WM_HOTKEY: HandleHotkey(Msg.wParam);
    else inherited HandleThreadWindowMessage(Msg);
  end;
end;

procedure TExtendedClipboard.DoubleCopyingDetected;
begin
  if GetCurrentThreadId = FThreadID then
  begin
    // some prechecks
    if (FHistory.FirstItem <> nil) and (FHistory.FirstItem.Content = FCurrentContent) then
    begin
      {$IfDef Logging}Logger.Add('Strg-C-C detected, last copied item is valid');{$EndIf}
      if FAutoReplace then
      begin
        {$IfDef Logging}Logger.Add('  --> Replacing patterns, removing format');{$EndIf}
        FReplacer.Replace(FCurrentContent);
        WriteText(FCurrentContent);
        FHistory.FirstItem.Content := FCurrentContent;
        // da zweites Kopieren, muss nicht auf ein eventuelles weiteres Kopieren gewartet werden
        SelectCurrentItemAccordingToCopyMode(true);
        DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(FHistory.FirstItem), MakeMenuString(FHistory.FirstItem.Content), true);
      end;
    end{$IfDef Logging}else Logger.Add('Strg-C-C detected, but last history item is nil or unequal to current clipboard content.'){$EndIf};
  end else DoThreadProc(@DoubleCopyingDetected);
end;

procedure TExtendedClipboard.HandleHotkey(HotkeyID: Integer);
  function ActiveEntryNotNil: Boolean;
  begin
    Result := ActiveEntry <> nil;
    if not Result then
    begin
      {$IfDef Logging}Logger.Add('No active entry (last was empty or unsupported)');{$EndIf}
      // non-supported format was copied (or empty clipboard)
      // just CopyW the first entry back to clipboard
      ActiveEntry := FHistory.FirstItem;
      if ActiveEntry <> nil then
      begin
        {$IfDef Logging}Logger.Add('Writing back first entry');{$EndIf}
        WriteText(ActiveEntry.Content);
        DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(ActiveEntry), '', true);
        FTextFenster.Text := ActiveEntry.Content;
      end else begin
        {$IfDef Logging}Logger.Add('Clipboard history is empty');{$EndIf}
        FTextFenster.Text := '';
      end;
      FTextFenster.Show;
    end;
  end;
begin
  case HotkeyID of
    Ord(HID_NAVIGATE_UP), Ord(HID_NAVIGATE_UP2):
      begin
        if ActiveEntryNotNil then
        begin
          {$IfDef Logging}Logger.Add('Navigating UP');{$EndIf}
          if not ActiveEntry.IsFirst then
          begin
            {$Message Warn 'Code Duplication? See SetActiveEntryID'}
            {$Message Warn 'Inhalt erst reinpacken, wenn UserSelection zu Ende?'}
            ActiveEntry := ActiveEntry.PreviousItem;
            {$IfDef Debug}if ActiveEntry = nil then raise Exception.Create('Failed to navigate up: PreviousItem is NIL although ActiveItem was not the first item');{$EndIf}
            WriteText(ActiveEntry.Content);
            DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(ActiveEntry), '', true);
          end;// else DoCallbackProc(DoSelectionChanged, FActiveHistoryID, FActiveHistoryID, true);
          FTextFenster.Text := ActiveEntry.Content;
          FTextFenster.Show;
        end {$IfDef Logging}else Logger.Add('Navigating UP (no active entry');{$EndIf}
      end;
    Ord(HID_NAVIGATE_DOWN), Ord(HID_NAVIGATE_DOWN2):
      begin
        if ActiveEntryNotNil then
        begin
          {$IfDef Logging}Logger.Add('Navigating DOWN');{$EndIf}
          if not ActiveEntry.IsLast then
          begin
            ActiveEntry := ActiveEntry.NextItem;
            {$IfDef Debug}if ActiveEntry = nil then raise Exception.Create('Failed to navigate down: NextItem is NIL although ActiveItem was not the last item');{$EndIf}
            WriteText(ActiveEntry.Content);
            DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(ActiveEntry), '', true);
          end;// else DoCallbackProc(DoSelectionChanged, FActiveHistoryID, FActiveHistoryID, true);
          FTextFenster.Text := ActiveEntry.Content;
          FTextFenster.Show;
        end {$IfDef Logging}else Logger.Add('Navigating UP (no active entry');{$EndIf}
      end;
  end;
end;

procedure TExtendedClipboard.SetAutoReplace(AValue: Boolean);
begin
  if FAutoReplace = AValue then Exit;
  FAutoReplace := AValue;

  if fAutoReplace and (fStrgCCKeyPattern = nil) then
  begin
    fStrgCCKeyPattern := TKeyPattern_StrgCC.Create;
    fStrgCCKeyPattern.OnExecute := @DoubleCopyingDetected;
    UserInputLog.RegisterKeyPattern(fStrgCCKeyPattern);
  end else if not fAutoReplace then
  begin
    UserInputLog.UnregisterKeyPattern(fStrgCCKeyPattern);
    TryFreeAndNil(fStrgCCKeyPattern);
  end;
end;

function TExtendedClipboard.MakeMenuString(Value: UnicodeString): UnicodeString;
{procedure MakePrintable(var Str: UnicodeString);
var
  i: Integer;
begin
  for i := 1 to Length(Str) do
  begin
    if ((Integer(Str[i]) < 32) or (Integer(Str[i]) > 126)) and not ((Integer(Str[i]) = 10) or (Integer(Str[i]) = 13)) then Str[i] := '?';
  end;
end;}
var
  i, Len: Integer;
begin
  i := 1;
  {$B-} // Kurzauswertung nötig sonst krachts
  while (Length(Value)-i >= 0) and (Value[i] = ' ') do
  begin
    Inc(i);
  end;

  Len := Length(Value) - i + 1; // calc length after removing trailing whitespaces
  if (i > 1) or (Len > FMaxEntryLength) then Value := CopyU(Value, i, min(FMaxEntryLength, Len));
  if Len > FMaxEntryLength then Value := Value + ' ...';
  //MakePrintable(Value);
  Value := UnicodeStringReplace(Value, #13 + #10, ' ', [rfReplaceAll]);
  Value := UnicodeStringReplace(Value, #13, ' ', [rfReplaceAll]);
  Value := UnicodeStringReplace(Value, #10, ' ', [rfReplaceAll]);

  if Value = '-' then Value := '- ';
  Result := Value;
end;

procedure TExtendedClipboard.ClipboardPasted(PID: Cardinal);
var
  tmp: TClipboardItem;
begin
  inherited ClipboardPasted(PID);

  {$IfDef Logging} Logger.Add('Handling paste event ...'){$Endif};

  if (ActiveEntry = nil) then
  begin
    {$IfDef Logging} Logger.Add('    --> No ActiveEntry => last copied item was unsupported format'){$Endif};
    exit;
  end;

  ActiveEntry.AlreadyPasted := true;
  tmp := nil;
  case FCopyMode of
//    CM_NORMAL: begin

//    end;
    CM_SMARTCOPY: begin
      tmp := FHistory.GetNewestUnpastedItem(ActiveEntry, FInsertCopyBlockTimeLimit);
    end;
    CM_SMARTCOPY_REVERSED: begin
      tmp := FHistory.GetOldestUnpastedItem(ActiveEntry, FCopyBlockTimeLimit);
    end;
    CM_NORMAL_INSERTED_TO_TOP: begin
      if not ActiveEntry.IsFirst then
      begin
        FHistory.Delete(ActiveEntry, false);
        FHistory.Insert(0, ActiveEntry);
        DoClipboardChanged(CCA_ADD, 0, MakeMenuString(FHistory.FirstItem.Content), true);
        {$IfDef Logging}Logger.Add('    --> Moving Entry ' + IntToStr(ActiveEntryID) + ' to top.');{$Endif}
      end {$IfDef Logging} else Logger.Add('    --> Not moving anything. Inserted entry is already the first one.'){$Endif};
    end;
  end;
  if (tmp <> nil) then
  begin
    ActiveEntry := tmp;
    WriteText(ActiveEntry.Content);
    {$IfDef Logging}Logger.Add('    --> Selecting next uninserted entry.');{$Endif};
    DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(ActiveEntry), '', true);
  end else begin
    if CopyMode <> FStandardCopyMode then CopyMode := FStandardCopyMode;
  end;
end;

{function TExtendedClipboard.FalseClipboardChangedEvent: boolean;
var
  NewOwner: HWND;
begin
  Result := true;
  NewOwner := GetClipboardOwner;
  if NewOwner = LastOwner then
  begin
    Inc(ProgramSpecificClipboardNumber);

    if ProgramSpecificClipboardNumber > 0 then
    begin
      if (NewOwner = 0) then
      begin
        //Inc(AdobeReaderSpecificClipboardNumber);
        AdobeReaderCleanerTimer := SetTimer(FThreadWindow, AdobeReaderCleanerTimer, ARFIX_COMPENSATION_TIME, nil);
        if FCurrentContent = '' then
        begin
          Dec(ProgramSpecificClipboardNumber);
        end;
        exit;
        //begin
          //while PeekMessage(tagMsg_, FThreadWindow, WM_CLIPBOARDUPDATE, WM_CLIPBOARDUPDATE, PM_REMOVE) do;
          //while PeekMessage(tagMsg_, FThreadWindow, WM_DRAWCLIPBOARD, WM_DRAWCLIPBOARD, PM_REMOVE) do;
        //end;
      end else if (LowerCase(GetWindowModuleFileName(LastOwner)) = 'excel.exe') then
      begin
        case ProgramSpecificClipboardNumber of
          1: exit;
          // die erste Kopiernachricht von EXCEL.EXE muss verworfen werden, da Excel immer 2x kopiert
          // (sofern Auto-Ersetzen bei doppelt kopieren ein)

          2: ProgramSpecificClipboardNumber := 0;
        end;
      end;
    end;
  end else
  begin
    ProgramSpecificClipboardNumber := 0;
    LastOwner := NewOwner;
  end;
  Result := false;
end;   }

procedure TExtendedClipboard.SelectCurrentItemAccordingToCopyMode(DoItImmediately: boolean);
var
  tmp_item: TClipboardItem;
begin
  ActiveEntry := FHistory.FirstItem;
  if CopyMode = CM_SMARTCOPY_REVERSED then
  begin
    // do this stuff delayed ...
    if DoItImmediately then
    begin
      tmp_item := FHistory.GetOldestUnpastedItem(nil, FCopyBlockTimeLimit);
      if (tmp_item <> ActiveEntry) and (tmp_item <> nil) then
      begin
        {$IfDef Logging}Logger.Add('SMARTCOPY-REVERSED-MODE; Selecting oldest item');{$EndIf}
        WriteText(tmp_item.Content);
        ActiveEntry := tmp_item;
        DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(tmp_item), '', true);
      end else
      begin
        //ActiveEntry := FHistory.FirstItem;
        {$IfDef Logging}Logger.Add('SMARTCOPY-REVERSED-MODE; oldest unpasted item is NULL or the same as the active entry');{$EndIf}
      end;
    end else
    begin
      if SelectNextEntryTimer <> 0 then KillTimer(FThreadWindow, SelectNextEntryTimer);
      SelectNextEntryTimer := SetTimer(FThreadWindow, SELECT_NEXT_ENTRY_TIMER_ID, SELECT_NEXT_ENTRY_DELAY, nil);
      {$IfDef Logging}Logger.Add('SMARTCOPY-REVERSED-MODE; SelectNextEntry-Timer started');{$EndIf}
    end;
  end else if (CopyMode = CM_SMARTCOPY) and (FHistory.FirstItem <> nil) and (LowerCase(GetWindowModuleFileName(ClipboardOwner)) = 'editpadlite7.exe') then
  begin
    // bei EditpadLite7 den Text zurück-kopieren, damit Editpadlite den Inhalt nicht in einem
    // eigenen Zwischenspeicher speichert, denn würde es das tun, bekäme FensterTools nicht
    // mit, wann EditpadLite einfügt, da es nie GetClipboardData aufrufen würde
    {$IfDef Logging}Logger.Add('SMARTCOPY-MODE; Preventing EditpadLite from buffering Clipboard');{$EndIf}
    WriteText(FHistory.FirstItem.Content);
  end;
  //if CopyMode <> CM_SMARTCOPY_REVERSED then ActiveEntry := FHistory.FirstItem;
end;


procedure TExtendedClipboard.ClipboardChanged();
begin
  // Anpassungen zwecks Doppel-Kopier-Probleme in Excel, Adobe Reader, ...
  // if FalseClipboardChangedEvent then exit;

  // wurde die Zwischenablage nur geleert oder ein NICHT-TEXT-EINTRAG
  if FCurrentContent <> '' then
  begin
    // okay, wir haben Text
    // prüfe, ob es sich um doppeltes Kopieren handelt (Zeitbegrenzung + gleicher Text)
    {if (FHistory.FirstItem <> nil) and (GetTickCount64 - LastCopyTimestamp <= fTimeLimitForDoubleCopy) and (FHistory.FirstItem.Content = FCurrentContent) then
    begin
      InterlockedDecrement(FSequenceNumber);
      {$IfDef Logging}Logger.Add('  --> Detected double copying, time left: ' + IntToStr(fTimeLimitForDoubleCopy-GetTickCount64+LastCopyTimestamp) + ' ms');{$EndIf}
      if FAutoReplace then
      begin                
        {$IfDef Logging}Logger.Add('  --> Replacing patterns, removing format');{$EndIf}
        FReplacer.Replace(FCurrentContent);
        WriteText(FCurrentContent);
        FHistory.FirstItem.Content := FCurrentContent;
        // da zweites Kopieren, muss nicht auf ein eventuelles weiteres Kopieren gewartet werden
        SelectCurrentItemAccordingToCopyMode(true);
        DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(FHistory.FirstItem), MakeMenuString(FHistory.FirstItem.Content), true);
      end;
    end }
    // prüfe ob Fortsetzung des letzten Eintrages
    {else }if (FHistory.FirstItem <> nil) and CompareStrUntilLenOfShorterStr(FCurrentContent, FHistory.FirstItem.Content) then
    begin
      // es war eine Fortsetzung
      if Length(FHistory.FirstItem.Content) <> Length(FCurrentContent) then
      begin
        {$IfDef Logging}Logger.Add('  --> Extending last entry');{$EndIf}
        FHistory.FirstItem.Content := FCurrentContent;
        DoClipboardChanged(CCA_CHANGE, FHistory.IndexOf(FHistory.FirstItem), MakeMenuString(FHistory.FirstItem.Content), true);
      end else
      begin
        {$IfDef Logging}Logger.Add('  --> Same text, ignore it');{$EndIf}
      end;
    end else begin
      // keine Fortsetzung, also ein ganz normaler neuer Eintrag

      //{$IfDef Logging}Logger.Add('  --> Inserting new entry');{$EndIf}

      FHistory.Insert(0, TClipboardItem.Create(FCurrentContent));

      //{$IfDef Logging} if FHistory.FirstItem = nil then Logger.Add('FHistory.FirstItem = nil', LT_ERROR); {$EndIf}

      DoClipboardChanged(CCA_ADD, 0, MakeMenuString(FHistory.FirstItem.Content), true);

      FHistory.CutOff(FMaxHistoryLen);

      // da erstes Kopieren, muss auf ein eventuelles doppeltes Kopieren gewartet werden
      SelectCurrentItemAccordingToCopyMode(false);
    end;
  end else
  begin
    {$IfDef Logging}Logger.Add('  --> Non-supported format or empty clipboard');{$EndIf}

    // nein das war ein Eintrag ohne Text
    ActiveEntry := nil;

    // also Haken entfernen                                                          // Index 0 entspricht FHistory.IndexOf(FHistory.FirstItem)
    if FHistory.FirstItem <> nil then
      DoClipboardChanged(CCA_CHANGE, 0, '', false);
  end;



//  if FCurrentContent = '' then InterlockedDecrement(FSequenceNumber);
end;

procedure TExtendedClipboard.DoClipboardChanged(Action: TClipboardChangedAction; ID: Integer = 0; Str: UnicodeString = ''; Check: boolean = false);
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    {$IfDef Debug}if GetCurrentThreadId <> MainThreadID then raise Exception.Create('DoClipboardChanged wird im falschen Thread ausgeführt.');{$EndIf}
    //{$IfDef Logging}Logger.Add('    --> Executing DoClipboardChanged');{$EndIf}
    if Assigned(FOnClipboardChanged) then FOnClipboardChanged(Action, ID, Str, Check);
  end else
  begin
    //{$IfDef Logging}Logger.Add('    --> Calling DoClipboardChanged via Callback');{$EndIf}
    DoCallbackProc(@DoClipboardChanged, Action, ID, Str, Check);
  end;
end;

procedure TExtendedClipboard.RebuildClipboardMenu;
var
  item: TClipboardItem;
  i: Integer;
begin
  if FHistory.FirstItem = nil then exit;
  if GetCurrentThreadId = FThreadID then
  begin
    DoClipboardChanged(CCA_START_REBUILD, FHistory.IndexOf(FHistory.FirstItem), MakeMenuString(FHistory.FirstItem.Content), true);
    item := FHistory.FirstItem; i := 0;
    while item <> nil do
    begin
      DoClipboardChanged(CCA_ADD, i, MakeMenuString(item.Content), (item = ActiveEntry));

      item := item.NextItem;
      Inc(i);
    end; 
  end else
  begin
    DoThreadProc(@RebuildClipboardMenu);
  end;
end;

procedure TExtendedClipboard.SetMaxEntryLength(Value: Integer);
begin
  FMaxEntryLength := Value;
  if ListeningToCBChanges then RebuildClipboardMenu;
end;
    {
procedure TExtendedClipboard.SetReplaceList(Value: TReplaceList);
begin
  fReplaceList := Value;
  if ListeningToCBChanges then
  begin
    fReplacer.ReplaceList := Value; // ist thread-sicher
  end;
end; }

(*procedure TExtendedClipboard.SetInsertedEntryToTop(Value: boolean);
begin
  {$IfDef Debug}
  if GetCurrentThreadId <> MainThreadID then raise Exception.Create('SetInsertedEntryToTop -> falscher Thread');
  {$EndIf}
  FInsertedEntryToTop := Value;
  ListeningForCBPaste := FInsertedEntryToTop or (FCopyMode <> CM_NORMAL);
end;*)

procedure TExtendedClipboard.SetStandardCopyMode(Value: TCopyMode);
begin
  {$IfDef Debug}
  if GetCurrentThreadId <> MainThreadID then raise Exception.Create('SetCopyMode -> falscher Thread');
  {$EndIf}
  FStandardCopyMode := Value;
  CopyMode := FStandardCopyMode;
end;

procedure TExtendedClipboard.SetCopyMode(Value: TCopyMode);
//var
  //newVal: Boolean;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    FCopyMode := Value;

    //newVal := (FCopyMode <> CM_NORMAL); // prevents repatching every time, the CopyW mode was changed
    //if ListeningForCBPaste <> newVal then ListeningForCBPaste := newVal;
    ListeningForCBPaste := (FCopyMode <> CM_NORMAL);

    if Assigned(FCopyModeChangeEvent) then FCopyModeChangeEvent(FCopyMode);
    {$IfDef Logging}
    case FCopyMode of
      CM_NORMAL: Logger.Add('Switched copy mode to normal');
      CM_SMARTCOPY: Logger.Add('Switched copy mode to smart copy');
      CM_SMARTCOPY_REVERSED: Logger.Add('Switched copy mode to reversed smart copy');
      CM_NORMAL_INSERTED_TO_TOP: Logger.Add('Switched copy mode to inserted entry on top');
    end;
    {$EndIf}
  end else
  begin
    DoCallbackProc(@SetCopyMode, Value); 
  end;
end;

function TExtendedClipboard.GetActiveEntryID: Integer;
begin
  Result := FHistory.IndexOf(ActiveEntry);
end;

procedure TExtendedClipboard.SetActiveEntryID(Value: Integer);
var
  item: TClipboardItem;
begin
  if GetCurrentThreadId = FThreadID then
  begin
    item := FHistory[Value];
    if item <> nil then
    begin                                                    
      ActiveEntry := item;
      WriteText(ActiveEntry.Content);
      DoClipboardChanged(CCA_CHANGE, Value, '', true);
    end;
  end else DoThreadProc(@SetActiveEntryID, Value);
end;

procedure TExtendedClipboard.SaveToFile;
var                                         
  F: TFileStream;
  StrLen: Integer;
  //i, count: Integer;
  Item: TClipboardItem;
  FileSignature: TClipboardFileSignature;
begin
  {$IfDef Debug}if GetCurrentThreadId <> FThreadID then raise Exception.Create('SaveToFile wird im VCL-Thread ausgeführt.');{$EndIf}

  {$IfDef Logging}Logger.Add('Saving Clipboard file');{$EndIf}
  try
    F := TFileStream.Create(ClipboardFilename, fmOpenWrite or fmCreate);
    try
      FileSignature.Header := CBFS_Header;
      FileSignature.Version := CBFS_Version;
      F.Write(FileSignature, SizeOf(TClipboardFileSignature));
      Item := FHistory.LastItem;
      while Item <> nil do
      begin
        StrLen := Length(Item.Content);
        F.Write(StrLen, SizeOf(Integer));
        F.Write(Item.Content[1], StrLen*SizeOf(UnicodeChar));
        Item := Item.PreviousItem;
      end;
      {$IfDef Logging}Logger.Add('Clipboard file successfully saved');{$EndIf}
    finally
      F.Free;
    end;
  except on E: Exception do
    begin
      {$IfDef Logging}Logger.Add('Error: ' + E.Message);{$EndIf}
    end;
  end;
end;                                               

procedure TExtendedClipboard.LoadFromFile;
var
  F: TFileStream;
  Str: UnicodeString;
  AnsiStr: AnsiString;
  StrLen: Integer;
  {i, FileEntriesCount, }countBefore: Integer;
  FileSignature: TClipboardFileSignature;
begin
  if not FileExists(ClipboardFilename) then exit;

  if GetCurrentThreadId <> FThreadID then
  begin                                                        
    DoThreadProc(@LoadFromFile);
    exit;
  end;
  {$IfDef Debug}if GetCurrentThreadId <> FThreadID then raise Exception.Create('LoadFromFile wird im VCL-Thread ausgeführt.');{$EndIf}

  if not FileExists(ClipboardFilename) then exit;

  {$IfDef Logging}Logger.Add('Loading Clipboard file');{$EndIf}
  try
    F := TFileStream.Create(ClipboardFilename, fmOpenRead);
    try
      ZeroMemory(@FileSignature, SizeOf(TClipboardFileSignature));
      F.Read(FileSignature, SizeOf(TClipboardFileSignature));
      if FileSignature.Header <> CBFS_Header then raise Exception.Create('Invalid file signature');

      countBefore := FHistory.Count;

      //for i := 0 to FileEntriesCount-1 do
      while F.Position < F.Size do
      begin
        F.Read(StrLen{%H-}, SizeOf(Cardinal));
        if FileSignature.Version = 1 then
        begin
          SetLength(AnsiStr, StrLen);
          F.Read(AnsiStr[1], StrLen);
          Str := AnsiStr;
        end else if (FileSignature.Version = 2) then
        begin
          SetLength(Str, StrLen);
          F.Read(Str[1], StrLen*SizeOf(UnicodeChar));
        end;
        // wenn es der letzte Eintrag war und der gleich dem aktuellen Inhalt ist, dann überspringen
        if (F.Position >= F.Size) and (FCurrentContent = Str) then break;
        FHistory.Insert(countBefore, TClipboardItem.Create(Str, true));
        DoClipboardChanged(CCA_ADD, countBefore, MakeMenuString(Str), {(countBefore=0)and(F.Position >= F.Size)}false);
      end;
      {$IfDef Logging}Logger.Add('Clipboard file successfully loaded');{$EndIf}
    finally
      F.Free;
    end;
  except on E: Exception do
    begin
      {$IfDef Logging}
        Logger.Add('Error: ' + E.Message, LT_ERROR);
        Logger.Add('Loading of clipboard file aborted');
      {$EndIf}
    end;
  end;
  {$Message Hint 'Copy back last item when EnumClipboardFormats returns zero (Empty CB).'}

 // if (countBefore = 0) and (FileEntriesCount > 0) then WriteText(FHistory[0].Content);

  Sysutils.DeleteFile(ClipboardFilename);
end;

procedure TExtendedClipboard.ThreadInternalDestroy;
begin
  {$IfDef Debug}if GetCurrentThreadId <> FThreadID then raise Exception.Create('ThreadInternalDestroy wird im VCL-Thread ausgeführt.');{$EndIf}
  if FRestoreCB then SaveToFile;

  FReplacer.Free;

  FHistory.Free;

  inherited ThreadInternalDestroy;
end;

destructor TExtendedClipboard.Destroy();
begin
  if fStrgCCKeyPattern <> nil then
  begin
    UserInputLog.UnregisterKeyPattern(fStrgCCKeyPattern);
    fStrgCCKeyPattern.Free;
  end;
  inherited Destroy;
end;

end.
