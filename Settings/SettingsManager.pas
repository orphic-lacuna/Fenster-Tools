unit SettingsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fpjsonrtti, FileUtil, Dialogs, typinfo, SettingsBase, SettingsBaseTypes, SettingsDerivedBaseTypes, Hotkey, Registry, ReplaceList, Graphics, Hotkey.List{$IfDef Logging}, Logging{$EndIf};

// wäre besser, wenn das in einer extra Unit läge

//operator:=(a: TObject) b: TSettingsBase;

operator := (a: TObject) b: Boolean;
operator := (a: TObject) b: Integer;
operator := (a: TObject) b: Cardinal;
operator := (a: TObject) b: TShortcut;
operator := (a: TObject) b: TColor;
operator := (a: TObject) b: String;

operator := (a: TSettingsBaseBool) b: Boolean;
operator := (a: TSettingsBaseInteger) b: Integer;
operator := (a: TSettingsBaseFloat) b: Single;
operator := (a: TSettingsBaseScrollSettings) b: TJSONObject;
operator := (a: TSettingsBaseHotkey) b: TShortcut;
operator := (a: TSettingsBaseInteger) b: TColor;
operator := (a: TSettingsBaseString) b: String;
operator := (a: TSettingsBaseString) b: UnicodeString;

type
  TClipboardSettings = class;
  TMouseSettings = class;
  TClipboardWindowSettings = class;
  //THotkeySettings = class;
  TUpdateSettings = class;

  { TSettings }

  TSettings = class(TSettingsBaseTop)
  private
    fClipboard: TClipboardSettings;
    fMouse: TMouseSettings;
    fHotkeys: THotkeyList;
    fAutostart: TSettingsBaseBool;
    fLastUsername, fLastMailAddress: TSettingsBaseString;
    fUpdate: TUpdateSettings;
  public
    constructor Create(aParent: TSettingsBase = nil); override;
    destructor Destroy; override;
  published
    property Clipboard: TClipboardSettings read fClipboard write fClipboard;
    property Mouse: TMouseSettings read fMouse write fMouse;
    property Hotkeys: THotkeyList read fHotkeys write fHotkeys;
    property Autostart: TSettingsBaseBool read fAutostart write fAutostart;
    property LastUsername: TSettingsBaseString read fLastUsername write fLastUsername;
    property LastMailAddress: TSettingsBaseString read fLastMailAddress write fLastMailAddress;
    property Update: TUpdateSettings read fUpdate write fUpdate;
  end;

  { TUpdateSettings }

  TUpdateMode = (umInstall = 0, umOnlySearch = 1, umNever = 2);
  TUpdateSettings = class(TSettingsBase)
  private
    fMode, fInterval: TSettingsBaseInteger;
    fLastCheck: TSettingsBaseFloat;
  published
    property Mode: TSettingsBaseInteger read fMode write fMode;
    property Interval: TSettingsBaseInteger read fInterval write fInterval;
    property LastCheck: TSettingsBaseFloat read fLastCheck write fLastCheck;
  end;

  { TClipboardSettings }

  TClipboardSettings = class(TSettingsBase)
  private
    //fStatusWindow: TClipboardWindowSettings;
    fWindow: TClipboardWindowSettings;
    fMaxEntryCount, fMaxMenuEntryLength, fReadDelay, fPasteEventDelay, fPasteEventDeadTimeAfterCopy, fStandardCopyMode, fTimeLimitForDoubleCopy, fTimeLimitCopyBlock, fTimeLimitInsertCopyBlock: TSettingsBaseInteger;
    fEnableClipboardExtension, fRestoreAfterReboot, {fPatchOpenClipboard, }fAutoReplace: TSettingsBaseBool;
    fAutoInsert: TSettingsBaseBool;

    fReplaceList: TReplaceList;
  public
    constructor Create(aParent: TSettingsBase = nil); override;
    destructor Destroy; override;
  published
    property Window: TClipboardWindowSettings read fWindow write fWindow;
    //property StatusWindow: TClipboardWindowSettings read fStatusWindow write fStatusWindow;
    property EnableClipboardExtension: TSettingsBaseBool read fEnableClipboardExtension write fEnableClipboardExtension;
    property RestoreAfterReboot: TSettingsBaseBool read fRestoreAfterReboot write fRestoreAfterReboot;
    //property PatchOpenClipboard: TSettingsBaseBool read fPatchOpenClipboard write fPatchOpenClipboard;
    property MaxEntryCount: TSettingsBaseInteger read fMaxEntryCount write fMaxEntryCount;
    property MaxMenuEntryLength: TSettingsBaseInteger read fMaxMenuEntryLength write fMaxMenuEntryLength;

    property ReadDelay: TSettingsBaseInteger read fReadDelay write fReadDelay;
    property PasteEventDelay: TSettingsBaseInteger read fPasteEventDelay write fPasteEventDelay;
    property PasteEventDeadTimeAfterCopy: TSettingsBaseInteger read fPasteEventDeadTimeAfterCopy write fPasteEventDeadTimeAfterCopy;
    property TimeLimitForDoubleCopy: TSettingsBaseInteger read fTimeLimitForDoubleCopy write fTimeLimitForDoubleCopy;
    property TimeLimitCopyBlock: TSettingsBaseInteger read fTimeLimitCopyBlock write fTimeLimitCopyBlock;
    property TimeLimitInsertCopyBlock: TSettingsBaseInteger read fTimeLimitInsertCopyBlock write fTimeLimitInsertCopyBlock;

    property StandardCopyMode: TSettingsBaseInteger read fStandardCopyMode write fStandardCopyMode;
    property AutoReplace: TSettingsBaseBool read fAutoReplace write fAutoReplace;
    property ReplaceList: TReplaceList read fReplaceList write fReplaceList;
    property AutoInsert: TSettingsBaseBool read fAutoInsert write fAutoInsert;
  end;

  { TClipboardWindowSettings }

  TClipboardWindowSettings = class(TSettingsBase)
  private
    fActiveEntryHeight: TSettingsBaseFloat;
    fBackgroundColor: TSettingsBaseDirect2DColor;
    fDefaultEntryHeight: TSettingsBaseFloat;
    fEntryBackgroundColor: TSettingsBaseDirect2DColor;
    fEntryFont: TSettingsBaseString;
    fEntryFontsize: TSettingsBaseFloat;
    fEntryRectRoundingRadius: TSettingsBaseFloat;
    fEntryTextColor: TSettingsBaseDirect2DColor;
    fIconHeight: TSettingsBaseFloat;
    fIconMarginLeft: TSettingsBaseFloat;
    fIconMarginRight: TSettingsBaseFloat;
    fIconMarginTop: TSettingsBaseFloat;
    fIconWidth: TSettingsBaseFloat;
    fMarginEntryEntry: TSettingsBaseFloat;
    fMarginLeft: TSettingsBaseFloat;
    fMarginRight: TSettingsBaseFloat;
    fPaddingBottom: TSettingsBaseFloat;
    fPaddingLeft: TSettingsBaseFloat;
    fPaddingRight: TSettingsBaseFloat;
    fPaddingTop: TSettingsBaseFloat;
    fPosition: TWindowSizeSettings;
    fScrollAnimationDuration: TSettingsBaseFloat;
    fFadeoutDelay: TSettingsBaseFloat;
    fFadeoutDuration: TSettingsBaseFloat;
    fSegmentationLineDarkColor: TSettingsBaseDirect2DColor;
    fSegmentationLineLightColor: TSettingsBaseDirect2DColor;
    fSegmentationLineMargin: TSettingsBaseFloat;
    fTimestampFont: TSettingsBaseString;
    fTimestampFontsize: TSettingsBaseFloat;
    fTimestampTextColor: TSettingsBaseDirect2DColor;
  published
    // window dimensions
    property Position: TWindowSizeSettings read fPosition write fPosition;

    // main element colors
    property BackgroundColor: TSettingsBaseDirect2DColor read fBackgroundColor write fBackgroundColor;
    property EntryBackgroundColor: TSettingsBaseDirect2DColor read fEntryBackgroundColor write fEntryBackgroundColor;

    // fonts
    property EntryFont: TSettingsBaseString read fEntryFont write fEntryFont;
    property TimestampFont: TSettingsBaseString read fTimestampFont write fTimestampFont;
    property EntryFontsize: TSettingsBaseFloat read fEntryFontsize write fEntryFontsize;
    property TimestampFontsize: TSettingsBaseFloat read fTimestampFontsize write fTimestampFontsize;
    property EntryFontColor: TSettingsBaseDirect2DColor read fEntryTextColor write fEntryTextColor;
    property TimestampFontColor: TSettingsBaseDirect2DColor read fTimestampTextColor write fTimestampTextColor;

    // main element dimensions
    property DefaultEntryHeight: TSettingsBaseFloat read fDefaultEntryHeight write fDefaultEntryHeight;
    property ActiveEntryHeight: TSettingsBaseFloat read fActiveEntryHeight write fActiveEntryHeight;
    property MarginEntryEntry: TSettingsBaseFloat read fMarginEntryEntry write fMarginEntryEntry;
    property MarginLeft: TSettingsBaseFloat read fMarginLeft write fMarginLeft;
    property MarginRight: TSettingsBaseFloat read fMarginRight write fMarginRight;
    property PaddingLeft: TSettingsBaseFloat read fPaddingLeft write fPaddingLeft;
    property PaddingRight: TSettingsBaseFloat read fPaddingRight write fPaddingRight;
    property PaddingTop: TSettingsBaseFloat read fPaddingTop write fPaddingTop;
    property PaddingBottom: TSettingsBaseFloat read fPaddingBottom write fPaddingBottom;

    // Segmentation line
    property SegmentationLineDarkColor: TSettingsBaseDirect2DColor read fSegmentationLineDarkColor write fSegmentationLineDarkColor;
    property SegmentationLineLightColor: TSettingsBaseDirect2DColor read fSegmentationLineLightColor write fSegmentationLineLightColor;
    property SegmentationLineMargin: TSettingsBaseFloat read fSegmentationLineMargin write fSegmentationLineMargin;

    // icon
    property IconMarginLeft: TSettingsBaseFloat read fIconMarginLeft write fIconMarginLeft;
    property IconMarginTop: TSettingsBaseFloat read fIconMarginTop write fIconMarginTop;
    property IconMarginRight: TSettingsBaseFloat read fIconMarginRight write fIconMarginRight;
    property IconWidth: TSettingsBaseFloat read fIconWidth write fIconWidth;
    property IconHeight: TSettingsBaseFloat read fIconHeight write fIconHeight;

    // delays and timings
    property ScrollAnimationDuration: TSettingsBaseFloat read fScrollAnimationDuration write fScrollAnimationDuration;
    property FadeoutDelay: TSettingsBaseFloat read fFadeoutDelay write fFadeoutDelay;
    property FadeoutDuration: TSettingsBaseFloat read fFadeoutDuration write fFadeoutDuration;

    // misc
    property EntryRectRoundingRadius: TSettingsBaseFloat read fEntryRectRoundingRadius write fEntryRectRoundingRadius;
  end;

  TMouseSettings = class(TSettingsBase)
  private
    fScrollWindowUnderMouse, fKineticScrollingEnabled, fMiddleMouseBtnMakesDblClick{, fMiddleMouseButtonOpensClipboardMenu}: TSettingsBaseBool;
    fKineticScrollingOptions: TSettingsBaseScrollSettings;
  published
    property ScrollWindowUnderMouse: TSettingsBaseBool read fScrollWindowUnderMouse write fScrollWindowUnderMouse;
    property KineticScrollingEnabled: TSettingsBaseBool read fKineticScrollingEnabled write fKineticScrollingEnabled;
    property MiddleMouseBtnDoubleClick: TSettingsBaseBool read fMiddleMouseBtnMakesDblClick write fMiddleMouseBtnMakesDblClick;
    //property MiddleMouseBtnClipboardMenu: TSettingsBaseBool read fMiddleMouseButtonOpensClipboardMenu write fMiddleMouseButtonOpensClipboardMenu;
    property KineticScrollingOptions: TSettingsBaseScrollSettings read fKineticScrollingOptions write fKineticScrollingOptions;
	end;

type

	{ TSettingsManager }

  TSettingsManager = class
  private
    fFilename: UnicodeString;
		procedure LoadInitialValues;
    procedure AutostartChange(Sender: TObject);
    //function ParseOldConfigFile: Boolean;
    function CheckVersion(x: TJSONObject): Boolean;
  public
    property Filename: UnicodeString read fFilename;

    function LoadFromFile: Boolean;
    procedure SaveToFile;

    constructor Create(aFilename: UnicodeString);
    destructor Destroy; override;

	end;

var
  Settings: TSettings;
  SettingsMan: TSettingsManager;

implementation

uses FensterToolsCommon, UserInputProvider, UpdateCheck, Hotkey.Actions;

{$R DefaultConfig.rc}

operator:=(a: TObject) b: Boolean;
begin
  {$IfDef Debug}if a is TSettingsBaseBool then{$EndIf}
  b := (a as TSettingsBaseBool).Value
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseBool implicitly although it was not a bool-type.');{$EndIf}
end;

operator := (a: TObject) b: Integer;
begin
  {$IfDef Debug}if a is TSettingsBaseInteger then{$EndIf}
  b := (a as TSettingsBaseInteger).Value
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseInteger implicitly although it was not a integer-type.');{$EndIf}
end;

operator:=(a: TObject)b: Cardinal;
begin
  {$IfDef Debug}if a is TSettingsBaseInteger then{$EndIf}
  b := Cardinal((a as TSettingsBaseInteger).Value)
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseInteger implicitly although it was not a integer-type.');{$EndIf}
end;

operator:=(a: TObject)b: TShortcut;
begin
  {$IfDef Debug}if a is TSettingsBaseHotkey then{$EndIf}
  b := (a as TSettingsBaseHotkey).Value
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseHotkey implicitly although it was not a Hotkey-type.');{$EndIf}
end;

operator:=(a: TObject)b: TColor;
begin
  {$IfDef Debug}if a is TSettingsBaseInteger then{$EndIf}
  b := TColor((a as TSettingsBaseInteger).Value)
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseInteger implicitly although it was not a integer-type.');{$EndIf}
end;

operator := (a: TObject) b: String;
begin
  {$IfDef Debug}if a is TSettingsBaseString then{$EndIf}
  b := (a as TSettingsBaseString).Value
  {$IfDef Debug}else raise Exception.Create('Tried to convert TObject to TSettingsBaseString implicitly although it was not a string-type.');{$EndIf}
end;

operator := (a: TSettingsBaseBool) b: Boolean;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseInteger) b: Integer;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseFloat) b: Single;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseScrollSettings) b: TJSONObject;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseHotkey) b: TShortcut;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseInteger) b: TColor;
begin
  b := TColor(a.Value);
end;

operator := (a: TSettingsBaseString) b: String;
begin
  b := a.Value;
end;

operator := (a: TSettingsBaseString) b: UnicodeString;
begin
  b := a.Value;
end;

{ TSettings }

constructor TSettings.Create(aParent: TSettingsBase);
begin
  inherited Create(aParent);
end;

destructor TSettings.Destroy;
begin
  inherited Destroy;
end;

{ TClipboardSettings }

constructor TClipboardSettings.Create(aParent: TSettingsBase);
begin
  inherited Create(aParent);
  fReplaceList := TReplaceList.Create(ChangeHandler);
end;

destructor TClipboardSettings.Destroy;
begin
  fReplaceList.Free;
  inherited Destroy;
end;

constructor TSettingsManager.Create(aFilename: UnicodeString);
begin
  fFilename := aFilename;
  Settings := TSettings.Create{%H-};
  //Settings.Clipboard.ReplaceList := TReplaceList.Create(Settings.Clipboard.ChangeHandler);

  Settings.Autostart.ChangeHandler.Add(@AutostartChange);

  LoadInitialValues;
end;

procedure TSettingsManager.LoadInitialValues;
var
  ds: TJSONDeStreamer;
  config: String;
begin
  ds := TJSONDeStreamer.Create(nil);
  with TResourceStream.Create(HINSTANCE, 'DefaultConfig.json', 'RC_DATA') do
  begin
    SetLength(config, Size);
    Read(config[1], Size);
    Free;
	end;

  ds.OnRestoreProperty := @Settings.RestorePropertyDefaultValues;
  Settings.Clipboard.ReplaceList.BeginUpdate;
  ds.JSONToObject(config, Settings);
  Settings.Clipboard.ReplaceList.EndUpdate;

  ds.Free;
end;

function TSettingsManager.LoadFromFile: Boolean;
var
  f: TFileStream;
  p: TJSONParser;
  x: TJSONData;
begin
  Result := false;
  if FileExists(fFilename) then
  begin
    f := TFileStream.Create(fFilename, fmOpenRead);
    p := TJSONParser.Create(f);
    //p.UseUTF8 := true;
    x := p.Parse;
    p.Free; f.Free;

    if x is TJSONObject then
    begin
      CheckVersion(TJSONObject(x));

      Settings.AsJSON := TJSONObject(x);
      x.Free;
		end;
	end;
end;

procedure TSettingsManager.SaveToFile;
var
  x: TJSONObject;
  config: String;
begin
  x := Settings.AsJSON;
  x.Add('Version', CurrentProgramVersion);
  if x <> nil then
  begin
    config := x.FormatJSON([]);
    x.Free;

    with TFileStream.Create(fFilename, fmOpenWrite or fmCreate) do
    begin
      Write(config[1], Length(config));
      Free;
    end;
	end{$IfNDef Debug};{$Else} else raise Exception.Create('Could not convert Settings to TJSONObject');{$EndIf}
end;

procedure TSettingsManager.AutostartChange(Sender: TObject);
var
  Reg: TRegistry;
begin
  if Sender = Settings.Autostart then
  begin
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', true);
    if Boolean(TSettingsBase(Sender)) then
      Reg.WriteString('FensterTools', ParamStr(0))
    else
      Reg.DeleteValue('FensterTools');
    Reg.CloseKey;
    Reg.Free;
	end;
end;

function TSettingsManager.CheckVersion(x: TJSONObject): Boolean;

  function MakeHotkeyArrayItem(actionID: THotkeyActionID; Shortcut: TJSONObject): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.Add('Action', GetEnumName(TypeInfo(actionID), Ord(actionID)));
    Result.Add('Shortcut', Shortcut.Clone);
  end;

  procedure ConvertOldHotkeyFormat;
  var
    hotkeys, h: TJSONObject;
    hotkeys_neu: TJSONArray;
  begin
    hotkeys := TJSONObject(x.Find('Hotkeys', jtObject));
    if hotkeys <> nil then
    begin
      hotkeys_neu := TJSONArray.Create;

      h := TJSONObject(hotkeys.Find('CloseAllWindows', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haCloseAllWindows, h));
      h := TJSONObject(hotkeys.Find('HideWindow', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haHideWindow, h));
      h := TJSONObject(hotkeys.Find('MakeWindowTopMost', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haMakeWindowTopmost, h));

      h := TJSONObject(hotkeys.Find('NavigateDown', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haNavigateDown, h));
      h := TJSONObject(hotkeys.Find('NavigateDown2', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haNavigateDown, h));
      h := TJSONObject(hotkeys.Find('NavigateUp', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haNavigateUp, h));
      h := TJSONObject(hotkeys.Find('NavigateUp2', jtObject));
      if h <> nil then hotkeys_neu.Add(MakeHotkeyArrayItem(haNavigateUp, h));

      x.Remove(hotkeys);
      x.Add('Hotkeys', hotkeys_neu);
    end;
  end;

var
  version: TJSONData;
begin
  Result := false;
  version := x.Find('Version', jtNumber);
  if (version <> nil) and (version.InheritsFrom(TJSONNumber)) then
  begin
    if TJSONNumber(version).AsInteger = CurrentProgramVersion then
    begin
      Result := true;
      exit;
    end;{ else begin
      case TJSONNumber(version).AsInteger of
        5092: begin

        end;
      end;
    end;}
  end else begin
    ConvertOldHotkeyFormat;
  end;
end;


   {
function TSettingsManager.ParseOldConfigFile: Boolean;
  function StrToShortcut(Str: String; Default: TShortcut): TShortcut;
  var
    i: integer;
    Text: String;
  begin
    Text := '';
    Result.Key := 0;
    Result.Modifiers := [];
    if Str = '' then exit; //raise EConvertError.Create('Could not convert string to shortcut');
    try
      if Str[Length(Str)] <> ',' then Str := Str + ',';
      for i := 1 to Length(Str) do
      begin
        if Str[i] <> ',' then
        begin
          Text := Text + Str[i];
        end else
        begin
          if lowercase(Text) = 'shift' then
            Result.Modifiers := Result.Modifiers + [ssShift]
          else if lowercase(Text) = 'alt' then
            Result.Modifiers := Result.Modifiers + [ssAlt]
          else if lowercase(Text) = 'ctrl' then
            Result.Modifiers := Result.Modifiers + [ssCtrl]
          else if lowercase(Text) = 'win' then
            Result.Modifiers := Result.Modifiers + [ssWin]
          else
            Result.Key := StrToInt(Text);
          Text := '';
        end;
      end;
    finally
    end;
  end;
  function Decode64(S: string): string;
  var
    i: Integer;
    a: Integer;
    x: Integer;
    b: Integer;
  const
    Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
  begin
    Result := '';
    a := 0;
    b := 0;
    for i := 1 to Length(s) do
    begin
      x := Pos(s[i], codes64) - 1;
      if x >= 0 then
      begin
        b := b * 64 + x;
        a := a + 6;
        if a >= 8 then
        begin
          a := a - 8;
          x := b shr a;
          b := b mod (1 shl a);
          x := x mod 256;
          Result := Result + chr(x);
        end;
      end
      else
        Exit;
    end;
  end;

var
  OldConfigFilename: AnsiString;
  IniFile: TIniFile;
  i: Integer;
  item: TReplaceListItem;
begin
  {$IfDef Logging}Logger.Add('TSettingsManager.ParseOldConfigFile ...');{$EndIf}

  Result := false;
  if not (lowercase(ParamStr(1)) = '/updated') then
  begin
    {$IfDef Logging}Logger.Add('missing /updated parameter, exit');{$EndIf}
    exit;
  end;
  OldConfigFilename := AppDataDir + UTF8Decode('\Config.cfg');
  {$IfDef Logging}Logger.Add('OldConfigFilename = ' + OldConfigFilename);{$EndIf}
  if FileExists(OldConfigFilename) and not FileExists(fFilename) then
  begin
    {$IfDef Logging}Logger.Add('Conversion started');{$EndIf}
    IniFile := TIniFile.Create(OldConfigFilename);
    //CONFIG_FILE_VERSION := Ini.ReadInteger('General', 'ConfigFileVersion', 0);
    Settings.Clipboard.MaxEntryCount.Value := IniFile.ReadInteger('Clipboard', 'MaxEntryCount', Settings.Clipboard.MaxEntryCount.Value);
    Settings.Clipboard.MaxMenuEntryLength.Value := IniFile.ReadInteger('Clipboard', 'MaxEntryLength', Settings.Clipboard.MaxMenuEntryLength.Value);
    Settings.Clipboard.Window.DisplayDuration.Value := IniFile.ReadInteger('Clipboard', 'Anzeigedauer', Settings.Clipboard.Window.DisplayDuration.Value);

{      try
  //      MessageBox(0, 'Loading Scroll-Values', '', 64);
        StringSetting[SETTING_SCROLLVALUES] := Ini.ReadString('Scrolling', 'ScrollValues', SV_SCROLLVALUES);
  //      MessageBox(0, 'Loading Scroll-Values succeeded.', '', 64);
      except
  //      MessageBox(0, 'Scroll-Values failed + Trying to reload standards:', '', 64);
        StringSetting[SETTING_SCROLLVALUES] := SV_SCROLLVALUES;
      end;}


    {$IfDef Logging}Logger.Add('Block 1 done');{$EndIf}

    Settings.Hotkeys.CloseAllWindows.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'CloseAllWindows', ''), Settings.Hotkeys.CloseAllWindows.Value);
    Settings.Hotkeys.MakeWindowTopMost.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'MakeWindowTopMost', ''), Settings.Hotkeys.MakeWindowTopMost.Value);
    Settings.Hotkeys.NavigateUp.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'NavigateClipboardUp', ''), Settings.Hotkeys.NavigateUp.Value);
    Settings.Hotkeys.NavigateDown.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'NavigateClipboardDown', ''), Settings.Hotkeys.NavigateDown.Value);
    Settings.Hotkeys.NavigateUp2.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'NavigateClipboardUp2', ''), Settings.Hotkeys.NavigateUp2.Value);
    Settings.Hotkeys.NavigateDown2.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'NavigateClipboardDown2', ''), Settings.Hotkeys.NavigateDown2.Value);
    Settings.Hotkeys.HideWindow.Value := StrToShortcut(IniFile.ReadString('Hotkeys', 'HideWindow', ''), Settings.Hotkeys.HideWindow.Value);

    {$IfDef Logging}Logger.Add('Block 2 done');{$EndIf}

    Settings.Mouse.ScrollWindowUnderMouse.Value := IniFile.ReadBool('Scrolling', 'ImprovedScrolling', Settings.Mouse.ScrollWindowUnderMouse.Value);
    Settings.Mouse.KineticScrollingEnabled.Value := IniFile.ReadBool('Scrolling', 'ScrollOverrun', Settings.Mouse.KineticScrollingEnabled.Value);
    Settings.Clipboard.EnableClipboardExtension.Value := IniFile.ReadBool('Scrolling', 'ExtendedClipboard', Settings.Clipboard.EnableClipboardExtension.Value);
    Settings.Clipboard.RestoreAfterReboot.Value := IniFile.ReadBool('Scrolling', 'RestoreAfterReboot', Settings.Clipboard.RestoreAfterReboot.Value);
    Settings.Clipboard.AutoReplace.Value := IniFile.ReadBool('Scrolling', 'AutoReplace', Settings.Clipboard.AutoReplace.Value);
    //Settings.Clipboard.Arfix.Value := IniFile.ReadBool('Scrolling', 'EnableARFIX', Settings.Mouse.ScrollWindowUnderMouse.Value);

    {$IfDef Logging}Logger.Add('Block 3 done');{$EndIf}

    if IniFile.ReadBool('Clipboard', 'ForceHotkeyNavigateCB', false) then
    begin
      Settings.Hotkeys.NavigateUp.SetForce(true);
      Settings.Hotkeys.NavigateUp2.SetForce(true);
      Settings.Hotkeys.NavigateDown.SetForce(true);
      Settings.Hotkeys.NavigateDown2.SetForce(true);
    end;

    {$IfDef Logging}Logger.Add('Block 4 done');{$EndIf}

    Settings.Clipboard.Window.BackColor.Value := IniFile.ReadInteger('ClipboardWindow', 'BackgroundColor', Settings.Clipboard.Window.BackColor.Value);
    Settings.Clipboard.Window.ForeColor.Value := IniFile.ReadInteger('ClipboardWindow', 'ForegroundColor', Settings.Clipboard.Window.ForeColor.Value);
    Settings.Clipboard.Window.Transparency.Value := IniFile.ReadInteger('ClipboardWindow', 'Transparency', Settings.Clipboard.Window.Transparency.Value);
    Settings.Clipboard.Window.FontSize.Value := IniFile.ReadInteger('ClipboardWindow', 'FontSize', Settings.Clipboard.Window.FontSize.Value);
    Settings.Clipboard.Window.FontName.Value := IniFile.ReadString('ClipboardWindow', 'Fontname', Settings.Clipboard.Window.FontName.Value);

    {$IfDef Logging}Logger.Add('Block 5 done');{$EndIf}

    Settings.Clipboard.StandardCopyMode.Value := IniFile.ReadInteger('Clipboard', 'CopyMode', Settings.Clipboard.StandardCopyMode.Value);
    //IntegerSetting[SETTING_COPY_BLOCK_TIME_LIMIT] := Ini.ReadInteger('Clipboard', 'CopyBlockTimeLimit', SV_COPY_BLOCK_TIME_LIMIT);
    //IntegerSetting[SETTING_INSERT_COPY_BLOCK_TIME_LIMIT] := Ini.ReadInteger('Clipboard', 'InsertCopyBlockTimeLimit', SV_INSERT_COPY_BLOCK_TIME_LIMIT);

    //{$Message Warn 'Diese beiden Settings sollten sich gegenseitig ausschließen, oder anders abbilden'}
    //Settings.Mouse.MiddleMouseBtnClipboardMenu.Value := IniFile.ReadBool('Clipboard', 'MiddleMouseButtonOpensClipboardMenu', Settings.Mouse.MiddleMouseBtnClipboardMenu.Value);
    Settings.Mouse.MiddleMouseBtnDoubleClick.Value := IniFile.ReadBool('Clipboard', 'MiddleMouseButtonMakesDoubleClick', Settings.Mouse.MiddleMouseBtnDoubleClick.Value);

    {$IfDef Logging}Logger.Add('Block 6 done');{$EndIf}

    i := 0;
    Settings.Clipboard.ReplaceList.BeginUpdate;
    Settings.Clipboard.ReplaceList.Clear;
    while IniFile.SectionExists('ReplaceList' + IntToStr(i)) do
    begin
      {$IfDef Logging}Logger.Add('ReplaceListItem no '+ INtToStr(i));{$EndIf}
      item := TReplaceListItem.Create(Settings.Clipboard.ReplaceList);
      item.CaseSensitive := IniFile.ReadBool('ReplaceList' + IntToStr(i), 'CaseSensitive', false);
      try
        item.LookFor := Decode64(IniFile.ReadString('ReplaceList' + IntToStr(i), 'Find', ''));
      except
        item.LookFor := '';
      end;
      try
        item.ReplaceWith := Decode64(IniFile.ReadString('ReplaceList' + IntToStr(i), 'Replace', ''));
      except
        item.ReplaceWith := '';
      end;
      item.Name := IniFile.ReadString('ReplaceList' + IntToStr(i), 'Name', '');
      item.IsRegEx := IniFile.ReadBool('ReplaceList' + IntToStr(i), 'IsRegEx', false);
      item.OnlyExtract := IniFile.ReadBool('ReplaceList' + IntToStr(i), 'OnlyExtract', false);
      Inc(i);
    end;
    Settings.Clipboard.ReplaceList.EndUpdate;
    {$IfDef Logging}Logger.Add('Block 7 done');{$EndIf}
    IniFile.Free;

    {$IfDef Logging}Logger.Add('Saving new config');{$EndIf}
    SaveToFile;
    {$IfDef Logging}Logger.Add('Deleting old');{$EndIf}
    DeleteFile(OldConfigFilename);
    {$IfDef Logging}Logger.Add('Voila. Done :)');{$EndIf}
  end;
end; }

destructor TSettingsManager.Destroy;
begin
  FreeAndNil(Settings);
end;


end.

