unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
	ComCtrls,
	Menus, Buttons, FensterToolsScrollGraphSettings, ListBoxForReplaceList,
  (*SETTINGS*)
  SettingsManager, SettingsBase, SettingsBaseTypes, Hotkey.Actions, Hotkey.Manager,
  (*CLIPBOARD*)
  ReplaceList, Hotkey,
  ThemedListView,
  Clipboard.Manager,
  (*PATCH SYSTEM*)
  LivePatching,
  (*WINDOW MANAGER*)
  WindowManager,
  (*UPDATER*)
  UpdateCheck,
  (*HOOK SYSTEM*)
  UserInputProvider, MouseBehaviour, KeyPatternMatching,
	BCButton, LResources, AsyncHTTPRequest, ExtendedThreads, DPIAwareForm, TrayIconEx, FensterToolsCommon, Windows, Types, dateutils, RSAEncryption;

type

	{ Tfrm_Settings }

  Tfrm_Settings = class(TDPIAwareForm)
				ApplicationProperties1: TApplicationProperties;
        btnCheckForUpdate: TBCButton;
				btnReplaceRuleAdd: TBCButton;
				btnReplaceRuleDown: TBCButton;
				btnReplaceRuleUp: TBCButton;
				btnReplaceRuleDelete: TBCButton;
        btnSend: TButton;
        btnShowDebugConsole: TButton;
				cbx_ImprovedScrolling: TCheckBox;
				cbx_KineticScrollMode: TCheckBox;
				cbxEnableClipboardExtension: TCheckBox;
				cbxRestoreCBAfterReboot: TCheckBox;
				cbxAutostart: TCheckBox;
				cbxMMBTN_DblClick: TCheckBox;
				cbxAutoReplace: TCheckBox;
        cbxForceCBHotkeys: TCheckBox;
        cbxForceWindowHotkeys: TCheckBox;
        cbxAutoInsert: TCheckBox;
        ColorDialog1: TColorDialog;
        cbStandardCopyMode: TComboBox;
        cbxFeedbackKind: TComboBox;
        cbUpdateMode: TComboBox;
        edit_Name: TEdit;
        edit_mailaddress: TEdit;
        FontDialog1: TFontDialog;
				FTScrollGraphSettings1: TFTScrollGraphSettings;
				gbProgram: TGroupBox;
				gb_ClipboardWindow: TGroupBox;
        GroupBox2: TGroupBox;
				gbMouse: TGroupBox;
				gbReplaceRules: TGroupBox;
				gb_navigate_clipboard: TGroupBox;
				hk_MakeWindowTopMost: THotkey;
				hk_HideWindow: THotkey;
				hk_down2: THotkey;
				hk_up: THotkey;
				hk_down: THotkey;
				hk_CloseAllWindows: THotkey;
				hk_up2: THotkey;
        hkBlockCopy: THotkey;
        imgLogoGeneral: TImage;
        imgFeedback: TImage;
        imgLogoAbout: TImage;
        lblForRequests: TLabel;
				lbl_CbWndTransparency: TLabel;
				lbl_transparent: TLabel;
				lbl_opaque: TLabel;
        lblAboutHeader: TLabel;
        lblMail: TLabel;
        lblVersionGeneral: TLabel;
        lbl_HideWindow: TLabel;
        lbl_MakeWindowTopMost: TLabel;
        lblCloseAllWindows: TLabel;
        lblForceWindowHotkeys: TLabel;
        lblFeedbackResult: TLabel;
        lblMessage: TLabel;
        lblName: TLabel;
        lblFeedbackKind: TLabel;
        lblAbout: TLabel;
        lblUpdate: TLabel;
        lblStandardCopyMode: TLabel;
        lbl_CBWND_FontColor: TLabel;
        lbl_CBWND_BackColor: TLabel;
        lbl_CBWND_FontSize: TLabel;
				lbl_nav_up: TLabel;
				lblAutoReplace: TLabel;
				lbl_nav_down: TLabel;
				lbl_nav2: TLabel;
				lbx_ReplaceList: TListBoxForReplaceList;
        memoMessage: TMemo;
        mi_BlockDirection: TMenuItem;
        mi_bdAsCopied: TMenuItem;
        mi_bdReversed: TMenuItem;
        mi_ShowClipboardInspector: TMenuItem;
				mi_ShowDebugConsole: TMenuItem;
        MenuItem3: TMenuItem;
        mi_ClearClipboard: TMenuItem;
        mi_Reset: TMenuItem;
        mi_Edit: TMenuItem;
				mi_SeparatorHiddenWindows: TMenuItem;
				mi_HiddenWindows: TMenuItem;
				mi_cmBlockCopy: TMenuItem;
				mi_cmBlockPaste: TMenuItem;
				mi_CopyModeSeparator: TMenuItem;
				mi_cmNormal: TMenuItem;
				mi_Quit: TMenuItem;
				mi_Settings: TMenuItem;
				mi_SeparatorClipboardExtension: TMenuItem;
				mi_CopyMode: TMenuItem;
				PageControl1: TPageControl;
        pnl_CBWND_Font: TPanel;
        pnl_CBWND_ForeColor: TPanel;
				pmTaskbarIcon: TPopupMenu;
        pnl_CBWND_BackColor: TPanel;
        ListView1: TThemedListView;
        pmExtendedConfig: TPopupMenu;
        pmClipboard: TPopupMenu;
        timerLastUpdateCheck: TTimer;
        TrayIcon: TTrayIconEx;
        tsAbout: TTabSheet;
				tsAllgemein: TTabSheet;
				tsClipboard: TTabSheet;
				tsScrollBehaviour: TTabSheet;
				tsExtended: TTabSheet;
				tsWindows: TTabSheet;
				tsAutoReplace: TTabSheet;
				tb_CBWND_Transparency: TTrackBar;
		procedure ApplicationProperties1EndSession(Sender: TObject);
  procedure btnReplaceRuleAddClick(Sender: TObject);
		procedure btnReplaceRuleDeleteClick(Sender: TObject);
		procedure btnReplaceRuleDownClick(Sender: TObject);
		procedure btnReplaceRuleUpClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnCheckForUpdateClick(Sender: TObject);
    procedure CbExtEnabledChange(Sender: TObject);
    procedure CbWndColorsChanged(Sender: TObject);
    procedure CbWndFontChanged(Sender: TObject);
    procedure cbxForceCBHotkeysClick(Sender: TObject);
    procedure cbxForceWindowHotkeysClick(Sender: TObject);
	procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure gb_navigate_clipboardResize(Sender: TObject);
		procedure HotkeyAlreadyInUse(Sender: TObject; Msg: UnicodeString);
		procedure lbx_ReplaceListDblClick(Sender: TObject);
    procedure lbx_ReplaceListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mi_ClearClipboardClick(Sender: TObject);
    procedure mi_cmBlockCopyClick(Sender: TObject);
    procedure mi_cmBlockPasteClick(Sender: TObject);
    procedure mi_cmNormalClick(Sender: TObject);
    procedure mi_ShowClipboardInspectorClick(Sender: TObject);
		{$IfDef Logging}procedure mi_ShowDebugConsoleClick(Sender: TObject);{$EndIf}
		procedure mi_QuitClick(Sender: TObject);
    procedure mi_ResetClick(Sender: TObject);
		procedure mi_SettingsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure pnl_CBWND_FontClick(Sender: TObject);
    procedure pnl_CBWND_BackColorClick(Sender: TObject);
    procedure pnl_CBWND_ForeColorClick(Sender: TObject);
    procedure ClearFeedbackResult(Sender: TObject);
    procedure timerLastUpdateCheckTimer(Sender: TObject);
    procedure TrayIconBalloonClick(Sender: TObject);
		procedure TrayIconClick(Sender: TObject);
    procedure tsScrollBehaviourResize(Sender: TObject);
  private
    //procedure ClipboardChanged(ccAction: TClipboardChangedAction; ID: Integer = -1; Str: UnicodeString = ''; Check: boolean = false);
    //procedure CopyModeChanged(CopyMode: TCopyMode);
    procedure CBMenuItemClicked(Sender: TObject);
    procedure CopyModeChanged(Sender: TObject);

    { Is called by ServiceConnection when response from service to install update command has arrived }
    procedure InstallUpdateResultReceived(UpdateInstallationReady: Boolean);

    { Is called by Updater }
    procedure UpdateStatusChanged(Sender: TUpdater);

    { Is called by Updater }
    procedure UpdateNotifyUser(Sender: TUpdater);

    procedure FeedbackRequestStatusUpdate(Sender: TObject);
    procedure OpenCBMenu(var msg: TMessage); message WM_SHOWClIPBOARDPOPUP;
    procedure ServiceConnectionEstablished(Sender: TObject);
    //procedure ForceHotkeyCbxChange(Sender: TObject);
  public
  end;

var
  frm_Settings: Tfrm_Settings;
  WindowsShutdown: Boolean = false;
const
  STANDARD_ICON_SIZE_X = 24;
  STANDARD_ICON_SIZE_Y = 24;
//  NEW_VERSION_INSTALLED_HINT_TIMEOUT = 12*60*60*1000; // 12 h

implementation

uses ToolBox, Regeleditor, {$IfDef Logging}DebugConsole, Logging, {$EndIf}ServiceConnection, ClipboardInspector, ApplicationRestart;

{$R *.lfm}
{$R Manifest.rc}

{.$Define DisableMouseHook}

{ Tfrm_Settings }


procedure Tfrm_Settings.FormCreate(Sender: TObject);
var
  //x: Byte;
  tmpIcon : TIcon;
  tmpSize : TSize;
begin
  {try
    x := 100;
    x := x * 3;
    DebugLn(IntToStr(x));
  except on E: Exception do
    DebugLn('RangeChecks are ON');
  end;}
  DesignTimeDPI := 120;

  lblVersionGeneral.Caption := GetCurrentVersion(true);
  lblAbout.Caption := GetCurrentVersion(true, true, true) + #13 + #10 + {$I CompileDate.inc} + #13 + #10 + '© Adrian Zinke';

  tmpIcon := TIcon.Create;
  tmpIcon.LoadFromResourceName(hInstance, 'MAINICON');

  tmpSize.cx := GetSystemMetrics(SM_CXSMICON); tmpSize.cy := GetSystemMetrics(SM_CYSMICON);
  frm_Settings.Icon.Assign(tmpIcon);
  frm_Settings.Icon.Current := frm_Settings.Icon.GetBestIndexForSize(tmpSize);

  TrayIcon.Icon.Assign(tmpIcon);
  TrayIcon.Icon.Current := TrayIcon.Icon.GetBestIndexForSize(tmpSize);
  TrayIcon.Show;

  tmpSize.cx := imgLogoGeneral.Width;
  tmpSize.cy := imgLogoGeneral.Height;
  tmpIcon.Current := tmpIcon.GetBestIndexForSize(tmpsize);
  imgLogoGeneral.Picture.Assign(tmpIcon);
  imgLogoAbout.Picture.Assign(tmpIcon);
  tmpIcon.free;
  {$IfDef Debug}mi_ShowDebugConsole.Visible := true;{$EndIf}

  {$IfDef Logging}
	Logger := TDebugLog.Create(@frm_DebugConsole.LogNotification); // frm_DebugConsole must already be created at this point
  Logger.RegisterThread('Main');
  Logger.Add('FensterTools, ' + GetCurrentVersion(true, true, true));
  ListView1.AnchorSideBottom.Side := asrTop;
  ListView1.AnchorSideBottom.Control := btnShowDebugConsole;
  btnShowDebugConsole.Visible := true;
  mi_ShowDebugConsole.Visible := true;
  btnShowDebugConsole.OnClick := @mi_ShowDebugConsoleClick;
  mi_ShowDebugConsole.OnClick := @mi_ShowDebugConsoleClick;
  {$EndIf}

  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  if Application.StartParams.MustShow then Visible := true;

  SettingsMan := TSettingsManager.Create(ConfigFileName);
  {$IfDef Logging}Logger.Add('SettingsManager created.');{$EndIf}
  // LoadFromFile must be done before any component binding or actor class creation occurs
  SettingsMan.LoadFromFile;

  // now bind all components
  Settings.Clipboard.EnableClipboardExtension.Bind(cbxEnableClipboardExtension);
  //Settings.Clipboard.PatchOpenClipboard.Bind(cbxPatchCB);
  Settings.Clipboard.RestoreAfterReboot.Bind(cbxRestoreCBAfterReboot);
  Settings.Clipboard.AutoReplace.Bind(cbxAutoReplace);
  Settings.Clipboard.ReplaceList.Bind(lbx_ReplaceList);
  Settings.Clipboard.StandardCopyMode.Bind(cbStandardCopyMode);
  Settings.Clipboard.AutoInsert.Bind(cbxAutoInsert);

  Settings.LastUsername.Bind(edit_Name);
  Settings.LastMailAddress.Bind(edit_mailaddress);

  Settings.Mouse.ScrollWindowUnderMouse.Bind(cbx_ImprovedScrolling);
  if WindowsVersion.IsWin10OrGreater then
  begin
    cbx_ImprovedScrolling.Enabled := false;
    Settings.Mouse.ScrollWindowUnderMouse.Value := true;
  end;
  Settings.Mouse.KineticScrollingEnabled.Bind(cbx_KineticScrollMode);
  Settings.Mouse.KineticScrollingOptions.Bind(FTScrollGraphSettings1);

  Settings.Hotkeys.GetHotkey(haNavigateDown).Bind(hk_down);
  Settings.Hotkeys.GetHotkey(haNavigateDown, 1).Bind(hk_down2);
  Settings.Hotkeys.GetHotkey(haNavigateUp).Bind(hk_up);
  Settings.Hotkeys.GetHotkey(haNavigateUp, 1).Bind(hk_up2);
  Settings.Hotkeys.GetHotkey(haBlockCopy).Bind(hkBlockCopy);

  Settings.Hotkeys.GetHotkey(haCloseAllWindows).Bind(hk_CloseAllWindows);
  Settings.Hotkeys.GetHotkey(haMakeWindowTopmost).Bind(hk_MakeWindowTopMost);
  Settings.Hotkeys.GetHotkey(haHideWindow).Bind(hk_HideWindow);

  Settings.Autostart.Bind(cbxAutostart);
  Settings.Update.Mode.Bind(cbUpdateMode);
  Settings.Mouse.MiddleMouseBtnDoubleClick.Bind(cbxMMBTN_DblClick);

  //Settings.Clipboard.Window.FontSize.ChangeHandler.Add(@CbWndFontChanged);
  //Settings.Clipboard.Window.FontName.ChangeHandler.Add(@CbWndFontChanged);
  //Settings.Clipboard.Window.ForeColor.ChangeHandler.Add(@CbWndColorsChanged);
  //Settings.Clipboard.Window.BackColor.ChangeHandler.Add(@CbWndColorsChanged);

  Settings.Clipboard.EnableClipboardExtension.ChangeHandler.Add(@CbExtEnabledChange);

  // wrapper settings
{  Settings.Hotkeys.ForceClipboardHotkeys.Bind(cbxForceCBHotkeys);
  Settings.Hotkeys.ForceWindowHotkeys.Bind(cbxForceWindowHotkeys);
  Settings.Hotkeys.ForceClipboardHotkeys.AddChangeHandler(@ForceHotkeyCbxChange);
  Settings.Hotkeys.ForceWindowHotkeys.AddChangeHandler(@ForceHotkeyCbxChange); }

  // create all actor classes (aka Controller-Objects)
  HelperSvc := TServiceConnection.Create; // HelperSvc is needed by Patcher
  HelperSvc.OnInstallUpdateResultReceived := @InstallUpdateResultReceived;
  Patcher := TPatcher.Create; // patcher must be created next

  UserInput := TUserInputProvider.Create; // must be created before HotkeyHandler
  MouseBehaviourManager := TMouseBehaviourManager.Create;
  UserInputLog := TUserInputLog.Create;

  HotkeyActionManager := THotkeyActionManager.Create;
  HotkeyManager := THotkeyManager.Create;          // must be created before Clipboard and WindowManager
  ClipbrdManager := TClipboardManager.Create;
  //Clipboard.Patcher := Patcher;
  //Clipboard.OnClipboardChanged := @ClipboardChanged;
  ClipbrdManager.OnCopyModeChanged := @CopyModeChanged;

  WndManager := TWindowManager.Create(mi_HiddenWindows, mi_SeparatorHiddenWindows);

  Updater := TUpdater.Create;
  Updater.OnUpdateStatusChanged := @UpdateStatusChanged;
  Updater.OnUpdateNotifyUser := @UpdateNotifyUser;

  // and now update all bindings at once
  Settings.UpdateAll;
  HelperSvc.OnConnectionEstablished := @ServiceConnectionEstablished;
  HotkeyManager.Active := true;
end;

procedure Tfrm_Settings.ServiceConnectionEstablished(Sender: TObject);
begin
  Patcher.Active := true;
end;

procedure Tfrm_Settings.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Visible then
  begin
    CanClose := false;
    Hide;
	end;
end;

procedure Tfrm_Settings.btnReplaceRuleAddClick(Sender: TObject);
var
  newItem: TReplaceListItem;
begin
  Settings.Clipboard.ReplaceList.BeginUpdate;
  newItem := TReplaceListItem(Settings.Clipboard.ReplaceList.Add);
  frm_Regeleditor.ReplaceListItem := newItem;
  if frm_Regeleditor.ShowModal <> mrOK then newItem.Free;
  Settings.Clipboard.ReplaceList.EndUpdate;
end;

procedure Tfrm_Settings.ApplicationProperties1EndSession(Sender: TObject);
begin
  WindowsShutdown := true;
  {if Patcher <> nil then }Patcher.Active := false; // to prevent unneccessary unpatching
  {$Message Warn 'Cleanup here.'}
  // Application.Terminate needn't be called, Windows will end the Application
end;

procedure Tfrm_Settings.btnReplaceRuleDeleteClick(Sender: TObject);
begin
  if (lbx_ReplaceList.SelectedReplaceListItem <> nil) then
  begin
    if MessageBoxExW(Handle, PWideChar(UTF8Decode('Möchten Sie die Ersetzungsregel "') + lbx_ReplaceList.SelectedReplaceListItem.UnicodeName + UTF8Decode('" wirklich löschen?')), PWideChar(UTF8Decode('Frage')), MB_YESNO or MB_ICONQUESTION, LANG_GERMAN) = IDYES then
    begin
      Settings.Clipboard.ReplaceList.BeginUpdate;
      lbx_ReplaceList.SelectedReplaceListItem.Free;
      Settings.Clipboard.ReplaceList.EndUpdate;
    end;
  end;
end;

procedure Tfrm_Settings.btnReplaceRuleDownClick(Sender: TObject);
begin
  if (lbx_ReplaceList.SelectedReplaceListItem <> nil) and (lbx_ReplaceList.SelectedIndex < lbx_ReplaceList.Items.Count-1) then
  begin
    lbx_ReplaceList.SwapItems(lbx_ReplaceList.SelectedIndex, lbx_ReplaceList.SelectedIndex+1);
    lbx_ReplaceList.SelectedIndex := lbx_ReplaceList.SelectedIndex + 1;
    // oder
    { Settings.Clipboard.ReplaceList.BeginUpdate;
    Settings.Clipboard.ReplaceList.Exchange(lbx_ReplaceList.SelectedIndex, lbx_ReplaceList.SelectedIndex+1);
    Settings.Clipboard.ReplaceList.EndUpdate; }
	end;
end;

procedure Tfrm_Settings.btnReplaceRuleUpClick(Sender: TObject);
begin
  if (lbx_ReplaceList.SelectedReplaceListItem <> nil) and (lbx_ReplaceList.SelectedIndex > 0) then
  begin
    lbx_ReplaceList.SwapItems(lbx_ReplaceList.SelectedIndex, lbx_ReplaceList.SelectedIndex-1);
    lbx_ReplaceList.SelectedIndex := lbx_ReplaceList.SelectedIndex - 1;
    // oder
    { Settings.Clipboard.ReplaceList.BeginUpdate;
    Settings.Clipboard.ReplaceList.Exchange(lbx_ReplaceList.SelectedIndex, lbx_ReplaceList.SelectedIndex-1);
    Settings.Clipboard.ReplaceList.EndUpdate; }
	end;
end;

procedure Tfrm_Settings.ClearFeedbackResult(Sender: TObject);
begin
  imgFeedback.Picture.Clear;
  lblFeedbackResult.Caption := '';
end;

procedure Tfrm_Settings.timerLastUpdateCheckTimer(Sender: TObject);
begin
  UpdateStatusChanged(Updater);
end;

procedure Tfrm_Settings.TrayIconBalloonClick(Sender: TObject);
begin
  if not Visible then
  begin
    Show;
    PageControl1.ActivePage := tsAllgemein;
  end;
  Updater.CheckForUpdate(true);
end;

procedure Tfrm_Settings.FeedbackRequestStatusUpdate(Sender: TObject);
begin
  if TAsyncHTTPRequest(Sender).RequestStatus = rsCompleted then
  begin
    lblFeedbackResult.Caption := 'Vielen Dank';
    lblFeedbackResult.Font.Color := clGreen;
    imgFeedback.LoadImageFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_CHECK), imgFeedback.Width, imgFeedback.Height);
  end else if TAsyncHTTPRequest(Sender).RequestStatus = rsFailed then
  begin
    lblFeedbackResult.Caption := 'Fehler beim Versenden';
    lblFeedbackResult.Font.Color := clRed;
    imgFeedback.Picture.LoadFromFile(RelativeToAbsolutePath(REL_PATH_ICON_CROSS));
  end;

  if (TAsyncHTTPRequest(Sender).RequestStatus = rsCompleted) or (TAsyncHTTPRequest(Sender).RequestStatus = rsFailed) then
  begin
    btnSend.Enabled := true;
    edit_Name.Enabled := true;
    edit_mailaddress.Enabled := true;
    cbxFeedbackKind.Enabled := true;
    memoMessage.Enabled := true;
    memoMessage.Lines.Clear;
    Sender.Free;
  end;
end;

procedure Tfrm_Settings.OpenCBMenu(var msg: TMessage);
begin
  //pmTaskbarIcon.PopUp;
  pmClipboard.PopUp(msg.wParam, msg.lParam);
end;

procedure Tfrm_Settings.btnSendClick(Sender: TObject);
var
  req: TAsyncMultipartHTTPRequest;
  //j: TJSONObject;
begin
  if Length(edit_Name.Text) < 1 then
  begin
    MessageBoxW(Handle, PWideChar(UTF8Decode('Bitte geben Sie Ihren Namen an.')), PWideChar(UTF8Decode('Fehler')), MB_OK or MB_ICONEXCLAMATION);
    exit;
  end;
  if Length(edit_mailaddress.Text) < 5 then
  begin
    MessageBoxW(Handle, PWideChar(UTF8Decode('Bitte geben Sie Ihre Emailadresse für eventuelle Rückfragen an.')), PWideChar(UTF8Decode('Fehler')), MB_OK or MB_ICONEXCLAMATION);
    exit;
  end;
  if Length(memoMessage.Lines.Text) < 4 then
  begin
    MessageBoxW(Handle, PWideChar(UTF8Decode('Bitte geben Sie eine aussagekräftige Nachricht ein.')), PWideChar(UTF8Decode('Fehler')), MB_OK or MB_ICONEXCLAMATION);
    exit;
  end;

  btnSend.Enabled := false;
  edit_Name.Enabled := false;
  edit_mailaddress.Enabled := false;
  cbxFeedbackKind.Enabled := false;
  memoMessage.Enabled := false;
  lblFeedbackResult.Font.Color := clBlack;
  lblFeedbackResult.Caption := 'Daten werden übertragen ...';
  req := TAsyncMultipartHTTPRequest.Create(UTF8Decode(FeedbackURL));
  req.ThreadClass := TBasicThreadWithCallbackWnd;
  req.OnStatusUpdate := @FeedbackRequestStatusUpdate;

  req.AddPart('name', edit_Name.Text);
  req.AddPart('email', edit_mailaddress.Text);
  req.AddPart('kind', cbxFeedbackKind.Text);

  req.AddPart('text', memoMessage.Text);

  req.Post;
{  j := TJSONObject.Create;
  j.Add('Name', edit_Name.Text);
  j.Add('Email', edit_mailaddress.Text);
  j.Add('Kind', cbxFeedbackKind.Text);
  j.Add('Text', memoMessage.Lines.Text);
  req.Post(j);
  j.Free;  }
end;

procedure Tfrm_Settings.btnCheckForUpdateClick(Sender: TObject);
begin
  Updater.CheckForUpdate(true);
end;

procedure BuildSettingsTree(Parent: TSettingsBase);
var
  sb: TObject;
  lvi: TListItem;
begin
  if Parent = nil then
  begin
    frm_Settings.ListView1.Clear;
    Parent := Settings;
  end;

  for sb in Parent.Properties do
  begin
    if not sb.InheritsFrom(TSettingsBase) then continue;
    if sb.InheritsFrom(TSettingsBaseScrollSettings) or sb.InheritsFrom(TSettingsBaseHotkey) then continue;
    if TSettingsBase(sb).Properties.Count > 0 then
      BuildSettingsTree(TSettingsBase(sb))
    else begin
      lvi := frm_Settings.ListView1.Items.Add;
      lvi.Caption := TSettingsBase(sb).Name;
      lvi.Data := sb;
      lvi.SubItems.Add(TSettingsBase(sb).ToString);
    end;
  end;
end;

procedure Tfrm_Settings.CbExtEnabledChange(Sender: TObject);
begin
  mi_CopyMode.Visible := Settings.Clipboard.EnableClipboardExtension;
  mi_SeparatorClipboardExtension.Visible := Settings.Clipboard.EnableClipboardExtension;
  gb_navigate_clipboard.Enabled := Settings.Clipboard.EnableClipboardExtension;
  gb_ClipboardWindow.Enabled := Settings.Clipboard.EnableClipboardExtension;
  lblStandardCopyMode.Enabled := Settings.Clipboard.EnableClipboardExtension;
  cbStandardCopyMode.Enabled := Settings.Clipboard.EnableClipboardExtension;
  cbxRestoreCBAfterReboot.Enabled := Settings.Clipboard.EnableClipboardExtension;
  pnl_CBWND_Font.Enabled := Settings.Clipboard.EnableClipboardExtension;
  hk_down.Enabled := Settings.Clipboard.EnableClipboardExtension;
end;

procedure Tfrm_Settings.CbWndColorsChanged(Sender: TObject);
begin
  //pnl_CBWND_ForeColor.Color := Settings.Clipboard.Window.ForeColor;
  //pnl_CBWND_BackColor.Color := Settings.Clipboard.Window.BackColor;
end;

procedure Tfrm_Settings.CbWndFontChanged(Sender: TObject);
begin
  //pnl_CBWND_Font.Caption := Settings.Clipboard.Window.FontName.Value + ', ' + IntToStr(Settings.Clipboard.Window.FontSize);
end;

procedure Tfrm_Settings.cbxForceCBHotkeysClick(Sender: TObject);
begin
  // read Force-Hotkey Settings from Form
  if cbxForceCBHotkeys.State = cbChecked then
  begin
    Settings.Hotkeys.GetHotkey(haNavigateUp).SetForce(true);
    Settings.Hotkeys.GetHotkey(haNavigateUp, 1).SetForce(true);
    Settings.Hotkeys.GetHotkey(haNavigateDown).SetForce(true);
    Settings.Hotkeys.GetHotkey(haNavigateDown, 1).SetForce(true);
  end else if cbxForceCBHotkeys.State = cbUnchecked then
  begin
    Settings.Hotkeys.GetHotkey(haNavigateUp).SetForce(false);
    Settings.Hotkeys.GetHotkey(haNavigateUp, 1).SetForce(false);
    Settings.Hotkeys.GetHotkey(haNavigateDown).SetForce(false);
    Settings.Hotkeys.GetHotkey(haNavigateDown, 1).SetForce(false);
  end;
end;

procedure Tfrm_Settings.cbxForceWindowHotkeysClick(Sender: TObject);
begin
  if cbxForceWindowHotkeys.State = cbChecked then
  begin
    Settings.Hotkeys.GetHotkey(haCloseAllWindows).SetForce(true);
    Settings.Hotkeys.GetHotkey(haMakeWindowTopmost).SetForce(true);
    Settings.Hotkeys.GetHotkey(haHideWindow).SetForce(true);
  end else if cbxForceWindowHotkeys.State = cbUnchecked then
  begin
    Settings.Hotkeys.GetHotkey(haCloseAllWindows).SetForce(false);
    Settings.Hotkeys.GetHotkey(haMakeWindowTopmost).SetForce(false);
    Settings.Hotkeys.GetHotkey(haHideWindow).SetForce(false);
  end;
end;

   {
procedure Tfrm_Settings.ForceHotkeyCbxChange(Sender: TObject);
begin
  // hier wird kein ChangeHandler getriggert, aber das ist kein Problem, weil
  // die Hotkeys erst beim Schließen des Formulars auf Active := true gesetzt werden
  // und dabei wird ...Value.Force neu verarbeitet
  // und beim Schließen wird es auch korrekt weggespeichert
  Settings.Hotkeys.NavigateDown.Value.Force := Settings.Hotkeys.ForceClipboardHotkeys;
  Settings.Hotkeys.NavigateDown2.Value.Force := Settings.Hotkeys.ForceClipboardHotkeys;
  Settings.Hotkeys.NavigateUp.Value.Force := Settings.Hotkeys.ForceClipboardHotkeys;
  Settings.Hotkeys.NavigateUp2.Value.Force := Settings.Hotkeys.ForceClipboardHotkeys;

  Settings.Hotkeys.CloseAllWindows.Value.Force := Settings.Hotkeys.ForceWindowHotkeys;
  Settings.Hotkeys.HideWindow.Value.Force := Settings.Hotkeys.ForceWindowHotkeys;
  Settings.Hotkeys.MakeWindowTopMost.Value.Force := Settings.Hotkeys.ForceWindowHotkeys;
end;     }

procedure Tfrm_Settings.FormDestroy(Sender: TObject);
begin
  // reversed order as in FormCreate
  TryFreeAndNil(Updater);
  TryFreeAndNil(WndManager);
  TryFreeAndNil(ClipbrdManager);

  TryFreeAndNil(HotkeyManager);
  TryFreeAndNil(HotkeyActionManager);

  TryFreeAndNil(UserInputLog);
  TryFreeAndNil(MouseBehaviourManager);
  TryFreeAndNil(UserInput);

  TryFreeAndNil(Patcher);
  SettingsMan.SaveToFile;
  SettingsMan.Free;
  TryFreeAndNil(HelperSvc);

  {$IfDef Logging}TryFreeAndNil(Logger);{$EndIf}
end;

procedure Tfrm_Settings.FormHide(Sender: TObject);
begin
  timerLastUpdateCheck.Enabled := false;
  SettingsMan.SaveToFile;
  HotkeyManager.Active := true;
  Settings.Clipboard.Window.DeferChangeHandler := false;
end;

procedure Tfrm_Settings.FormShow(Sender: TObject);
begin
  Settings.Clipboard.Window.DeferChangeHandler := true;

  HotkeyManager.Active := false;

  // Write Force-Hotkey-Settings to Checkboxes
  if Settings.Hotkeys.GetHotkey(haHideWindow).Value.Force and Settings.Hotkeys.GetHotkey(haCloseAllWindows).Value.Force and Settings.Hotkeys.GetHotkey(haMakeWindowTopmost).Value.Force then
    cbxForceWindowHotkeys.State := cbChecked
  else if not Settings.Hotkeys.GetHotkey(haHideWindow).Value.Force and not Settings.Hotkeys.GetHotkey(haCloseAllWindows).Value.Force and not Settings.Hotkeys.GetHotkey(haMakeWindowTopmost).Value.Force then
    cbxForceWindowHotkeys.State := cbUnchecked
  else
    cbxForceWindowHotkeys.State := cbGrayed;

  if Settings.Hotkeys.GetHotkey(haNavigateDown).Value.Force and Settings.Hotkeys.GetHotkey(haNavigateDown, 1).Value.Force and Settings.Hotkeys.GetHotkey(haNavigateUp).Value.Force and Settings.Hotkeys.GetHotkey(haNavigateUp, 1).Value.Force then
    cbxForceCBHotkeys.State := cbChecked
  else if not Settings.Hotkeys.GetHotkey(haNavigateDown).Value.Force and not Settings.Hotkeys.GetHotkey(haNavigateDown, 1).Value.Force and not Settings.Hotkeys.GetHotkey(haNavigateUp).Value.Force and not Settings.Hotkeys.GetHotkey(haNavigateUp, 1).Value.Force then
    cbxForceCBHotkeys.State := cbUnchecked
  else
    cbxForceCBHotkeys.State := cbGrayed;

  UpdateStatusChanged(Updater);
end;

procedure Tfrm_Settings.gb_navigate_clipboardResize(Sender: TObject);
begin
  gb_navigate_clipboard.DisableAutoSizing;
  imgFeedback.Height := imgFeedback.Width;
  hk_up.Width := gb_navigate_clipboard.Width div 2 - 30;
  hk_down.Width := hk_up.Width;
  gb_navigate_clipboard.EnableAutoSizing;
end;

procedure Tfrm_Settings.HotkeyAlreadyInUse(Sender: TObject; Msg: UnicodeString);
begin
  if not Visible then
  begin
    TrayIcon.BalloonIconType := bitWarning;
    TrayIcon.ShowBalloonHint('Tastenkombination belegt', UTF8Encode(Msg));
	end;
end;

procedure Tfrm_Settings.lbx_ReplaceListDblClick(Sender: TObject);
begin
  if lbx_ReplaceList.SelectedReplaceListItem <> nil then
  begin
    frm_Regeleditor.ReplaceListItem := lbx_ReplaceList.SelectedReplaceListItem;
    frm_Regeleditor.ShowModal;
	end;
end;

procedure Tfrm_Settings.lbx_ReplaceListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then btnReplaceRuleDeleteClick(Sender);
end;

procedure Tfrm_Settings.ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  {$Message Warn 'This causes an error.'}
//  if not TSettingsBase(Item.Data).IsDefaultValue then Sender.Canvas.Font.Style := [fsBold];
end;

procedure Tfrm_Settings.ListView1DblClick(Sender: TObject);
var
  s: String;
begin
  if ListView1.Selected = nil then exit;
  if TSettingsBase(Listview1.Selected.Data) is TSettingsBaseBool then
  begin
    TSettingsBaseBool(Listview1.Selected.Data).Value := not TSettingsBaseBool(Listview1.Selected.Data).Value;
  end else if TSettingsBase(Listview1.Selected.Data) is TSettingsBaseInteger then
  begin
    TSettingsBaseInteger(Listview1.Selected.Data).Value := InputQueryInt(Listview1.Selected.Caption, 'Bitte einen Wert eingeben', TSettingsBaseInteger(Listview1.Selected.Data).Value);
  end else if TSettingsBase(Listview1.Selected.Data) is TSettingsBaseString then
  begin
    s := TSettingsBaseString(Listview1.Selected.Data).Value;
    if InputQuery(Listview1.Selected.Caption, 'Bitte einen Wert eingeben', s) then
      TSettingsBaseString(Listview1.Selected.Data).Value := s;
  end;{ else if TSettingsBase(Listview1.Selected.Data) is TSettingsBaseScrollSettings then
  begin
    s := TSettingsBaseScrollSettings(Listview1.Selected.Data).Value.AsJSON;
    if InputQuery(Listview1.Selected.Caption, 'Bitte einen Wert eingeben', s) then;
      //TSettingsBaseScrollSettings(Listview1.Selected.Data).Value := s;
  end;}
  Listview1.Selected.SubItems.Strings[0] := TSettingsBase(Listview1.Selected.Data).ToString;
end;

procedure Tfrm_Settings.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  Listview1.PopupMenu.AutoPopup := Selected;
  if (item = nil) then exit;
  if TSettingsBase(item.Data) is TSettingsBaseBool then
    mi_Edit.Caption := 'Umschalten'
  else
    mi_Edit.Caption := 'Bearbeiten';
end;

procedure Tfrm_Settings.mi_ClearClipboardClick(Sender: TObject);
begin
  if ClipbrdManager <> nil then ClipbrdManager.ClearClipboard;
end;

procedure Tfrm_Settings.mi_cmBlockCopyClick(Sender: TObject);
begin
  if ClipbrdManager <> nil then ClipbrdManager.CopyMode := cmBlockCopy;
end;

procedure Tfrm_Settings.mi_cmBlockPasteClick(Sender: TObject);
begin
  if ClipbrdManager <> nil then ClipbrdManager.CopyMode := cmBlockPaste;
end;

procedure Tfrm_Settings.mi_cmNormalClick(Sender: TObject);
begin
  if ClipbrdManager <> nil then ClipbrdManager.CopyMode := cmNormal;
end;

procedure Tfrm_Settings.mi_ShowClipboardInspectorClick(Sender: TObject);
begin
  frm_ClipboardInspector.Show;
end;

{$IfDef Logging}
procedure Tfrm_Settings.mi_ShowDebugConsoleClick(Sender: TObject);
begin
  frm_DebugConsole.Show;
end;
{$EndIf}

procedure Tfrm_Settings.mi_QuitClick(Sender: TObject);
begin
  if Visible then SettingsMan.SaveToFile;
  Application.Terminate;
end;

procedure Tfrm_Settings.mi_ResetClick(Sender: TObject);
begin
  if Listview1.Selected <> nil then
  begin
    TSettingsBase(Listview1.Selected.Data).ResetToDefault;
    Listview1.Selected.SubItems.Strings[0] := TSettingsBase(Listview1.Selected.Data).ToString;
  end;
end;

procedure Tfrm_Settings.mi_SettingsClick(Sender: TObject);
begin
  frm_Settings.Show;
end;

procedure Tfrm_Settings.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = tsExtended then
  begin
    ListView1.BeginUpdate;
    ListView1.Clear;
    BuildSettingsTree(nil);
    ListView1.EndUpdate;
    Listview1.SortColumn := 0;
    Listview1.Column[1].AutoSize := true;
  end else if PageControl1.ActivePage = tsAbout then
  begin
    ClearFeedbackResult(Sender);
  end;
end;

procedure Tfrm_Settings.pnl_CBWND_FontClick(Sender: TObject);
begin
  //FontDialog1.Font.Color := TColor(Settings.Clipboard.Window.ForeColor);
  //FontDialog1.Font.Name := Settings.Clipboard.Window.FontName;
  //FontDialog1.Font.Size := Settings.Clipboard.Window.FontSize;
  //if FontDialog1.Execute then
  //begin
    //Settings.Clipboard.Window.FontName.Value := FontDialog1.Font.Name;
    //Settings.Clipboard.Window.FontSize.Value := FontDialog1.Font.Size;
    //Settings.Clipboard.Window.ForeColor.Value := FontDialog1.Font.Color;
  //end;
end;

procedure Tfrm_Settings.pnl_CBWND_BackColorClick(Sender: TObject);
begin
  //ColorDialog1.Color := TColor(Settings.Clipboard.Window.BackColor);
  //if ColorDialog1.Execute then
    //Settings.Clipboard.Window.BackColor.Value := Integer(ColorDialog1.Color);
end;

procedure Tfrm_Settings.pnl_CBWND_ForeColorClick(Sender: TObject);
begin
  //ColorDialog1.Color := TColor(Settings.Clipboard.Window.ForeColor);
  //if ColorDialog1.Execute then
    //Settings.Clipboard.Window.ForeColor.Value := Integer(ColorDialog1.Color);
end;

procedure Tfrm_Settings.TrayIconClick(Sender: TObject);
begin
  pmTaskbarIcon.PopUp;
end;

procedure Tfrm_Settings.tsScrollBehaviourResize(Sender: TObject);
begin
  FTScrollGraphSettings1.Width := tsScrollBehaviour.Width - 16;
  FTScrollGraphSettings1.Height := tsScrollBehaviour.Height - 8 - FTScrollGraphSettings1.Top;
end;

procedure Tfrm_Settings.InstallUpdateResultReceived(UpdateInstallationReady: Boolean);
begin
  if UpdateInstallationReady then
    lblUpdate.Caption := 'FensterTools startet neu ...'
  else begin
    lblUpdate.Caption := 'Installation fehlgeschlagen';
    Updater.ResetState;
  end;
end;

procedure Tfrm_Settings.UpdateStatusChanged(Sender: TUpdater);
begin
  {$IfDef Debug}if Sender = nil then raise Exception.Create('UpdateStatusChanged was called without valid Sender instance');{$EndIf}

  timerLastUpdateCheck.Enabled := false;
  //btnCheckForUpdate.Enabled := (Sender.UpdateState = usIdle) or (Sender.UpdateState = usCheckFinished);
  lblUpdate.Font.Style := [];

  case Sender.UpdateState of
    usIdle: begin
      if (lowercase(ParamStr(1)) = '/updated') then
      begin
        lblUpdate.Caption := GetCurrentVersion(true, true, true) + ' installiert';
        lblUpdate.Font.Style := [fsBold];
        btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_CHECK));
      end else begin
        if YearsBetween(Now, Settings.Update.LastCheck.Value) > 10 then
          lblUpdate.Caption := 'Noch nicht nach Updates gesucht.'
        else begin
          lblUpdate.Caption := 'Kein Update verfügbar, geprüft vor ' + UTF8Encode(TimeDiffToGoodLookingStr(Now, Settings.Update.LastCheck.Value));
          timerLastUpdateCheck.Enabled := true;
        end;
        btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_REFRESH));
      end;
    end;
    usCheckRunning: begin
      lblUpdate.Caption := 'Suche läuft ...';
      btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_REFRESH));
    end;
    usCheckFinished: begin
      if not Sender.CheckInformation.CheckSuccessful then
      begin
        lblUpdate.Caption := 'Suche nach Updates fehlgeschlagen.' + #13+#10 + UTF8Encode(Sender.ErrorString)
      end
      else begin
        if Sender.CheckInformation.UpdateAvailable then
        begin
          lblUpdate.Caption := 'Version ' + UTF8Encode(Sender.CheckInformation.NewVersion) + ' verfügbar';
          btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_DOWNLOAD_UPDATE));
        end else begin
          lblUpdate.Caption := 'Kein Update verfügbar, geprüft vor ' + UTF8Encode(TimeDiffToGoodLookingStr(Now, Settings.Update.LastCheck.Value));
          btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_REFRESH));
          timerLastUpdateCheck.Enabled := true;
        end;
      end;
    end;
    usDownloadRunning: begin
      if Visible then
      begin
        lblUpdate.Caption := 'Update wird heruntergeladen ...    ' + UTF8Encode(FormatFloat('0.00', Sender.DownloadInformation.BytesReceived / (1024*1024))) + ' / ' + UTF8Encode(FormatFloat('0.00 MB', Sender.DownloadInformation.BytesTotal / (1024*1024)));
      end;
    end;
    usDownloadFinished: begin
      if Sender.DownloadInformation.DownloadSuccessful then
        lblUpdate.Caption := 'Update wird installiert ...'
      else if not Sender.DownloadInformation.DownloadSuccessful then
      begin
        lblUpdate.Caption := 'Download fehlgeschlagen';
        btnCheckForUpdate.LoadGlyphFromIconFile(RelativeToAbsolutePath(REL_PATH_ICON_REFRESH));
        //btnCheckForUpdate.Enabled := true;
      end;
    end;
  end;
end;

procedure Tfrm_Settings.UpdateNotifyUser(Sender: TUpdater);
begin
  // only gets called if update is not user invoked and not allowed to install automatically
  if Sender.CheckInformation.CheckSuccessful and Sender.CheckInformation.UpdateAvailable then
  begin
    TrayIcon.BalloonTitle := 'FensterTools ' + UTF8Encode(Sender.CheckInformation.NewVersion) + ' verfügbar';
    TrayIcon.BalloonHint := ' Klicken Sie hier, um das Update zu installieren.';
    TrayIcon.BalloonIconType := bitUser;

    TrayIcon.ShowBalloonHint;
  end;
end;

{procedure Tfrm_Settings.ClipboardChanged(ccAction: TClipboardChangedAction; ID: Integer; Str: UnicodeString; Check: boolean);
  {procedure UncheckAllItems(ParentItem: TMenuItem);
var
  i: Integer;
begin
  for i := 0 to ParentItem.Count-1 do
  begin
    ParentItem[i].Checked := false;
  end;
end;}
procedure ExecuteActionOnMenuItem(ParentItem: TMenuItem; Action: TClipboardChangedAction; ID: Integer = -1; Str: UnicodeString = ''; Check: boolean = false);
var
  Item: TMenuItem;
begin
  // ParentItem.Tag fungiert als CheckedItemID
  case Action of
    CCA_START_REBUILD: begin
        ParentItem.Clear;
        {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('Starting menu rebuilding');{$EndIf}
      end;
    CCA_CLEAR: begin
        {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('Clearing menu');{$EndIf}
        ParentItem.Clear;
        Item := TMenuItem.Create(ParentItem);
        Item.Enabled := false;
        Item.Caption := 'Zwischenablage ist leer';
        ParentItem.Add(Item);
      end;
    CCA_DELETE: begin
        {$IfDef Debug}
        if ID > ParentItem.Count-1 then
          raise Exception.Create('ClipboardChanged, Action: Delete, Eintrag mit ID ' + IntToStr(ID) + ' nicht gefunden.')
        else if ParentItem[ID].Enabled = false then
          raise Exception.Create('ClipboardChanged, Action: Delete, Eintrag mit ID ' + IntToStr(ID) + ' ist ein Dummy-Item und kann nicht gel����t werden.');
        {$EndIf}
        {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('Deleting menu-ID ' + IntToStr(ID));{$EndIf}
        ParentItem.Delete(ID);
        if ParentItem.Count = 0 then ClipboardChanged(CCA_CLEAR);
      end;
    CCA_ADD: begin
        {$IfDef Logging}
          if ParentItem = mi_Zwischenablage then
          begin
            Logger.Add('Adding menu entry');
          end;
        {$EndIf}

        if (ParentItem.Count = 1) and (ParentItem[0].Enabled = false) then
        begin
          ParentItem.Clear;
          {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('  --> Removing empty entry');{$EndIf}
        end;

        Item := TMenuItem.Create(ParentItem);
        Item.OnClick := @CBMenuItemClicked;

        Item.Caption := UTF8Encode(Str);

        if Check then
        begin

          Item.Checked := true;
          if (ParentItem.Tag >= 0) and (ParentItem.Tag < ParentItem.Count) then
          begin
            ParentItem[ParentItem.Tag].Checked := false;
            {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('  --> Checking current entry, unchecking ID ' + IntToStr(ParentItem.Tag));{$EndIf}
          end else
          begin
            {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('  --> Checking current entry, not unchecking any entry');{$EndIf}
          end;
          ParentItem.Tag := ID
        end;

        {if ID > ParentItem.Count then }ParentItem.Insert(ID, Item);
      end;
    CCA_CHANGE: begin
        if {Check and }(ParentItem.Tag >= 0) and (ParentItem.Tag < ParentItem.Count) then
        begin
          {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('Called Change on ID ' + IntToStr(ID) + ', unchecking previous ID ' + IntToStr(ParentItem.Tag));{$EndIf}
          ParentItem[ParentItem.Tag].Checked := false;
        end else
        begin
          {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('Called Change on ID ' + IntToStr(ID));{$EndIf}
        end;

        {$IfDef Debug}
        if ID > ParentItem.Count-1 then
          raise Exception.Create('ClipboardChanged, Action: Change, Eintrag mit ID ' + IntToStr(ID) + ' nicht gefunden.')
        else if ParentItem[ID].Enabled = false then
          raise Exception.Create('ClipboardChanged, Action: Change, Eintrag mit ID ' + IntToStr(ID) + ' ist ein Dummy-Item und kann nicht ge寤ert werden.');
        {$EndIf}
        if Str <> '' then ParentItem[ID].Caption := UTF8Encode(Str);
        ParentItem[ID].Checked := Check;

        if Check then
        begin
          ParentItem.Tag := ID;
          {$IfDef Logging}if ParentItem = mi_Zwischenablage then Logger.Add('  --> Checking ID ' + IntToStr(ID));{$EndIf}
        end else ParentItem.Tag := -1;
      end;
  end;
end;
begin
  ExecuteActionOnMenuItem(pmClipboard.Items, ccAction, ID, Str, Check);
  ExecuteActionOnMenuItem(mi_Zwischenablage, ccAction, ID, Str, Check);
end;

procedure Tfrm_Settings.CopyModeChanged(CopyMode: TCopyMode);
begin
  mi_cmNormal.Checked := (CopyMode = CM_NORMAL);
  mi_cmBlockCopy.Checked := (CopyMode = CM_SMARTCOPY);
  mi_cmBlockPaste.Checked := (CopyMode = CM_SMARTCOPY_REVERSED);
  mi_CopyModeSeparator.Checked := (CopyMode = CM_NORMAL_INSERTED_TO_TOP);
end;}

procedure Tfrm_Settings.CBMenuItemClicked(Sender: TObject);
begin
  //Clipboard.ActiveEntryID := TMenuItem(Sender).MenuIndex;
end;

procedure Tfrm_Settings.CopyModeChanged(Sender: TObject);
begin
  mi_cmNormal.Checked := ClipbrdManager.CopyMode = cmNormal;
  mi_cmBlockCopy.Checked := ClipbrdManager.CopyMode = cmBlockCopy;
  mi_cmBlockPaste.Checked := ClipbrdManager.CopyMode = cmBlockPaste;
end;



end.

