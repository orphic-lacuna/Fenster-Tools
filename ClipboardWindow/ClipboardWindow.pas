unit ClipboardWindow;

{$mode objfpc}{$H+}

{$IfDef Logging}
  {$Define EnableLog}
{$EndIf}

interface

uses
  Classes, SysUtils, Windows, Win32Extra, BasicAnimatedWindow, Clipboard.List, ClipboardWindow.Renderer, ToolBox, Dialogs, ExtendedThreads, parametricanimation, DwmApi, ExtendedWinapi, ClipboardWindow.Entry, ClipboardWindow.Settings{$IfDef Logging}, Logging in '../Logging.pas'{$EndIf};

type

  { TClipboardWindow }

  TClipboardWindow = class(TBasicAnimatedWindow)
  private
    fRenderer: TCBWndRenderer;
    fSettings: TCBWND_Settings;

    fEntryList: TCBWndEntryList;
    fScrollAnimation: TSinusoidalAnimation;
    fFadingAnimation: TLinearAnimation;

    fFreezedItemIndex: Integer;
    fFreezedPixelOffset: Single;
    fScrollAnimationSuspended: HANDLE;

    procedure OnStartScrollAnimation(Sender: TObject);
    procedure OnStopFadingAnimation(Sender: TObject);
    procedure OnStopScrollAnimation(Sender: TObject);
    procedure OnSuspendScrollAnimation(Sender: TObject);
    procedure OnResumeScrollAnimation(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure ApplyNewSettings(newSettings: TObject);

    procedure SuspendAnimations;
    procedure ResumeAnimations;

    procedure ResetFadingAnimation;
  protected
    function CreateWindow: Boolean; override;
    procedure DestroyWindow; override;

    procedure AcquireAnimationValues; override;
    procedure FadeOut;

    procedure ThreadInternalCreate; override;
    procedure ThreadInternalMain; override;
    procedure ThreadInternalDestroy; override;
  public
//    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex; // represents the item index in ClipboardStack, not in the CBWndEntryList !!!

    { BuildWindow is just a wrapper function for StartThread. The actual window creation mechanism is realised within the thread. }
    procedure BuildWindow;
    procedure Animate(animSource, animDest: Clipboard.List.TClipboardItem);

    procedure Show; override;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses SettingsManager, SettingsDerivedBaseTypes;

{ TClipboardWindow }

constructor TClipboardWindow.Create;
begin
  inherited Create;

  fScrollAnimationSuspended := CreateEvent(nil, true, false, nil);

  fScrollAnimation := TSinusoidalAnimation.Create;
  fScrollAnimation.OnStart := @OnStartScrollAnimation;
  fScrollAnimation.OnSuspend := @OnSuspendScrollAnimation;
  fScrollAnimation.OnResume := @OnResumeScrollAnimation;
  fScrollAnimation.OnStop := @OnStopScrollAnimation;
  Animations.Add(fScrollAnimation);

  fFadingAnimation := TLinearAnimation.Create;
  fFadingAnimation.StartValue := 255;
  fFadingAnimation.OnStop := @OnStopFadingAnimation;
  Animations.Add(fFadingAnimation);

  fFreezedItemIndex := -1;

  fEntryList := TCBWndEntryList.Create;

  Settings.Clipboard.Window.ChangeHandler.Add(@SettingsChanged);
end;

procedure TClipboardWindow.SettingsChanged(Sender: TObject);
var
  newSettings: TCBWND_Settings;
  screenWidth, screenHeight: LongInt;
begin
  newSettings := TCBWND_Settings.Create;

  screenWidth := GetSystemMetrics(SM_CXSCREEN);
  screenHeight := GetSystemMetrics(SM_CYSCREEN);

  {$IfDef EnableLog}Logger.Add('TClipboardWindow.SettingsChanged');{$EndIf}

  // window dimensions
  newSettings.Left := Round(screenWidth*Settings.Clipboard.Window.Position.Left.Value);
  newSettings.Top := Round(screenHeight*Settings.Clipboard.Window.Position.Top.Value);
  newSettings.Right := Round(screenWidth*Settings.Clipboard.Window.Position.Right.Value);
  newSettings.Bottom := Round(screenHeight*Settings.Clipboard.Window.Position.Bottom.Value);

  // main element colors
  newSettings.WindowBackgroundColor := Settings.Clipboard.Window.BackgroundColor;
  newSettings.EntryBackgroundColor := Settings.Clipboard.Window.EntryBackgroundColor;

  // fonts
  newSettings.EntryFont := Settings.Clipboard.Window.EntryFont;
  newSettings.EntryFontColor := Settings.Clipboard.Window.EntryFontColor;
  newSettings.EntryFontsize := Settings.Clipboard.Window.EntryFontsize;
  newSettings.TimestampFont := Settings.Clipboard.Window.TimestampFont;
  newSettings.TimestampFontColor := Settings.Clipboard.Window.TimestampFontColor;
  newSettings.TimestampFontsize := Settings.Clipboard.Window.TimestampFontsize;

  // main element dimensions
  newSettings.DefaultEntryHeight := Settings.Clipboard.Window.DefaultEntryHeight;
  newSettings.ActiveEntryHeight := Settings.Clipboard.Window.ActiveEntryHeight;
  newSettings.MarginEntryEntry := Settings.Clipboard.Window.MarginEntryEntry;
  newSettings.MarginLeft := Settings.Clipboard.Window.MarginLeft;
  newSettings.MarginRight := Settings.Clipboard.Window.MarginRight;
  newSettings.PaddingLeft := Settings.Clipboard.Window.PaddingLeft;
  newSettings.PaddingRight := Settings.Clipboard.Window.PaddingRight;
  newSettings.PaddingTop := Settings.Clipboard.Window.PaddingTop;
  newSettings.PaddingBottom := Settings.Clipboard.Window.PaddingBottom;

  // Segmentation line
  newSettings.SegmentationLineDarkColor := Settings.Clipboard.Window.SegmentationLineDarkColor;
  newSettings.SegmentationLineLightColor := Settings.Clipboard.Window.SegmentationLineDarkColor;
  newSettings.SegmentationLineMargin := Settings.Clipboard.Window.SegmentationLineMargin;

  // icon
  newSettings.IconMarginLeft := Settings.Clipboard.Window.IconMarginLeft;
  newSettings.IconMarginTop := Settings.Clipboard.Window.IconMarginTop;
  newSettings.IconMarginRight := Settings.Clipboard.Window.IconMarginRight;
  newSettings.IconWidth := Settings.Clipboard.Window.IconWidth;
  newSettings.IconHeight := Settings.Clipboard.Window.IconHeight;

  // delays and timings
  newSettings.ScrollAnimationDuration := Settings.Clipboard.Window.ScrollAnimationDuration;
  newSettings.FadeoutDelay := Round(Settings.Clipboard.Window.FadeoutDelay.Value*1000);
  newSettings.FadeoutDuration := Settings.Clipboard.Window.FadeoutDuration;

  // misc
  newSettings.EntryRectRoundingRadius := Settings.Clipboard.Window.EntryRectRoundingRadius;

  DoThreadProc(@ApplyNewSettings, newSettings, false);
end;

{function TClipboardWindow.GetSelectedIndex: Integer;
begin
  Result := fEntryList.ToIndex;
end;

procedure TClipboardWindow.SetSelectedIndex(AValue: Integer);
begin
  fEntryList.SetAnimation(AValue, AValue);
end;}

procedure TClipboardWindow.ApplyNewSettings(newSettings: TObject);
var
  oldSettings: TCBWND_Settings;
begin
  if newSettings is TCBWND_Settings then
  begin
    {$IfDef EnableLog}Logger.Add('TClipboardWindow.ApplyNewSettings: New CBWND settings arrived');{$EndIf}

    oldSettings := fSettings;
    fSettings := TCBWND_Settings(newSettings);
    oldSettings.Free;

    fRenderer.Settings := fSettings;
    SetBounds(fSettings.Left, fSettings.Top, fSettings.Right-fSettings.Left, fSettings.Bottom-fSettings.Top);

    fRenderer.UpdateRenderTarget;

    fScrollAnimation.MaxDuration := fSettings.ScrollAnimationDuration;
    fFadingAnimation.MaxDuration := fSettings.FadeoutDuration;
  end;
end;

procedure TClipboardWindow.OnStartScrollAnimation(Sender: TObject);
begin
  fScrollAnimation.StartValue := fRenderer.ItemIndexToScrollValue(fEntryList.FromIndex);
  fScrollAnimation.EndValue := fRenderer.ItemIndexToScrollValue(fEntryList.ToIndex);

  // we assume that window only
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.OnStartScrollAnimation: Pixel values determined by fEntryList.FromIndex % -> % and .ToIndex % -> %', [fEntryList.FromIndex, fScrollAnimation.StartValue, fEntryList.ToIndex, fScrollAnimation.EndValue]);{$EndIf}
end;

procedure TClipboardWindow.OnStopFadingAnimation(Sender: TObject);
begin
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.OnStopFadingAnimation: Hiding Window');{$EndIf}
  Hide;
end;

procedure TClipboardWindow.OnStopScrollAnimation(Sender: TObject);
begin
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.OnStopScrollAnimation: FadeOut in 3s');{$EndIf}
  DoThreadProcDelayed(@FadeOut, fSettings.FadeoutDelay, false);
end;

procedure TClipboardWindow.OnSuspendScrollAnimation(Sender: TObject);
begin
  fRenderer.ScrollValue := fScrollAnimation.Value;
  fRenderer.GetItemIndexByCurrentScrollValue(fFreezedItemIndex, fFreezedPixelOffset);
  SetEvent(fScrollAnimationSuspended);
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.OnSuspendScrollAnimation: ItemIndex=%, PixelOffset=%', [fFreezedItemIndex, fFreezedPixelOffset]);{$EndIf}
end;

procedure TClipboardWindow.OnResumeScrollAnimation(Sender: TObject);
begin
  ResetEvent(fScrollAnimationSuspended);
  fScrollAnimation.StartValue := fRenderer.ItemIndexToScrollValue(fEntryList.FromIndex)+fFreezedPixelOffset;
  fScrollAnimation.EndValue := fRenderer.ItemIndexToScrollValue(fEntryList.ToIndex);
  {$IfDef EnableLog}
  Logger.Add('TClipboardWindow.OnResumeScrollAnimation:'#13#10'  Previously freezed at Item Index % (which is now %), new ScrollValue is %, Pixel Offset %'#13#10'Target pixel values determined by fEntryList.ToIndex % -> %', [fFreezedItemIndex, fEntryList.FromIndex, fRenderer.ItemIndexToScrollValue(fFreezedItemIndex), fFreezedPixelOffset, fEntryList.ToIndex, fScrollAnimation.EndValue]);
  {$EndIf}
end;

procedure TClipboardWindow.SuspendAnimations;
begin
  if not IsRunInThread then
  begin
    DoThreadProc(@SuspendAnimations);
    {$IfDef Debug}if {$EndIf}WaitForSingleObject(fScrollAnimationSuspended, INFINITE){$IfDef Debug} <> WAIT_OBJECT_0 then raise Exception.Create('TClipboardWindow.SuspendScrollAnimation: Could not suspend scroll animation.'){$EndIf};
  end else
  begin
    Animations.SuspendAll;
  end;
end;

procedure TClipboardWindow.ResumeAnimations;
begin
  if not IsRunInThread then
    DoThreadProc(@ResumeAnimations)
  else
    Animations.ResumeAll;
end;

procedure TClipboardWindow.ResetFadingAnimation;
begin
  if IsRunInThread then
  begin
    fFadingAnimation.Reset;
    AcquireAnimationValues;
    {$IfDef EnableLog}Logger.Add('TClipboardWindow.Show: FadingAnimation resetted.');{$EndIf}
  end else DoThreadProc(@ResetFadingAnimation);
end;

function TClipboardWindow.CreateWindow: Boolean;
begin
  Result := inherited CreateWindow;
//  ProcessMessages(0); // process all window creation messages (otherwise GetWindowRect in UpdateRenderTarget retrieves zeros for window rect)
  fRenderer := TCBWndRenderer.Create(Handle, fEntryList);
end;

procedure TClipboardWindow.DestroyWindow;
begin
  {$IfDef Debug}if not IsRunInThread then raise Exception.Create('DestroyWindow must run in ClipboardWindow Thread.');{$EndIf}
  TryFreeAndNil(fRenderer);
  inherited DestroyWindow;
end;

procedure TClipboardWindow.AcquireAnimationValues;
begin
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.AcquireAnimationValues');{$EndIf}
  fRenderer.Opacity := Round(fFadingAnimation.Value);
  fRenderer.Flip;
  fRenderer.ScrollValue := fScrollAnimation.Value;
  fRenderer.Render;
end;

procedure TClipboardWindow.FadeOut;
begin
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.FadeOut');{$EndIf}
  fFadingAnimation.StartValue := 255;
  fFadingAnimation.EndValue := 0;
  fFadingAnimation.MaxDuration := fSettings.FadeoutDuration;
  {$IfDef EnableLog}Logger.Add('--> Animations.StartAnimation(fFadingAnimation)');{$EndIf}
  Animations.StartAnimation(fFadingAnimation);
  {$IfDef EnableLog}Logger.Add('--> StartAnimationLoop');{$EndIf}
  StartAnimationLoop;
end;

procedure TClipboardWindow.ThreadInternalCreate;
begin
  {$IfDef Logging}Logger.RegisterThread('Clipboard Window');{$EndIf}
  inherited ThreadInternalCreate;
end;

procedure TClipboardWindow.ThreadInternalMain;
begin
  DoCallbackProc(@SettingsChanged, Settings.Clipboard.Window, false);
  inherited ThreadInternalMain;
end;


procedure TClipboardWindow.ThreadInternalDestroy;
begin
  inherited ThreadInternalDestroy;
  {$IfDef Logging}Logger.EndThread;{$EndIf}
end;

procedure TClipboardWindow.BuildWindow;
begin
  StartThread();
end;

procedure TClipboardWindow.Animate(animSource, animDest: Clipboard.List.TClipboardItem);
begin
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.Animate ...');{$EndIf}
  // .Animate is called by ClipboardManager when user navigates through clipboard history
  if not IsRunInThread then
  begin
    if fScrollAnimation.State = asRunning then // previous animation is still running
    begin
      if animSource <> animDest then
      begin
        {$IfDef EnableLog}Logger.Add('TClipboardWindow.Animate: Animation still running, freezing animation');{$EndIf}
        SuspendAnimations;

        {$IfDef EnableLog}Logger.Add('TClipboardWindow.Animate: Animation freezed, rebuilding list');{$EndIf}
        fEntryList.RebuildList(animSource, animDest, fFreezedItemIndex);

        {$IfDef EnableLog}Logger.Add('--> ResumeAnimations');{$EndIf}
        ResumeAnimations;
      end;
    end else
    begin
      //if animSource <> animDest then
      //begin
        // previous animation has already stopped, everything is fine
        {$IfDef EnableLog}Logger.Add('TClipboardWindow.Animate: Animation not running, rebuilding list');{$EndIf}
        fEntryList.RebuildList(animSource, animDest); // rebuild the entry list
        {$IfDef EnableLog}Logger.Add('--> Animation.StartAnimation');{$EndIf}
        Animations.StartAnimation(fScrollAnimation);
        Show;
        {$IfDef EnableLog}Logger.Add('--> StartAnimationLoop');{$EndIf}
        StartAnimationLoop;
      //end else begin
        //Show;
        //DoThreadProcDelayed(@FadeOut, fSettings.FadeoutDelay, false);
      //end;
    end;
  end{$IfDef Debug}else raise Exception.Create('TClipboardWindow.Animate must be called from clipboard thread.'){$EndIf};
end;

procedure TClipboardWindow.Show;
begin
  (*if not IsRunInThread then
  begin
    inherited Show;
    DoThreadProc(@Show);
    {$IfDef EnableLog}Logger.Add('TClipboardWindow.Show: Clipboard window is visible.');{$EndIf}
  end else
  begin
    fFadingAnimation.Reset;
    CancelDelayedThreadProc(@FadeOut); // kill timer, if fadeout timer is running
    {$IfDef EnableLog}Logger.Add('TClipboardWindow.Show: FadingAnimation resetted.');{$EndIf}
  end;*)
  inherited Show;
  {$IfDef EnableLog}Logger.Add('TClipboardWindow.Show: Clipboard window is visible.');{$EndIf}
  CancelDelayedThreadProc(@FadeOut); // kill timer, if fadeout timer is running
  ResetFadingAnimation;
end;

destructor TClipboardWindow.Destroy;
begin
  fFadingAnimation.Free;
  fScrollAnimation.Free;

  CloseHandle(fScrollAnimationSuspended);

  // thread is already stopped here (it is stopped within .BeforeDestruction)
  fEntryList.Free;
  inherited Destroy;
end;

end.

