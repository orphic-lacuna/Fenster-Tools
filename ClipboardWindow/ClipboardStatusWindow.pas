unit ClipboardStatusWindow;

{$mode objfpc}{$H+}

{$IfDef Logging}
  {$Define EnableLog}
{$EndIf}

interface

uses
  Windows, Classes, SysUtils, SettingsManager, BasicAnimatedWindow, ClipboardStatusWindow.Settings, ClipboardStatusWindow.Renderer{$IfDef Logging}, Logging{$EndIf};

type

  { TClipboardStatusWindow }

  TClipboardStatusWindow = class(TBasicAnimatedWindow)
  private
    fRenderer: TCBStatusWndRenderer;
    fSettings: TCBSTATUSWND_Settings;

    fFadingAnimation: TLinearAnimation;

    procedure OnStopFadingAnimation(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure ApplyNewSettings(newSettings: TObject);
  protected
    function CreateWindow: Boolean; override;
    procedure DestroyWindow; override;

    procedure AcquireAnimationValues; override;


    procedure ThreadInternalCreate; override;
    procedure ThreadInternalMain; override;
    procedure ThreadInternalDestroy; override;

    procedure ResetFadingAnimation;
  public
    { BuildWindow is just a wrapper function for StartThread. The actual window creation mechanism is realised within the thread. }
    procedure BuildWindow;

    procedure Show; override;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TClipboardStatusWindow }

procedure TClipboardStatusWindow.OnStopFadingAnimation(Sender: TObject);
begin
  {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.OnStopFadingAnimation: Hiding Window');{$EndIf}
  Hide;
end;

procedure TClipboardStatusWindow.SettingsChanged(Sender: TObject);
var
  newSettings: TCBSTATUSWND_Settings;
  screenWidth, screenHeight: LongInt;
begin
  newSettings := TCBSTATUSWND_Settings.Create;

  screenWidth := GetSystemMetrics(SM_CXSCREEN);
  screenHeight := GetSystemMetrics(SM_CYSCREEN);

  {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.SettingsChanged');{$EndIf}

  // window dimensions
  newSettings.Left := Round(screenWidth*Settings.Clipboard.Window.Position.Left.Value);
  newSettings.Top := Round(screenHeight*Settings.Clipboard.Window.Position.Top.Value);
  newSettings.Right := Round(screenWidth*Settings.Clipboard.Window.Position.Right.Value);
  newSettings.Bottom := Round(screenHeight*Settings.Clipboard.Window.Position.Bottom.Value);

  DoThreadProc(@ApplyNewSettings, newSettings, false);
end;

procedure TClipboardStatusWindow.ApplyNewSettings(newSettings: TObject);
var
  oldSettings: TCBSTATUSWND_Settings;
begin
  if newSettings is TCBSTATUSWND_Settings then
  begin
    {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.ApplyNewSettings: New CBSTATUSWND settings arrived');{$EndIf}

    oldSettings := fSettings;
    fSettings := TCBSTATUSWND_Settings(newSettings);
    oldSettings.Free;

    fRenderer.Settings := fSettings;
    SetBounds(fSettings.Left, fSettings.Top, fSettings.Right-fSettings.Left, fSettings.Bottom-fSettings.Top);

    fRenderer.UpdateRenderTarget;

    fFadingAnimation.MaxDuration := fSettings.FadeoutDuration;
  end;
end;

function TClipboardStatusWindow.CreateWindow: Boolean;
begin
  Result := inherited CreateWindow;
  fRenderer := TCBStatusWndRenderer.Create(Handle);
end;

procedure TClipboardStatusWindow.DestroyWindow;
begin
  {$IfDef Debug}if not IsRunInThread then raise Exception.Create('DestroyWindow must run in ClipboardWindow Thread.');{$EndIf}
  TryFreeAndNil(fRenderer);
  inherited DestroyWindow;
end;

procedure TClipboardStatusWindow.AcquireAnimationValues;
begin
  {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.AcquireAnimationValues');{$EndIf}
  fRenderer.Opacity := Round(fFadingAnimation.Value);
  fRenderer.Flip;
  fRenderer.Render;
end;

procedure TClipboardStatusWindow.ThreadInternalCreate;
begin
  {$IfDef Logging}Logger.RegisterThread('Clipboard Status Window');{$EndIf}
  inherited ThreadInternalCreate;
end;

procedure TClipboardStatusWindow.ThreadInternalMain;
begin
  DoCallbackProc(@SettingsChanged, Settings.Clipboard.StatusWindow, false);
  inherited ThreadInternalMain;
end;

procedure TClipboardStatusWindow.ThreadInternalDestroy;
begin
  inherited ThreadInternalDestroy;
  {$IfDef Logging}Logger.EndThread;{$EndIf}
end;

procedure TClipboardStatusWindow.ResetFadingAnimation;
begin
  if IsRunInThread then
  begin
    fFadingAnimation.Reset;
    AcquireAnimationValues;
    {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.Show: FadingAnimation resetted.');{$EndIf}
  end else DoThreadProc(@ResetFadingAnimation);
end;

procedure TClipboardStatusWindow.BuildWindow;
begin
  StartThread();
end;

procedure TClipboardStatusWindow.Show;
begin
  inherited Show;
  {$IfDef EnableLog}Logger.Add('TClipboardStatusWindow.Show: Clipboard status window is visible.');{$EndIf}
  CancelDelayedThreadProc(@FadeOut); // kill timer, if fadeout timer is running
  ResetFadingAnimation;
end;

constructor TClipboardStatusWindow.Create;
begin
  fFadingAnimation := TLinearAnimation.Create;
  fFadingAnimation.StartValue := 255;
  fFadingAnimation.OnStop := @OnStopFadingAnimation;
  Animations.Add(fFadingAnimation);

  Settings.Clipboard.Window.ChangeHandler.Add(@SettingsChanged);
end;

destructor TClipboardStatusWindow.Destroy;
begin
  fFadingAnimation.Free;
  inherited Destroy;
end;

end.

