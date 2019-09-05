unit BasicAnimatedWindow;

{$mode objfpc}{$H+}

interface

{$IfDef Logging}
  {.$Define EnableLog}
{$EndIf}

uses
  Classes, SysUtils, BasicWindow, Windows, parametricanimation{$IfDef EnableLog}, logging in '..\logging.pas'{$EndIf};

type

  { TBasicAnimatedWindow }

  TBasicAnimatedWindow = class(TBasicWindow)
  private
    fAnimations: TAnimationController;

    AnimationLoopRunning: Boolean;

    function WaitFunction: Boolean;
  protected
    property Animations: TAnimationController read fAnimations;

    procedure AnimationLoop; virtual;
    procedure StartAnimationLoop;
    procedure AcquireAnimationValues; virtual; abstract;
  public

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TBasicAnimatedWindow }

constructor TBasicAnimatedWindow.Create;
begin
  inherited Create(false);

  // these objects should exist before window thread begins
  fAnimations := TAnimationController.Create;
  fAnimations.DesiredFPS := 50;
end;

function TBasicAnimatedWindow.WaitFunction: Boolean;
begin
  // If WM_QUIT was received, WaitFunction returns false
  Result := ProcessMessages(5) >= 0;
end;

procedure TBasicAnimatedWindow.AnimationLoop;
begin
  {$IfDef EnableLog}Logger.Add('TBasicAnimatedWindow.AnimationLoop');{$EndIf}
  while fAnimations.Tick(true, @WaitFunction) do
  begin
    if fAnimations.CheckStateOfAtLeastOneAnimation(asRunning) then
      AcquireAnimationValues;
  end;
  {$IfDef EnableLog}Logger.Add('--> TBasicAnimatedWindow.AnimationLoop done.');{$EndIf}
end;

procedure TBasicAnimatedWindow.StartAnimationLoop;
begin
  if not IsRunInThread then
  begin
    DoThreadProc(@StartAnimationLoop);
  end else
  begin
    if AnimationLoopRunning then
    begin
      {$IfDef EnableLog}Logger.Add('TBasicAnimatedWindow.StartAnimationLoop: Animation loop still running, not restarted.');{$EndIf}
//      {$IfDef Debug}raise Exception.Create('Reentry of AnimationLoop');{$EndIf}
      exit;
    end;
    AnimationLoopRunning := true;
    AnimationLoop;
    AnimationLoopRunning := false;
  end;
end;

destructor TBasicAnimatedWindow.Destroy;
begin
  fAnimations.Free;
  inherited Destroy;
end;

end.

