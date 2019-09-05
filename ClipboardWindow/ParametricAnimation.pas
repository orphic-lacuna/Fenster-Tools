unit parametricanimation;

{$mode objfpc}{$H+}

interface

{$IfDef Logging}
  {.$Define EnableLog}
{$EndIf}

uses
  Classes, SysUtils, Windows, fgl{$IfDef EnableLog}, logging in '..\logging.pas'{$EndIf};

type

  TAnimationState = (asStarting, asRunning, asStopping, asStopped, asSuspending, asSuspended, asResuming);

  { TBaseAnimation }

  TBaseAnimation = class
  private
    fOnResume: TNotifyEvent;
    fOnStart: TNotifyEvent;
    fOnStop: TNotifyEvent;
    fOnSuspend: TNotifyEvent;
    fTime: Single;

    function GetValue: Single;

    procedure MakeTimestep(DeltaT: Single);
  protected
    fMaxDuration: Single;
    fStartValue, fEndValue: Single;

    fState: TAnimationState;

    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoSuspend; virtual;
    procedure DoResume; virtual;
  public
    property OnStart: TNotifyEvent read fOnStart write fOnStart;
    property OnStop: TNotifyEvent read fOnStop write fOnStop;
    property OnSuspend: TNotifyEvent read fOnSuspend write fOnSuspend;
    property OnResume: TNotifyEvent read fOnResume write fOnResume;
    property StartValue: Single read fStartValue write fStartValue;
    property EndValue: Single read fEndValue write fEndValue;
    property MaxDuration: Single read fMaxDuration write fMaxDuration;

    { Returns the current animation value based upon the current timestamp. }
    property Value: Single read GetValue;
    { Returns the current timestamp. This is exact and not aligned to timesteps according to the desired frame rate. }
    property Time: Single read fTime;

    property State: TAnimationState read fState;

    function CalculateValueAt(aTime: Single): Single; virtual; abstract;

    procedure Reset;

    constructor Create({aStartValue, anEndValue, aMaxDuration: Single});
  end;

  { TSinusoidalAnimation }

  { Defines a mathematical model of animation behaviour. You must provide start and end value. TParametricAnimation calculates the current animation value based on time. }
  TSinusoidalAnimation = class(TBaseAnimation)
  private
    VelocityOffset, AccelerationMultiplier1, AccelerationMultiplier2: Single;
    // intermediate values
    iv1: Array[0..2] of Single;
    iv2: Array[0..2] of Single;
    procedure CalcIntermediateValues;
  protected
    procedure DoStart; override;
    procedure DoSuspend; override;
    procedure DoResume; override;
  public
    function CalculateValueAt(aTime: Single): Single; override;
  end;

  { TLinearAnimation }

  TLinearAnimation = class(TBaseAnimation)
  private
  public
    function CalculateValueAt(aTime: Single): Single; override;
  end;

  { TAnimationController }

  TWaitFunction = function: Boolean of object;

  TAnimationController = class(specialize TFPGList<TBaseAnimation>)
  private
    fDesiredFPS: Integer;
    pcFreq, pcLastValue: Int64;
//    function GetNetTimeStep: Single;
  protected
  public
    property DesiredFPS: Integer read fDesiredFPS write fDesiredFPS;
    { Returns the timestamp of the ideal time for the next frame based on the desired frame rate and and the current timestamp. }
//    property NextTimeStep: Single read GetNetTimeStep;

    { Returns true if all animations have given state. }
    function CheckStateOfAllAnimations(aState: TAnimationState): Boolean;
    { Returns true if at least one animation has given state. }
    function CheckStateOfAtLeastOneAnimation(aState: TAnimationState): Boolean;

    { Determines the time difference relative to the preceding call of Tick() and updates the current timestamp. }
    function Tick(WaitForDesiredFPS: Boolean; aWaitFunction: TWaitFunction = nil): Boolean;

    procedure StartAnimation(anAnimation: TBaseAnimation); overload;
    procedure StartAnimation(anAnimationIndex: Integer); overload;
    procedure StopAnimation(anAnimation: TBaseAnimation); overload;
    procedure StopAnimation(anAnimationIndex: Integer); overload;

    procedure SuspendAll;
    procedure ResumeAll;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TLinearAnimation }

function TLinearAnimation.CalculateValueAt(aTime: Single): Single;
begin
  if fState = asRunning then
  begin
    Result := fStartValue + aTime * (fEndValue - fStartValue) / fMaxDuration;
    if ((fStartValue < fEndValue) and (Result >= fEndValue)) or ((fStartValue > fEndValue) and (Result <= fEndValue)) then
    begin
      Result := fEndValue;
      fState := asStopping;
    end;
  end else Result := fStartValue;
end;

{ TAnimationController }

function TAnimationController.CheckStateOfAllAnimations(aState: TAnimationState): Boolean;
var
  item: TBaseAnimation;
begin
  Result := true;
  {$IfDef EnableLog}Logger.Add('TAnimationController.CheckStateOfAllAnimations: %', [aState]);{$EndIf}
  for item in Self do
  begin
    {$IfDef EnableLog}Logger.Add('    %', [item.State]);{$EndIf}
    if (item.State <> aState) then
    begin
      Result := false;
      break;
    end;
  end;
end;

function TAnimationController.CheckStateOfAtLeastOneAnimation(aState: TAnimationState): Boolean;
var
  item: TBaseAnimation;
begin
  Result := false;
  {$IfDef EnableLog}Logger.Add('TAnimationController.CheckStateOfAtLeastOneAnimation: %', [aState]);{$EndIf}
  for item in Self do
  begin
    {$IfDef EnableLog}Logger.Add('    %', [item.State]);{$EndIf}
    if (item.State = aState) then
    begin
      Result := true;
      break;
    end;
  end;
end;

   {
function TAnimationController.GetNetTimeStep: Single;
begin
  Result := fTime + (1/fDesiredFPS);
end;
    }
function TAnimationController.Tick(WaitForDesiredFPS: Boolean; aWaitFunction: TWaitFunction): Boolean;
var
  pcCurVal: Int64;
  DeltaT: Single;
  item: TBaseAnimation;
begin
  Result := false;
  //if not AnimationRunning then exit;
  {$IfDef EnableLog}Logger.Add('Tick');{$EndIf}
  QueryPerformanceCounter(pcCurVal{%H-});
  if WaitForDesiredFPS then
  begin
    repeat
      if Assigned(aWaitFunction) then
      begin
        Result := aWaitFunction();
        if not Result then
          exit;
      end else sleep(1);
      QueryPerformanceCounter(pcCurVal);
    until ((pcCurVal-pcLastValue)/pcFreq >= (1/fDesiredFPS)) or not CheckStateOfAtLeastOneAnimation(asRunning)
  end;
  DeltaT := (pcCurVal-pcLastValue)/pcFreq;
  for item in Self do item.MakeTimestep(DeltaT);
  pcLastValue := pcCurVal;
  Result := not CheckStateOfAllAnimations(asStopped);
  {$IfDef EnableLog}Logger.Add('Tick left, result=%', [Result]);{$EndIf}
end;

procedure TAnimationController.StartAnimation(anAnimation: TBaseAnimation);
begin
  if anAnimation.fState = asStopped then anAnimation.fState := asStarting;
end;

procedure TAnimationController.StartAnimation(anAnimationIndex: Integer);
begin
  StartAnimation(Items[anAnimationIndex]);
end;

procedure TAnimationController.StopAnimation(anAnimation: TBaseAnimation);
begin
  if anAnimation.fState = asRunning then anAnimation.fState := asStopping;
end;

procedure TAnimationController.StopAnimation(anAnimationIndex: Integer);
begin
  StopAnimation(Items[anAnimationIndex]);
end;

procedure TAnimationController.SuspendAll;
var
  item: TBaseAnimation;
begin
  for item in Self do
  begin
    if item.fState = asRunning then
      item.fState := asSuspending;
  end;
end;

procedure TAnimationController.ResumeAll;
var
  item: TBaseAnimation;
begin
  for item in Self do
  begin
    if item.fState = asSuspended then
      item.fState := asResuming;
  end;
end;

constructor TAnimationController.Create;
begin
  inherited Create;
  QueryPerformanceFrequency(pcFreq);
end;

destructor TAnimationController.Destroy;
var
  item: TBaseAnimation;
begin
{  for item in Self do
  begin
    item.Free;
  end;}
  inherited Destroy;
end;

{ TSinusoidalAnimation }

function TSinusoidalAnimation.CalculateValueAt(aTime: Single): Single;
begin
  {$IfDef Debug}if aTime < 0 then raise Exception.Create('TParametricAnimation.CalculateValue() was called with negative time.');{$EndIf}

  if (fState = asRunning) or (fState = asSuspending) or (fState = asSuspended) then
  begin
    if (aTime <= MaxDuration*0.5) then
    begin
      Result := fStartValue + VelocityOffset*aTime + iv1[0]*(aTime-(iv1[1]*sin(iv1[2]*aTime)));
    end else
    begin
      Result := iv2[0] + aTime*iv2[1] - iv2[2]*(aTime+iv1[1]*sin(aTime*iv1[2]));
    end;

    if ((fStartValue <= fEndValue) and (Result >= fEndValue)) or ((fStartValue > fEndValue) and (Result <= fEndValue)) then
    begin
      Result := fEndValue;
      fState := asStopping;
    end;
  end else Result := fEndValue;
end;

procedure TSinusoidalAnimation.CalcIntermediateValues;
begin
  //AccelerationMultiplier := (2*Pi*(fEndValue-fStartValue-VelocityOffset*MaxDuration))/(MaxDuration*(MaxDuration-(MaxDuration*sin(2*Pi*MaxDuration/MaxDuration)/(2*Pi))));
  //x := 2*Pi*MaxDuration/MaxDuration;
  AccelerationMultiplier2 := 2 * Pi * (fEndValue - fStartValue - (VelocityOffset * MaxDuration * 0.25)) / (MaxDuration * MaxDuration);
  AccelerationMultiplier1 := AccelerationMultiplier2 - (VelocityOffset * Pi / MaxDuration);

  // precalculate intermediate results
  iv1[0] := AccelerationMultiplier1 * MaxDuration / (2 * Pi);
  iv1[1] := MaxDuration / (2 * Pi);
  iv1[2] := 2 * Pi / MaxDuration;
  iv2[0] := fStartValue + (AccelerationMultiplier2 - AccelerationMultiplier1) * MaxDuration * MaxDuration / (4 * Pi);
  iv2[1] := VelocityOffset + (AccelerationMultiplier1 * MaxDuration / Pi);
  iv2[2] := AccelerationMultiplier2 * MaxDuration / (2 * Pi);
end;

procedure TSinusoidalAnimation.DoStart;
begin
  inherited DoStart;
  VelocityOffset := 0;
  CalcIntermediateValues;
end;

procedure TSinusoidalAnimation.DoSuspend;
begin
  inherited DoSuspend;

  if fTime < fMaxDuration then
  begin
    // wenn Animation noch läuft, dann aktuelle Geschwindigkeit als Offset einfach mitnehmen
    if (fTime <= MaxDuration*0.5) then
    begin
      VelocityOffset := VelocityOffset + AccelerationMultiplier1 * MaxDuration * (1-cos(2*Pi*Time/MaxDuration)) / (2*Pi);
    end else
    begin
      VelocityOffset := VelocityOffset + (AccelerationMultiplier1 * MaxDuration / Pi) - AccelerationMultiplier2*MaxDuration*(1+cos(2*Pi*Time/MaxDuration))/(2*Pi);
    end;
  end else
  begin
    // sonst ist die Offset-Geschwindigkeit = 0
    VelocityOffset := 0;
  end;
  {$IfDef EnableLog}Logger.Add('TSinusoidalAnimation.DoSuspend: VelocityOffset=%', [VelocityOffset]);{$EndIf}
end;

procedure TSinusoidalAnimation.DoResume;
begin
  inherited DoResume;
  fTime := 0;
  CalcIntermediateValues;
end;

{ TBaseAnimation }

constructor TBaseAnimation.Create;
begin
  //fStartValue := aStartValue; fEndValue := anEndValue; fMaxDuration := aMaxDuration;
  //QueryPerformanceFrequency(pcFreq);
  Reset;
end;

function TBaseAnimation.GetValue: Single;
begin
  Result := CalculateValueAt(fTime);
end;

procedure TBaseAnimation.MakeTimestep(DeltaT: Single);
begin
  case fState of
    asStarting:
      begin
        DoStart;
      end;
    asSuspending:
      begin
        fTime := fTime + DeltaT;
        DoSuspend;
      end;
    asResuming:
      begin
        DoResume;
      end;
    asRunning:
      begin
        fTime := fTime + DeltaT;
      end;
    asStopping:
      begin
        fTime := fTime + DeltaT;
        DoStop;
      end;
  end;
end;

procedure TBaseAnimation.DoStart;
begin
  fState := asRunning;
  fTime := 0;
  if Assigned(fOnStart) then fOnStart(Self);
end;

procedure TBaseAnimation.DoStop;
begin
  fState := asStopped;
  if Assigned(fOnStop) then fOnStop(Self);
end;

procedure TBaseAnimation.DoSuspend;
begin
  fState := asSuspended;
  if Assigned(fOnSuspend) then fOnSuspend(Self);
end;

procedure TBaseAnimation.DoResume;
begin
  fState := asRunning;
  if Assigned(fOnResume) then fOnResume(Self);
end;

procedure TBaseAnimation.Reset;
begin
  fTime := 0;
  fState := asStopped;
end;


end.

