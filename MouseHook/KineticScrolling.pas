unit KineticScrolling;

{.$Define ScrollDebugMode}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses Windows, Messages, Sysutils, Classes, ExtendedThreads, Math, fpJSON, FensterToolsCommon, ToolBox, Splines {$IfDef ScrollDebugMode},shfolder{$EndIf}, ExtendedWinAPI, UserInputProvider;

{
//////////////////////////////////////////////////////////////////////////////////////
IMPORTANT HINT:
**************************************************************************************
IF THE KineticScroller CLASS CAN'T RECIEVE ITS TIMER MESSAGES THEN CHECK WHETHER YOU
ARE CALLING THE DispatchMessages() FUNCTION. THIS IS IMPORTANT IN ODER TO DELIVER THE
WM_TIMER MESSAGE TO THE HIDDEN MESSAGE RECEIVER WINDOW OF THE KineticScroller CLASS!
//////////////////////////////////////////////////////////////////////////////////////
}
const v_threshold = 5*120;
const MaxVReal = 20000*120;          // Zeilen pro Sekunde
//const Speed_Decel_const = 2.25;     // in Zeilen/s^2
const IntegerPartAccuracy = 0.001;
const DecelLowerLimit = 0.25;   // muss verschwinden (~0)
const vi_LowerLimit = 0.1*120;      // mal testen mit weniger
const Freq = 50;
const TimeStep = 1/Freq;

type

  TOnSendScrollMsg = procedure (const aUserInput: TUserInput; MessageIsDirectlyFromHook: Boolean; var PassToSystem: Boolean) of object;

  { TKineticScroller }

  TKineticScroller = class
  private
    {$IfDef ScrollDebugMode}
    FPfad: String;
    {$EndIf}

    Bounded: boolean;
    fOnSendScrollMsg: TOnSendScrollMsg;

    vr, vi, scrollmsg_rest: Single;
    LastMsg: TUserInput;
    timestamp_current, timestamp_last: Int64;
    FCurrentWindow: HWND;
    //exename: String;
    ScrollPoint: Point;
    UpscalingFactor: TCubicSpline;
    DecelerationFactor: TCubicSpline;
    //FKineticMode: Boolean;

    //FOnScrollEvent: TOnScrollEvent;
//    FFreq: Cardinal;
    FTickTimer: PtrUInt;
    //TimerStartTime: Cardinal;
    //FOwner: HWND;

    PerformanceCounterFrequency: Int64;
    procedure Tick;
    procedure StartTimer;
    procedure StopTimer;
    procedure OnThreadMessage(var message: TMessage);

    //procedure SendScrollMsg(Window: HANDLE; WheelDelta: SmallInt); overload;
    procedure SendScrollMsg(const aUserInput: TUserInput; MessageIsDirectlyFromHook: Boolean; var PassToSystem: Boolean); inline;

    //procedure SetKineticMode(Value: Boolean);

    procedure SetScrollingOptions(const Value: TJSONObject);
    function MakeUserInput(Window: HANDLE; WheelDelta: SmallInt): TUserInput;
  public
    //property KineticMode: boolean read FKineticMode write SetKineticMode;

    property OnSendScrollMsg: TOnSendScrollMsg read fOnSendScrollMsg write fOnSendScrollMsg;
    property ScrollingOptions: TJSONObject write SetScrollingOptions;

    property CurrentWindow: HWND read FCurrentWindow;
    property IsNotScrolling: Boolean read Bounded;

    procedure ProcessScrollMsgKinetic(aUserInput: TUserInput; var PassToSystem: Boolean);//Window: HWND; const ScrollEventInfo: MSLLHOOKSTRUCT);
    function StopScrolling: boolean; // gibt zurück, ob es am Scrollen war
    //procedure HandleScrollMsg(const aUserInput: TUserInput; var PassToSystem: Boolean);

    constructor Create({OnScrollEvent: TOnScrollEvent});
    destructor Destroy; override;
  end;

{$IfDef ScrollDebugMode}
var
  F: TMemoryStream;
  Pfad: String;
{$EndIf}

//function GetScrollingDistance(MouseData: Cardinal): Integer;

implementation

{$IfDef ScrollDebugMode}
//uses unit1;
{$EndIf}
           {
procedure SetPerformanceCounterValue(var Struct: MSLLHOOKSTRUCT; Value: Int64);
var
  Rec: Int64Rec;
begin
  Rec := Int64Rec(Value);
  Struct.dwExtraInfo := Rec.Hi;
  Struct.time := Rec.Lo;
end;

function GetPermanceCounterValue(Struct: MSLLHOOKSTRUCT): Int64;
var
  Rec: Int64Rec;
begin
  Rec.Lo := Struct.time;
  Rec.Hi := Struct.dwExtraInfo;
  Result := Int64(Rec);
end;

function MakePoint(x, y: longint): Point;
begin
  Result.x := x;
  Result.y := y;
end;
           }
function IntegerPartWithLimitedAccuracy(Value, Epsilon: Single): Integer;
  function CeilWithLimitedAccuracy(Value: Single): Integer;
  begin
    if SameValue(Value, Round(Value), Epsilon) then Value := Round(Value);
    Result := Ceil(Value);
  end;
  function FloorWithLimitedAccuracy(Value: Single): Integer;
  begin
    if SameValue(Value, Round(Value), Epsilon) then Value := Round(Value);
    Result := Floor(Value);
  end;
begin
  if Value > 0 then
    Result := FloorWithLimitedAccuracy(Value) // Abrunden (wegen positiv)
  else if Value < 0 then
    Result := CeilWithLimitedAccuracy(Value) // Aufrunden (wegen negativ)
  else Result := 0;
end;

function UpLimitValue(Value, Limit: Single): Single;
begin
  if Abs(Value) > Limit then
  begin
    if Value > 0 then
      Result := Limit
    else
      Result := -Limit;
  end else Result := Value;
end;

function BottomLimitValue(Value, Limit: Single): Single;
begin
  {$IfDef ScrollDebugMode}
  if Limit <= 0 then raise Exception.Create('Limit darf nicht kleiner als 0 sein.'); 
  {$EndIf}
  if Abs(Value) < Limit then
  begin
    if Value > 0 then
      Result := Limit
    else
      Result := -Limit;
  end else Result := Value;
end;

constructor TKineticScroller.Create({OnScrollEvent: TOnScrollEvent});
begin
  {$IfDef ScrollDebugMode}
  F := TMemoryStream.Create;
  F.Position := 0;
  {$EndIf}

  QueryPerformanceFrequency(PerformanceCounterFrequency);
  //FKineticMode := false;

  Bounded := true;

  FTickTimer := 0;
  UserInput.AddMessageHandler(@OnThreadMessage);

    {$IfDef ScrollDebugMode}
  FPfad := Pfad;
    {$EndIf}
end;

{procedure TKineticScroller.SetKineticMode(Value: Boolean);
begin
  if not Value then
  begin
    StopTimer;
    Bounded := true;
  end;
  FKineticMode := Value;
end;  }

procedure TKineticScroller.Tick;
var
  Deceleration: Single;
  vi_old: Single;
  //WheelDelta: Integer;
  {$IfDef ScrollDebugMode}
  Str: String;
  {$EndIf}
  pts: Boolean;
begin
//  TimeStep := 1/FFreq;

  {if not Bounded then
  begin}
  scrollmsg_rest := scrollmsg_rest + (vi * Timestep);



  //if exename = 'firefox.exe' then
  //begin
    if (scrollmsg_rest*vi) > 0 then
    begin
      SendScrollMsg(MakeUserInput(CurrentWindow,  Round(scrollmsg_rest)), False, pts);
    end;
    scrollmsg_rest := 0;
  {end else
  begin
     if (scrollmsg_rest >= 1) then
    begin
      SendScrollMsg(Floor(scrollmsg_rest*120));
      scrollmsg_rest := scrollmsg_rest - Floor(scrollmsg_rest);
    end else if (scrollmsg_rest <= -1) then
    begin
      SendScrollMsg(Ceil(scrollmsg_rest*120));
      scrollmsg_rest := scrollmsg_rest - Ceil(scrollmsg_rest);
    end;
  end;  }

  {end;}

  Deceleration := DecelerationFactor.GetValue(Abs(vi));
  if ((vi > 0) and (Deceleration < 0)) or ((vi < 0) and (Deceleration > 0)) then Deceleration := (-1) * Deceleration;

  {$IfDef ScrollDebugMode}
  Str := 'vi: ' + FloatToStr(vi) + '; DecelerationFactor: ' + FloatToStr(Deceleration)
  + '; BottomLimited: ' + FloatToStr(BottomLimitValue(Deceleration, DecelLowerLimit))
  + 'TimeStep: ' + FloatToStr(TimeStep) + '; TimeStep*Deceleration: ' + FloatToStr(Deceleration * (TimeStep)) + '; New vi: ' + FloatToStr(vi - (Deceleration * (TimeStep))) + #13 + #10;
  F.Write(Str[1], Length(Str));
  {$EndIf}
  Deceleration := BottomLimitValue(Deceleration, DecelLowerLimit);
  // Deceleration ist in Zeilen/s² -> vor jedem Tick sind also 1/FFreq Sekunden vergangen
  vi_old := vi;
  {--->}vi := vi - (Deceleration * (TimeStep));

  // Untergrenze unterschritten oder Richtungswechsel (Nulldurchgang durch die x-Achse, denn Sprung kann so groß sein, dass die Untergrenze nicht unterschritten wird)
  if (Abs(vi) < vi_LowerLimit) or ((vi >= 0) and (vi_old <= 0)) or ((vi <= 0) and (vi_old >= 0)) then
  begin
    {$IfDef ScrollDebugMode}
    Str := 'vi underrun; Rebound! ' +#13+#10;
    F.Write(Str[1], Length(Str));
    {$EndIf}
    {vi := 0;
    scrollmsg_rest := 0;
    Bounded := true;
    StopTimer;}
    StopScrolling;
  end;
end;

function TKineticScroller.StopScrolling: boolean;
begin
  Result := not Bounded;
  vi := 0;
  scrollmsg_rest := 0;
  Bounded := true;
  StopTimer;
end;

{function GetScrollingDistance(MouseData: Cardinal): Integer;
begin
  {if HiWord(MouseData) >= 32768 then // Höchstes Bit gesetzt, also ist der Wert negativ
  begin
    Result := (HiWord(MouseData)-65536);
  end else Result := HiWord(MouseData);

  Result := Result div 120;  }

  Result := SmallInt(HiWord(MouseData)) div 120;
end;      }

procedure TKineticScroller.StartTimer;
begin
  if FTickTimer = 0 then
  begin
    FTickTimer := SetTimer(0, 0, 1000 div Freq, nil);
  end;
end;

procedure TKineticScroller.StopTimer;
begin                                 
  if FTickTimer <> 0 then
  begin
    KillTimer(0, FTickTimer);
    FTickTimer := 0;
  end;
end;

procedure TKineticScroller.OnThreadMessage(var message: TMessage);
begin
  case message.msg of
    WM_TIMER: begin
      if PtrUInt(message.wParam) = FTickTimer then
      begin
        Tick;
        message.Result := 1;
      end;
    end;
  end;
end;

{procedure TKineticScroller.SendScrollMsg(Window: HANDLE; WheelDelta: SmallInt);
begin
  if WindowsVersion.IsWin8OrGreater then
  begin
	  x.typ := INPUT_MOUSE;
    x.mi.dwExtraInfo := 0;
    x.mi.time := GetTickCount64; x.mi.dwFlags := MOUSEEVENTF_WHEEL;
    x.mi.mouseData := WheelDelta;
    SendInput(1, @x, SizeOf(x));
	end else
  begin
    PostMessage(Window, WM_MOUSEWHEEL, SmallInt(WheelDelta) shl 16, ScrollPoint.X + (ScrollPoint.Y shl 16));
  end;
end;          }

procedure TKineticScroller.SendScrollMsg(const aUserInput: TUserInput; MessageIsDirectlyFromHook: Boolean; var PassToSystem: Boolean);
begin
  if Assigned(OnSendScrollMsg) then OnSendScrollMsg(aUserInput, MessageIsDirectlyFromHook, PassToSystem);
end;
        {
procedure TKineticScroller.HandleScrollMsg(const aUserInput: TUserInput; var PassToSystem: Boolean);
begin
  if aUserInput.InputType = itKey then
  begin
    StopScrolling;
  end else begin
    PassToSystem := false;
    if {not FKineticMode or} (UpscalingFactor = nil) or (DecelerationFactor = nil) then
      SendScrollMsg(aUserInput)
    else
      ProcessScrollMsgKinetic(aUserInput);
  end;
end; }


procedure TKineticScroller.ProcessScrollMsgKinetic(aUserInput: TUserInput; var PassToSystem: Boolean);
var
  weg1, weg2, weg_gesamt: Integer;
  time_difference: Single;
  new_window: HANDLE;
  {$IfDef ScrollDebugMode}
  Str: String; msg_delay: Cardinal;
  {$EndIf}
begin
  if (UpscalingFactor = nil) or (DecelerationFactor = nil) then
  begin
    //SendScrollMsg(aUserInput);
    PassToSystem := true;
    exit;
  end;

  PassToSystem := false;

  QueryPerformanceCounter(timestamp_current{%H-});

  {$IfDef ScrollDebugMode}
  msg_delay := GetTickCount-aUserInput.Timestamp;
  {$EndIf}

  new_window := WindowFromPoint(aUserInput.Point);
  if FCurrentWindow <> new_window then FCurrentWindow := new_window;
                                       // exename := FensterToolsCommon.GetWindowModuleFileName(FCurrentWindow);

  ScrollPoint := aUserInput.Point;

  weg1 := LastMsg.WheelDelta; weg2 := aUserInput.WheelDelta;
  if weg1*weg2 < 0 then
  begin
    // Richtungswechsel
    {$IfDef ScrollDebugMode}
    Str := 'New Msg: Direction changed. (msg_delay=' + IntToStr(msg_delay) + ')' +#13+#10;
    F.Write(Str[1], Length(Str));
    {$EndIf}
    if Bounded then // solange vmax nicht erreicht, also Nachrichten durchgereicht werden
      vr := 1     // muss vr auf 1, damit am Ende der Prozedur die Nachricht durchgereicht wird
    else begin    // wenn vi von realer Mausradbewegung unabhängig (not Bounded)
      vr := 0; // dann vr := 0, damit die Scrollnachricht nicht weitergereicht wird, da diese Richtungswechselnachricht nur das Stopsignal fürs Weiterscrollen war
      Bounded := true;
      StopTimer();
    end;
  end else
  begin
    // normal weiter
    weg_gesamt := weg1 + weg2;
    time_difference := (timestamp_current-timestamp_last)/PerformanceCounterFrequency;
    vr := weg_gesamt / time_difference;
    vr := UpLimitValue(vr, MaxVReal);// FScrollingOptions.MaxSpeed);
    if (Bounded = false) and (Abs(vr)*UpscalingFactor.GetValue(Abs(vr)) >= Abs(vi)) then
    begin
      // Geschwindigkeit steigt oder konstant
           //StopTimer();  darf hier nicht passieren, sonst scrollts nicht weiter, wenn trotz vmax>vmax_threshold der letzte vr-wert größer war als der vorige
      vi := vr*UpscalingFactor.GetValue(Abs(vr));
      {$IfDef ScrollDebugMode}
      Str := 'New Msg, vi is rising: ' + FloatToStr(vi) + '; vr: ' + FloatToStr(vr) + '; (msg_delay=' + IntToStr(msg_delay) + ')' +#13+#10;
      F.Write(Str[1], Length(Str));
      {$EndIf}

    end else
    begin
      // Wenn noch nicht ungebunden und vr einen Maxwert überschreitet, dann vi von vr lösen (Scrolling geht weiter)
      if Bounded and (Abs(vr) > v_threshold) then
      begin
        // vi ist von vr unabhängig, vi wird jetzt von Tick berechnet+Nachrichten versendet:
        Bounded := false;
        vi := vr*UpscalingFactor.GetValue(Abs(vr));
        {$IfDef ScrollDebugMode}
        Str := 'New Msg, vi exceeds Threshold: Unbound! vi: ' + FloatToStr(vi) + '; (msg_delay=' + IntToStr(msg_delay) + ')' +#13+#10;
        F.Write(Str[1], Length(Str));
        {$EndIf}
        // Timer starten:
        StartTimer();
      end;
    end;
  end;
  LastMsg := aUserInput;
  timestamp_last := timestamp_current;
  if Bounded then vi := vr;
  if vr <> 0 then // vr ist 0, wenn die Richtung gewechselt wurde, sonst reale Geschwindigkeit
  begin
    SendScrollMsg(MakeUserInput(new_window, aUserInput.WheelDelta), true, PassToSystem);
    if not Bounded then
    begin
      {$IfDef ScrollDebugMode}
      Str := 'Message Passthroughed. scrollmsg_rest=' + FloatToStr(scrollmsg_rest) + '; scrollmsg_rest-weg2=' + FloatToStr(scrollmsg_rest - weg2) + #13 + #10;
      F.Write(Str[1], Length(Str));
      //Beep;
      {$EndIf}
      // den schon versendeten Scrollweg aufaddieren, damit Tick den Weg nicht doppelt sendet
      // scrollmsg_rest geht z.B. von 0 auf 1, Tick zieht 0.1 ab, scrollmsg_rest = 0.9, was < 1 somit sendet Tick erstmal keine Nachricht -> fühlt sich flüssig an
      scrollmsg_rest := scrollmsg_rest - weg2;
    end else begin
      {$IfDef ScrollDebugMode}
      Str := 'Message Passthroughed. new_window=' + IntToStr(new_window) + '; WheelDelta=' + IntToStr(aUserInput.WheelDelta) + #13 + #10;
      F.Write(Str[1], Length(Str));
      //Beep;
      {$EndIf}
    end;
  end;
end;

procedure TKineticScroller.SetScrollingOptions(const Value: TJSONObject);
begin
  if (Value = nil) then exit;
  if UpscalingFactor = nil then UpscalingFactor := TCubicSpline.Create;
  if DecelerationFactor = nil then DecelerationFactor := TCubicSpline.Create;
  {$Message Warn 'ScrollOptions erst validieren? oder bei Settings, was ja aber nur ausgeführt wird, wenn Form geöffnet wird'}
  // Settings enthält alle Werte in Zeilen, wir rechnen hier mit 120 Schritten/Zeile (Windows-Standard)
  UpscalingFactor.PointsFromJSON(Value.Arrays['Upscaling'], 120);
  UpscalingFactor.MakeSpline;
  DecelerationFactor.PointsFromJSON(Value.Arrays['Deceleration'], 120, 120);
  DecelerationFactor.MakeSpline;
end;

function TKineticScroller.MakeUserInput(Window: HANDLE; WheelDelta: SmallInt): TUserInput;
begin
  Result.InputType := itScroll;
  Result.Window := Window;
  Result.WheelDelta := WheelDelta;
  Result.Timestamp := GetTickCount;
  Result.Point := ScrollPoint;
end;

destructor TKineticScroller.Destroy;
begin
  StopTimer;
  //DeallocateHWndThreadSafe(FOwner);

  UserInput.RemoveMessageHandler(@OnThreadMessage);

  TryFreeAndNil(UpscalingFactor);
  TryFreeAndNil(DecelerationFactor); 

  {$IfDef ScrollDebugMode}
  F.SaveToFile(FPfad + '\ScrollDebug.txt');
  F.Free;
  {$EndIf}

  inherited Destroy;
end;

{$IfDef ScrollDebugMode}
var
  AppPfad: Array [0..MAX_PATH] of Char;
  Datum: String;
initialization
  SHGetFolderPath(0, CSIDL_APPDATA, Cardinal(-1), 0, @AppPfad[0]);
  DateTimeToString(Datum, 'yyyy-mm-dd_hh-nn-ss', Now);
  Pfad := AppPfad + '\FensterTools\' + Datum;// IntToStr(Y) + '-' + IntToStr(M) + '-' + IntToStr(D) + '_' + IntToStr(H) + '-' + IntToStr(Mi) + '-' + IntToStr(S);
  CreateDir(Pfad);
{$EndIf}
end.

