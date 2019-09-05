unit KeyPatternMatching;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtendedWinAPI, Windows, ToolBox{$IfDef Logging}, Logging{$EndIf}, UserInputProvider;

type
  TUserInputLog = class;

  TKeyPatternEvent = packed record
    Pressed: Boolean;
    MatchRelevant: Boolean;
    //Optional: Boolean;
    PassMsgToSystem: Boolean;
    Key: Byte;
  end;
  TKeySequence = Array of TKeyPatternEvent;

  { TKeyPattern }

  TKeyPattern = class abstract
  public
    function Match(aUserInputLog: TUserInputLog; var PassToSystem: Boolean): Boolean; virtual; abstract;
  end;

  { TSimpleKeyPattern }

  TSimpleKeyPattern = class(TKeyPattern)
  protected
    fKeySequence: TKeySequence;
    fOnExecute: TNotifyEvent;
    procedure Execute; virtual;
  public
    property OnExecute: TNotifyEvent read fOnExecute write fOnExecute;
    function Match(aUserInputLog: TUserInputLog; var PassToSystem: Boolean): Boolean; override;
    constructor Create(aLength: Integer);
  end;

  { TUserInputLog }

  TKeyPatternList = specialize TFPGThreadList<TKeyPattern>;
  TUserInputRingBuffer = specialize TRingBuffer<TUserInput>;
  TKeyState = Array [0..255] of ByteBool;
  TUserInputLog = class
  private
    fCurrentKeyState: TKeyState;
    fPatternList: TKeyPatternList;

    RingBuffer: TUserInputRingBuffer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TUserInput;
    function GetKeyPressed(Index: Byte): Boolean;

    procedure OnUserInput(const aUserInput: TUserInput; var PassToSystem: Boolean);
  public
    property Count: Integer read GetCount;
    property KeyPressed[Index: Byte]: Boolean read GetKeyPressed;
    property Items[Index: Integer]: TUserInput read GetItem;

    procedure RegisterKeyPattern(aPattern: TKeyPattern);
    procedure UnregisterKeyPattern(aPattern: TKeyPattern);
    function MatchKeyPattern(var PassToSystem: Boolean): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  { TKeyPattern_StrgCC }

  TKeyPattern_StrgCC = class(TKeyPattern)
  private
    fOnExecute: TProcedureOfObject;
    //Seq1, Seq2, Seq3: TSimpleKeyPattern;
    procedure Execute;
  public
    property OnExecute: TProcedureOfObject read fOnExecute write fOnExecute;
    function Match(aUserInputLog: TUserInputLog; var PassToSystem: Boolean): Boolean; override;
    constructor Create;
  end;

var
  UserInputLog: TUserInputLog;

implementation

{ TSimpleKeyPattern }

constructor TSimpleKeyPattern.Create(aLength: Integer);
begin
  SetLength(fKeySequence, aLength);
end;

procedure TSimpleKeyPattern.Execute;
begin
  if Assigned(fOnExecute) then fOnExecute(Self);
end;

function TSimpleKeyPattern.Match(aUserInputLog: TUserInputLog; var PassToSystem: Boolean): Boolean;
var
  IndexKeySequence, IndexKeyLog, MatchAtIndexKeySequence: Integer;
begin
  Result := false;

  // trying to find a match by correlating fKeySequence and aKeyLog
  IndexKeyLog := 0; MatchAtIndexKeySequence := -1;
  IndexKeySequence := High(fKeySequence);
  while IndexKeySequence >= 0 do
  begin
    while (fKeySequence[IndexKeySequence].Key <> aUserInputLog.Items[IndexKeyLog].Key) or (fKeySequence[IndexKeySequence].Pressed <> aUserInputLog.Items[IndexKeyLog].Pressed) do
    begin
      if fKeySequence[IndexKeySequence].MatchRelevant then
        exit
      else begin
        Dec(IndexKeySequence);
        if IndexKeyLog > High(fKeySequence) then exit;
        MatchAtIndexKeySequence := -1;
      end;
    end;

    if MatchAtIndexKeySequence < 0 then
    begin
      MatchAtIndexKeySequence := IndexKeySequence;
    end;

    Dec(IndexKeySequence);
    Inc(IndexKeyLog);
  end;

  // if we are here, then we have found a match. The current entered key pattern position is MatchAtIndexKeySequence=KeyLog.Items[0];

  if not fKeySequence[MatchAtIndexKeySequence].PassMsgToSystem then PassToSystem := false;

  // if the KeyPatternEvent at MatchAtIndexKeySequence is MatchRelevant, then we have an exact match, thus execute handler function ...
  // if some MatchRelevant key events are still not fired, we don't get a match
  // and if some non MatchRelevant events are within the match, then the key event at Index MatchAtIndexKeySequence is not MatchRelevant
  // thus:
  if fKeySequence[MatchAtIndexKeySequence].MatchRelevant then
  begin
    Result := true;
    Execute;
  end;
end;

{ TKeyPattern_StrgCC }

procedure TKeyPattern_StrgCC.Execute;
begin
  {$IfDef Logging}Logger.Add('Strg-C-C detected.');{$EndIf}
  if Assigned(fOnExecute) then fOnExecute;
end;

function CompareKeyCodesWithoutLeftRightDistinction(key_with_left_right_distinction, key_without_left_right_distinction: Byte): Boolean;
begin
  Result := key_with_left_right_distinction = key_without_left_right_distinction;
  if not Result then
  begin
    if ((key_with_left_right_distinction = VK_LCONTROL) or (key_with_left_right_distinction = VK_RCONTROL)) and (key_without_left_right_distinction = VK_CONTROL) then Result := true
    else if ((key_with_left_right_distinction = VK_LMENU) or (key_with_left_right_distinction = VK_RMENU)) and (key_without_left_right_distinction = VK_MENU) then Result := true
    else if ((key_with_left_right_distinction = VK_LSHIFT) or (key_with_left_right_distinction = VK_RSHIFT)) and (key_without_left_right_distinction = VK_SHIFT) then Result := true
  end;
end;

function TKeyPattern_StrgCC.Match(aUserInputLog: TUserInputLog; var PassToSystem: Boolean): Boolean;
  function FindPrevious_StrgC: Boolean;
  var
    i: Integer;
  begin
    Result := false;

    for i := 1 to aUserInputLog.Count-2 do
    begin
      //{$IfDef Logging}
      //Logger.Add('        Key-code: ' + IntToStr(aKeyLog.Items[i].VirtualKeyCode) + ', Pressed: ' + BoolToStr(aKeyLog.Items[i].IsPressed, true));
      //if (aKeyLog.Items[i].VirtualKeyCode = VK_C) and (aKeyLog.Items[i].IsPressed) then
      //begin
        //Logger.Add('        --> Found C at i=' + IntToStr(i));
        //Logger.Add('        --> i+1: Key-code: ' + IntToStr(aKeyLog.Items[i+1].VirtualKeyCode) + ', Pressed: ' + BoolToStr(aKeyLog.Items[i+1].IsPressed, true));
      //end;
      //{$EndIf}
      if (aUserInputLog.Items[i].Key = VK_C) and (aUserInputLog.Items[i].Pressed) and CompareKeyCodesWithoutLeftRightDistinction(aUserInputLog.Items[i+1].Key, VK_CONTROL) and (aUserInputLog.Items[i+1].Pressed) then
      begin
        Result := true;
        //{$IfDef Logging}Logger.Add('Previous Strg-C found');{$EndIf}
        exit;
      end;
      if (aUserInputLog.Items[i].Key <> VK_C) and not CompareKeyCodesWithoutLeftRightDistinction(aUserInputLog.Items[i].Key, VK_CONTROL) then exit;
    end;
    //{$IfDef Logging}Logger.Add('Previous Strg-C not found');{$EndIf}
  end;
begin
  //{$IfDef Logging}Logger.Add('Key-code: ' + IntToStr(aKeyLog.Items[0].VirtualKeyCode) + ', Pressed: ' + BoolToStr(aKeyLog.Items[0].IsPressed, true) + ', Ctrl: ' + BoolToStr(aKeyLog.KeyPressed[VK_CONTROL], true));{$EndIf}

  // bei einem Match ist: Key[0] = C, und alles vorher (bis zum Ctrl-Down und C-Down, was beides direkt aufeinander folgen muss) muss entweder Ctrl oder C sein (also bei Maustaste ist es auch raus)
  if (aUserInputLog.Items[0].Key = VK_C) and aUserInputLog.KeyPressed[VK_CONTROL] then
  begin
    //{$IfDef Logging}if aKeyLog.Items[0].IsPressed then Logger.Add('Detected: Strg-C, checking previous Strg-C');{$EndIf}
    if FindPrevious_StrgC then
    begin
      PassToSystem := false;
      if aUserInputLog.Items[0].Pressed then Execute;
    end;
  end;
end;

constructor TKeyPattern_StrgCC.Create;
begin

end;


{ TUserInputLog }

constructor TUserInputLog.Create;
begin
  ZeroMemory(@fCurrentKeyState[0], Length(fCurrentKeyState)*SizeOf(fCurrentKeyState[0]));
  RingBuffer := TUserInputRingBuffer.Create;
  RingBuffer.Count := 20;
  fPatternList := TKeyPatternList.Create;
end;

procedure TUserInputLog.RegisterKeyPattern(aPattern: TKeyPattern);
var
  filter: TUserInputFilter;
begin
  {$IfDef Debug}if fPatternList = nil then raise Exception.Create('fPatternList = nil');{$EndIf}
  {$IfDef Debug}if aPattern = nil then raise Exception.Create('aPattern = nil');{$EndIf}
  fPatternList.AddIfNotExists(aPattern);

  {$IfDef Logging}Logger.Add('Registering UserInputCallback for UserInputLog (KeyPatterns)');{$EndIf}
  filter := TUserInputFilter.Create(true);
  filter.InputTypes := [itKey];
  UserInput.RegisterCallback(@OnUserInput, filter);
end;

procedure TUserInputLog.UnregisterKeyPattern(aPattern: TKeyPattern);
begin
  fPatternList.Lock;
  fPatternList.Remove(aPattern);
  if fPatternList.Count = 0 then
  begin
    {$IfDef Logging}Logger.Add('Unregistering UserInputCallback for UserInputLog (KeyPatterns)');{$EndIf}
    UserInput.UnregisterCallback(@OnUserInput);
  end;
  fPatternList.Unlock;
end;

function TUserInputLog.MatchKeyPattern(var PassToSystem: Boolean): Boolean;
var
  p: TKeyPattern;
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.MatchKeyPattern was called outside the hook thread. That is not expected.');{$EndIf}

  Result := false;
  fPatternList.Lock;
  for p in fPatternList do
  begin
    // p.Match returns true, if an exact match was found
    Result := Result or p.Match(UserInputLog, PassToSystem);
  end;
  fPatternList.Unlock;
end;

function TUserInputLog.GetKeyPressed(Index: Byte): Boolean;
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.GetKeyPressed was called outside the hook thread. That is not expected.');{$EndIf}
  Result := fCurrentKeyState[Index];
end;

procedure TUserInputLog.OnUserInput(const aUserInput: TUserInput; var PassToSystem: Boolean);
begin
  // remember: we are in the hook-thread, and we must be FAST !!!
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.GetItem was called outside the hook thread. That is not expected.');{$EndIf}

  fCurrentKeyState[aUserInput.Key] := ByteBool(aUserInput.Pressed);

  fCurrentKeyState[VK_CONTROL] := ByteBool(fCurrentKeyState[VK_LCONTROL] or fCurrentKeyState[VK_RCONTROL]);
  fCurrentKeyState[VK_MENU] := ByteBool(fCurrentKeyState[VK_LMENU] or fCurrentKeyState[VK_RMENU]);
  fCurrentKeyState[VK_SHIFT] := ByteBool(fCurrentKeyState[VK_LSHIFT] or fCurrentKeyState[VK_RSHIFT]);

  RingBuffer.Write(aUserInput);

  MatchKeyPattern(PassToSystem);
end;

function TUserInputLog.GetItem(Index: Integer): TUserInput;
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.GetItem was called outside the hook thread. That is not expected.');{$EndIf}
  Result := RingBuffer.Items[Index];
end;

function TUserInputLog.GetCount: Integer;
begin
  {$IfDef Debug}if not UserInput.IsRunInThread then raise Exception.Create('TUserInputLog.GetCount was called outside the hook thread. That is not expected.');{$EndIf}
  Result := RingBuffer.Count;
end;

destructor TUserInputLog.Destroy;
begin
  {$IfDef Debug}if fPatternList.Count > 0 then raise Exception.Create('Unclean exit: UserInputLog was freed before all KeyPattern have been unregistered.');{$EndIf}
  fPatternList.Free;
  RingBuffer.Free; // may be freed here, because it is ensured, that UserInputLog.OnUserInput has been unregistered and thus can never be called again by the hook thread
  inherited Destroy;
end;

end.

