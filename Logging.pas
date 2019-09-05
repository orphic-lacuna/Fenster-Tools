unit Logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtendedThreads, fgl, SlimReaderWriterLock, Windows, FensterToolsCommon, strutils, Math;

type

  { TFPGThreadListWithSRWLock }

  generic TFPGThreadListWithSRWLock<TKey, TData> = class(specialize TFPGMap<TKey, TData>)
  private
    fLock: SRWLock;
    //function GetKeyData(const AKey: TKey): TData;
  public
    property Lock: SRWLock read fLock;
    //property KeyData[const AKey: TKey]: TData read GetKeyData;
    function Add(const AKey: TKey; const AData: TData): Integer;
    function Remove(const AKey: TKey): Integer;
    constructor Create;
  end;

  TThreadDebugLogList = specialize TFPGThreadListWithSRWLock<DWord, UnicodeString>;

type
  TDebugLog = class;

  { TLogParameterObject }

  TLogType = (LT_NORMAL, LT_WARNING, LT_ERROR);

  TLogParameterObject = class
  private
    fLogType: TLogType;
    fText: UnicodeString;
    fTimestamp: TDateTime;
    fThreadID: TThreadID;
    fParent: TDebugLog;
  public
    constructor Create(aText: UnicodeString; aType: TLogType; aParent: TDebugLog);
    procedure WriteToStream(aStream: TStream);
    function ToString: ansistring; override;
  end;

  TLogNotification = procedure(aLogEntry: TLogParameterObject) of object;

  { TDebugLog }

  TDebugLog = class(TThreadEx)
  private
    ThreadDebugLogList: TThreadDebugLogList;
    fLogFile: TFileStream;
    fLogNotificationMethod: TLogNotification;
    procedure WriteTextToFile(aParameterObject: TObject);
  protected
    procedure ThreadInternalCreate; override;
    procedure ThreadInternalDestroy; override;
  public
    constructor Create(aLogNotificationMethod: TLogNotification); reintroduce;

    procedure RegisterThreadID(ThreadID: Cardinal; Name: UnicodeString);
    procedure RegisterThread(Name: UnicodeString);
    procedure EndThread; overload;
    procedure RemoveThreadFromList(ThreadID: NativeInt); overload;
    procedure Add(Text: UnicodeString; LogType: TLogType = LT_NORMAL); overload;
    procedure Add(Text: UnicodeString; values: Array of const; LogType: TLogType = LT_NORMAL); overload;

    destructor Destroy; override;
  end;

var
  Logger: TDebugLog;
  LogFileName: UnicodeString;

function DebugLookupClipboardFormat(Format: Cardinal): String;
function DebugLookupHookType(idHook: Cardinal): String;

implementation

function DebugLookupClipboardFormat(Format: Cardinal): String;
var
  x: PAnsiChar;
begin
  case Format of
    CF_TEXT: Result := 'CF_TEXT';
    CF_LOCALE: Result := 'CF_LOCALE';
    CF_OEMTEXT: Result := 'CF_OEMTEXT';
    CF_UNICODETEXT: Result := 'CF_UNICODETEXT';
    CF_BITMAP: Result := 'CF_BITMAP';
    CF_DIB: Result := 'CF_DIB';
    else begin
      x := StrAlloc(256);
      GetClipboardFormatNameA(Format, x, 255);
      Result := IntToStr(Format) + ' (' + x + ')';
    end;
  end;
end;

function DebugLookupHookType(idHook: Cardinal): String;
begin
  case idHook of
    WH_KEYBOARD_LL: Result := 'WH_KEYBOARD_LL';
    WH_MOUSE_LL: Result := 'WH_MOUSE_LL';
    else Result := IntToStr(idHook);
  end;
end;

{ TLogParameterObject }

constructor TLogParameterObject.Create(aText: UnicodeString; aType: TLogType; aParent: TDebugLog);
begin
  fLogType := aType;
  fText := aText;
  fThreadID := GetCurrentThreadId;
  fTimestamp := Now;
  fParent := aParent;
end;

procedure TLogParameterObject.WriteToStream(aStream: TStream);
var
  strToWrite: AnsiString;
begin
  strToWrite := ToString;
  aStream.Write(strToWrite[1], Length(strToWrite));
end;

function TLogParameterObject.ToString: ansistring;
var
  timestamp, threadName: UnicodeString;
  logTypeStr: AnsiString;
  i: Integer;
begin
  timestamp := UTF8Decode('[') + FormatDateTime('YYYY-MM-DD HH:mm:ss:zzz', fTimestamp) + UTF8Decode(']');

  fParent.ThreadDebugLogList.Lock.AcquireShared;

  if fParent.ThreadDebugLogList.Find(fThreadID, i) then
    threadName := UTF8Decode('[') + fParent.ThreadDebugLogList.Data[i] + UTF8Decode('] ')
  else
    threadName := UTF8Decode('[unknown] ');

  fParent.ThreadDebugLogList.Lock.ReleaseShared;

  case fLogType of
    LT_NORMAL: logTypeStr := '';
    LT_WARNING:
      begin
        //Inc(fWarningCount);
        logTypeStr := 'WARNING: ';
      end;
    LT_ERROR:
      begin
        //Inc(fErrorCount);
        logTypeStr := 'ERROR: ';
      end;
  end;
  Result := UTF8Encode(timestamp) + UTF8Encode(threadName) + logTypeStr + UTF8Encode(fText) + #13 + #10;
end;

{ TFPGThreadListWithSRWLock }

constructor TFPGThreadListWithSRWLock.Create;
begin
  fLock.Init;
  Lock.AcquireExclusive;
  inherited Create;
  Sorted := true;
  Lock.ReleaseExclusive;
end;

{function TFPGThreadListWithSRWLock.GetKeyData(const AKey: TKey): TData;
begin
  Lock.AcquireShared;
  Result := inherited KeyData[AKey];
  Lock.ReleaseShared;
end;}

function TFPGThreadListWithSRWLock.Add(const AKey: TKey; const AData: TData): Integer;
begin
  Lock.AcquireExclusive;
  Result := inherited Add(AKey, AData);
  Lock.ReleaseExclusive;
end;

function TFPGThreadListWithSRWLock.Remove(const AKey: TKey): Integer;
begin
  Lock.AcquireExclusive;
  Result := inherited Remove(AKey);
  Lock.ReleaseExclusive;
end;

{ TDebugLog }

constructor TDebugLog.Create(aLogNotificationMethod: TLogNotification);
begin
  fLogNotificationMethod := aLogNotificationMethod;
  inherited Create(true);
end;

procedure TDebugLog.WriteTextToFile(aParameterObject: TObject);
begin
  {$IfDef Debug}if not aParameterObject.InheritsFrom(TLogParameterObject) then raise Exception.Create('TDebugLog.WriteTextToFile was called with invalid parameters object.');{$EndIf}
  TLogParameterObject(aParameterObject).WriteToStream(fLogFile);

  if Assigned(fLogNotificationMethod) then
  begin
    DoCallbackProc(TThreadMethodParamObjectContainer.MethodType(fLogNotificationMethod), aParameterObject, true);
  end else aParameterObject.Free; {$Message Hint 'Ist das korrekt oder typecast nötig?'}
end;

procedure TDebugLog.ThreadInternalCreate;
begin
  inherited ThreadInternalCreate;
  ThreadDebugLogList := TThreadDebugLogList.Create;
  fLogFile := TFileStream.Create(LogFileName, fmOpenWrite or fmCreate or fmShareDenyWrite);
end;

procedure TDebugLog.RegisterThreadID(ThreadID: Cardinal; Name: UnicodeString);
begin
  ThreadDebugLogList.Add(ThreadID, Name); // is thread-safe
end;

procedure TDebugLog.RegisterThread(Name: UnicodeString);
begin
  RegisterThreadID(GetCurrentThreadId, Name);
end;

procedure TDebugLog.EndThread;
var
  creation, exit, kernel, user: _FILETIME;
begin
  GetThreadTimes(GetCurrentThread, creation{%H-}, exit{%H-}, kernel{%H-}, user{%H-});
  Add(UTF8Decode('Ending Thread ... CPU usage: ') + FloatToStrF(Int64(user.dwHighDateTime shl 32 or user.dwLowDateTime)/10000000, ffNumber, 10, 2) + UTF8Decode(' s (USER), ') + FloatToStrF(Int64(kernel.dwHighDateTime shl 32 or kernel.dwLowDateTime)/10000000, ffNumber, 10, 2) + UTF8Decode(' s (KERNEL)'));
  DoThreadProc(@RemoveThreadFromList, NativeInt(GetCurrentThreadId));
end;

procedure TDebugLog.RemoveThreadFromList(ThreadID: NativeInt);
begin
  ThreadDebugLogList.Remove(TThreadID(ThreadID));
end;

procedure TDebugLog.Add(Text: UnicodeString; LogType: TLogType);
begin
  DoThreadProc(@WriteTextToFile, TLogParameterObject.Create(Text, LogType, Self), false);
end;

procedure TDebugLog.Add(Text: UnicodeString; values: array of const; LogType: TLogType);
  function FormatConst(const buf: TVarRec): String;
  begin
    case buf.VType of
      vtBoolean: Result := BoolToStr(buf.VBoolean, true);
      vtInteger: Result := IntToStr(buf.VInteger);
      vtAnsiString: Result := PAnsiString(buf.VAnsiString)^;
      vtWideString: Result := UTF8Decode(PWideString(buf.VWideString)^);
      vtChar: Result := buf.VChar;
      vtWideChar: Result := UTF8Decode(buf.VWideChar);
      vtPChar: Result := buf.VPChar;
      vtPWideChar: Result := UTF8Decode(buf.VPWideChar);
      vtExtended: Result := FloatToStr(buf.VExtended^);
      else Result := '[unknown variable type]';
    end;
  end;

var
  p, offset, argnr: Integer;
  newstr: String;
begin
  offset := 1;
  newstr := '';
  argnr := 0;
  repeat
    p := PosEx('%', Text, offset);
    if p > 0 then
    begin
      newstr := newstr + Copy(Text, offset, p-offset) + FormatConst(values[argnr]);
      Inc(argnr);
      offset := p+1;
    end;
  until p <= 0;
  newstr := newstr + Copy(Text, offset, Length(Text)-offset+1);

  Add(newstr, LogType);
end;

destructor TDebugLog.Destroy;
begin
  inherited Destroy;
end;

procedure TDebugLog.ThreadInternalDestroy;
var
  str: AnsiString;
begin
  str := 'Execution finished';
  fLogFile.Write(str[1], Length(str));
  fLogFile.Free;
  ThreadDebugLogList.Free;
  inherited ThreadInternalDestroy;
end;

end.

