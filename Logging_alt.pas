unit Logging;

{$mode objfpc}{$H+}

interface

uses Windows, Messages, Classes, Sysutils, Forms, FensterToolsCommon, fgl, SlimReaderWriterLock, ToolBox, strutils;

const
  WM_DEBUG_LOG = WM_USER + 15;

//type
  //TOnNewLogEntry = procedure(LogEntry: String) of object;

type
  TLogType = (LT_NORMAL, LT_WARNING, LT_ERROR);

type

  { TFPGThreadListWithSRWLock }

  generic TFPGThreadListWithSRWLock<TKey, TData> = class(specialize TFPGMap<TKey, TData>)
  private
    fLock: SRWLock;
    function GetKeyData(const AKey: TKey): TData;
  public
    property Lock: SRWLock read fLock;
    property KeyData[const AKey: TKey]: TData read GetKeyData;
    function Add(const AKey: TKey; const AData: TData): Integer;
    function Remove(const AKey: TKey): Integer;
    constructor Create;
  end;

  { TThreadDebugLog }

  TThreadDebugLog = class
  private
    fFile: TFileStream;
    fName: UnicodeString;
    fErrorCount, fWarningCount: Integer;
    procedure Write(Text: UnicodeString);
    procedure EndThread;
  public
    procedure Add(Text: UnicodeString; LogType: TLogType = LT_NORMAL);

    constructor Create(aName: UnicodeString);
    destructor Destroy; override;
  end;

  TThreadDebugLogList = specialize TFPGThreadListWithSRWLock<DWord, TThreadDebugLog>;

type

  { TDebugLog }

  TDebugLog = class
  private
    FDebugConsoleWindow: HWND;
    ThreadDebugLogList: TThreadDebugLogList;

    function GetCurrentThreadLog: TThreadDebugLog;
  public
    property DebugConsoleWindow: HWND read FDebugConsoleWindow write FDebugConsoleWindow;
    property CurrentThreadLog: TThreadDebugLog read GetCurrentThreadLog;

    constructor Create;
    procedure RegisterThreadID(ThreadID: Cardinal; Name: String);
    procedure RegisterThread(Name: String);
    procedure EndThread;
    procedure Add(Text: UnicodeString; LogType: TLogType = LT_NORMAL); overload;
    procedure Add(Text: AnsiString; LogType: TLogType = LT_NORMAL); overload;
    procedure Add(Text: AnsiString; values: Array of const; LogType: TLogType = LT_NORMAL); overload;

    function Dump: String;
//    procedure Perform(Action: String; Params: Array of String; Result: String=''; IsError: boolean=false);
//    procedure Enter(Name: String; Params: Array of String);
//    procedure Leave(Result: String);
    destructor Destroy; override;

  end;


var
  Logger: TDebugLog;
  LogFileBaseName, LogFileExtension: UnicodeString;

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

function BuildStringErrorCount(Errors, Warnings: Integer): String;
begin
  Result := 'Errors ' + IntToStr(Errors) + ', Warnings ' + IntToStr(Warnings);
end;
               {
function CheckErrorCountStringForUnexpectedEvents(str: String): Boolean;
var
  errors, warnings, pos: Integer;
begin
  Result := false;
  try
    pos := PosEx(',', str, 8);
    if pos > 0 then
    begin
      errors := StrToInt(Copy(str, 8, pos-8));
      str := Copy(str, pos+11, Length(str)-pos-10);
      warnings := StrToInt(str);
      if (errors = 0) and (warnings = 0) then Result := true;
    end;
  except
  end;
end;     }

{ TThreadDebugLog }

constructor TThreadDebugLog.Create(aName: UnicodeString);
begin
  fName := aName;
  fFile := TFileStream.Create(UTF8Encode(LogFileBaseName+aName+LogFileExtension), fmOpenWrite or fmCreate or fmShareDenyWrite);
  fErrorCount := 0; fWarningCount := 0;
  Add(UTF8Decode('Thread started'));
end;

procedure TThreadDebugLog.Write(Text: UnicodeString);
var
  utf8Text: RawByteString;
begin
  utf8Text := UTF8Encode(Text) + #13 + #10;
  fFile.Write(utf8Text[1], Length(utf8Text));
end;

procedure TThreadDebugLog.Add(Text: UnicodeString; LogType: TLogType);
var
  timestamp: UnicodeString;
begin
  timestamp := UTF8Decode('[') + FormatDateTime('YYYY-MM-DD HH:mm:ss:zzz', Now) + UTF8Decode('] ');
  case LogType of
    LT_NORMAL: Write(timestamp + Text);
    LT_WARNING:
      begin
        Inc(fWarningCount);
        Write(UTF8Decode('WARNING: ') + timestamp + Text);
      end;
    LT_ERROR:
      begin
        Inc(fErrorCount);
        Write(UTF8Decode('ERROR: ') + timestamp + Text);
      end;
  end;
end;

procedure TThreadDebugLog.EndThread;
var
  creation, exit, kernel, user: _FILETIME;
begin
  GetThreadTimes(GetCurrentThread, creation{%H-}, exit{%H-}, kernel{%H-}, user{%H-});
  Add(UTF8Decode('Ending Thread ... CPU usage: ') + FloatToStrF(Int64(user.dwHighDateTime shl 32 or user.dwLowDateTime)/10000000, ffNumber, 10, 2) + UTF8Decode(' s (USER), ') + FloatToStrF(Int64(kernel.dwHighDateTime shl 32 or kernel.dwLowDateTime)/10000000, ffNumber, 10, 2) + UTF8Decode(' s (KERNEL)'));
  fFile.Free;
end;

destructor TThreadDebugLog.Destroy;
begin
  EndThread;
  inherited Destroy;
end;

{ TFPGThreadListWithSRWLock }

constructor TFPGThreadListWithSRWLock.Create;
begin
  fLock.Init;
  Lock.AcquireExclusive;
  inherited Create;
  Lock.ReleaseExclusive;
end;

function TFPGThreadListWithSRWLock.GetKeyData(const AKey: TKey): TData;
begin
  Lock.AcquireShared;
  Result := inherited KeyData[AKey];
  Lock.ReleaseShared;
end;

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
{procedure TDebugLog.TestCrashState;
var
  s: TStringList;
begin
  FCrashed := false;
  if FileExists(LogFileName) then
  begin
    try
      s := TStringList.Create;
      s.LoadFromFile(LogFileName);
      if s.Count > 0 then
      begin
        if s.Strings[s.Count-1] <> 'Execution finished' then FCrashed := true;
        if not CheckErrorCountStringForUnexpectedEvents(s.Strings[0]) or FCrashed then
          MoveFile(PChar(LogFileName), PChar(ChangeFileExt(LogFileName,'')+'_'+FormatDateTime('YYYY.mm.dd_hh.mm', Now)+'.log'));
      end;
		finally
      s.Free;
		end;
	end;
end;}

function TDebugLog.GetCurrentThreadLog: TThreadDebugLog;
begin
  Result := ThreadDebugLogList.KeyData[GetCurrentThreadId];
end;

    {
procedure TDebugLog.DebugMsgWindowProc(var msg: TMessage);
var
  Text: String;
begin
  if msg.Msg = WM_DEBUG_LOG then
  begin
    try
      Text := StrPas(PAnsiChar(msg.WParam));
      FFile.Write(Text[1], Length(Text));
      if FDebugConsoleWindow <> 0 then PostMessage(FDebugConsoleWindow, WM_DEBUG_LOG, Integer(StrNew(PChar(Text))), 0);
      StrDispose(PAnsiChar(msg.WParam));
    except
      Text := '[...] Could not retrieve DebugMsg'+#13+#10;
      FFile.Write(Text[1], Length(Text));
    end;
  end;
end;
      }
constructor TDebugLog.Create;
begin
  ThreadDebugLogList := TThreadDebugLogList.Create;
  //InitializeCriticalSection(cs_file); InitializeCriticalSection(cs_ThreadIDList);
  //DebugMsgWindow := AllocateHWndThreadSafe(DebugMsgWindowProc);

  //ThreadIDs := TStringList.Create;
  //ThreadIDs.NameValueSeparator := '=';

  //depth := 0; SetLength(stack, 0);

  //TestCrashState;

  //FFile := TFileStream.Create(LogFileName, fmCreate or fmShareDenyWrite);
  //Write(BuildStringErrorCount(0, 0) + #13 + #10 + #13 + #10);
end;

procedure TDebugLog.RegisterThreadID(ThreadID: Cardinal; Name: String);
begin
  ThreadDebugLogList.Add(ThreadID, TThreadDebugLog.Create(Name));
end;

procedure TDebugLog.RegisterThread(Name: String);
begin
  RegisterThreadID(GetCurrentThreadID, Name);
end;

{procedure TDebugLog.Write(Text: String);
begin
  if FDebugConsoleWindow <> 0 then PostMessage(FDebugConsoleWindow, WM_DEBUG_LOG, {%H-}NativeInt(StrNew(PChar(Text))), 0);
end;}

procedure TDebugLog.Add(Text: UnicodeString; LogType: TLogType);
begin
  CurrentThreadLog.Add(Text, LogType);
end;

procedure TDebugLog.Add(Text: AnsiString; LogType: TLogType);
begin
  CurrentThreadLog.Add(Text, LogType);
end;

procedure TDebugLog.Add(Text: AnsiString; values: array of const; LogType: TLogType);
  function FormatConst(const buf: TVarRec): String;
  begin
    case buf.VType of
      vtBoolean: Result := BoolToStr(buf.VBoolean, true);
      vtInteger: Result := IntToStr(buf.VInteger);
      vtAnsiString: Result := PAnsiString(buf.VAnsiString)^;
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

  CurrentThreadLog.Add(newstr, LogType);
end;

procedure TDebugLog.EndThread;
begin
  CurrentThreadLog.Free;
  ThreadDebugLogList.Remove(GetCurrentThreadId);
end;


function TDebugLog.Dump: String;
begin
  Result := '';
end;
              {
procedure TLogger.Perform(Action: String; Params: Array of String; Result: String=''; IsError: boolean=false);
var
  i: Integer;
begin
  if IsError then FErrorOccured := true;
  Action := DateToStr(Date) + ' ' + TimeToStr(Time) + ' ' + Action;
  for i := 0 to depth-1 do Action := '    ' + Action;
  if Length(Params) > 0 then Action := Action + '(';
  FFile.Write(Action[1], Length(Action));
  for i := 0 to High(Params) do
  begin
    if i = High(Params) then Params[i] := Params[i] + ')' else Params[i] := Params[i]+',';
    FFile.Write(Params[i][1], Length(Params[i]));
  end;
  if Result <> '' then
  begin
    Result := '=' + Result + ';' + #13 + #10;
  end else Result := ';' + #13 + #10;
    FFile.Write(Result[1], Length(Result));
end;

procedure TLogger.Enter(Name: String; Params: Array of String);
var
  i: Integer;
begin
  Inc(depth);
  SetLength(Stack, depth);
  Stack[depth-1] := Name;
  for i := 0 to depth-2 do Name := '    ' + Name;
  Name := '--> ' + Name;
  if Length(Params) > 0 then Name := Name + '(' else Name := Name + ';' + #13 + #10;
  FFile.Write(Name[1], Length(Name));
  for i := 0 to High(Params) do
  begin
    if i = High(Params) then Params[i] := Params[i] + ');' + #13 + #10 else Params[i] := Params[i]+',';
    FFile.Write(Params[i][1], Length(Params[i]));
  end;
end;

procedure TLogger.Leave(Result: String);
var
  i: Integer;
  Ergebnis: String;
begin
  Ergebnis := '';
  for i := 0 to depth-1 do Ergebnis := '    ' + Ergebnis;
  Ergebnis := Ergebnis + '<-- ' + Stack[depth-1];
  if Result <> '' then Ergebnis := Ergebnis + '=' + Result;
  Ergebnis := Ergebnis + ';' + #13 + #10;
  FFile.Write(Ergebnis[1], Length(Ergebnis));
  Dec(depth);
  SetLength(Stack, depth);
end;
        }
destructor TDebugLog.Destroy;
var
  i: Integer;
begin
  for i := 0 to ThreadDebugLogList.Count-1 do
  begin
    ThreadDebugLogList.Data[i].Free;
  end;
  ThreadDebugLogList.Free;

  inherited Destroy;
end;

end.
