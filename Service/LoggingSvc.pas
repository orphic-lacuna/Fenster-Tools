unit logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

var
  logFile: TFileStream;

procedure Log(Str: String; AddLineFeed: Boolean = true);

implementation

var
  cs: _CRITICAL_SECTION;
  lastTimestamp: UInt64;

procedure Log(Str: String; AddLineFeed: Boolean = true);
begin
  EnterCriticalSection(cs);
  str := '[' + IntToStr(GetTickCount64-lastTimestamp) + ' ms] ' + str;
  lastTimestamp := GetTickCount64;
  if AddLineFeed then str := str + #13 + #10;
  logFile.Write(str[1], Length(str));
  LeaveCriticalSection(cs);
end;

initialization
  InitializeCriticalSection(cs);
  {$IfDef Win32}
  logFile := TFileStream.Create(ExtractFilePath(ParamStr(0))+'Log32.txt', fmCreate or fmOpenWrite or fmShareDenyNone);
  {$Else}
  logFile := TFileStream.Create(ExtractFilePath(ParamStr(0))+'Log64.txt', fmCreate or fmOpenWrite or fmShareDenyNone);
  {$EndIf}

finalization
  logFile.Free;
  DeleteCriticalSection(cs);



end.

