program CurrentDate;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils
  { you can add units after this };

var
  f: TFileStream;
  str: String;
begin
{  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('Could not find input file "', ParamStr(1) + '"');
    exit;
  end;}
  if ParamCount < 1 then exit;
  f := TFileStream.Create(ParamStr(1), fmOpenWrite or fmCreate);
  str := '''' + FormatDateTime('dd.mm.YYYY', Date, [fdoInterval]) + '''';
  f.Write(str[1], Length(str));
  f.Free;
  WriteLn('Current date successfully saved to "' + ParamStr(1) + '"');
end.

