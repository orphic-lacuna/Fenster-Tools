program MakeMD5;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, md5, Sysutils;

var
  f: TFileStream;
  hash: TMD5Digest;
  str: String;
  i, j: Integer;
begin
  ExitCode := 1;
  f := nil;
  try
    f := TFileStream.Create(ParamStr(Paramcount), fmOpenWrite or fmCreate);
    str := 'const'#13#10;
    f.Write(str[1], Length(str));
    str := '  FILE_HASHES: Array [0..' + IntToStr(ParamCount-2) + '] of TMD5Digest = (';
    for j := 1 to Paramcount-1 do
    begin
      if not FileExists(ParamStr(j)) then
      begin
        WriteLn('Could not find input file "', ParamStr(j) + '"');
        break;
      end;
      hash := MD5File(ParamStr(j));
      str := str + '(';
      for i := Low(hash) to High(hash) do
      begin
        str := str + '$' + IntToHex(hash[i], 2) + ', ';
      end;
      SetLength(str, Length(str)-2);
      str := str + '),';
      WriteLn('Hash of file "', ParamStr(j) + '" succesfully computed.');
    end;
    str[Length(str)] := ')';
    str := str + ';';
    f.Write(str[1], Length(str));
    ExitCode := 0;
  finally
    if f <> nil then f.Free;
  end;
  WriteLn('"' + ParamStr(ParamCount) + '" created. All done :-)');
end.

