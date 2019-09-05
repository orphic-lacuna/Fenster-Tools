program SignTool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, RSAEncryption, base64, FileUtil;

type

  { TSignTool }

  TSignTool = class(TCustomApplication)
  protected
    fInputFile, fPrivateKeyFilename: String;
    rsa: TRSAEncrypter;

    procedure DoRun; override;
    procedure CreateBase64EncodedInstaller(outFilename: String);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp;
    destructor Destroy; override;
  end;

{ TSignTool }

procedure TSignTool.DoRun;
begin
  ExitCode := 2;
  // quick check parameters
  if not HasOption('f', 'file') then
  begin
    WriteLn('Missing input file.');
    WriteLn('');
    WriteHelp;
    Terminate;
    Exit;
  end else fInputFile := GetOptionValue('f', 'file');

  if not FileExists(fInputFile) then
  begin
    WriteLn('Input file not found.');
    Terminate;
    Exit;
  end;

  if not HasOption('k', 'key') then
  begin
    WriteLn('Missing private key file.');
    WriteLn('');
    WriteHelp;
    Terminate;
    Exit;
  end else fPrivateKeyFilename := GetOptionValue('k', 'key');

  if not FileExists(fPrivateKeyFilename) then
  begin
    WriteLn('Private key file not found.');
    Terminate;
    Exit;
  end;

  rsa.LoadKeysFromFile(fPrivateKeyFilename, '');
  try
    WriteLn('Signing ' + ExtractFileName(fInputFile) + ' ...');
    rsa.SignFile(fInputFile, cfHex);
    Dec(ExitCode);
    WriteLn('    +OK Certificate saved as ' + ExtractFileName(rsa.GetCertificateFilename(fInputFile)));
  except
    WriteLn('    Could not sign file ' + ExtractFileName(fInputFile) + '.');
  end;

  if HasOption('base64') then
  begin
    try
      WriteLn('Base64-Encoding of ' + ExtractFileName(fInputFile) + ' ...');
      CreateBase64EncodedInstaller(ExtractFileNameWithoutExt(fInputFile) + '.txt');
      WriteLn('    +OK Base64 encoded installer saved as ' + ExtractFileNameWithoutExt(ExtractFileName(fInputFile)) + '.txt');
      Dec(ExitCode);
    except
      WriteLn('    Could not write to file ' + ExtractFileNameWithoutExt(fInputFile) + '.txt');
    end;
  end else Dec(ExitCode);

  // stop program loop
  Terminate;
end;

procedure TSignTool.CreateBase64EncodedInstaller(outFilename: String);
var
  installerStream, encodedStream: TFileStream;
  encoder: TBase64EncodingStream;
begin
  installerStream := TFileStream.Create(fInputFile, fmOpenRead);
  encodedStream := TFileStream.Create(outFilename, fmOpenWrite or fmCreate);

  encoder := TBase64EncodingStream.Create(encodedStream);
  encoder.CopyFrom(installerStream, installerStream.Size);
  encoder.Free;

  encodedStream.Free;
  installerStream.Free;
end;

constructor TSignTool.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  rsa := TRSAEncrypter.Create;
end;

procedure TSignTool.WriteHelp;
begin
  WriteLn('Usage:');
  WriteLn('SignTool.exe -f [filename] -k [filename of private key]');
end;

destructor TSignTool.Destroy;
begin
  rsa.Free;
  inherited Destroy;
end;

var
  Application: TSignTool;
begin
  Application := TSignTool.Create(nil);
  Application.Title := 'SignTool';
  Application.Run;
  Application.Free;
end.

