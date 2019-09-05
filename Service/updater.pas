unit Updater;

{$mode objfpc}{$H+}

interface

{$IfDef Win32}
  {$Define ImplementInstallUpdate}
{$EndIf}

uses
  Classes, SysUtils, ToolBox, ToolBoxServices, {$IfDef ImplementInstallUpdate}RSAEncryption, {$EndIf}Logging;

{$I UpdateInformation.inc}

type

  { TUpdater }

  TUpdater = class
  private
  public
    class function InstallUpdate(FilenameInstaller: UnicodeString; VerySilent: Boolean): Boolean;
  end;

implementation

{$IfDef ImplementInstallUpdate}
  {$R rsa_key.rc}
{$EndIf}

{ TUpdater }

class function TUpdater.InstallUpdate(FilenameInstaller: UnicodeString; VerySilent: Boolean): Boolean;
{$IfDef ImplementInstallUpdate}
var
  rsaEncrypter: TRSAEncrypter;
  parameters: UnicodeString;
{$EndIf}
begin
  Result := false;
  {$IfDef ImplementInstallUpdate}
  Log('Creating rsaEncrypter ...');
  rsaEncrypter := TRSAEncrypter.Create;
  Log('Loading keys ...');
  rsaEncrypter.LoadKeysFromResource('', 'public-key');
  parameters := UTF8Decode('/SP-');
  Log('Composing parameters ...');
  if VerySilent then parameters := parameters + ' /verysilent' else parameters := parameters + ' /silent';
  Log('Verifying ' + FilenameInstaller);
  if rsaEncrypter.VerifyFile(FilenameInstaller, cfHex) then
  begin
    Log('Creating updater process ... ', false);
    if CreateProcessUnderActiveSession(FilenameInstaller, parameters, cpusSystemAccount) then
    begin
      Result := true;
      Log('+OK');
    end else Log('-FAILED');
  end else begin
    Log('Verification of installer file failed');
  end;
  rsaEncrypter.Free;
  {$EndIf}
end;

end.

