unit ApplicationRestart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Windows;

type
  TWindowAction = (waNone = 0, waToggleTopMost, waHide);

  { TAppRestartParams }

  {is a container class for all parameters / other information that have to be stored for next restart of application.}
  TAppRestartParams = class
  private
    //fAskForAdminRights: Boolean;
    fTargetAction: TWindowAction;
    fTargetActionHandle: HWND;
  public
    //property AskForAdminRights: Boolean read fAskForAdminRights;
    property TargetActionHandle: HWND read fTargetActionHandle write fTargetActionHandle;
    property TargetAction: TWindowAction read fTargetAction write fTargetAction;

    constructor Create;
  end;

  { TAppStartParams }

  {is a container class for all parameters / other information that have given to this instance of application. It evaluates parameters.}
  TAppStartParams = class
  private
    fIsAdmin: Boolean;
    fIsUpdated: Boolean;
    fMustShow: Boolean;
    fAppHasBeenRestarted: Boolean;
  public
    property IsAdmin: Boolean read fIsAdmin;
    property IsUpdated: Boolean read fIsUpdated;
    property MustShow: Boolean read fMustShow;
    property AppHasBeenRestarted: Boolean read fAppHasBeenRestarted;
    constructor Create;
  end;

  { TApplicationRestarter }

  { extends TApplication. It provides access to RestartParams for next restart of application and a function to request application restart from helper service. }
  TApplicationRestarter = class helper for TApplication
  private
    function GetAppRestartParams: TAppRestartParams;
    function GetAppStartParams: TAppStartParams;

  public
    property RestartParams: TAppRestartParams read GetAppRestartParams;
    property StartParams: TAppStartParams read GetAppStartParams;

    //function RequestRestart: Boolean;
    procedure Restart({ElevateProcess: Boolean = false});
  end;

var
  AppRestartParams: TAppRestartParams;
  AppStartParams: TAppStartParams;

implementation

uses FensterToolsCommon, WindowManager, ServiceConnection, AdminCheck, RSAEncryption;

{ TAppStartParams }

constructor TAppStartParams.Create;
begin
  fIsUpdated := (lowercase(ParamStr(1)) = '/updated');
  fMustShow := (lowercase(ParamStr(2)) = '/show');
  fIsAdmin := IsProcessElevated;

  if fIsUpdated then
  begin
    if FileExists(UpdateTmpFilename) then DeleteFileW(PWideChar(UpdateTmpFilename));
    if FileExists(TRSAEncrypter.GetCertificateFilename(UpdateTmpFilename)) then DeleteFileW(PWideChar(TRSAEncrypter.GetCertificateFilename(UpdateTmpFilename)));
    fAppHasBeenRestarted := true;
  end else
  begin
    fAppHasBeenRestarted := (lowercase(ParamStr(1)) = '/restarted');
  end;
end;

{ TAppRestartParams }

constructor TAppRestartParams.Create;
begin
  inherited Create;

  fTargetActionHandle := 0; fTargetAction := waNone;
end;

{ TApplicationRestarter }

function TApplicationRestarter.GetAppRestartParams: TAppRestartParams;
begin
  Result := AppRestartParams;
end;

function TApplicationRestarter.GetAppStartParams: TAppStartParams;
begin
  Result := AppStartParams;
end;

procedure TApplicationRestarter.Restart({ElevateProcess: Boolean = false});
begin
  WndManager.SaveToFile(AppRestartParams.TargetAction, AppRestartParams.TargetActionHandle);
{  if Clipboard <> nil then
  begin
    //Clipboard.SaveToFile(ClipboardFilename);
    Clipboard.RestoreCB := true;
  end;}

  {if (ElevateProcess) and (WindowsVersion.IsWinVistaOrGreater) then
  begin
    ShellExecute(0, 'runas', PChar(ExtractFilename(paramStr(0))), '', PChar(ExtractFilePath(paramStr(0))), SW_SHOW)
  end else
  begin
    ShellExecute(0, 'open', PChar(ExtractFilename(paramStr(0))), '', PChar(ExtractFilePath(paramStr(0))), SW_SHOW);
  end; }

  HelperSvc.RestartApplicationElevated;
end;

initialization
  AppStartParams := TAppStartParams.Create;
  AppRestartParams := TAppRestartParams.Create;

finalization
  AppRestartParams.Free;
  AppRestartParams := nil;
  AppStartParams.Free;
  AppStartParams := nil;

end.

