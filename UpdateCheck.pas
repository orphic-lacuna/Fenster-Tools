unit UpdateCheck;

{$mode objfpc}{$H+}
{$ModeSwitch AdvancedRecords}

interface

uses
  Windows, JwaWindows, Classes, ExtendedThreads, ExtCtrls, RSAEncryption, SysUtils, Dialogs{$IfDef Logging}, Logging{$EndIf}, FensterToolsCommon, ToolBox, fpjson, jsonparser, AsyncHTTPRequest, dateutils, PowerNotification, base64;

{
  Roadmap:

  ResumeAfterStandby or OnTimer -> UpdateCheckNecessary? --no--> SetUpdaterTimerInterval (skipped, if UpdateTimer = nil; this occurs if UpdateMode = umNever)
                                              |
                                              |yes
                                              -> CheckForUpdate (calls SetUpdaterTimerInterval again)
                                                |
                             UpdateCheckRequest | succeeded
                                     and update | available
                                                |
          |--------------------------------------------------------------------------|
          |       UpdateMode = umInstall                 UpdateMode = umOnlySearch   |
          |                                                                          |
          |                                                                          |
          | User is not present                                               User is|present
           ------------> Install update    Update form and show notification <-------|
          |                                             |Notification click          |
          |                                             |                            |
          |                                      Install update                      |User is not present
          |User is present                                                           |
          |------------> Do nothing                                                  ------> Do nothing
          (installation will be triggered                                            (notification will be triggered by
          by TPowerNotification once user                                            TPowerNotification once user is
          is not present anymore)                                                    present again)
          assumed that UpdateState = usCheckFinished                                 assumed that UpdateState = usCheckFinished
}

type
  TUpdater = class;
  type TUpdateNotifyEvent = procedure(Sender: TUpdater) of object;

  { TUpdater }

  { TUpdateCheckInformation }

  TUpdateCheckInformation = record
  private
    DownloadLink: UnicodeString;
    Certificate: String;
    procedure Reset;
  public
    CheckSuccessful: Boolean;
    UpdateAvailable: Boolean;
    UserInvoked: Boolean;
    UserHasBeenNotified: Boolean;
    NewVersion: UnicodeString;
    NewVersionDate: UnicodeString;
    NewVersionHints: UnicodeString;
  end;

  { TUpdateDownloadInformation }

  TUpdateDownloadInformation = record
  public
    BytesTotal, BytesReceived: Integer;
    DownloadSuccessful: Boolean;
    procedure Reset;
  end;

  TUpdateState = (usIdle, usCheckRunning, usCheckFinished, usDownloadRunning, usDownloadFinished);

  TUpdater = class
  private
    fPowerNotification: TPowerNotification;
    fErrorString: UnicodeString;
    httpReq: TAsyncHTTPRequest;
    fOnUpdateStatusChanged, fOnUpdateNotifyUser: TUpdateNotifyEvent;
    fUpdateState: TUpdateState;

    fUpdateCheckInformation: TUpdateCheckInformation;
    fUpdateDownloadInformation: TUpdateDownloadInformation;
    UpdateTimer: TTimer;
    nextUpdateTime: TDateTime;

    function CheckConditionsForUpdateDownload: Boolean;
    procedure HTTPRequestStatusUpdate_UpdateCheck(Sender: TObject);
    procedure HTTPRequestStatusUpdate_UpdateDownload(Sender: TObject);
    procedure OnResumeFromStandby(Sender: TObject);
    procedure OnUserPresenceChanged(Sender: TObject);
    function UpdateCheckNecessary: Boolean;
    procedure SetUpdateTimerInterval;
    procedure OnSettingsChanged(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure DownloadUpdate;
    procedure InstallUpdate(VerySilent: Boolean);
  protected
  public
    property OnUpdateStatusChanged: TUpdateNotifyEvent read fOnUpdateStatusChanged write fOnUpdateStatusChanged;
    property OnUpdateNotifyUser: TUpdateNotifyEvent read fOnUpdateNotifyUser write fOnUpdateNotifyUser;
    property UpdateState: TUpdateState read fUpdateState;

    property CheckInformation: TUpdateCheckInformation read fUpdateCheckInformation;
    property DownloadInformation: TUpdateDownloadInformation read fUpdateDownloadInformation;
    property ErrorString: UnicodeString read fErrorString;

    function CheckForUpdate(aUserInvoked: Boolean = false): Boolean;
    procedure ResetState;

    constructor Create;
    destructor Destroy; override;
  end;

var
  Updater: TUpdater;
{$I Service/UpdateInformation.inc}

implementation

uses ServiceConnection, SettingsManager;

{$IfDef Debug}
var
  InstanceCounter: Integer = 0;
{$EndIf}

{ TUpdateCheckInformation }

procedure TUpdateCheckInformation.Reset;
begin
  CheckSuccessful := false;
  UpdateAvailable := false;
  UserInvoked := false;
  UserHasBeenNotified := false;
  DownloadLink := '';
  NewVersion := '';
  NewVersionDate := '';
  NewVersionHints := '';
  Certificate := '';
  //UserInvoked := false;
end;

{ TUpdateDownloadInformation }

procedure TUpdateDownloadInformation.Reset;
begin
  BytesTotal := 0; BytesReceived := 0; DownloadSuccessful := false;
end;

constructor TUpdater.Create();
begin
  {$IfDef Debug}
  Inc(InstanceCounter);
  if InstanceCounter > 1 then raise Exception.Create('Es darf nur eine Instanz von TUpdater geben!');
  {$EndIf}
  fUpdateState := usIdle;
  PowerNotifications.OnResumeFromStandby.Add(@OnResumeFromStandby);
  PowerNotifications.OnUserPresenceChanged.Add(@OnUserPresenceChanged);
  Settings.Update.ChangeHandler.Add(@OnSettingsChanged);
end;

function GetHardDiskSerial(): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
begin
  GetVolumeInformation('C:\', nil, 0, @VolumeSerialNumber, NotUsed, VolumeFlags, nil, 0);
  Result := IntToStr(VolumeSerialNumber);
end;

function GetUsername: UnicodeString;
var
  size: Cardinal;
begin
  size := 0;
  Windows.GetUserNameW(nil, size);
  SetLength(Result, size);
  Windows.GetUsernameW(@Result[1], size);
  SetLength(Result, size-1);
end;
function GetComputername: UnicodeString;
var
  size: Cardinal;
begin
  size := 0;
  Windows.GetComputerNameW(nil, size);
  SetLength(Result, size);
  Windows.GetComputerNameW(@Result[1], size);
  SetLength(Result, size);
end;

function TUpdater.UpdateCheckNecessary: Boolean;
begin
  // Settings.Update.Interval is measured in days
  // Settings.Update.LastCheck is TDateTime (=TSettingsBaseFloat -> Double)
  nextUpdateTime := IncDay(Settings.Update.LastCheck.Value, Settings.Update.Interval);
  Result := nextUpdateTime < Now;
end;

procedure TUpdater.SetUpdateTimerInterval;
var
  ms: Int64;
begin
  if UpdateTimer <> nil then
  begin
    UpdateTimer.Enabled := false;
    // Settings.Update.Interval is measured in days
    // Settings.Update.LastCheck is TDateTime (=TSettingsBaseFloat -> Double)

    if UpdateCheckNecessary then ms := {$IfDef Debug}1000{$Else}60000{$EndIf}
    else begin
      ms := MilliSecondsBetween(Now, nextUpdateTime) + 60000;
      if ms > MaxInt then ms := 1000*3600*24*7;
    end;
    UpdateTimer.Interval := ms;
    UpdateTimer.Enabled := true;

    //if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);
  end;
end;

procedure TUpdater.OnSettingsChanged(Sender: TObject);
begin
  if Sender = Settings.Update.Interval then
  begin
    SetUpdateTimerInterval; // will be skipped, if UpdateTimer = nil
  end else if Sender = Settings.Update.Mode then
  begin
    if TUpdateMode(Settings.Update.Mode.Value) <> umNever then
    begin
      if UpdateTimer = nil then
      begin
        UpdateTimer := TTimer.Create(nil);
        UpdateTimer.OnTimer := @OnTimer;
      end;
      SetUpdateTimerInterval;
    end else begin
      TryFreeAndNil(UpdateTimer);
    end;
  end;
end;

procedure TUpdater.OnTimer(Sender: TObject);
begin
  UpdateTimer.Enabled := false;
  if UpdateCheckNecessary then
    CheckForUpdate
  else
    SetUpdateTimerInterval;
end;

procedure TUpdater.HTTPRequestStatusUpdate_UpdateCheck(Sender: TObject);
var
  json_response: TJSONObject;
begin
  // ist schon wieder der LCL-Thread
  if TAsyncHTTPJSONRequest(Sender).RequestStatus = rsCompleted then
  begin
    json_response := TJSONObject(TAsyncHTTPJSONRequest(Sender).ResponseAsJSON);
    if (json_response <> nil) and (json_response.InheritsFrom(TJSONObject)) then
    begin
      try
        fUpdateCheckInformation.UpdateAvailable := json_response.Booleans['UpdateAvailable'];
        if fUpdateCheckInformation.UpdateAvailable then
        begin
          fUpdateCheckInformation.NewVersion := UnicodeString(json_response.Strings['NewVersion']);
          fUpdateCheckInformation.NewVersionDate := UnicodeString(json_response.Strings['Date']);
          fUpdateCheckInformation.NewVersionHints := UnicodeString(json_response.Strings['Hints']);
          fUpdateCheckInformation.DownloadLink := UnicodeString(json_response.Strings['DownloadLink']);
          fUpdateCheckInformation.Certificate := json_response.Strings['Certificate'];
          fUpdateCheckInformation.Certificate.ToFile(TRSAEncrypter.GetCertificateFilename(UpdateTmpFilename));
        end;
        fUpdateCheckInformation.CheckSuccessful := true;

        {$IfDef Logging}Logger.Add('Update check successful, update available: ' + BoolToStr(fUpdateCheckInformation.UpdateAvailable, true));{$EndIf}
      except
        {$IfDef Logging}Logger.Add('Invalid server response to update request: ' + json_response.AsJSON, LT_ERROR);{$EndIf}
        fUpdateCheckInformation.CheckSuccessful := false;
        fErrorString := 'Ungültige Antwort vom Server';
      end;
    end else begin
      {$IfDef Logging}Logger.Add('Invalid server response to update request: no json response, received: ' + TAsyncHTTPJSONRequest(Sender).ResponseText, LT_ERROR);{$EndIf}
      fErrorString := 'Ungültige Antwort vom Server';
    end;
    TryFreeAndNil(json_response);
  end else if TAsyncHTTPJSONRequest(Sender).RequestStatus = rsFailed then
  begin
    {$IfDef Logging}Logger.Add('Invalid server response, http status code: ' + IntToStr(TAsyncHTTPJSONRequest(Sender).StatusCode), LT_ERROR);{$EndIf}
    if TAsyncHTTPJSONRequest(Sender).StatusCode = 0 then
      fErrorString := 'Verbindung fehlgeschlagen'
    else
      fErrorString := 'Fehler, Server antwortet: ' + UnicodeString(IntToStr(TAsyncHTTPJSONRequest(Sender).StatusCode));
  end;

  if (TAsyncHTTPJSONRequest(Sender).RequestStatus = rsFailed) or (TAsyncHTTPJSONRequest(Sender).RequestStatus = rsCompleted) then
  begin
    Settings.Update.LastCheck.Value := Now;
    SetUpdateTimerInterval;
    httpReq := nil;
    Sender.Free;
    fUpdateState := usCheckFinished;
    if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);

    if CheckConditionsForUpdateDownload then DownloadUpdate;
  end;
end;

procedure TUpdater.HTTPRequestStatusUpdate_UpdateDownload(Sender: TObject);
var
  decoder: TBase64DecodingStream;
  f: TFileStream;
  decoded_successfully: Boolean;
begin
  // ist schon wieder der LCL-Thread
  case TAsyncHTTPRequest(Sender).RequestStatus of
    rsHeaderReceived: fUpdateDownloadInformation.BytesTotal := TAsyncHTTPRequest(Sender).ContentLength;
    rsReceivingResponse: begin
      fUpdateDownloadInformation.BytesReceived := TAsyncHTTPRequest(Sender).BytesReceived;
      if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);
    end;
    rsCompleted: begin
      {$IfDef Logging}Logger.Add('Update download successful');{$EndIf}
      decoded_successfully := false;
      TAsyncHTTPRequest(Sender).ResponseStream.Position := 0;
      decoder := TBase64DecodingStream.Create(TAsyncHTTPRequest(Sender).ResponseStream);
      try
        f := TFileStream.Create(UpdateTmpFilename, fmOpenWrite or fmCreate);
        f.CopyFrom(decoder, decoder.Size);
        f.Free;
        decoded_successfully := true;
      except
        {$IfDef Logging}Logger.Add('Could not decode base64-encoded installer file', LT_ERROR);{$EndIf}
      end;
      decoder.Free;
      if decoded_successfully then fUpdateDownloadInformation.DownloadSuccessful := true;
    end;
    rsFailed: begin
      {$IfDef Logging}Logger.Add('Invalid server response, http status code: ' + IntToStr(TAsyncHTTPRequest(Sender).StatusCode), LT_ERROR);{$EndIf}
    end;
  end;

  if (TAsyncHTTPRequest(Sender).RequestStatus = rsFailed) or (TAsyncHTTPRequest(Sender).RequestStatus = rsCompleted) then
  begin
    //TAsyncHTTPRequest(Sender).ResponseStream.Free; // is now an automatic created and freed memory stream (managed by TAsyncHTTPRequest)
    if fUpdateDownloadInformation.DownloadSuccessful then InstallUpdate(not fUpdateCheckInformation.UserInvoked);
    httpReq := nil;
    fUpdateState := usDownloadFinished;
    if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);
    Sender.Free;
  end;
end;

procedure TUpdater.OnResumeFromStandby(Sender: TObject);
begin
  OnTimer(Sender);
end;

procedure TUpdater.OnUserPresenceChanged(Sender: TObject);
begin
  if CheckConditionsForUpdateDownload then DownloadUpdate;
end;

function TUpdater.CheckConditionsForUpdateDownload: Boolean;
begin
  Result := false;
  if (fUpdateState = usCheckFinished) and fUpdateCheckInformation.CheckSuccessful and fUpdateCheckInformation.UpdateAvailable then
  begin
    if not fUpdateCheckInformation.UserInvoked then
    begin
      if TUpdateMode(Settings.Update.Mode.Value) <> umInstall then
      begin
        // only inform user, if update is not user invoked and is not permitted to be installed automatically
        // and check, that the user is present, so he sees the notification
        if not fUpdateCheckInformation.UserHasBeenNotified and PowerNotifications.IsUserPresent and Assigned(fOnUpdateNotifyUser) then
        begin
          fOnUpdateNotifyUser(Self);
          fUpdateCheckInformation.UserHasBeenNotified := true;
        end;
      end else
      begin
        // non user invoked update must be deferred until user is not present anymore
        if not PowerNotifications.IsUserPresent then Result := true;
      end;
    end else Result := true; // user invoked update may be done immediately
  end;
end;

function TUpdater.CheckForUpdate(aUserInvoked: Boolean): Boolean;
var
  j: TJSONObject;
begin
  Result := false;

  if aUserInvoked then
  begin
    // last check was probably not user invoked, but this "Check" (user clicked the update available baloon hint)
    // is user invoked, so force the download and do not show the hint again
    fUpdateCheckInformation.UserInvoked := true;

    if CheckConditionsForUpdateDownload then
    begin
      DownloadUpdate;
      Result := true;
      exit;
    end;
  end;

  if (fUpdateState = usCheckRunning) or (fUpdateState = usDownloadRunning) or (fUpdateState = usDownloadFinished) then
  begin
    {$IfDef Logging}Logger.Add('TUpdater.CheckForUpdate: Previous request still running (UpdateState)', LT_WARNING);{$EndIf}
    exit;
  end;
  {$IfDef Debug}if httpReq <> nil then raise Exception.Create('CheckForUpdate was called, but httpReq <> nil');{$EndIf}

  fUpdateCheckInformation.Reset;
  fUpdateCheckInformation.UserInvoked := aUserInvoked;

  fUpdateState := usCheckRunning;
  if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);

  httpReq := TAsyncHTTPJSONRequest.Create(UTF8Decode(UpdateSource));
  httpReq.ThreadClass := TBasicThreadWithCallbackWnd;

  j := TJSONObject.Create;
  j.Add('WindowsVersion', GetWinVersionJSON);
  j.Add('Config', Settings.AsJSON);
  j.Add('Username', UTF8Encode(GetUsername));
  j.Add('Computername', UTF8Encode(GetComputername));
  j.Add('HDDSerial', GetHardDiskSerial);
  j.Add('CurrentVersion', CurrentProgramVersion);

  httpReq.OnStatusUpdate := @HTTPRequestStatusUpdate_UpdateCheck;
  TAsyncHTTPJSONRequest(httpReq).Post(j);

  j.Free;

  Result := true;
end;

procedure TUpdater.ResetState;
begin
  fUpdateState := usIdle;
  if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);
end;

procedure TUpdater.DownloadUpdate;
begin
  {$IfDef Debug}if httpReq <> nil then raise Exception.Create('Old http request was not freed properly.');{$EndIf};

  if fUpdateState <> usCheckFinished then
  begin
    {$IfDef Logging}Logger.Add('Update download cancelled (fUpdateState <> usCheckFinished)', LT_WARNING);{$EndIf}
    exit;
  end;
  if not fUpdateCheckInformation.CheckSuccessful or not fUpdateCheckInformation.UpdateAvailable then
  begin
    {$IfDef Logging}Logger.Add('Update download cancelled, CheckSuccessful='+BoolToStr(fUpdateCheckInformation.CheckSuccessful, true) + '; UpdateAvailable='+BoolToStr(fUpdateCheckInformation.UpdateAvailable, true), LT_WARNING);{$EndIf}
    exit;
  end;

  {$IfDef Logging}Logger.Add('Downloading update ...');{$EndIf}

  fUpdateState := usDownloadRunning;
  if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);

  httpReq := TAsyncHTTPRequest.Create(fUpdateCheckInformation.DownloadLink);
  httpReq.ThreadClass := TBasicThreadWithCallbackWnd;

  {try
    TFileStream.Create(UpdateTmpFilename, fmCreate or fmOpenWrite);
  except
    FreeAndNil(httpReq);
    {$IfDef Logging}Logger.Add('Could not create "' + UpdateTmpFilename + '", update download cancelled.', LT_ERROR);{$EndIf}
    fUpdateState := usDownloadFinished;
    fUpdateDownloadInformation.DownloadSuccessful := false;
    if Assigned(fOnUpdateStatusChanged) then fOnUpdateStatusChanged(Self);
    exit;
  end;  }

  httpReq.OnStatusUpdate := @HTTPRequestStatusUpdate_UpdateDownload;
  httpReq.Get;
end;

procedure TUpdater.InstallUpdate(VerySilent: Boolean);
begin
  {$IfDef Logging}Logger.Add('Installing update (VerySilent='+BoolToStr(VerySilent, true)+'...');{$EndIf}
  HelperSvc.InstallUpdate(VerySilent);
end;

destructor TUpdater.Destroy;
begin
  PowerNotifications.OnResumeFromStandby.Remove(@OnResumeFromStandby);
  PowerNotifications.OnUserPresenceChanged.Remove(@OnUserPresenceChanged);
  Settings.Update.ChangeHandler.Remove(@OnSettingsChanged);
  TryFreeAndNil(UpdateTimer);
  fPowerNotification.Free;
  {$IfDef Debug}
  Dec(InstanceCounter);
  {$EndIf}
  inherited Destroy;
end;

end.

