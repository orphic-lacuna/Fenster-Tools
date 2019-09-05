unit HelperSvcDefinitions;

{$mode objfpc}{$H+}

interface

const
  HelperSvcSuffix32 = '32';
  HelperSvcSuffix64 = '64';
  {$ifdef WIN32}
  HelperSvcSuffix = HelperSvcSuffix32;
  {$else}
  HelperSvcSuffix = HelperSvcSuffix64;
  {$endif}
  HelperSvcPipename = 'FT_HELPER_SVC_' + HelperSvcSuffix;
  HelperSvcName = 'FensterToolsHelper' + HelperSvcSuffix;
  HelperSvcPipename32 = 'FT_HELPER_SVC_' + HelperSvcSuffix32;
  HelperSvcName32 = 'FensterToolsHelper' + HelperSvcSuffix32;
  HelperSvcPipename64 = 'FT_HELPER_SVC_' + HelperSvcSuffix64;
  HelperSvcName64 = 'FensterToolsHelper' + HelperSvcSuffix64;
  HelperSvcDisplayName = 'FensterTools ' + HelperSvcSuffix + '-bit Helper Service';
  HelperSvcDescription = 'FensterTools Helper Service provides extended functionality for standard users without administrative privileges. It is needed for smart copy modes and automated updates.';

type
  THelperSvcCommandType = (ctInstallUpdate, ctRestartElevated, ctPatch, ctUnpatch);
  THookFuncType = (hfSetWindowsHookEx, hfGetClipboardData);
  //TUpdateDownloadStatus = (udsUpdateRequested, udsDownloading, udsConnectionError, udsCheckSumError, udsInvalidResponse, udsFailedToStartInstaller, udsCompleted);
  //TUpdateInstallStatus = (uisFileNotFound, uisInstallerStarted);

  THelperSvcCommand = record
    CommandID: DWord;
    DataLength: Integer;
    case Command: THelperSvcCommandType of
      ctInstallUpdate: (
        VerySilent: Boolean;
      );
      ctRestartElevated:();
      ctPatch, ctUnpatch: (
        PID: Cardinal;
        CallbackWnd: Int64;
        CallbackMsg: Cardinal;
        HookFunc: THookFuncType;
      );
  end;
  PHelperSvcCommand = ^THelperSvcCommand;

  THelperSvcInstallUpdateInformation = record
    FilenameInstaller, FilenameCertificate: UnicodeString
  end;

  THelperSvcCommandResponse = record
    CommandID: DWord;
    Result: LongBool;
    case Command: THelperSvcCommandType of
      ctInstallUpdate: (CertificateValid, UpdateInstallerStartedSuccessfully: Boolean);
      //ctInstallUpdate: (UpdateInstallStatus: TUpdateInstallStatus);
      ctRestartElevated: ();
      ctPatch, ctUnpatch: (
        PID: Cardinal;
        CallbackWnd: Int64;
        CallbackMsg: Cardinal;
        HookFunc: THookFuncType;
      );
  end;

implementation

end.

