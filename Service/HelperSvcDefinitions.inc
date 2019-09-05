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
  HelperSvcDescription = 'xy';

type
  THelperSvcCommandType = (ctDoUpdate, ctRestartElevated, ctPatch, ctUnpatch);
  THookFuncType = (hfSetWindowsHookEx, hfGetClipboardData);
  TUpdateStatus = (udsUpdateRequested, udsDownloading, udsConnectionError, udsCheckSumError, udsInstalling, udsCompleted);

  THelperSvcCommand = record
    CommandID: DWord;
    case Command: THelperSvcCommandType of
      ctDoUpdate:();
      ctRestartElevated:();
      ctPatch, ctUnpatch: (
        PID: Cardinal;
        CallbackWnd: Int64;
        CallbackMsg: Cardinal;
        HookFunc: THookFuncType;
      );
  end;
  THelperSvcCommandResponse = record
    CommandID: DWord;
    Result: LongBool;
    case Command: THelperSvcCommandType of
      ctDoUpdate: (UpdateStatus: TUpdateStatus; BytesReceived, BytesTotal: DWord);
      ctRestartElevated: ();
      ctPatch, ctUnpatch: (
        PID: Cardinal;
        CallbackWnd: Int64;
        CallbackMsg: Cardinal;
        HookFunc: THookFuncType;
      );
  end;
