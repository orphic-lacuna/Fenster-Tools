unit AdminCheck;

{$mode objfpc}{$H+}

interface

uses Windows, Sysutils;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  DOMAIN_ALIAS_RID_USERS      = $00000221;
  DOMAIN_ALIAS_RID_GUESTS     = $00000222;
  DOMAIN_ALIAS_RID_POWER_USERS= $00000223;

function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall; external advapi32;

type TOKEN_ELEVATION_TYPE = (TokenElevationTypeDefault = 1, TokenElevationTypeFull, TokenElevationTypeLimited);
type PTOKEN_ELEVATION_TYPE = ^TOKEN_ELEVATION_TYPE;
type _TOKEN_INFORMATION_CLASS = (
  TokenUser = 1,
  TokenGroups,
  TokenPrivileges,
  TokenOwner,
  TokenPrimaryGroup,
  TokenDefaultDacl,
  TokenSource,
  TokenType,
  TokenImpersonationLevel,
  TokenStatistics,
  TokenRestrictedSids,
  TokenSessionId,
  TokenGroupsAndPrivileges,
  TokenSessionReference,
  TokenSandBoxInert,
  TokenAuditPolicy,
  TokenOrigin,
  TokenElevationType,
  TokenLinkedToken,
  TokenElevation,
  TokenHasRestrictions,
  TokenAccessInformation,
  TokenVirtualizationAllowed,
  TokenVirtualizationEnabled,
  TokenIntegrityLevel,
  TokenUIAccess,
  TokenMandatoryPolicy,
  TokenLogonSid,
  TokenIsAppContainer,
  TokenCapabilities,
  TokenAppContainerSid,
  TokenAppContainerNumber,
  TokenUserClaimAttributes,
  TokenDeviceClaimAttributes,
  TokenRestrictedUserClaimAttributes,
  TokenRestrictedDeviceClaimAttributes,
  TokenDeviceGroups,
  TokenRestrictedDeviceGroups,
  TokenSecurityAttributes,
  TokenIsRestricted,
  MaxTokenInfoClass);

type TTokenInformationClass = _TOKEN_INFORMATION_CLASS;
type PTOKEN_INFORMATION_CLASS = ^TTokenInformationClass;

const SECURITY_MAX_SID_SIZE = 68;

type WELL_KNOWN_SID_TYPE = (
  WinNullSid                                   = 0,
  WinWorldSid                                  = 1,
  WinLocalSid                                  = 2,
  WinCreatorOwnerSid                           = 3,
  WinCreatorGroupSid                           = 4,
  WinCreatorOwnerServerSid                     = 5,
  WinCreatorGroupServerSid                     = 6,
  WinNtAuthoritySid                            = 7,
  WinDialupSid                                 = 8,
  WinNetworkSid                                = 9,
  WinBatchSid                                  = 10,
  WinInteractiveSid                            = 11,
  WinServiceSid                                = 12,
  WinAnonymousSid                              = 13,
  WinProxySid                                  = 14,
  WinEnterpriseControllersSid                  = 15,
  WinSelfSid                                   = 16,
  WinAuthenticatedUserSid                      = 17,
  WinRestrictedCodeSid                         = 18,
  WinTerminalServerSid                         = 19,
  WinRemoteLogonIdSid                          = 20,
  WinLogonIdsSid                               = 21,
  WinLocalSystemSid                            = 22,
  WinLocalServiceSid                           = 23,
  WinNetworkServiceSid                         = 24,
  WinBuiltinDomainSid                          = 25,
  WinBuiltinAdministratorsSid                  = 26,
  WinBuiltinUsersSid                           = 27,
  WinBuiltinGuestsSid                          = 28,
  WinBuiltinPowerUsersSid                      = 29,
  WinBuiltinAccountOperatorsSid                = 30,
  WinBuiltinSystemOperatorsSid                 = 31,
  WinBuiltinPrintOperatorsSid                  = 32,
  WinBuiltinBackupOperatorsSid                 = 33,
  WinBuiltinReplicatorSid                      = 34,
  WinBuiltinPreWindows2000CompatibleAccessSid  = 35,
  WinBuiltinRemoteDesktopUsersSid              = 36,
  WinBuiltinNetworkConfigurationOperatorsSid   = 37,
  WinAccountAdministratorSid                   = 38,
  WinAccountGuestSid                           = 39,
  WinAccountKrbtgtSid                          = 40,
  WinAccountDomainAdminsSid                    = 41,
  WinAccountDomainUsersSid                     = 42,
  WinAccountDomainGuestsSid                    = 43,
  WinAccountComputersSid                       = 44,
  WinAccountControllersSid                     = 45,
  WinAccountCertAdminsSid                      = 46,
  WinAccountSchemaAdminsSid                    = 47,
  WinAccountEnterpriseAdminsSid                = 48,
  WinAccountPolicyAdminsSid                    = 49,
  WinAccountRasAndIasServersSid                = 50,
  WinNTLMAuthenticationSid                     = 51,
  WinDigestAuthenticationSid                   = 52,
  WinSChannelAuthenticationSid                 = 53,
  WinThisOrganizationSid                       = 54,
  WinOtherOrganizationSid                      = 55,
  WinBuiltinIncomingForestTrustBuildersSid     = 56,
  WinBuiltinPerfMonitoringUsersSid             = 57,
  WinBuiltinPerfLoggingUsersSid                = 58,
  WinBuiltinAuthorizationAccessSid             = 59,
  WinBuiltinTerminalServerLicenseServersSid    = 60,
  WinBuiltinDCOMUsersSid                       = 61,
  WinBuiltinIUsersSid                          = 62,
  WinIUserSid                                  = 63,
  WinBuiltinCryptoOperatorsSid                 = 64,
  WinUntrustedLabelSid                         = 65,
  WinLowLabelSid                               = 66,
  WinMediumLabelSid                            = 67,
  WinHighLabelSid                              = 68,
  WinSystemLabelSid                            = 69,
  WinWriteRestrictedCodeSid                    = 70,
  WinCreatorOwnerRightsSid                     = 71,
  WinCacheablePrincipalsGroupSid               = 72,
  WinNonCacheablePrincipalsGroupSid            = 73,
  WinEnterpriseReadonlyControllersSid          = 74,
  WinAccountReadonlyControllersSid             = 75,
  WinBuiltinEventLogReadersGroup               = 76,
  WinNewEnterpriseReadonlyControllersSid       = 77,
  WinBuiltinCertSvcDComAccessGroup             = 78,
  WinMediumPlusLabelSid                        = 79,
  WinLocalLogonSid                             = 80,
  WinConsoleLogonSid                           = 81,
  WinThisOrganizationCertificateSid            = 82,
  WinApplicationPackageAuthoritySid            = 83,
  WinBuiltinAnyPackageSid                      = 84,
  WinCapabilityInternetClientSid               = 85,
  WinCapabilityInternetClientServerSid         = 86,
  WinCapabilityPrivateNetworkClientServerSid   = 87,
  WinCapabilityPicturesLibrarySid              = 88,
  WinCapabilityVideosLibrarySid                = 89,
  WinCapabilityMusicLibrarySid                 = 90,
  WinCapabilityDocumentsLibrarySid             = 91,
  WinCapabilitySharedUserCertificatesSid       = 92,
  WinCapabilityEnterpriseAuthenticationSid     = 93,
  WinCapabilityRemovableStorageSid             = 94
);

function GetTokenInformation(TokenHandle: THandle; TokenInformationClass: _TOKEN_INFORMATION_CLASS;
 TokenInformation: Pointer; TokenInformationLength: DWord; var ReturnLength: DWord): BOOL; stdcall; external advapi32;
function CreateWellKnownSid(WellKnownSidType: WELL_KNOWN_SID_TYPE; DomainSid: PSID; pSid: PSID; var cbSid: DWord): Bool; stdcall; external advapi32;

type _TOKEN_ELEVATION = record
    TokenIsElevated: DWord;
  end;
type TOKEN_ELEVATION = _TOKEN_ELEVATION;
type PTOKEN_ELEVATION = ^_TOKEN_ELEVATION;

type TOKEN_MANDATORY_LABEL = record
    Label_: SID_AND_ATTRIBUTES;
  end;
type _TOKEN_MANDATORY_LABEL = TOKEN_MANDATORY_LABEL;
type PTOKEN_MANDATORY_LABEL = ^TOKEN_MANDATORY_LABEL;

const SECURITY_MANDATORY_UNTRUSTED_RID = $00000000;
const SECURITY_MANDATORY_LOW_RID = $00001000;
const SECURITY_MANDATORY_MEDIUM_RID =$00002000;
const SECURITY_MANDATORY_HIGH_RID =$00003000;
const SECURITY_MANDATORY_SYSTEM_RID = $00004000;
const SECURITY_MANDATORY_PROTECTED_PROCESS_RID = $00005000;

function IsRunAsAdmin: Boolean;
function IsUserInAdminGroup: Boolean;
function IsProcessElevated: Boolean;
//function GetProcessIntegrityLevel: String;

implementation

uses ToolBox;

function IsRunAsAdmin: Boolean; // checks whether the executing user is in admin-group (and the process is elevated (on Vista or later))
var
  pAdministratorsGroup: PSID;
  NtAuthority: SID_IDENTIFIER_AUTHORITY;
  fIsRunAsAdmin: LongBool;
begin
  pAdministratorsGroup := nil; fIsRunAsAdmin := false;
  NtAuthority := SECURITY_NT_AUTHORITY;
  if AllocateAndInitializeSid(NtAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, pAdministratorsGroup) then
  begin
    if CheckTokenMembership(0, pAdministratorsGroup, fIsRunAsAdmin) then Result := fIsRunAsAdmin;
  end;
  if pAdministratorsGroup <> nil then
  begin
    FreeSid(pAdministratorsGroup);
    pAdministratorsGroup := nil;
  end;
end;

function IsProcessElevated: Boolean; // checs whether the process is elevated
var
  dwSize: DWord;
  hToken: HANDLE;
  elevation: TOKEN_ELEVATION;
begin
  if not WindowsVersion.IsWinVistaOrGreater then
  begin
    Result := IsRunAsAdmin;
    exit;
  end;
  hToken := 0; dwSize := 0;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken) then
  begin
    if GetTokenInformation(hToken, TokenElevation, @elevation, sizeof(elevation), dwSize) then
      Result := (elevation.TokenIsElevated <> 0);
  end;
  if hToken <> 0 then CloseHandle(hToken);
end;

function IsUserInAdminGroup: Boolean; // checks whether the executing user in admin-group (even if the process is currently not elevated)
var
  fInAdminGroup: LongBool;
  cbSize: DWord;
  hToken, hTokenToCheck: HANDLE;
  elevType: TOKEN_ELEVATION_TYPE;
  elevTypeCardinal: Cardinal;
  adminSID: PSID;
begin
  hTokenToCheck := 0; cbSize := 0; hToken := 0; fInAdminGroup := false;
  if (OpenProcessToken(GetCurrentProcess, TOKEN_QUERY or TOKEN_DUPLICATE, hToken)) then
  begin
    // Determine whether system is running Windows Vista or later operating
    // systems (major version >= 6) because they support linked tokens, but
    // previous versions (major version < 6) do not.
    if (WindowsVersion.IsWinVistaOrGreater) then
    begin
        // Running Windows Vista or later (major version >= 6).
        // Determine token type: limited, elevated, or default.
        if not GetTokenInformation(hToken, TokenElevationType, @elevTypeCardinal,
        sizeof(elevTypeCardinal), cbSize) then raise Exception.Create(SysErrorMessage(GetLastError)) else
        elevType := TOKEN_ELEVaTION_TYPE(elevTypeCardinal);

        // If limited, get the linked elevated token for further check.
        if (elevType = TokenElevationTypeLimited) then
        begin
          if not GetTokenInformation(hToken, TokenLinkedToken, @hTokenToCheck,
                sizeof(hTokenToCheck), cbSize) then raise Exception.Create(SysErrorMessage(GetLastError));
        end;
    end;

    // CheckTokenMembership requires an impersonation token. If we just got a
    // linked token, it already is an impersonation token.  If we did not get
    // a linked token, duplicate the original into an impersonation token for
    // CheckTokenMembership.
    if (hTokenToCheck = 0) then
    begin
      if not DuplicateToken(hToken, SecurityIdentification, @hTokenToCheck) then raise Exception.Create(SysErrorMessage(GetLastError));
    end;

    // Create the SID corresponding to the Administrators group.
    cbSize := SECURITY_MAX_SID_SIZE;//sizeof(adminSID2);

    adminSID := GetMemory(SECURITY_MAX_SID_SIZE);

    if not CreateWellKnownSid(WinBuiltinAdministratorsSid, nil, adminSID,
        cbSize) then
          raise Exception.Create(SysErrorMessage(GetLastError));

    // Check if the token to be checked contains admin SID.
    // http://msdn.microsoft.com/en-us/library/aa379596(VS.85).aspx:
    // To determine whether a SID is enabled in a token, that is, whether it
    // has the SE_GROUP_ENABLED attribute, call CheckTokenMembership.
    if CheckTokenMembership(hTokenToCheck, adminSID, fInAdminGroup) then
      Result := fInAdminGroup
    else raise Exception.Create(SysErrorMessage(GetLastError));
  end else raise Exception.Create(SysErrorMessage(GetLastError));

  // Centralized cleanup for all allocated resources.
  if (hToken <> 0) then CloseHandle(hToken);
  if (hTokenToCheck <> 0) then CloseHandle(hTokenToCheck);
  FreeMemory(adminSID);
end;
           {
//
//   FUNCTION: GetProcessIntegrityLevel()
//
//   PURPOSE: The function gets the integrity level of the current process.
//   Integrity level is only available on Windows Vista and newer operating
//   systems, thus GetProcessIntegrityLevel throws a C++ exception if it is
//   called on systems prior to Windows Vista.
//
//   RETURN VALUE: Returns the integrity level of the current process. It is
//   usually one of these values:
//
//     SECURITY_MANDATORY_UNTRUSTED_RID (SID: S-1-16-0x0)
//     Means untrusted level. It is used by processes started by the
//     Anonymous group. Blocks most write access.
//
//     SECURITY_MANDATORY_LOW_RID (SID: S-1-16-0x1000)
//     Means low integrity level. It is used by Protected Mode Internet
//     Explorer. Blocks write acess to most objects (such as files and
//     registry keys) on the system.
//
//     SECURITY_MANDATORY_MEDIUM_RID (SID: S-1-16-0x2000)
//     Means medium integrity level. It is used by normal applications
//     being launched while UAC is enabled.
//
//     SECURITY_MANDATORY_HIGH_RID (SID: S-1-16-0x3000)
//     Means high integrity level. It is used by administrative applications
//     launched through elevation when UAC is enabled, or normal
//     applications if UAC is disabled and the user is an administrator.
//
//     SECURITY_MANDATORY_SYSTEM_RID (SID: S-1-16-0x4000)
//     Means system integrity level. It is used by services and other
//     system-level applications (such as Wininit, Winlogon, Smss, etc.)
//
//   EXCEPTION: If this function fails, it throws a C++ DWORD exception
//   which contains the Win32 error code of the failure. For example, if
//   GetProcessIntegrityLevel is called on systems prior to Windows Vista,
//   the error code will be ERROR_INVALID_PARAMETER.


function GetProcessIntegrityLevel: String;
var
  dwIntegrityLevel, cbTokenIL: DWord;
  hToken: Cardinal;
  pTokenIL: PTOKEN_MANDATORY_LABEL;
  osver: _OSVERSIONINFOA;
begin
  Result := 'Unknown'; hToken := 0; cbTokenIL := 0;

  osver.dwOSVersionInfoSize := SizeOf(osver);
  if not GetVersionEx(osver) then
    raise Exception.Create(SysErrorMessage(GetLastError));
  if (osver.dwMajorVersion < 6) then exit;

  // Open the primary access token of the process with TOKEN_QUERY.
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken) then
  begin
    // Query the size of the token integrity level information. Note that
    // we expect a FALSE result and the last error ERROR_INSUFFICIENT_BUFFER
    // from GetTokenInformation because we have given it a NULL buffer. On
    // exit cbTokenIL will tell the size of the integrity level information.
    if not GetTokenInformation(hToken, TokenIntegrityLevel, nil, 0, cbTokenIL) and (GetLastError = 122) then
    begin
      // When the process is run on operating systems prior to Windows
      // Vista, GetTokenInformation returns FALSE with the
      // ERROR_INVALID_PARAMETER error code because TokenElevation
      // is not supported on those operating systems.

      // Now we allocate a buffer for the integrity level information.
      pTokenIL := PTOKEN_MANDATORY_LABEL(LocalAlloc(LPTR, cbTokenIL));

      if (pTokenIL = nil) then raise Exception.Create(SysErrorMessage(GetLastError));

      // Retrieve token integrity level information.
      if GetTokenInformation(hToken, TokenIntegrityLevel, pTokenIL,
          cbTokenIL, cbTokenIL) then
        // Integrity Level SIDs are in the form of S-1-16-0xXXXX. (e.g.
        // S-1-16-0x1000 stands for low integrity level SID). There is one and
        // only one subauthority.
        dwIntegrityLevel := GetSidSubAuthority(pTokenIL^.Label_.Sid, 0)^
      else raise Exception.Create(SysErrorMessage(GetLastError));
    end else raise Exception.Create(SysErrorMessage(GetLastError));
  end else raise Exception.Create(SysErrorMessage(GetLastError));

  // Centralized cleanup for all allocated resources.
  if (hToken <> 0) then CloseHandle(hToken);

  if (pTokenIL <> nil) then
  begin
    LocalFree(Cardinal(pTokenIL));
  end;

  if (dwIntegrityLevel < SECURITY_MANDATORY_LOW_RID) then
    Result := 'Untrusted (0/5)'
  else if (dwIntegrityLevel < SECURITY_MANDATORY_MEDIUM_RID) then
    Result := 'Low (1/5)'
  else if (dwIntegrityLevel < SECURITY_MANDATORY_HIGH_RID) then
    Result := 'Medium (2/5)'
  else if (dwIntegrityLevel < SECURITY_MANDATORY_SYSTEM_RID) then
    Result := 'High (3/5)'
  else if (dwIntegrityLevel < SECURITY_MANDATORY_PROTECTED_PROCESS_RID) then
    Result := 'System (4/5)'
  else if (dwIntegrityLevel >= SECURITY_MANDATORY_PROTECTED_PROCESS_RID) then
    Result := 'Protected (5/5)';
end;      }

end.
