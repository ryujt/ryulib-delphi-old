unit CreateLowProcess;

interface

uses
  Windows, SysUtils;

/// 권한 상승 중인 프로그램에서 권한이 낮은 상태로 프로그램 실행시키기
procedure CreateLowProc(ACommandLine:WideString);

implementation

type
  TOKEN_MANDATORY_LABEL = record
    Label_ : SID_AND_ATTRIBUTES;
  end;

function ConvertStringSidToSid(StringSid: LPCSTR; var Sid: PSID): BOOL; stdcall;
         external 'Advapi32.dll' name 'ConvertStringSidToSidA';

function ConvertStringSidToSidW(StringSid: PWideChar; var Sid: PSID): BOOL; stdcall;
         external 'Advapi32.dll' name 'ConvertStringSidToSidW';

function SetTokenMediumIntegrity(Token: Cardinal; var Sid: PSID): Boolean;
const
  TokenIntegrityLevelMy = 25;
  MEDIUM_INTEGRITY_SID: PAnsiChar = 'S-1-16-8192';
  SE_GROUP_INTEGRITY = $00000020;
var
  lbl: ^TOKEN_MANDATORY_LABEL;
  dwSize: Cardinal;
begin
  Result := False;

  lbl := nil;

  if (not GetTokenInformation(Token,
    TTokenInformationClass(TokenIntegrityLevelMy), lbl, 0, dwSize)) and
    (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    GetMem(lbl, dwSize);
    try
      if GetTokenInformation(Token,
        TTokenInformationClass(TokenIntegrityLevelMy), lbl, dwSize, dwSize) then
        if ConvertStringSidToSid(MEDIUM_INTEGRITY_SID, Sid) then
        begin
          lbl.Label_.Sid := Sid;
          lbl.Label_.Attributes := SE_GROUP_INTEGRITY;
          Result := SetTokenInformation(Token,
            TTokenInformationClass(TokenIntegrityLevelMy), lbl,
            SizeOf(TOKEN_MANDATORY_LABEL) + GetLengthSid(Sid));
        end;
    finally
      FreeMem(lbl)
    end;
  end;
end;

procedure CreateLowProc(ACommandLine: WideString);
var
  hToken, hNewToken: THandle;
  pIntegritySid: PSID;
  ProcInfo: TProcessInformation;
  STINFO: TStartupInfo;
  TokenIntegrityLevel: TTokenInformationClass;
begin
  FillChar(ProcInfo, SizeOf(ProcInfo), 0);

  FillChar(STINFO, SizeOf(STINFO), 0);
  STINFO.cb := SizeOf(STINFO);

  FillChar(TokenIntegrityLevel, sizeof(TokenIntegrityLevel), 0);

  pIntegritySid := nil;

  try
    if not OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken) then
      raise Exception.Create('CreateLowProcess.pas (CreateLowProc) - OpenProcessToken Error ' + SysErrorMessage(GetLastError));

    if not DuplicateTokenEx(hToken, MAXIMUM_ALLOWED, nil, SecurityImpersonation, TokenPrimary, hNewToken) then
      raise Exception.Create('CreateLowProcess.pas (CreateLowProc) - DuplicateTokenEx Error ' + SysErrorMessage(GetLastError));

    if not SetTokenMediumIntegrity(hNewToken, pIntegritySid) then
      raise Exception.Create('CreateLowProcess.pas (CreateLowProc) - SetTokenMediumIntegrity ' + SysErrorMessage(GetLastError));

    CreateProcessAsUser(hNewToken, nil, PWideChar(ACommandLine), nil, nil, FALSE, 0, nil, nil, STINFO, ProcInfo);
  finally
    LocalFree(integer(pIntegritySid));
    CloseHandle(hNewToken);
    CloseHandle(hToken);
  end;
end;

end.
