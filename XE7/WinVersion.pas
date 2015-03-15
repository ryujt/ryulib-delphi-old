unit WinVersion;

interface

uses
  Windows;

type
  TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME,
    wvWin2000, wvWinXP, wvWinVista);

function GetWinVersion:TWinVersion;

implementation

function GetWinVersion:TWinVersion;
var
  osVerInfo: TOSVersionInfo;
  majorVersion, minorVersion: Integer;
begin
  Result := wvUnknown;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    minorVersion := osVerInfo.dwMinorVersion;
    majorVersion := osVerInfo.dwMajorVersion;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
          if majorVersion <= 4 then
            Result := wvWinNT
          else if (majorVersion = 5) and (minorVersion = 0) then
            Result := wvWin2000
          else if (majorVersion = 5) and (minorVersion = 1) then
            Result := wvWinXP
          else if (majorVersion = 6) then
            Result := wvWinVista;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          if (majorVersion = 4) and (minorVersion = 0) then
            Result := wvWin95
          else if (majorVersion = 4) and (minorVersion = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              Result := wvWin98SE
            else
              Result := wvWin98;
          end
          else if (majorVersion = 4) and (minorVersion = 90) then
            Result := wvWinME
          else
            Result := wvUnknown;
        end;
    end;
  end;
end;

end.
