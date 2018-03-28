unit ServiceCtrl;

interface

uses
  Windows, SysUtils, Classes, WinSvc;

function IsServiceInstalled(AService: string): boolean;
function GetServiceStatus(AService: string): DWord;
function ServiceStart(AService: string): boolean;
function ServiceStop(AService: string): boolean;
function ServiceStopAndWait(AService: string; ATimeOut:integer=0): boolean;

implementation

function IsServiceInstalled(AService: string): boolean;
var
  schm, schs: SC_Handle;
begin
  Result := false;

  schm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (schm > 0) then begin
    schs := OpenService(schm, PChar(AService), SERVICE_START or SERVICE_QUERY_STATUS);
    Result := schs > 0;
    if Result then CloseServiceHandle(schs);

    CloseServiceHandle(schm);
  end;
end;

function GetServiceStatus(AService: string): DWord;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  dwStat: DWord;
begin
  dwStat := 0;

  schm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (schm > 0) then begin
    schs := OpenService(schm, PChar(AService), SERVICE_QUERY_STATUS);
    if (schs > 0) then begin
      if (QueryServiceStatus(schs, ss)) then dwStat := ss.dwCurrentState;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;

  Result := dwStat;
end;

function ServiceStart(AService: string): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  psTemp: PChar;
  dwChkP: DWord;
begin
  ss.dwCurrentState := 1; // originally -1, corrected by Henk Mulder
  schm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (schm > 0) then
  begin
    schs := OpenService(schm, PChar(AService), SERVICE_START or SERVICE_QUERY_STATUS);
    if (schs > 0) then
    begin
      psTemp := nil;
      if (StartService(schs, 0, psTemp)) then
        if (QueryServiceStatus(schs, ss)) then
          while (SERVICE_RUNNING <> ss.dwCurrentState) do
          begin
            dwChkP := ss.dwCheckPoint;
            Sleep(ss.dwWaitHint);
            if (not QueryServiceStatus(schs, ss)) then
              Break;
            if (ss.dwCheckPoint < dwChkP) then
              Break;
          end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  Result := SERVICE_RUNNING = ss.dwCurrentState;
end;

function ServiceStop(AService: string): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  dwChkP: DWord;
begin
  schm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (schm > 0) then
  begin
    schs := OpenService(schm, PChar(AService), SERVICE_STOP or
      SERVICE_QUERY_STATUS);
    if (schs > 0) then
    begin
      if (ControlService(schs, SERVICE_CONTROL_STOP, ss)) then
        if (QueryServiceStatus(schs, ss)) then
          while (SERVICE_STOPPED <> ss.dwCurrentState) do
          begin
            dwChkP := ss.dwCheckPoint;
            Sleep(ss.dwWaitHint);
            if (not QueryServiceStatus(schs, ss)) then
              Break;
            if (ss.dwCheckPoint < dwChkP) then
              Break;
          end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  Result := SERVICE_STOPPED = ss.dwCurrentState;
end;

function ServiceStopAndWait(AService: string; ATimeOut:integer=0): boolean;
var
  OldTick, Tick : Cardinal;
begin
  Result := false;

  OldTick := GetTickCount;

  repeat
    ServiceStop(AService);
    Result := SERVICE_STOPPED = GetServiceStatus(AService);

    Tick := GetTickCount;
    if Tick > OldTick then ATimeOut := ATimeOut - (Tick - OldTick);

    OldTick := Tick;
  until Result or (ATimeOut <= 0);
end;

end.
