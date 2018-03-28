unit PortForward;

interface

uses
  ThreadPool,
  Windows, Messages, Classes, SysUtils;

const
  WM_SetPortForwarding = WM_USER + 1;

function SetPortForwarding(ALocalIP,ADescription:string; AInternalPort:integer; var AExternalPort:integer):integer; overload;
procedure SetPortForwarding(AHandle:THandle; ALocalIP,ADescription:string; AInternalPort,AExternalPort:integer); overload;

implementation

function _SetPortForwarding(ALocalIP:PAnsiChar; ADescription:PAnsiChar; AInternalPort:integer; var AExternalPort:integer):integer;
         cdecl;
         external 'libPortForward.dll';

function SetPortForwarding(ALocalIP,ADescription:string; AInternalPort:integer; var AExternalPort:integer):integer;
begin
  Result := _SetPortForwarding(
    PAnsiChar(AnsiString(ALocalIP)),
    PAnsiChar(AnsiString(ADescription)),
    AInternalPort, AExternalPort
  );
end;

type
  TSetPortForwardingParams = class
  private
    FHandle : THandle;
    FLocalIP : string;
    FDescription : string;
    FInternalPort : integer;
    FExternalPort : integer;
  public
    constructor Create(AHandle:THandle; ALocalIP,ADescription:string; AInternalPort,AExternalPort:integer); reintroduce;
  end;

{ TSetPortForwardingParams }

constructor TSetPortForwardingParams.Create(AHandle:THandle; ALocalIP,ADescription:string; AInternalPort,AExternalPort:integer);
begin
  inherited Create;

  FHandle := AHandle;
  FLocalIP := ALocalIP;
  FDescription := ADescription;
  FInternalPort := AInternalPort;
  FExternalPort := AExternalPort;
end;

function SetPortForwardingThreadFunction(AContext:pointer):integer; stdcall;
var
  iErrorCode : integer;
  SetPortForwardingParams : TSetPortForwardingParams absolute AContext;
begin
  Result := 0;

  try
    try
      with SetPortForwardingParams do begin
        iErrorCode := SetPortForwarding(FLocalIP, FDescription, FInternalPort, FExternalPort);

        PostMessage(FHandle, WM_SetPortForwarding, iErrorCode, FExternalPort);
      end;
    finally
      SetPortForwardingParams.Free;
    end;
  except
  end;
end;

procedure SetPortForwarding(AHandle:THandle; ALocalIP,ADescription:string; AInternalPort,AExternalPort:integer);
begin
  QueueWorkItem(
    SetPortForwardingThreadFunction,
    TSetPortForwardingParams.Create(AHandle, ALocalIP, ADescription, AInternalPort, AExternalPort)
  );
end;

end.
