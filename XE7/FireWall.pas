unit FireWall;

interface

uses
  ActiveX, ComObj,
  Classes, SysUtils;

const
  NET_FW_PROFILE2_DOMAIN = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC = 4;

  NET_FW_SCOPE_ALL = 0;
  NET_FW_IP_VERSION_ANY = 2;

  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_ACTION_ALLOW = 1;

procedure AddPortToFirewall(Caption:string; PortNumber:Cardinal);
procedure AddExceptionToFirewall(Caption, Executable: string);

implementation

procedure AddPortToFirewall(Caption:string; PortNumber:Cardinal);
var
  fwMgr,port:OleVariant;
  profile:OleVariant;
begin
  fwMgr := CreateOLEObject('HNetCfg.FwMgr');
  profile := fwMgr.LocalPolicy.CurrentProfile;
  port := CreateOLEObject('HNetCfg.FWOpenPort');
  port.Name := Caption;
  port.Protocol := NET_FW_IP_PROTOCOL_TCP;
  port.Port := PortNumber;
  port.Scope := NET_FW_SCOPE_ALL;
  port.Enabled := true;
  profile.GloballyOpenPorts.Add(port);
end;

procedure AddExceptionToFirewall(Caption, Executable: string);
var
  FirewallMsg: OleVariant;
  Application: OleVariant;
  CurrentProfile: OleVariant;
begin
  CoInitialize(nil);
  try
    FirewallMsg := CreateOLEObject('HNetCfg.FwMgr');
    CurrentProfile := FirewallMsg.LocalPolicy.CurrentProfile;
    Application := CreateOLEObject('HNetCfg.FwAuthorizedApplication');
    Application.ProcessImageFileName := Executable;
    Application.Name := Caption;
    Application.Scope := NET_FW_SCOPE_ALL;
    Application.IpVersion := NET_FW_IP_VERSION_ANY;
    Application.Enabled := True;
    CurrentProfile.AuthorizedApplications.Add(Application);
  finally
    CoUninitialize;
  end;
end;

end.
