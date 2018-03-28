unit WebServer;

interface

uses
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebServerModule = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
  end;

procedure StartWebServer(APort:integer);
procedure StopWebServer;

implementation

var
  _WebServer : TIdHTTPWebBrokerBridge;

procedure StartWebServer(APort:integer);
begin
  _WebServer.Bindings.Clear;
  _WebServer.DefaultPort := APort;
  _WebServer.Active := True;
end;

procedure StopWebServer;
begin
  _WebServer.Active := False;
  _WebServer.Bindings.Clear;
end;

{$R *.dfm}

procedure TWebServerModule.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>Web Server Application</body></html>';
end;

initialization
  if WebRequestHandler <> nil then WebRequestHandler.WebModuleClass := TWebServerModule;

  _WebServer := TIdHTTPWebBrokerBridge.Create(nil);

finalization
  // fmMain.Free;
end.
