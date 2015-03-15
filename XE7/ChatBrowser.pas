unit ChatBrowser;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, OleCtrls, SHDocVw, ExtCtrls;

type
  TChatBrowser = class(TWebBrowser)
  private
    FTimer : TTimer;
    procedure on_Timer(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddBody(HTML : string);
    function Body : string;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TChatBrowser]);
end;

constructor TChatBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Navigate('about:blank');

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 5;
  FTimer.OnTimer := on_Timer;
end;

destructor TChatBrowser.Destroy;
begin
  FTimer.Free;

  inherited Destroy;
end;

procedure TChatBrowser.AddBody(HTML : string);
var
  Document : Variant;
begin
  Document := Self.Document;
  Document.Write(HTML);
  FTimer.Enabled := True;
end;

function GetIEScreenHandle(hWnd : Integer) : Integer;
begin
  Result := FindWindowEx(hWnd, 0, 'Shell DocObject View', nil);
  Result := FindWindowEx(Result, 0, 'Internet Explorer_Server', nil);
end;

procedure TChatBrowser.on_Timer(Sender : TObject);
begin
  FTimer.Enabled := False;
  SendMessage(GetIEScreenHandle(Handle), WM_VSCROLL, SB_Bottom, 0);
end;

function TChatBrowser.Body : string;
var
  Document : Variant;
begin
  Document := Self.Document;
  Result := Document.Body.innerHTML;
end;

end.

