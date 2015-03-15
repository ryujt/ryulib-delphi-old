unit _fmMain;

interface

uses
  PortForward,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moMsg: TMemo;
    btOpen: TButton;
    btOpenUsingThread: TButton;
    procedure btOpenClick(Sender: TObject);
    procedure btOpenUsingThreadClick(Sender: TObject);
  private
    procedure do_WM_SetPortForwarding(var AMsg:TMessage); message WM_SetPortForwarding;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
var
  iErrorCode, ExternalPort : integer;
begin
  ExternalPort := 13389;

  iErrorCode := SetPortForwarding( '192.168.10.218', 'Ryu-RDP', 3389, ExternalPort );

  moMsg.Lines.Add( Format('iErrorCode=%d, , ExternalPort=%d', [iErrorCode, ExternalPort]) );
end;

procedure TfmMain.btOpenUsingThreadClick(Sender: TObject);
begin
  SetPortForwarding( Handle, '192.168.10.13', 'Ryu', 658, 658 );
end;

procedure TfmMain.do_WM_SetPortForwarding(var AMsg: TMessage);
begin
  moMsg.Lines.Add( Format('iErrorCode=%d, , ExternalPort=%d', [AMsg.WParam, AMsg.LParam]) );
end;

end.
