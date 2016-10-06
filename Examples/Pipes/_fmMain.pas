unit _fmMain;

interface

uses
  Pipes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FServer : TPipeServer;
    FClient : TPipeClient;
    procedure on_ServerPipeMessage(Sender:TObject; Pipe:HPIPE; Stream:TStream);
    procedure on_ClientPipeMessage(Sender:TObject; Pipe:HPIPE; Stream:TStream);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  S : AnsiString;
begin
  S := 'Message from Server.';
  FServer.Write(FServer.Clients[0], S[1], Length(S));
end;

procedure TfmMain.Button2Click(Sender: TObject);
var
  S : AnsiString;
begin
  S := 'Message from Client.';
  FClient.Write(S[1], Length(S));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FServer := TPipeServer.Create(self);
  FServer.PipeName := 'Mediawave_Update';
  FServer.Active := True;
  FServer.OnPipeMessage := on_ServerPipeMessage;

  FClient := TPipeClient.Create(self);
  FClient.PipeName := 'Mediawave_Update';
  FClient.OnPipeMessage := on_ClientPipeMessage;

  if not FClient.Connect then moMsg.Lines.Add('Not connected');
end;

procedure TfmMain.on_ClientPipeMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
var
  S : AnsiString;
begin
  if Stream.Size > 0 then begin
    SetLength(S, Stream.Size);
    Stream.Read(S[1], Length(S));
    moMsg.Lines.Add('Client: ' + S);
  end;
end;

procedure TfmMain.on_ServerPipeMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
var
  S : AnsiString;
begin
  if Stream.Size > 0 then begin
    SetLength(S, Stream.Size);
    Stream.Read(S[1], Length(S));
    moMsg.Lines.Add('Server: ' + S);
  end;
end;

end.
