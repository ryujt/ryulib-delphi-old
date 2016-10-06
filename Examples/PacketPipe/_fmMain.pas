unit _fmMain;

interface

uses
  PacketPipe, SpeedGun,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FSpeedGun : TSpeedGun;
    FPacketPipe : TPacketPipe;
    procedure on_Packet(Sender:TObject; AData:pointer; ASize:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := false;
  FPacketPipe.Clear;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSpeedGun := TSpeedGun.Create;
  FSpeedGun.Start(1000);

  FPacketPipe := TPacketPipe.Create;
  FPacketPipe.Bandwidth := 3000;
  FPacketPipe.OnPacket := on_Packet;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPacketPipe);
//  FreeAndNil(FSpeedGun);
end;

procedure TfmMain.on_Packet(Sender: TObject; AData: pointer; ASize: integer);
begin
  FSpeedGun.IncSize( ASize );

  Caption := IntToStr( FSpeedGun.Speed);

//  Caption := IntToStr( LapTime div 1000 );
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
  FPacketPipe.Add( nil, 8 );
end;

end.
