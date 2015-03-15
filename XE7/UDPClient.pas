unit UDPClient;

interface

uses
  UDPSocketUtils, PacketBuffer,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle,
  Windows, SysUtils, Classes, ExtCtrls;

type
  TUDPClient = class (TComponent)
  private
    FPacketData : TPacketData;
    FPacketBuffer : TPacketBuffer;
    FIdUDPClient: TIdUDPServer;
    FTimer : TTimer;
    procedure on_Timer(Sender:TObject);
    procedure on_PRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
  private
    FHost: string;
    FPort: integer;
    function GetDataInBuffer: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Send(AData:pointer; ASize:integer); overload;
    procedure SendText(AText:string);
  published
    property Host : string read FHost write FHost;
    property Port : integer read FPort write FPort;
    property DataInBuffer : integer read GetDataInBuffer;
  end;

implementation

{ TUDPClient }

constructor TUDPClient.Create(AOwner: TComponent);
begin
  inherited;

  FPacketData := TPacketData.Create;
  FPacketBuffer := TPacketBuffer.Create;

  FIdUDPClient := TIdUDPServer.Create(Self);
  FIdUDPClient.ThreadedEvent := true;
  FIdUDPClient.OnUDPRead := on_PRead;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 5;
  FTimer.OnTimer := on_Timer;
  FTimer.Enabled := true;
end;

destructor TUDPClient.Destroy;
begin
  FTimer.OnTimer := nil;
  
  FreeAndNil(FTimer);
  FreeAndNil(FIdUDPClient);
  FreeAndNil(FPacketBuffer);
  FreeAndNil(FPacketData);

  inherited;
end;

function TUDPClient.GetDataInBuffer: integer;
begin
  Result := FPacketBuffer.Count;
end;


procedure TUDPClient.on_PRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
begin

end;

procedure TUDPClient.on_Timer(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    if FPacketBuffer.Count = 0 then Exit;
    
    if FPacketData.IsEmpty then begin
      FPacketData.LoadFromPacketBuffer(FPacketBuffer);
      if FPacketData.IsEmpty then Exit;
    end;

    FIdUDPClient.SendBuffer(Host, Port, FPacketData.Data^, FPacketData.Size);
  finally
    FTimer.Enabled := true;
  end;
end;

procedure TUDPClient.Send(AData: pointer; ASize: integer);
begin
  FPacketBuffer.Add(AData, ASize);
end;

procedure TUDPClient.SendText(AText: string);
var
  Data : pointer;
  Size : integer;
  ssData : TStringStream;
begin
  ssData := TStringStream.Create(AText);
  try
    ssData.Position := 0;

    Size := ssData.Size;
    if Size <= 0 then Exit;

    GetMem(Data, Size);
    try
      ssData.Read(Data^, Size);
      FPacketBuffer.Add(Data, Size);
    finally
      FreeMem(Data);
    end;
  finally
    ssData.Free;
  end;
end;

end.
