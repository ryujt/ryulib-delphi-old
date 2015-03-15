unit UDPSocketUtils;

interface

uses
  PacketBuffer,
  Classes, SysUtils, SyncObjs;
  
type
  TPacketHeader = packed record
    Key : array [0..16-1] of byte;
  end;
  PPacketHeader = ^TPacketHeader;
  
  TPacketData = class
  private
    FData : pointer;
    FSize : integer;
    FCS : TCriticalSection;
    procedure prepareHeader;
  private
    function GetIsEmpty: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromPacketBuffer(APacketBuffer:TPacketBuffer);

    property IsEmpty : boolean read GetIsEmpty;
    property Data : pointer read FData;
    property Size : integer read FSize;
  end;


implementation

{ TPacketData }

procedure TPacketData.Clear;
begin
  FCS.Enter;
  try
    FSize := 0;

    if FData <> nil then begin
      FreeMem(FData);
      FData := nil;
    end;
  finally
    FCS.Leave;
  end;
end;

constructor TPacketData.Create;
begin
  inherited;

  FData := nil;
  FSize := 0;

  FCS := TCriticalSection.Create;
end;

procedure TPacketData.prepareHeader;
var
  Loop: Integer;
  pHeader: PPacketHeader;
begin
  pPacketHeader := Pointer(FData);
  
  Randomize;
  for Loop := Low(pHeader^.Key) to High(pHeader^.Key) do pHeader^.Key[Loop] := Random(256);
end;

destructor TPacketData.Destroy;
begin
  Clear;

  FreeAndNil(FCS);

  inherited;
end;

function TPacketData.GetIsEmpty: boolean;
begin
  FCS.Enter;
  try
    Result := FData = nil;
  finally
    FCS.Leave;
  end;
end;

procedure TPacketData.LoadFromPacketBuffer(APacketBuffer: TPacketBuffer);
var
  pData : PPacketHeader;
  Data : pointer;
  Size : integer;
begin
  FCS.Enter;
  try
    if APacketBuffer.GetPacket(Data, Size) = false then begin
      Clear;
      Exit;
    end;

    try
      GetMem(FData, Size + SizeOf(TPacketData));

      pData := FData;
      Inc(pData);
      Move(Data^, pData^, Size);

      prepareHeader;
    finally
      if Data <> nil then FreeMem(Data);
    end;
  finally
    FCS.Leave;
  end;
end;

end.
