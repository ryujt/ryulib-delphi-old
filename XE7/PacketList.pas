unit PacketList;

interface

uses
  DynamicQueue,
  Classes, SysUtils, SyncObjs;

type
  {*
    List of [Pointer + Size] formed data.
    TPacketList is similar to TPacketBuffer.
    TPacketList just hold reference of data in it.
    TPacketBuffer, on the other hand, hold copy of data.
  }
  TPacketList = class
  private
    FCS : TCriticalSection;
    FList : TDynamicQueue;
  private
    FSize: int64;
    FCount: integer;
    function GetIsEmpty: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure CopyFrom(AFrom:TPacketList);
    procedure CopyFromWithoutLocking(AFrom:TPacketList);

    procedure Add(AData:pointer; ASize:integer); overload;
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;
    procedure Add(AData:pointer); overload;
    procedure Add(AData:pointer; ATag:pointer); overload;

    function Get(var AData:pointer; var ASize:integer):boolean; overload;
    function Get(var AData:pointer; var ASize:integer; var ATag:pointer):boolean; overload;
    function Get(var AData:pointer):boolean; overload;
    function Get(var AData, ATag:pointer):boolean; overload;

    property IsEmpty : boolean read GetIsEmpty;
    property Count : integer read FCount;
    property Size : int64 read FSize;
  end;

implementation

type
  TPacket = class
  private
  public
    Data : pointer;
    Size : integer;
    Tag : pointer;
  end;

{ TPacketList }

procedure TPacketList.Add(AData:pointer; ASize:integer);
begin
  Add(AData, ASize, nil);
end;

procedure TPacketList.Add(AData:pointer);
begin
  Add(AData, nil);
end;

procedure TPacketList.Add(AData: pointer; ASize: integer; ATag: pointer);
var
  Packet : TPacket;
begin
  Packet := TPacket.Create;
  Packet.Data := AData;
  Packet.Size := ASize;
  Packet.Tag := ATag;

  FCS.Enter;
  try
    FList.Push(Packet);
    FCount := FCount + 1;
    FSize := FSize + ASize;
  finally
    FCS.Leave;
  end;
end;

procedure TPacketList.Add(AData, ATag: pointer);
var
  Packet : TPacket;
begin
  Packet := TPacket.Create;
  Packet.Data := AData;
  Packet.Tag := ATag;

  // We don't know the size of AData.
  Packet.Size := 0;

  FCS.Enter;
  try
    FList.Push(Packet);
    FCount := FCount + 1;
  finally
    FCS.Leave;
  end;
end;

procedure TPacketList.Clear;
var
  Packet : TPacket;
begin
  FCS.Enter;
  try
    while FList.Pop(Pointer(Packet)) do Packet.Free;

    FCount := 0;
    FSize := 0;
  finally
    FCS.Leave;
  end;
end;

procedure TPacketList.CopyFrom(AFrom: TPacketList);
begin
  FCS.Enter;
  try
    CopyFromWithoutLocking(AFrom);
  finally
    FCS.Leave;
  end;
end;

procedure TPacketList.CopyFromWithoutLocking(AFrom: TPacketList);
var
  Packet : TPacket;
begin
  AFrom.FList.SimpleIterate(
    procedure (AItem:pointer) begin
      Packet := Pointer(AItem);

      FList.Push(Packet);
      FCount := FCount + 1;
      FSize := FSize + Packet.Size;
    end
  );
end;

constructor TPacketList.Create;
begin
  inherited;

  FCount := 0;
  FSize := 0;

  FCS := TCriticalSection.Create;
  FList := TDynamicQueue.Create(false);
end;

destructor TPacketList.Destroy;
begin
  Clear;

  FreeAndNil(FCS);
  FreeAndNil(FList);

  inherited;
end;

function TPacketList.Get(var AData:pointer; var ASize:integer): boolean;
var
  Tag : pointer;
begin
  Result := Get(AData, ASize, Tag);
end;

function TPacketList.Get(var AData:pointer):boolean;
var
  Tag : pointer;
begin
  Result := Get(AData, Tag);
end;

function TPacketList.Get(var AData: pointer; var ASize: integer;
  var ATag: pointer): boolean;
var
  Packet : TPacket;
begin
  AData := nil;
  ASize := 0;
  ATag  := nil;
  Result := false;

  FCS.Enter;
  try
    if not FList.Pop(Pointer(Packet)) then Exit;

    Result := true;

    AData := Packet.Data;
    ASize := Packet.Size;
    ATag  := Packet.Tag;

    Packet.Free;

    FCount := FCount - 1;
    FSize := FSize - ASize;
  finally
    FCS.Leave;
  end;
end;

function TPacketList.Get(var AData, ATag: pointer): boolean;
var
  Packet : TPacket;
begin
  AData := nil;
  ATag  := nil;
  Result := false;

  FCS.Enter;
  try
    if not FList.Pop(Pointer(Packet)) then Exit;

    Result := true;

    AData := Packet.Data;
    ATag  := Packet.Tag;

    FCount := FCount - 1;
    FSize := FSize - Packet.Size;

    Packet.Free;
  finally
    FCS.Leave;
  end;
end;

function TPacketList.GetIsEmpty: boolean;
begin
  Result := FList.IsEmpty;
end;

end.
