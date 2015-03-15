/// 일정 Bandwidth에 맞춰서 패킷량을 조절 하고 싶을 때
unit PacketPipe;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue,
  Windows, SysUtils, Classes, SyncObjs;

type
  {*
    네트워크 Bandwidth에 맞춰서 패킷을 고른 크기로 전송하고자 할 때 사용한다.
    Add() 메소드에 전달되는 APacket의 포인터 주소만을 내부에 저장한다.
    따라서, 메모리 복사 및 해제등은 일어나지 않는다.
    이점이 PacketProcessor와는 다르다.
  }
  TPacketPipe = class
  private
    FSize : int64;
    FCS : TCriticalSection;
    FQueue : TDynamicQueue;
    function get_Packet(var APacket:pointer):boolean;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  private
    FBandwidth: integer;
    FOnPacket: TDataEvent;
    FOnPacketAndTag: TDataAndTagEvent;
    FOnTerminate: TNotifyEvent;
    function GetCount: integer;
    function GetIsEmpty: boolean;
    function GetSize: int64;
    procedure SetBandwidth(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(APacket:pointer; ASize:integer); overload;
    procedure Add(APacket:pointer; ASize:integer; ATag:pointer); overload;
  public
    property IsEmpty : boolean read GetIsEmpty;
    property Count : integer read GetCount;
    property Size : int64 read GetSize;
    property Bandwidth : integer read FBandwidth write SetBandwidth;
    property OnPacket : TDataEvent read FOnPacket write FOnPacket;
    property OnPacketAndTag : TDataAndTagEvent read FOnPacketAndTag write FOnPacketAndTag;
    property OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

implementation

type
  TPacket = class
  private
  public
    Data : pointer;
    Size : integer;
    Tag : pointer;
    constructor Create(AData:pointer; ASize:integer; ATag:pointer); reintroduce; overload;
  end;

{ TPacket }

constructor TPacket.Create(AData: pointer; ASize: integer; ATag: pointer);
begin
  inherited Create;

  Size := ASize;
  Data := AData;
  Tag  := ATag;
end;

{ TPacketPipe }

procedure TPacketPipe.Add(APacket: pointer; ASize: integer);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(APacket, ASize, nil) );
    FSize := FSize + ASize;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketPipe.Add(APacket: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(APacket, ASize, ATag) );
    FSize := FSize + ASize;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketPipe.Clear;
begin
  FCS.Acquire;
  try
    FQueue.Clear;
    FSize := 0;
  finally
    FCS.Release;
  end;
end;

constructor TPacketPipe.Create;
begin
  inherited;

  FSize := 0;
  FBandwidth := 0;

  FCS := TCriticalSection.Create;
  FQueue := TDynamicQueue.Create(false);
  FSimpleThread := TSimpleThread.Create( on_Repeat );

  SetThreadPriority(FSimpleThread.Handle, THREAD_PRIORITY_HIGHEST);
end;

destructor TPacketPipe.Destroy;
begin
  Clear;

  FSimpleThread.Terminate;
  FSimpleThread.WakeUp;

  inherited;
end;

function TPacketPipe.GetCount: integer;
begin
  FCS.Acquire;
  try
    Result := FQueue.Count;
  finally
    FCS.Release;
  end;
end;

function TPacketPipe.GetIsEmpty: boolean;
begin
  FCS.Acquire;
  try
    Result := FQueue.Count = 0;
  finally
    FCS.Release;
  end;
end;

function TPacketPipe.GetSize: int64;
begin
  FCS.Acquire;
  try
    Result := FSize;
  finally
    FCS.Release;
  end;
end;

function TPacketPipe.get_Packet(var APacket: pointer): boolean;
begin
  Result := false;

  FCS.Acquire;
  try
    if FQueue.Count = 0 then Exit;

    FQueue.Pop( APacket );

    Result := true;
  finally
    FCS.Release;
  end;
end;

procedure TPacketPipe.on_Repeat(Sender: TObject);
const
  // 10 ms 에 해당하는 간격
  INTERVAL = 10 * 1000;
var
  Packet : TPacket;
  Frequency, ByteCount, ByteLimit, Term, OldTick, Tick, TickCount : int64;
  SimpleThread : TSimpleThread absolute Sender;
begin
  TickCount := 0;

  QueryPerformanceFrequency(Frequency);

  QueryPerformanceCounter(OldTick);

  while not SimpleThread.Terminated do begin
    QueryPerformanceCounter(Tick);

    if Tick > OldTick then begin
      Term := 1000000 * (Tick - OldTick) div Frequency;

      // 처리 속도 등의 이유로 Interval이 너무 벌어지지 않도록
      if Term > INTERVAL then Term := INTERVAL;

      TickCount := TickCount + Term;
    end;

    OldTick := Tick;

    ByteLimit := FBandwidth div 100;

    // Bandwidth 제한이 없거나, 시간 간격이 INTERVAL과 같거나 클 때 마다 패킷 전송
    if (FBandwidth = 0) or (TickCount >= INTERVAL) then begin
      TickCount := TickCount - INTERVAL;
      if TickCount < 0 then TickCount := 0;

      ByteCount := 0;

      // 저장 된 패킷 전송
      while get_Packet(Pointer(Packet)) do begin
        if Assigned(FOnPacket) then FOnPacket(Self, Packet.Data, Packet.Size);
        if Assigned(FOnPacketAndTag) then FOnPacketAndTag(Self, Packet.Data, Packet.Size, Packet.Tag);

        ByteCount := ByteCount + Packet.Size;

        if (FBandwidth > 0) and (ByteCount >= ByteLimit) then Break;
      end;
    end;

    SimpleThread.Sleep(1);
  end;

  if Assigned(FOnTerminate) then FOnTerminate(Self);

//  FreeAndNil(FCS);
//  FreeAndNil(FList);
end;

procedure TPacketPipe.SetBandwidth(const Value: integer);
begin
  FCS.Acquire;
  try
    FBandwidth := Value;
  finally
    FCS.Release;
  end;
end;

end.
