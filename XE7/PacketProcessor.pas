unit PacketProcessor;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue,
  Windows, SysUtils, Classes, SyncObjs;

type
  // TODO: TPacketProcessor, TaskQueue, TWorker, TScheduler 차이점 설명 또는 통합
  // TODO: Bandwidth 에 맞춰서 이벤트가 발생하도록 속성 추가

  TPacketProcessor = class
  private
    FSize : int64;
    FCS : TCriticalSection;
    FQueue : TDynamicQueue;
    function get_Packet(var APacket:pointer):boolean;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  private
    FOnData: TDataEvent;
    FOnDataAndTag: TDataAndTagEvent;
    FSeamlessProcessor: boolean;
    FOnTerminate: TNotifyEvent;
    function GetCount: integer;
    function GetIsEmpty: boolean;
    function GetSize: int64;
    procedure SetSeamlessProcessor(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(AData:pointer; ASize:integer); overload;
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;
    procedure Add(AStream:TStream); overload;
    procedure Add(AStream:TStream; ATag:pointer); overload;
  public
    property IsEmpty : boolean read GetIsEmpty;

    /// 데이터가 비어 있어도 1ms 마다 깨어나서 바로 동작 할 수 있도록 준비한다. 조금이라도 더 빠른 응답이 필요한 경우 사용한다.
    property SeamlessProcessor : boolean read FSeamlessProcessor write SetSeamlessProcessor;

    property Count : integer read GetCount;
    property Size : int64 read GetSize;
    property OnData : TDataEvent read FOnData write FOnData;
    property OnDataAndTag : TDataAndTagEvent read FOnDataAndTag write FOnDataAndTag;
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
    constructor Create(AStream:TStream; ATag:pointer); reintroduce; overload;
  end;

{ TPacket }

constructor TPacket.Create(AData: pointer; ASize: integer; ATag: pointer);
begin
  inherited Create;

  Size := ASize;
  if Size <= 0 then begin
    Data := nil;
  end else begin
    GetMem(Data, Size);
    Move(AData^, Data^, Size);
  end;

  Tag := ATag;
end;

constructor TPacket.Create(AStream: TStream; ATag: pointer);
begin
  inherited Create;

  Size := AStream.Size;
  if Size <= 0 then begin
    Data := nil;
  end else begin
    GetMem( Data, Size );
    AStream.Position := 0;
    AStream.Write( Data^, Size );
  end;

  Tag := ATag;
end;

{ TPacketProcessor }

procedure TPacketProcessor.Add(AData: pointer; ASize: integer);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(AData, ASize, nil) );
    FSize := FSize + ASize;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketProcessor.Add(AData: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(AData, ASize, ATag) );
    FSize := FSize + ASize;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketProcessor.Add(AStream: TStream);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(AStream, nil) );
    FSize := FSize + AStream.Size;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketProcessor.Add(AStream: TStream; ATag: pointer);
begin
  FCS.Acquire;
  try
    FQueue.Push( TPacket.Create(AStream, ATag) );
    FSize := FSize + AStream.Size;
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TPacketProcessor.Clear;
var
  Packet : TPacket;
begin
  FCS.Acquire;
  try
    FQueue.SimpleIterate(
      procedure(AItem:pointer) begin
        Packet := Pointer( AItem );
        if Packet.Data <> nil then FreeMem(Packet.Data);
      end
    );

    FQueue.Clear;

    FSize := 0;
  finally
    FCS.Release;
  end;
end;

constructor TPacketProcessor.Create;
begin
  inherited;

  FSize := 0;
  FSeamlessProcessor := false;

  FCS := TCriticalSection.Create;
  FQueue := TDynamicQueue.Create(false);

  FSimpleThread := TSimpleThread.Create( on_Repeat );

  SetThreadPriority(FSimpleThread.Handle, THREAD_PRIORITY_HIGHEST);
end;

destructor TPacketProcessor.Destroy;
begin
  Clear;

  FSimpleThread.Terminate;
  FSimpleThread.WakeUp;

  inherited;
end;

function TPacketProcessor.GetCount: integer;
begin
  FCS.Acquire;
  try
    Result := FQueue.Count;
  finally
    FCS.Release;
  end;
end;

function TPacketProcessor.GetIsEmpty: boolean;
begin
  FCS.Acquire;
  try
    Result := FQueue.Count = 0;
  finally
    FCS.Release;
  end;
end;

function TPacketProcessor.GetSize: int64;
begin
  FCS.Acquire;
  try
    Result := FSize;
  finally
    FCS.Release;
  end;
end;

function TPacketProcessor.get_Packet(var APacket: pointer): boolean;
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

procedure TPacketProcessor.on_Repeat(Sender: TObject);
var
  Packet : TPacket;
  SimpleThread : TSimpleThread absolute Sender;
begin
  while not SimpleThread.Terminated do begin
    while get_Packet(Pointer(Packet)) do begin
      try
        if Assigned(FOnData) then FOnData(Self, Packet.Data, Packet.Size);
        if Assigned(FOnDataAndTag) then FOnDataAndTag(Self, Packet.Data, Packet.Size, Packet.Tag);
      finally
        if Packet.Data <> nil then FreeMem(Packet.Data);
        Packet.Free;
      end;
    end;

    if not SimpleThread.Terminated then begin
      if FSeamlessProcessor then SimpleThread.Sleep(1)
      else SimpleThread.SleepTight;
    end;
  end;

  if Assigned(FOnTerminate) then FOnTerminate(Self);

//  FreeAndNil(FCS);
//  FreeAndNil(FList);
end;

procedure TPacketProcessor.SetSeamlessProcessor(const Value: boolean);
begin
  FSeamlessProcessor := Value;
end;

end.
