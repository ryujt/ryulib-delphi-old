unit LatencyBuffer;

interface

uses
  DebugTools, PacketBuffer,
  Classes, SysUtils, SyncObjs;

const
  DEFAULT_DURATION = 500; /// 초기 딜레이 시간 500ms
  DEFAULT_DURATION_LIMIT = 32 * 1000; /// 최대 딜레이 수준 = 32 초

type
  TState = (stNormal, stEmpty, stDelayed);

  {*
    허용 된 지연시간(Duration)을 먼저 채워서 사용하는 버퍼이다.
      - 버퍼가 Duration 크기만큼이 되어야 Get에서 데이터가 리턴된다.
      - 버퍼가 Empty 상태가 되면 Duration은 두 배 크기로 늘어나고, 버퍼가 Duration 크기 만큼 되기 전까지는 Get에서 false를 리턴한다.
  }
  TLatencyBuffer = class
  private
    FRealDuration : integer;
    FCS : TCriticalSection;
    FState : TState;
    procedure set_State(AValue:TState);
    procedure do_Clear;
    procedure do_Add;
    procedure do_Get;
  private
    FPacketBuffer : TPacketBuffer;
  private
    FRealTime: boolean;
    FDelayedTime: integer;
    FDuration: integer;
    FDurationLimit: integer;
    function GetIsDelayed: boolean;
    procedure SetDuration(const Value: integer);
    procedure SetRealTime(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    {*
      입력되는 데이터가 가지는 시간의 크기와, 즉, 재생 시간, 함께 데이터를 입력한다.
      @param AData
      @param ASize
      @param APlayTime 입력되는 데이터의 시간 크기.  시간과 관련없는 데이터는 0으로 입력한다.
    }
    procedure Add(AData:pointer; ASize,APlayTime:integer);

    function Get(var AData:pointer; var ASize:integer):boolean;
  public
    property RealTime : boolean read FRealTime write SetRealTime;  /// 지연없이 실시간으로 처리해야 하는 가?

    property IsDelayed : boolean read GetIsDelayed;
    property Duration : integer read FDuration write SetDuration;  /// 딜레이가 허용되는 버퍼의 시간 크기 (ms 단위)
    property DurationLimit : integer read FDurationLimit write FDurationLimit;  /// 딜레이가 늘어 날 수 있는 최대 시간 크기 (ms 단위)
    property DelayedTime : integer read FDelayedTime;  /// 딜레이 된 시간 = 버퍼 안의 데이터의 시간 크기 총 합 (ms 단위)
  end;

implementation

uses
  TypInfo;

{ TLatencyBuffer }

procedure TLatencyBuffer.Add(AData: pointer; ASize,APlayTime: integer);
begin
  FCS.Acquire;
  try
    FPacketBuffer.Add(AData, ASize, Pointer(APlayTime));
    FDelayedTime := FDelayedTime + APlayTime;
    do_Add;
  finally
    FCS.Release;
  end;
end;

procedure TLatencyBuffer.Clear;
begin
  FCS.Acquire;
  try
    FPacketBuffer.Clear;
    do_Clear;
  finally
    FCS.Release;
  end;
end;

constructor TLatencyBuffer.Create;
begin
  inherited;

  FRealTime := false;

  FDuration := DEFAULT_DURATION;
  FDurationLimit := DEFAULT_DURATION_LIMIT;

  do_Clear;

  FCS := TCriticalSection.Create;
  FPacketBuffer := TPacketBuffer.Create;
end;

destructor TLatencyBuffer.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FPacketBuffer);

  inherited;
end;

procedure TLatencyBuffer.do_Add;
begin
  if FRealTime then Exit;

  case FState of
    // 'FDelayedTime > FDuration' 조건을 사용하면 경계선에서 계속 춤을 추듯이 상태가 변경되면서 음질이 더 나빠질 수 있다.
    stNormal: if FDelayedTime >= (FRealDuration + (FRealDuration div 2)) then set_State(stDelayed);

    stEmpty: if FDelayedTime >= FRealDuration then set_State(stNormal);

    stDelayed: ;
  end;
end;

procedure TLatencyBuffer.do_Clear;
begin
  FRealDuration := FDuration;
  FDelayedTime := 0;

  set_State(stEmpty);
end;

procedure TLatencyBuffer.do_Get;
begin
  if FRealTime then Exit;

  case FState of
    stNormal: if FDelayedTime = 0 then begin
      FRealDuration := FRealDuration * 2;
      if FRealDuration > DEFAULT_DURATION_LIMIT then FRealDuration := DEFAULT_DURATION_LIMIT;

      set_State(stEmpty);
    end;

    stEmpty: ;

    stDelayed: if FDelayedTime <= FRealDuration then set_State(stNormal);
  end;
end;

function TLatencyBuffer.Get(var AData: pointer; var ASize: integer): boolean;
var
  pPlayTime : pointer;
begin
  Result := false;
  AData := nil;
  ASize := 0;

  FCS.Acquire;
  try
    if (FRealTime = false) and (FState = stEmpty) then Exit;

    Result := FPacketBuffer.GetPacket(AData, ASize, pPlayTime);

    if not Result then Exit;

    FDelayedTime := FDelayedTime - Integer(pPlayTime);

    do_Get;
  finally
    FCS.Release;
  end;
end;

function TLatencyBuffer.GetIsDelayed: boolean;
begin
  Result := (FState = stDelayed) and (FRealTime = false);
end;

procedure TLatencyBuffer.SetDuration(const Value: integer);
begin
  FDuration := Value;
  FRealDuration := Value;
end;

procedure TLatencyBuffer.SetRealTime(const Value: boolean);
begin
  FRealTime := Value;
  if Value then Clear;
end;

procedure TLatencyBuffer.set_State(AValue: TState);
begin
  FState := AValue;

  Trace(
    Format(
      'TMainBuffer.set_State - FState: %s, FDelayedTime: %d, FRealDuration: %d, FDuration: %d',
      [GetEnumName(TypeInfo(TState), Integer(FState)), FDelayedTime, FRealDuration, FDuration]
    )
  );
end;

end.
