unit MMTime;

interface

uses
  Generics.Collections,
  MMSystem, Windows,
  Classes, SysUtils;

type
  TMMTimer = class(TComponent)
  private
    FTimeCaps: TTimeCaps;
    FInterval: Cardinal;
    FTimerID : Cardinal;

    FEnabled: Boolean;
    FOnTime: TNotifyEvent;
    FTick: Cardinal;

    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);

    procedure do_Time;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled;
    property Interval:Cardinal read FInterval write SetInterval;
    property Tick:Cardinal read FTick;

    property OnTime:TNotifyEvent read FOnTime write FOnTime;
  end;

  TMMStopWatch = class(TComponent)
  private
    FTimeCaps: TTimeCaps;
    FTimerID : Cardinal;
    FTick: Cardinal;

    FLaps: TList<Cardinal>;

    procedure do_Tick;
    function GetLapCount: integer;
    function GetLaps(AIndex: integer): Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Start;
    function Stop:Cardinal;
    function Lap:Cardinal;
  public
    property LapCount:integer read GetLapCount;
    property Laps[AIndex:integer]:Cardinal read GetLaps;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TMMTimer, TMMStopWatch]);
end;

procedure MMTimerCallback(TimerID: UINT; Msg: UINT; User: DWORD; dw1: DWORD; dw2: DWORD); stdcall;
var
  ATimer: TMMTimer absolute User;
begin
  ATimer.do_Time;
end;

procedure MMStopWatchCallback(TimerID: UINT; Msg: UINT; User: DWORD; dw1: DWORD; dw2: DWORD); stdcall;
var
  AStopWatch: TMMStopWatch absolute User;
begin
  AStopWatch.do_Tick;
end;

{ TStopWatch }

constructor TMMStopWatch.Create(AOwner: TComponent);
begin
  inherited;

  FLaps:= TList<Cardinal>.Create;

  timeGetDevCaps(@FTimeCaps, SizeOf(TTimeCaps));
end;

destructor TMMStopWatch.Destroy;
begin
  Stop;
  FreeAndNil(FLaps);

  inherited;
end;

procedure TMMStopWatch.do_Tick;
begin
  Inc(FTick, FTimeCaps.wPeriodMin);
end;

function TMMStopWatch.GetLapCount: integer;
begin
  Result:= FLaps.Count;
end;

function TMMStopWatch.GetLaps(AIndex: integer): Cardinal;
begin
  Result:= FLaps[AIndex];
end;

function TMMStopWatch.Lap:Cardinal;
begin
  Result:= FTick;
  FLaps.Add(Result);
end;

procedure TMMStopWatch.Start;
begin
  if timeBeginPeriod(FTimeCaps.wPeriodMin) <> TIMERR_NOERROR then RaiseLastOSError;

  FTimerID:= timeSetEvent(
                FTimeCaps.wPeriodMin,
                FTimeCaps.wPeriodMin,
                @MMStopWatchCallback,
                NativeUInt(Self),
                TIME_PERIODIC or TIME_CALLBACK_FUNCTION
  );

  FTick:= 0;

  FLaps.Clear;
end;

function TMMStopWatch.Stop:cardinal;
begin
  timeKillEvent(FTimerID);
  timeEndPeriod(FTimeCaps.wPeriodMin);

  Result:= Lap;
end;

{ TMMTimer }

constructor TMMTimer.Create(AOwner: TComponent);
begin
  inherited;

  timeGetDevCaps(@FTimeCaps, SizeOf(TTimeCaps));
  FInterval:= FTimeCaps.wPeriodMin;
end;

procedure TMMTimer.do_Time;
begin
  Inc(FTick, FInterval);
  if Assigned(FOnTime) then FOnTime(Self);
end;

procedure TMMTimer.SetEnabled(const Value: Boolean);
begin
  if Value then
  begin
    if timeBeginPeriod(FTimeCaps.wPeriodMin) <> TIMERR_NOERROR then RaiseLastOSError;

    FTimerID:= timeSetEvent(
                  FInterval,
                  FInterval,
                  @MMTimerCallback,
                  NativeUInt(Self),
                  TIME_PERIODIC or TIME_CALLBACK_FUNCTION
      );
    FTick:= 0;
  end else
  begin
    timeKillEvent(FTimerID);
    timeEndPeriod(FInterval);
  end;

  FEnabled := Value;
end;

procedure TMMTimer.SetInterval(const Value: Cardinal);
begin
  if FEnabled then raise Exception.Create('Timer running');

  FInterval := Value;
  if FInterval < FTimeCaps.wPeriodMin then FInterval:= FTimeCaps.wPeriodMin
  else if FInterval > FTimeCaps.wPeriodMax then FInterval:= FTimeCaps.wPeriodMax;
end;

end.
