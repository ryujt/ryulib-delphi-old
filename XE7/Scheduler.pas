unit Scheduler;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue, QueryPerformance,
  Windows, SysUtils, Classes, SyncObjs;

const
  DEFAULT_INTERVAL = 10;

type
  {*
    스레드를 이용해서 데이터를 처리하고자 할 때 사용한다.
    Start, Stop이 처리 될 데이터와 같은 큐에서 처리된다.
    따라서, Start, Stop과 데이터 처리가 하나의 스레드에서 실행되어야 할 경우 유용하다.
  }
  TScheduler = class
  private
    FIsTerminated : integer;
    FCS : TCriticalSection;
    FTasks : TDynamicQueue;
    procedure do_Tasks;
    function get_Task:TObject;
  private
    TickCount, OldTick, Tick : Cardinal;
    procedure do_Timer;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  private
    FOnTask: TDataAndTagEvent;
    FOnTerminate: TNotifyEvent;
    FOnTimer: TNotifyEvent;
    FInterval: integer;
    FIsStarted: boolean;
    FOnStop: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnError: TStringEvent;
    procedure SetInterval(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Start;
    procedure Stop;

    procedure Add(AData:pointer; ASize:integer); overload;
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;
    procedure Add(ATag:pointer); overload;
  public
    property IsStarted : boolean read FIsStarted;
    property Interval : integer read FInterval write SetInterval;
  public
    property OnStart : TNotifyEvent read FOnStart write FOnStart;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property OnTask : TDataAndTagEvent read FOnTask write FOnTask;
    property OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
    property OnError : TStringEvent read FOnError write FOnError;
  end;

implementation

type
  TTaskType = (ttData, ttStart, ttStop);

  TTask = class
  private
    FTaskType : TTaskType;
    FData : pointer;
    FSize : integer;
    FTag : pointer;
  public
    constructor Create(ATaskType:TTaskType); reintroduce; overload;
    constructor Create(AData:pointer; ASize:integer; ATag:pointer); reintroduce; overload;
    destructor Destroy; override;
  end;

{ TTask }

constructor TTask.Create(AData: pointer; ASize: integer; ATag: pointer);
begin
  inherited Create;

  FTaskType := ttData;

  FSize := ASize;

  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  FTag := ATag;
end;

constructor TTask.Create(ATaskType: TTaskType);
begin
  inherited Create;

  FTaskType := ATaskType;

  FData := nil;
  FSize := 0;
  FTag := 0;
end;

destructor TTask.Destroy;
begin
  if FData <> nil then FreeMem(FData);

  inherited;
end;

{ TScheduler }

procedure TScheduler.Add(AData: pointer; ASize: integer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create(AData, ASize, nil) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TScheduler.Add(AData: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create(AData, ASize, ATag) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TScheduler.Add(ATag: pointer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create(nil, 0, ATag) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TScheduler.Clear;
var
  Task : TTask;
begin
  FCS.Acquire;
  try
    while FTasks.Pop(Pointer(Task)) do Task.Free;
  finally
    FCS.Release;
  end;
end;

constructor TScheduler.Create;
begin
  inherited;

  FIsTerminated := 0;
  FIsStarted := false;
  FInterval := DEFAULT_INTERVAL;

  FCS := TCriticalSection.Create;
  FTasks := TDynamicQueue.Create(false);

  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

destructor TScheduler.Destroy;
begin
  Clear;

  FSimpleThread.TerminateNow;

  if InterlockedExchange( FIsTerminated, 1 ) = 0 then begin
    if Assigned(FOnTerminate) then FOnTerminate(Self);
  end;

//  FreeAndNil(FCS);
//  FreeAndNil(FTasks);

  inherited;
end;

procedure TScheduler.do_Tasks;
var
  Task : TTask;
begin
  Task := Pointer( get_Task );

  while Task <> nil do begin
    try
      case Task.FTaskType of
        ttData: if Assigned(FOnTask) then FOnTask(Self, Task.FData, Task.FSize, Task.FTag);

        ttStart: begin
          FIsStarted := true;
          if Assigned(FOnStart) then FOnStart(Self);
        end;

        ttStop: begin
          FIsStarted := false;
          if Assigned(FOnStop) then FOnStop(Self);
        end;
      end;
    finally
      Task.Free;
    end;

    Task := Pointer( get_Task );
  end;
end;

procedure TScheduler.do_Timer;
begin
  // Fire OnTimer evnet immediately when FInterval = 0
  if FInterval = 0 then begin
    if Assigned(FOnTimer) then FOnTimer(Self);
    Exit;
  end;

  Tick := GetTick;

  if FIsStarted = false then begin
    OldTick := Tick;
    TickCount := 0;

    // Take a break when this has nothing todo.
    Sleep(1);

    Exit;
  end;

  // OldTick can greater than Tick on every 45 days after OS boot up.
  if Tick > OldTick then begin
    TickCount := TickCount + (Tick-OldTick);
    if TickCount >= FInterval then begin
      TickCount := 0;
      if Assigned(FOnTimer) then FOnTimer(Self);
    end else begin
      // Take a break when this has nothing todo.
      Sleep(1);
    end;
  end;

  OldTick := Tick;
end;

function TScheduler.get_Task: TObject;
begin
  Result := nil;

  FCS.Acquire;
  try
    FTasks.Pop(Pointer(Result));
  finally
    FCS.Release;
  end;
end;

procedure TScheduler.on_Repeat(Sender: TObject);
var
  SimpleThread : TSimpleThread absolute Sender;
begin
  TickCount := 0;

  OldTick := GetTick;

  while not SimpleThread.Terminated do begin
    try
      do_Tasks;
    except
      on E : Exception do
        if Assigned(FOnError) then FOnError(Self, 'TScheduler.on_Repeat - ' + E.Message)
        else raise Exception.Create('TScheduler.on_Repeat - ' + E.Message);
    end;

    try
      do_Timer;
    except
      on E : Exception do
        if Assigned(FOnError) then FOnError(Self, 'TScheduler.on_Repeat - ' + E.Message)
        else raise Exception.Create('TScheduler.on_Repeat - ' + E.Message);
    end;
  end;

  Clear;

  if InterlockedExchange( FIsTerminated, 1 ) = 0 then begin
    if Assigned(FOnTerminate) then FOnTerminate(Self);
  end;
end;

procedure TScheduler.SetInterval(const Value: integer);
begin
  FInterval := Value;
end;

procedure TScheduler.Start;
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create(ttStart) );
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

procedure TScheduler.Stop;
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create(ttStop) );
  finally
    FCS.Release;
  end;

  FSimpleThread.WakeUp;
end;

end.
