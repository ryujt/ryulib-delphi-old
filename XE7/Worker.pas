unit Worker;

interface

uses
  DebugTools, RyuLibBase, SimpleRepeater, PacketBuffer,
  Windows, SysUtils, Classes, SyncObjs;

type
  // TODO: TPacketProcessor, TaskQueue, TWorker, TScheduler 차이점 설명 또는 통합
  // TODO: TaskQueue, TWorker, TScheduler 차이점 설명

  {*
    스레드를 이용해서 데이터를 처리하고자 할 때 사용한다.
    Start, Stop이 처리 될 데이터와 같은 큐에서 처리된다.
    따라서, Start, Stop과 데이터 처리가 하나의 스레드에서 실행되어야 할 경우 유용하다.
  }
  TWorker = class
  private
    FTerminated : boolean;
    FCS : TCriticalSection;

    // TODO: DynamicQueue로 교체
    FWorks : TList;

    function check_Empty:boolean;
    function get_Work(var AWork:pointer):boolean;
    procedure do_Clear;
    procedure do_Start;
    procedure do_Stop;
    procedure do_Work(AData:pointer; ASize:integer; ATag:pointer);
    procedure do_Terminate;
  private
    FSimpleRepeater : TSimpleRepeater;
    procedure on_Repeat(Sender:TObject);
  private
    FStarted : boolean;
    FOnData: TDataAndTagEvent;
    FOnStop: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnClear: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sync(AMethod:TThreadMethod);

    procedure Clear;

    procedure Start;
    procedure Stop;

    /// 처리해야 할 데이터를 추가한다.
    procedure Add(AData:pointer; ASize:integer); overload;

    /// 처리해야 할 데이터를 추가한다.  ATag는 사용자 편의를 위해서 마련 된 것이다.
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;

    procedure Add(ATag:pointer); overload;
  public
    property IsActive : boolean read FStarted;
    property OnClear : TNotifyEvent read FOnClear write FOnClear;
    property OnStart : TNotifyEvent read FOnStart write FOnStart;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
    property OnData : TDataAndTagEvent read FOnData write FOnData;
    property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

implementation

type
  TWorkType = (wtClear, wtStart, wtWork, wtStop);

  TWork = class
  private
    FWorkType : TWorkType;
    FData : pointer;
    FSize : integer;
    FTag : pointer;
  public
    constructor Create(AWorkType:TWorkType); reintroduce; overload;
    constructor Create(AWorkType:TWorkType; AData:pointer; ASize:integer); reintroduce; overload;
    constructor Create(AWorkType:TWorkType; AData:pointer; ASize:integer; ATag:pointer); reintroduce; overload;
    destructor Destroy; override;
  end;

{ TWork }

constructor TWork.Create(AWorkType: TWorkType; AData: pointer; ASize: integer);
begin
  inherited Create;

  FWorkType := AWorkType;

  FSize := ASize;

  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  FTag := nil;
end;

constructor TWork.Create(AWorkType: TWorkType; AData: pointer; ASize: integer;
  ATag: pointer);
begin
  inherited Create;

  FWorkType := AWorkType;

  FSize := ASize;

  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  FTag := ATag;
end;

destructor TWork.Destroy;
begin
  if FData <> nil then FreeMem(FData);

  inherited;
end;

constructor TWork.Create(AWorkType: TWorkType);
begin
  inherited Create;

  FWorkType := AWorkType;
  FData := nil;
  FSize := 0;
  FTag := nil;
end;

{ TWorker }

procedure TWorker.Add(AData: pointer; ASize: integer);
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create(wtWork, AData, ASize) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.Add(AData: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create(wtWork, AData, ASize, ATag) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.Add(ATag: pointer);
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create(wtWork, nil, 0, ATag) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

function TWorker.check_Empty: boolean;
begin
  FCS.Acquire;
  try
    Result := FWorks.Count = 0;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.Clear;
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create( wtClear ) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

constructor TWorker.Create;
begin
  inherited;

  FStarted := false;
  FTerminated := false;

  FCS := TCriticalSection.Create;
  FWorks := TList.Create;

  FSimpleRepeater := TSimpleRepeater.Create( on_Repeat );
end;

destructor TWorker.Destroy;
begin
  do_Terminate;

  inherited;
end;

procedure TWorker.do_Clear;
var
  Loop: Integer;
begin
  FCS.Acquire;
  try
    for Loop := 0 to FWorks.Count-1 do TObject(FWorks[Loop]).Free;

    FWorks.Clear;

    if Assigned(FOnClear) then FOnClear(Self);    
  finally
    FCS.Release;
  end;
end;

procedure TWorker.do_Start;
begin
  FCS.Acquire;
  try
    if (not FTerminated) and Assigned(FOnStart) then FOnStart(Self);
  finally
    FCS.Release;
  end;
end;

procedure TWorker.do_Stop;
var
  Loop: Integer;
begin
  FCS.Acquire;
  try
    for Loop := 0 to FWorks.Count-1 do TObject(FWorks[Loop]).Free;

    FWorks.Clear;

    if (not FTerminated) and Assigned(FOnStop) then FOnStop(Self);
  finally
    FCS.Release;
  end;
end;

procedure TWorker.do_Terminate;
const
  TERMINATE_TIMEOUT = 5000;
var
  Loop: Integer;
begin
  FCS.Acquire;
  try
    FTerminated := true;

    for Loop := 0 to FWorks.Count-1 do TObject(FWorks[Loop]).Free;

    FWorks.Clear;
  finally
    FCS.Release;
  end;

  FSimpleRepeater.TerminateRepeater( TERMINATE_TIMEOUT );
end;

procedure TWorker.do_Work(AData: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    if (not FTerminated) and Assigned(FOnData) then FOnData(Self, AData, ASize, ATag);
  finally
    FCS.Release;
  end;
end;

function TWorker.get_Work(var AWork: pointer): boolean;
begin
  AWork := nil;
  Result := false;

  FCS.Acquire;
  try
    if FWorks.Count = 0 then Exit;

    AWork := Pointer( FWorks[0] );
    FWorks.Delete(0);

    Result := true;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.on_Repeat(Sender: TObject);
var
  Work : TWork;
  SimpleRepeater : TSimpleRepeater absolute Sender;
begin
  FStarted := false;

  while not SimpleRepeater.Terminated do begin
    while get_Work( Pointer(Work) ) do begin
      try
        case Work.FWorkType of
          wtClear: do_Clear;

          wtStart: begin
            if FStarted then do_Stop;
            do_Start;
            FStarted := true;
          end;

          wtWork: begin
            if FStarted then do_Work( Work.FData, Work.FSize, Work.FTag );
          end;

          wtStop: begin
            if FStarted then do_Stop;
            FStarted := false;
          end;
        end;
      finally
        Work.Free;
      end;
    end;

    SimpleRepeater.SleepTight;
  end;

  if FStarted then do_Stop;

  if Assigned(FOnDestroy) then FOnDestroy(Self);

//  FreeAndNil(FCS);
//  FreeAndNil(FWorks);
end;

procedure TWorker.Start;
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create( wtStart ) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.Stop;
begin
  FCS.Acquire;
  try
    if not FTerminated then begin
      FWorks.Add( TWork.Create( wtStop ) );
      FSimpleRepeater.WakeUp;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWorker.Sync(AMethod: TThreadMethod);
begin
  FSimpleRepeater.Sync( AMethod );
end;

end.
