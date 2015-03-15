unit TaskQueue;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue,
  Windows, SysUtils, Classes, SyncObjs;

type
  {*
    처리해야 할 작업을 큐에 넣고 차례로 실행한다.
    작업의 실행은 내부의 스레드를 이용해서 비동기로 실행한다.
    작업 요청이 다양한 스레드에서 진행되는데, 순차적인 동작이 필요 할 때 사용한다.
  }
  TTaskQueue = class
  private
    FIsTerminated : integer;
    FCS : TCriticalSection;
    FTasks : TDynamicQueue;
    procedure do_Tasks;
    function get_Task:TObject;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  private
    FOnTask: TDataAndTagEvent;
    FOnTerminate: TNotifyEvent;
    FOnError: TStringEvent;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(AData:pointer; ASize:integer); overload;
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;
    procedure Add(ATag:pointer); overload;
  public
    property Count : integer read GetCount;
    property OnTask : TDataAndTagEvent read FOnTask write FOnTask;
    property OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
    property OnError : TStringEvent read FOnError write FOnError;
  end;

implementation

type
  TTask = class
  private
    FData : pointer;
    FSize : integer;
    FTag : pointer;
  public
    constructor Create(AData:pointer; ASize:integer; ATag:pointer); reintroduce;
    destructor Destroy; override;
  end;

{ TTask }

constructor TTask.Create(AData: pointer; ASize: integer; ATag: pointer);
begin
  inherited Create;

  FSize := ASize;

  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  FTag := ATag;
end;

destructor TTask.Destroy;
begin
  if FData <> nil then FreeMem(FData);

  inherited;
end;

{ TTaskQueue }

procedure TTaskQueue.Add(AData: pointer; ASize: integer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create( AData, ASize, nil) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TTaskQueue.Add(AData: pointer; ASize: integer; ATag: pointer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create( AData, ASize, ATag) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TTaskQueue.Add(ATag: pointer);
begin
  FCS.Acquire;
  try
    FTasks.Push( TTask.Create( nil, 0, ATag) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TTaskQueue.Clear;
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

constructor TTaskQueue.Create;
begin
  inherited;

  FIsTerminated := 0;

  FCS := TCriticalSection.Create;
  FTasks := TDynamicQueue.Create(false);

  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

destructor TTaskQueue.Destroy;
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

procedure TTaskQueue.do_Tasks;
var
  Task : TTask;
begin
  Task := Pointer( get_Task );

  while Task <> nil do begin
    try
      if Assigned(FOnTask) then FOnTask(Self, Task.FData, Task.FSize, Task.FTag);
    finally
      Task.Free;
    end;

    Task := Pointer( get_Task );
  end;
end;

function TTaskQueue.GetCount: integer;
begin
  FCS.Acquire;
  try
    Result := FTasks.Count;
  finally
    FCS.Release;
  end;
end;

function TTaskQueue.get_Task: TObject;
begin
  Result := nil;

  FCS.Acquire;
  try
    FTasks.Pop(Pointer(Result));
  finally
    FCS.Release;
  end;
end;

procedure TTaskQueue.on_Repeat(Sender: TObject);
var
  SimpleThread : TSimpleThread absolute Sender;
begin
  while not SimpleThread.Terminated do begin
    try
      do_Tasks;
    except
      on E : Exception do
        if Assigned(FOnError) then FOnError(Self, 'TTaskQueue.on_Repeat - ' + E.Message)
        else raise Exception.Create('TTaskQueue.on_Repeat - ' + E.Message);
    end;

    SimpleThread.SleepTight;
  end;

  Clear;

  if InterlockedExchange( FIsTerminated, 1 ) = 0 then begin
    if Assigned(FOnTerminate) then FOnTerminate(Self);
  end;
end;

end.
