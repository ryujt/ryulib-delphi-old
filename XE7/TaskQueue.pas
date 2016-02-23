unit TaskQueue;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue,
  SysUtils, Classes, SyncObjs;

type
  // TODO: TPacketProcessor, TaskQueue, TWorker, TScheduler 차이점 설명 또는 통합

  {*
    처리해야 할 작업을 큐에 넣고 차례로 실행한다.
    작업의 실행은 내부의 스레드를 이용해서 비동기로 실행한다.
    작업 요청이 다양한 스레드에서 진행되는데, 순차적인 동작이 필요 할 때 사용한다.
    요청 된 작업이 요청한 스레드와 별개의 스레드에서 실행되어야 할 때 사용한다.  (비동기 실행)
  }
  TTaskQueue = class
  private
    FCS : TCriticalSection;
    FTasks : TDynamicQueue;
    procedure do_Tasks;
    function get_Task:TObject;
  private
    FSimpleThread : TSimpleThread;
    procedure on_FSimpleThread_Execute(ASimpleThread:TSimpleThread);
  private
    FOnTask: TDataAndTagEvent;
    FOnTerminate: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate;

    procedure Clear;

    procedure Add(AData:pointer; ASize:integer); overload;
    procedure Add(AData:pointer; ASize:integer; ATag:pointer); overload;
    procedure Add(ATag:pointer); overload;
  public
    property OnTask : TDataAndTagEvent read FOnTask write FOnTask;
    property OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
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

  FCS := TCriticalSection.Create;
  FTasks := TDynamicQueue.Create(false);

  FSimpleThread := TSimpleThread.Create('TTaskQueue.Create', on_FSimpleThread_Execute);
end;

destructor TTaskQueue.Destroy;
begin
  Clear;

  FSimpleThread.Terminate;

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

procedure TTaskQueue.on_FSimpleThread_Execute(ASimpleThread:TSimpleThread);
begin
  while not ASimpleThread.Terminated do begin
    do_Tasks;
    ASimpleThread.SleepTight;
  end;

  Clear;

  if Assigned(FOnTerminate) then FOnTerminate(Self);  

//  FreeAndNil(FCS);
//  FreeAndNil(FTasks);
end;

procedure TTaskQueue.Terminate;
begin
  FSimpleThread.Terminate;
end;

end.
