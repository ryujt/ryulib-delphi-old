unit TaskQueue;

interface

uses
  DebugTools, RyuLibBase, SimpleThread, SuspensionQueue,
  SysUtils, Classes;

type
  TTaskEnvet<TTaskType, TDataType> = procedure (ASender:Tobject; ATaskType:TTaskType; AData:TDataType) of object;
  TTimerEvent = procedure (ASender:Tobject; ATick:integer) of object;

  TTaskItem<TTaskType, TDataType> = class
  private
    FTaksType : TTaskType;
    FData : TDataType;
  public
    constructor Create(ATaskType:TTaskType; ADataType:TDataType); reintroduce;
  end;

  {*
    처리해야 할 작업을 큐에 넣고 차례로 실행한다.
    작업의 실행은 내부의 스레드를 이용해서 비동기로 실행한다.
    작업 요청이 다양한 스레드에서 진행되는데, 순차적인 동작이 필요 할 때 사용한다.
  }
  TTaskQueue<TTaskType, TDataType> = class
  private
    FQueue : TSuspensionQueue<TTaskItem<TTaskType, TDataType>>;
  private
    FSimpleThread : TSimpleThread;
    procedure on_FSimpleThread_Execute(ASimpleThread:TSimpleThread);
  private
    FOnTask: TTaskEnvet<TTaskType, TDataType>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ATaskType:TTaskType; ADataType:TDataType);
  public
    property OnTask : TTaskEnvet<TTaskType, TDataType> read FOnTask write FOnTask;
  end;

implementation

{ TItem<TTaskType, TDataType> }

constructor TTaskItem<TTaskType, TDataType>.Create(ATaskType: TTaskType;
  ADataType: TDataType);
begin
  FTaksType := ATaskType;
  FData := ADataType;
end;

{ TTaskQueue<TTaskType, TDataType> }

procedure TTaskQueue<TTaskType, TDataType>.Add(ATaskType: TTaskType;
  ADataType: TDataType);
begin
  FQueue.Push( TTaskItem<TTaskType,TDataType>.Create(ATaskType, ADataType) );
  FSimpleThread.WakeUp;
end;

constructor TTaskQueue<TTaskType, TDataType>.Create;
begin
  inherited;

  FQueue := TSuspensionQueue<TTaskItem<TTaskType, TDataType>>.Create;
  FSimpleThread := TSimpleThread.Create('TTaskQueue', on_FSimpleThread_Execute);
end;

destructor TTaskQueue<TTaskType, TDataType>.Destroy;
begin
  FSimpleThread.TerminateNow;

  FreeAndNil(FQueue);
  FreeAndNil(FSimpleThread);

  inherited;
end;

procedure TTaskQueue<TTaskType, TDataType>.on_FSimpleThread_Execute(
  ASimpleThread: TSimpleThread);
var
  Item : TTaskItem<TTaskType,TDataType>;
begin
  while ASimpleThread.Terminated = false do begin
    Item := FQueue.Pop;
    try
      if Assigned(FOnTask) then FOnTask(Self, Item.FTaksType, Item.FData);
    finally
      Item.Free;
    end;
  end;
end;

end.



