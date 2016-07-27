unit TaskQueue;

interface

uses
  RyuLibBase, SimpleThread, SuspensionQueue,
  SysUtils, Classes, SyncObjs;

type
  TTaskEnvet<TTaskType, TDataType> = procedure (ATaskType:TTaskType; ADataType:TDataType) of object;

  TItem<TTaskType, TDataType> = class
  private
    FTaksType : TTaskType;
    FDataType : TDataType;
  public
    constructor Create(ATaskType:TTaskType; ADataType:TDataType); reintroduce;
  end;

  {*
    처리해야 할 작업을 큐에 넣고 차례로 실행한다.
    작업의 실행은 내부의 스레드를 이용해서 비동기로 실행한다.
    작업 요청이 다양한 스레드에서 진행되는데, 순차적인 동작이 필요 할 때 사용한다.
    요청 된 작업이 요청한 스레드와 별개의 스레드에서 실행되어야 할 때 사용한다.  (비동기 실행)
  }
  TTaskQueue<TTaskType, TDataType> = class
  private
    FSuspensionQueue : TSuspensionQueue<TItem<TTaskType, TDataType>>;
  private
    FSimpleThread : TSimpleThread;
    procedure on_FSimpleThread_Execute(ASimpleThread:TSimpleThread);
  private
    FOnTask: TTaskEnvet<TTaskType, TDataType>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(ATaskType:TTaskType; ADataType:TDataType);
  public
    property OnTask : TTaskEnvet<TTaskType, TDataType> read FOnTask write FOnTask;
  end;

implementation

{ TItem<TTaskType, TDataType> }

constructor TItem<TTaskType, TDataType>.Create(ATaskType: TTaskType;
  ADataType: TDataType);
begin
  FTaksType := ATaskType;
  FDataType := ADataType;
end;

{ TTaskQueue<TTaskType, TDataType> }

procedure TTaskQueue<TTaskType, TDataType>.Add(ATaskType: TTaskType;
  ADataType: TDataType);
begin
  FSuspensionQueue.Push( TItem<TTaskType,TDataType>.Create(ATaskType, ADataType) );
end;

procedure TTaskQueue<TTaskType, TDataType>.Clear;
var
  Item : TItem<TTaskType,TDataType>;
begin
  while FSuspensionQueue.IsEmpty = false do begin
    Item := FSuspensionQueue.Pop;
    Item.Free;
  end;
end;

constructor TTaskQueue<TTaskType, TDataType>.Create;
begin
  inherited;

  FSuspensionQueue := TSuspensionQueue<TItem<TTaskType, TDataType>>.Create;

  FSimpleThread := TSimpleThread.Create('TTaskQueue', on_FSimpleThread_Execute);
  FSimpleThread.FreeOnTerminate := false;
end;

destructor TTaskQueue<TTaskType, TDataType>.Destroy;
begin
  Clear;

  FSimpleThread.TerminateNow;

  FreeAndNil(FSuspensionQueue);
  FreeAndNil(FSimpleThread);

  inherited;
end;

procedure TTaskQueue<TTaskType, TDataType>.on_FSimpleThread_Execute(
  ASimpleThread: TSimpleThread);
var
  Item : TItem<TTaskType,TDataType>;
begin
  while ASimpleThread.Terminated = false do begin
    Item := FSuspensionQueue.Pop;
    if Assigned(FOnTask) then FOnTask(Item.FTaksType, Item.FDataType);
  end;
end;

end.
