unit AsyncTasks;

interface

uses
  DebugTools,
  HandleComponent, ThreadPool,
  Windows, Messages, Classes, SysUtils;

type
  TAsyncTaskProcedure = reference to procedure;
  TAsyncTaskCallbackProcedure = reference to procedure(AUserData:pointer);

procedure AsyncTask(AProcedure:TAsyncTaskProcedure; ACallBack:TAsyncTaskCallbackProcedure; AUserData:pointer=nil);

implementation

type
  TAsyncTaskData = class
  private
  public
    AsyncTaskProcedure : TAsyncTaskProcedure;
    CallBack : TAsyncTaskCallbackProcedure;
    UserData : pointer;
  end;

  TAsyncTaskHandler = class (THandleComponent)
  private
    procedure do_WM_Downloaded(var AMsg:TMessage); message WM_USER;
  public
  end;

var
  AsyncTaskHandler : TAsyncTaskHandler = nil;

function InternalThreadFunction(lpThreadParameter: Pointer): Integer; stdcall;
var
  AsyncTaskData : TAsyncTaskData absolute lpThreadParameter;
begin
  Result := 0;
  try
    AsyncTaskData.AsyncTaskProcedure();
    PostMessage(AsyncTaskHandler.Handle, WM_USER, Integer(AsyncTaskData), 0);
  except
    on E : Exception do Trace('AsyncTasks.AsyncTask() - ' + E.Message);
  end;
end;

procedure AsyncTask(AProcedure:TAsyncTaskProcedure; ACallBack:TAsyncTaskCallbackProcedure; AUserData:pointer);
var
  AsyncTaskData : TAsyncTaskData;
begin
  AsyncTaskData := TAsyncTaskData.Create;
  AsyncTaskData.AsyncTaskProcedure := AProcedure;
  AsyncTaskData.CallBack := ACallBack;
  AsyncTaskData.UserData := AUserData;
  QueueIOWorkItem(InternalThreadFunction, Pointer(AsyncTaskData));
end;

{ TAsyncTaskHandler }

procedure TAsyncTaskHandler.do_WM_Downloaded(var AMsg: TMessage);
var
  AsyncTaskData : TAsyncTaskData;
begin
  AsyncTaskData := Pointer(AMsg.WParam);
  try
    AsyncTaskData.CallBack(AsyncTaskData.UserData);
  finally
    AsyncTaskData.Free;
  end;
end;

initialization
  AsyncTaskHandler := TAsyncTaskHandler.Create(nil);
end.
