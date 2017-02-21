unit SimpleThread;

interface

uses
  DebugTools,
  RyuLibBase, ThreadUtils,
  Windows, Classes, SysUtils;

type
  TSimpleThread = class;

  TSimpleThreadProcedure = reference to procedure(ASimpleThread:TSimpleThread);

  TSimpleThreadEvent = procedure (ASimpleThread:TSimpleThread) of object;

  {*
    A Simple thread class that can change stack size for it.
  }
  TSimpleThread = class
  private
    FEventHandle: THandle;
    FIsRunning : boolean;
    procedure init;
  private
    FOnExecute: TSimpleThreadEvent;
    procedure do_Execute;
  private
    FAnonymousProcedure : TSimpleThreadProcedure;
    procedure do_ExecuteAnonymous;
  protected
    procedure BeginSimpleThread(AStackSize: integer); virtual;
    procedure Execute; virtual;
  private
    FName: string;
    FHandle: THandle;
    FThreadID : TThreadID;
    FFreeOnTerminate: Boolean;
    FTerminated : boolean;
    FOnError: TStringEvent;
    FOnTerminated: TNotifyEvent;
  public
    class var StackSize : integer;

    constructor Create; overload;
    constructor Create(AName:string); overload;
    constructor Create(AName:string; AEventHandle:TSimpleThreadEvent); overload;
    constructor Create(AName:string; AProcedure:TSimpleThreadProcedure); overload;
    constructor Create(AName:string; AStackSize:integer); overload;
    constructor Create(AName:string; AStackSize:integer; AEventHandle:TSimpleThreadEvent); overload;

    procedure TerminateNow;

    procedure Terminate; overload;
    procedure Terminate(ATimeout:integer); overload;

    procedure Sleep(ATimeOut:DWORD);
    procedure SleepTight;

    procedure WakeUp;
  public
    property Name : string read FName write FName;
    property Handle : THandle read FHandle;
    property ThreadID : TThreadID read FThreadID;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Terminated : boolean read FTerminated;
  public
    property OnError : TStringEvent read FOnError write FOnError;
    property OnTerminated : TNotifyEvent read FOnTerminated write FOnTerminated;
  end;

implementation


{ TSimpleThread }

function SimpleThreadProc(Parameter : Pointer) : Integer;
begin
  Result := 0;
  try
    TSimpleThread(Parameter).do_Execute;
  finally
    EndThread(0);
    if TSimpleThread(Parameter).FFreeOnTerminate then TSimpleThread(Parameter).Free;
  end;
end;

procedure TSimpleThread.BeginSimpleThread(AStackSize: integer);
begin
  FHandle := BeginThread(nil, AStackSize, @SimpleThreadProc, Pointer(Self), 0, FThreadID);

  AddThreadObject( FHandle );
end;

constructor TSimpleThread.Create(AName:string);
begin
  FName := AName;

  init;
  BeginSimpleThread(StackSize);
end;

constructor TSimpleThread.Create(AName:string; AStackSize: integer);
begin
  FName := AName;

  init;
  BeginSimpleThread(AStackSize);
end;

constructor TSimpleThread.Create(AName:string; AEventHandle: TSimpleThreadEvent);
begin
  FName := AName;

  FOnExecute := AEventHandle;
  init;
  BeginSimpleThread(StackSize);
end;

constructor TSimpleThread.Create(AName:string; AStackSize: integer; AEventHandle: TSimpleThreadEvent);
begin
  FName := AName;

  FOnExecute := AEventHandle;
  init;
  BeginSimpleThread(AStackSize);
end;

constructor TSimpleThread.Create;
begin
  init;
  BeginSimpleThread(StackSize);
end;

function AnonymousThreadProc(Parameter : Pointer) : Integer;
begin
  Result := 0;
  try
    TSimpleThread(Parameter).do_ExecuteAnonymous;
  finally
    EndThread(0);
    if TSimpleThread(Parameter).FFreeOnTerminate then TSimpleThread(Parameter).Free;
  end;
end;

constructor TSimpleThread.Create(AName:string; AProcedure: TSimpleThreadProcedure);
begin
  FName := AName;

  FAnonymousProcedure := AProcedure;

  init;

  FHandle := BeginThread(nil, StackSize, @AnonymousThreadProc, Pointer(Self), 0, FThreadID);
end;

procedure TSimpleThread.do_Execute;
begin
  FIsRunning := true;

  try
    Execute;
  except
    on E: Exception do begin
      if Assigned(FOnError) then FOnError(Self, E.Message);
      Trace( Format('TSimpleThread.do_Execute(%s) - %s', [Name, E.Message]) );
    end;
  end;

  FIsRunning := false;

  RemoveThreadObject( FHandle );

  if Assigned(FOnTerminated) then FOnTerminated(Self);
end;

procedure TSimpleThread.do_ExecuteAnonymous;
begin
  FAnonymousProcedure( Self );
end;

procedure TSimpleThread.Execute;
begin
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

procedure TSimpleThread.init;
begin
  IsMultiThread := true;

  FFreeOnTerminate := true;
  FTerminated := false;
  FIsRunning := false;

  FEventHandle := CreateEvent(nil, false, false, nil);
end;

procedure TSimpleThread.Sleep(ATimeOut: DWORD);
begin
  if not FTerminated then WaitforSingleObject(FEventHandle, ATimeOut);
end;

procedure TSimpleThread.SleepTight;
begin
  if not FTerminated then WaitforSingleObject(FEventHandle, INFINITE);
end;

procedure TSimpleThread.Terminate;
begin
  FTerminated := true;
end;

procedure TSimpleThread.Terminate(ATimeout: integer);
var
  OldTick, Tick : integer;
begin
  FTerminated := true;
  WakeUp;

  OldTick := GetTickCount;

  while (FIsRunning) and (ATimeout > 0) do begin
    Tick := GetTickCount;

    if Tick > OldTick then
      ATimeout := ATimeout - (Tick - OldTick);

    OldTick := Tick;

    if ATimeout > 0 then Sleep(5);
  end;
end;

procedure TSimpleThread.TerminateNow;
begin
  FFreeOnTerminate := false;
  FTerminated := true;

  TerminateThread( FHandle, 0 );

  if FIsRunning and Assigned(FOnTerminated) then FOnTerminated(Self);
end;

procedure TSimpleThread.WakeUp;
begin
  SetEvent(FEventHandle);
end;

initialization
  TSimpleThread.StackSize := 32 * 1024;
end.
