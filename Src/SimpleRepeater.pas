unit SimpleRepeater;

interface

uses
  DebugTools,
  RyuLibBase, ThreadUtils,
  Windows, Classes, SysUtils;

type
  TSimpleRepeater = class (TThread)
  private
    FEventHandle: THandle;
    FIsRunning : boolean;
    FOnExecute: TNotifyEvent;
  protected
    procedure Execute; override;
  private
    FOnError: TStringEvent;
    FName: string;
  public
    constructor Create(AEventHandle:TNotifyEvent); reintroduce;

    procedure Sleep(ATimeOut:DWord);
    procedure SleepTight;
    procedure WakeUp;

    procedure TerminateRepeater; overload;
    procedure TerminateRepeater(ATimeout:integer); overload;

    procedure Sync(AMethod:TThreadMethod);

    property Terminated;
  public
    property Name : string read FName write FName;
    property OnError : TStringEvent read FOnError write FOnError;
  end;

implementation

{ TSimpleRepeater }

constructor TSimpleRepeater.Create(AEventHandle: TNotifyEvent);
begin
  FreeOnTerminate := true;
  FIsRunning := false;
  FOnExecute := AEventHandle;

  FEventHandle := CreateEvent(nil, false, false, nil);

  inherited Create(false);

  AddThreadObject( Self.Handle );
end;

procedure TSimpleRepeater.Execute;
begin
  FIsRunning := true;

  try
    if Assigned(FOnExecute) then FOnExecute(Self);
  except
    on E: Exception do begin
      if Assigned(FOnError) then FOnError(Self, E.Message);
      Trace( Format('TSimpleRepeater.Execute(%s) - %s', [Name, E.Message]) );
    end;
  end;

  FIsRunning := false;

  RemoveThreadObject( Self.Handle );
end;

procedure TSimpleRepeater.Sleep(ATimeOut: DWord);
begin
  if not Terminated then WaitforSingleObject(FEventHandle, ATimeOut);
end;

procedure TSimpleRepeater.SleepTight;
begin
  if not Terminated then WaitforSingleObject(FEventHandle, INFINITE);
end;

procedure TSimpleRepeater.Sync(AMethod: TThreadMethod);
begin
  Synchronize(AMethod);
end;

procedure TSimpleRepeater.TerminateRepeater;
begin
  Terminate;
  WakeUp;
end;

procedure TSimpleRepeater.TerminateRepeater(ATimeout: integer);
var
  OldTick, Tick : Cardinal;
begin
  Terminate;
  WakeUp;

  OldTick := GetTickCount;

  while (FIsRunning) and (ATimeout > 0) do begin
    Tick := GetTickCount;

    if Tick > OldTick then
      ATimeout := ATimeout - (Tick - OldTick);

    OldTick := Tick;

    if ATimeout > 0 then Sleep(1);
  end;
end;

procedure TSimpleRepeater.WakeUp;
begin
  SetEvent(FEventHandle);
end;

end.
