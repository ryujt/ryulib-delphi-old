unit ThreadRepeater;

interface

uses
  DebugTools,
  RyuLibBase,
  SimpleThread,
  Windows, Classes, SysUtils, SyncObjs;

type
  TThreadRepeater = class (TComponent)
  private
    FCS : TCriticalSection;
    FMethodHandle : TNotifyEvent;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
    procedure on_Error(Sender:TObject; const AString:string);
  private
    FStackSize: integer;
    FOnError: TStringEvent;
    function GetStoped: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute(AMethodHandle:TNotifyEvent);

    procedure Terminate; overload;
    procedure Terminate(ATimeout:integer); overload;
  published
    property Stoped : boolean read GetStoped;
    property StackSize : integer read FStackSize write FStackSize;

    property OnError : TStringEvent read FOnError write FOnError;
  end;

implementation

{ TThreadRepeater }

constructor TThreadRepeater.Create(AOwner: TComponent);
begin
  inherited;

  IsMultiThread := true;

  FSimpleThread := nil;
  FStackSize := 0;
  FMethodHandle := nil;

  FCS := TCriticalSection.Create;
end;

destructor TThreadRepeater.Destroy;
begin
  Terminate;

  FreeAndNil(FCS);

  inherited;
end;

procedure TThreadRepeater.Execute(AMethodHandle: TNotifyEvent);
begin
  FCS.Enter;
  try
    if FSimpleThread <> nil then
      raise Exception.Create('ThreadRepeater is already Executed.');

    FMethodHandle := AMethodHandle;

    FSimpleThread := TSimpleThread.Create(FStackSize, on_Repeat);
    FSimpleThread.Name := Name;
  finally
    FCS.Leave;
  end;
end;

function TThreadRepeater.GetStoped: boolean;
begin
  Result := FSimpleThread = nil;
end;

procedure TThreadRepeater.on_Error(Sender: TObject; const AString: string);
begin
  if Assigned(FOnError) then FOnError(Self, AString);
end;

procedure TThreadRepeater.on_Repeat(Sender: TObject);
var
  MethodHandle : TNotifyEvent;
  Thread : TSimpleThread absolute Sender;
begin
  Thread.OnError := on_Error;

  try
    while not Thread.Terminated do begin
      MethodHandle := FMethodHandle;
      if not Assigned(MethodHandle) then Break;

      MethodHandle(Self);
    end;
  except
    on E: Exception do begin
      if Assigned(FOnError) then FOnError(Self, E.Message);
      Trace( Format('TThreadRepeater.on_Repeat - %s, %s', [Name, E.Message]) );
    end;
  end;
end;

procedure TThreadRepeater.Terminate(ATimeout: integer);
var
  SimpleThread : TSimpleThread;
begin
  FCS.Enter;
  try
    SimpleThread := FSimpleThread;

    FMethodHandle := nil;
    FSimpleThread := nil;
  finally
    FCS.Leave;
  end;

  if SimpleThread <> nil then SimpleThread.Terminate(ATimeOut);
end;

procedure TThreadRepeater.Terminate;
var
  SimpleThread : TSimpleThread;
begin
  FCS.Enter;
  try
    SimpleThread := FSimpleThread;

    FMethodHandle := nil;
    FSimpleThread := nil;
  finally
    FCS.Leave;
  end;

  if SimpleThread <> nil then SimpleThread.Terminate;
end;

end.

