unit Pipes;

interface

uses
  SimpleThread,
  Windows, SysUtils, Classes;

const
  BUFFER_SIZE = 1024 * 16;
  WAIT_TIME = 100;

type
  TReceiveTextEvent = procedure (Sender:TObject; const AText:string) of object;

  TPipeReceiver = class
  private
    FHandle: THandle;
    FRecvBuffer : array [0..BUFFER_SIZE-1] of byte;
    FSecurityAttributes : TSecurityAttributes;
    FSecurityDescriptor : TSecurityDescriptor;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Execute(ASimpleThread:TSimpleThread);
  private
    FPipeName: string;
    FOnReceiveText: TReceiveTextEvent;
  public
    constructor Create(APipeName: string); reintroduce;
    destructor Destroy; override;

    property PipeName: string read FPipeName;

    property OnReceiveText : TReceiveTextEvent read FOnReceiveText write FOnReceiveText;
  end;

  TPipeSender = class
  private
    FPipeName: string;
  public
    constructor Create(APipeName: string); reintroduce;

    function SendText(AText:string):string;

    property PipeName: string read FPipeName;
  end;

implementation

{ TPipeReceiver }

constructor TPipeReceiver.Create(APipeName: string);
begin
  inherited Create;

  FPipeName := '\\.\pipe\' + APipeName;

  InitializeSecurityDescriptor(@FSecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@FSecurityDescriptor, true, nil, false);

  FillChar(FSecurityAttributes, SizeOf(FSecurityAttributes), 0);
  FSecurityAttributes.nLength := SizeOf(FSecurityAttributes);
  FSecurityAttributes.lpSecurityDescriptor := @FSecurityDescriptor;
  FSecurityAttributes.bInheritHandle := False;

  FHandle := CreateNamedPipe(
    PChar(FPipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    PIPE_UNLIMITED_INSTANCES,
    BUFFER_SIZE,
    BUFFER_SIZE,
    NMPWAIT_USE_DEFAULT_WAIT,
    @FSecurityAttributes
  );

  if (FHandle = 0) or (FHandle = INVALID_HANDLE_VALUE) then begin
    raise Exception.Create('TPipeReceiver.Create: CreateNamedPipe failed.');
  end else begin
    FSimpleThread := TSimpleThread.Create('TPipeReceiver', on_Execute);
  end;
end;

destructor TPipeReceiver.Destroy;
begin
  if (FHandle <> 0) and (FHandle <> INVALID_HANDLE_VALUE) then begin
    FSimpleThread.Terminate;
  end;

  inherited;
end;

procedure TPipeReceiver.on_Execute(ASimpleThread: TSimpleThread);
var
  ReadBytes : DWord;
  ssData : TStringStream;
begin
  while (not ASimpleThread.Terminated) and (FHandle <> 0) do begin
    if not ConnectNamedPipe(FHandle, nil) then begin
      Sleep(1);
      Continue;
    end;

    ReadFile(FHandle, FRecvBuffer, BUFFER_SIZE, ReadBytes, nil);
    if ReadBytes > 0 then begin
      ssData := TStringStream.Create;
      try
        ssData.Write(FRecvBuffer, ReadBytes);
        ssData.Position := 0;
        if Assigned(FOnReceiveText) then FOnReceiveText(Self, ssData.DataString);
      finally
        ssData.Free;
      end;
    end;

    DisconnectNamedPipe(FHandle);
  end;

  CloseHandle(FHandle);
end;

{ TPipeSender }

constructor TPipeSender.Create(APipeName: string);
begin
  inherited Create;

  FPipeName := '\\.\pipe\' + APipeName;
end;

function TPipeSender.SendText(AText: string):string;
var
  FHandle: THandle;
  ToWriteBytes, WriteBytes : DWord;
  ssData : TStringStream;
  SendBuffer : array [0..BUFFER_SIZE-1] of byte;
begin
  FHandle := CreateFile(PChar(FPipeName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if GetLastError = ERROR_PIPE_BUSY then begin
    if not WaitNamedPipe(PChar(FPipeName), WAIT_TIME) then
      raise Exception.Create('TPipeSender.SendText: Connection failed.');
  end;

  ssData := TStringStream.Create(AText);
  try
    ssData.Position := 0;
    ToWriteBytes := ssData.Size;
    if ToWriteBytes > BUFFER_SIZE then ToWriteBytes := BUFFER_SIZE;
    ssData.Read(SendBuffer, ToWriteBytes);
    WriteFile(FHandle, SendBuffer, ToWriteBytes, WriteBytes, nil);
  finally
    ssData.Free;

    DisconnectNamedPipe(FHandle);
    CloseHandle(FHandle);
  end;
end;

end.
