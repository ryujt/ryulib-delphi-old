unit DebugTools;

interface

uses
  RyuLibBase,
  Windows, Classes, SysUtils, SyncObjs;

procedure Trace(const AMsg:string);

procedure SetDebugMsg(AMsg:string);
function GetDebugMsg:string;

implementation

uses
  TaskQueue;

type
  TDebugTool = class
  private
    FTaskQueue : TTaskQueue;
    procedure on_Task(Sender:TObject; AData:pointer; ASize:integer; ATag:pointer);
  public
    constructor Create;
  end;

var
  DebugTool : TDebugTool = nil;

  FCS : TCriticalSection;
  DebugMsg : string = '';

procedure Trace(const AMsg:string);
begin
  if DebugTool = nil then DebugTool := TDebugTool.Create;
  DebugTool.FTaskQueue.Add( TMemory.Create(AMsg) );
end;

procedure SetDebugMsg(AMsg:string);
begin
  FCS.Acquire;
  try
    DebugMsg := AMsg;
  finally
    FCS.Release;
  end;
end;

function GetDebugMsg:string;
begin
  FCS.Acquire;
  try
    Result := DebugMsg;
  finally
    FCS.Release;
  end;
end;


{ TDebugTool }

constructor TDebugTool.Create;
begin
  inherited;

  FTaskQueue := TTaskQueue.Create;
  FTaskQueue.OnTask := on_Task;
end;

procedure TDebugTool.on_Task(Sender: TObject; AData: pointer; ASize: integer;
  ATag: pointer);
var
  Memory : TMemory absolute ATag;
begin
  try
    // DebugView에서 필터링하기 위해서 테그를 붙여 둠
    OutputDebugString( PChar('[Ryu] ' + Memory.ToString) );
  finally
    Memory.Free;
  end;
end;

initialization
  FCS := TCriticalSection.Create;
end.
