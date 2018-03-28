unit PagePool;

interface

uses
  Queue,
  Classes, SysUtils, SyncObjs;

type
  TPagePool = class
  private
    FPageSize : integer;
    FCapacity : integer;
    FQueue : TQueue;
    FCS : TCriticalSection;
  public
    constructor Create(APageSize:integer; ACapacity:integer); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure GetMem(var AData:pointer);
    procedure FreeMem(var AData:pointer);
  end;

implementation

{ TPagePool }

procedure TPagePool.Clear;
var
  pTemp : pointer;
begin
  FCS.Enter;
  try
    while not FQueue.IsEmpty do begin
      if FQueue.Pop(pTemp) then System.FreeMem(pTemp);
    end;
  finally
    FCS.Leave;
  end;
end;

constructor TPagePool.Create(APageSize:integer; ACapacity:integer);
begin
  inherited Create;

  FPageSize := APageSize;
  FCapacity := ACapacity;
  if FCapacity = 0 then FCapacity := 32;

  FQueue := TQueue.Create(ACapacity);
  FCS := TCriticalSection.Create;
end;

destructor TPagePool.Destroy;
begin
  Clear;

  FreeAndNil(FQueue);
  FreeAndNil(FCS);

  inherited;
end;

procedure TPagePool.FreeMem(var AData: pointer);
begin
  FCS.Enter;
  try
    if FQueue.IsFull then begin
      System.FreeMem(AData);
      Exit;
    end;

    FQueue.Push(AData);
  finally
    FCS.Leave;
  end;
end;

procedure TPagePool.GetMem(var AData: pointer);
begin
  FCS.Enter;
  try
    if FQueue.IsEmpty then begin
      System.GetMem(AData, FPageSize);
      Exit;
    end;

    if not FQueue.Pop(AData) then System.GetMem(AData, FPageSize);
  finally
    FCS.Leave;
  end;
end;

end.
