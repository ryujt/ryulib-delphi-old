unit SuspensionQueue;

interface

uses
  Generics.Collections,
  Windows, SysUtils, Classes;

type
  TSuspensionQueue<T> = class
  private
    FQueue : TQueue<T>;
  private
    function GetCount: integer;
    function GetIsEmpty: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Push(AItem:T);

    /// 데이터가 없을 경우에는 기다린다.  (Blocking)
    function Pop:T;

    /// 데이터가 없을 경우에는 nil을 리턴한다.
    function Get(var Aitem:T):boolean;

    function Peek:T;

    property Count : integer read GetCount;
    property IsEmpty : boolean read GetIsEmpty;
  end;

implementation

{ TSuspensionQueue }

procedure TSuspensionQueue<T>.Clear;
begin
  TMonitor.Enter(Self);
  try
    FQueue.Clear;
  finally
    TMonitor.Exit(Self);
  end;
end;

constructor TSuspensionQueue<T>.Create;
begin
  inherited;

  FQueue := TQueue<T>.Create;
end;

destructor TSuspensionQueue<T>.Destroy;
begin
  Clear;

  FreeAndNil(FQueue);

  inherited;
end;

function TSuspensionQueue<T>.Get(var Aitem: T): boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FQueue.Count > 0;
    if Result then Aitem := FQueue.Dequeue;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TSuspensionQueue<T>.GetCount: integer;
begin
  TMonitor.Enter(Self);
  try
    Result := FQueue.Count;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TSuspensionQueue<T>.GetIsEmpty: boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FQueue.Count = 0;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TSuspensionQueue<T>.Peek: T;
begin
  TMonitor.Enter(Self);
  try
    Result := FQueue.Peek;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TSuspensionQueue<T>.Pop: T;
begin
  TMonitor.Enter(Self);
  try
    while FQueue.Count = 0 do TMonitor.Wait(Self, INFINITE);
    Result := FQueue.Dequeue;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TSuspensionQueue<T>.Push(AItem: T);
begin
  TMonitor.Enter(Self);
  try
    FQueue.Enqueue(AItem);
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
