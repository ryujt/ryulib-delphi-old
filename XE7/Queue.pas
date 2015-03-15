unit Queue;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  {*
    A simple queue class with fixed capacity.
  }
  TQueue = class
  private
    FQueue : TObject;
  public
    constructor Create(ACapacity:integer; ANeedThreadSafe:boolean=true); reintroduce;
    destructor Destroy; override;

    var Clear : procedure of object;
    var Push : function (AItem:pointer):boolean of object;
    var Pop : function (var AItem:pointer):boolean of object;

    var IsEmpty : function:boolean of object;
    var IsFull : function:boolean of object;
    var Count : function:integer of object;
  end;
  
implementation

type
  TQueueBase = class abstract
  private
    FCS : TCriticalSection;
    FList : TList;
    FHead, FTail : integer;
  public
    constructor Create(ACapacity:integer); reintroduce;
    destructor Destroy; override;

    procedure Clear; virtual; abstract;
    function Push(AItem:pointer):boolean; virtual; abstract;
    function Pop(var AItem:pointer):boolean; virtual; abstract;

    function IsEmpty: boolean; virtual; abstract;
    function IsFull: boolean; virtual; abstract;
    function Count: integer; virtual; abstract;
  end;

  TQueueForSingleThread = class (TQueueBase)
  private
  public
    procedure Clear; override;
    function Push(AItem:pointer):boolean; override;
    function Pop(var AItem:pointer):boolean; override;

    function IsEmpty: boolean; override;
    function IsFull: boolean; override;
    function Count: integer; override;
  end;

  TQueueForMultiThread = class (TQueueBase)
  private
  public
    procedure Clear; override;
    function Push(AItem:pointer):boolean; override;
    function Pop(var AItem:pointer):boolean; override;

    function IsEmpty: boolean; override;
    function IsFull: boolean; override;
    function Count: integer; override;
  end;

{ TQueueBase }

constructor TQueueBase.Create(ACapacity: integer);
begin
  inherited Create;

  FHead := 0;
  FTail := 0;

  FCS := TCriticalSection.Create;

  FList := TList.Create;
  FList.Count := ACapacity;
end;

destructor TQueueBase.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FList);

  inherited;
end;

{ TQueueForSingleThread }

procedure TQueueForSingleThread.Clear;
begin
  FHead := 0;
  FTail := 0;
end;

function TQueueForSingleThread.Pop(var AItem: pointer): boolean;
begin
  AItem := nil;
  Result := not IsEmpty;

  // Å¥°¡ ºñ¾úÀ½
  if not Result then Exit;

  AItem := FList[FHead];
  Inc(FHead);
  FHead := FHead mod FList.Count;
end;

function TQueueForSingleThread.Count: integer;
begin
  Result := ((FTail - FHead) + FList.Count) mod FList.Count;
end;

function TQueueForSingleThread.IsEmpty: boolean;
begin
  Result := FHead = FTail;
end;

function TQueueForSingleThread.IsFull: boolean;
begin
  Result := FHead = ((FTail + 1) mod FList.Count);
end;

function TQueueForSingleThread.Push(AItem: pointer): boolean;
begin
  Result := not IsFull;

  // Å¥°¡ ²Ë Ã¡À½
  if not Result then Exit;

  FList[FTail] := AItem;
  Inc(FTail);
  FTail := FTail mod FList.Count;
end;

{ TQueueForMultiThread }

procedure TQueueForMultiThread.Clear;
begin
  FCS.Enter;
  try
    FHead := 0;
    FTail := 0;
  finally
    FCS.Leave;
  end;
end;

function TQueueForMultiThread.Count: integer;
begin
  FCS.Enter;
  try
    Result := ((FTail - FHead) + FList.Count) mod FList.Count;
  finally
    FCS.Leave;
  end;
end;

function TQueueForMultiThread.IsEmpty: boolean;
begin
  FCS.Enter;
  try
    Result := FHead = FTail;
  finally
    FCS.Leave;
  end;
end;

function TQueueForMultiThread.IsFull: boolean;
begin
  FCS.Enter;
  try
    Result := FHead = ((FTail + 1) mod FList.Count);
  finally
    FCS.Leave;
  end;
end;

function TQueueForMultiThread.Pop(var AItem: pointer): boolean;
begin
  FCS.Enter;
  try
    AItem := nil;
    Result := not IsEmpty;

    // Å¥°¡ ºñ¾úÀ½
    if not Result then Exit;

    AItem := FList[FHead];
    Inc(FHead);
    FHead := FHead mod FList.Count;
  finally
    FCS.Leave;
  end;
end;

function TQueueForMultiThread.Push(AItem: pointer): boolean;
begin
  FCS.Enter;
  try
    Result := not IsFull;

    // Å¥°¡ ²Ë Ã¡À½
    if not Result then Exit;

    FList[FTail] := AItem;
    Inc(FTail);
    FTail := FTail mod FList.Count;
  finally
    FCS.Leave;
  end;
end;

{ TQueue }

constructor TQueue.Create(ACapacity: integer; ANeedThreadSafe: boolean);
begin
  inherited Create;

  if ANeedThreadSafe then FQueue := TQueueForMultiThread.Create(ACapacity)
  else FQueue := TQueueForSingleThread.Create(ACapacity);

  // For the best performance.
  Clear := TQueueBase(FQueue).Clear;
  Push := TQueueBase(FQueue).Push;
  Pop := TQueueBase(FQueue).Pop;
  IsEmpty := TQueueBase(FQueue).IsEmpty;
  IsFull := TQueueBase(FQueue).IsFull;
  Count := TQueueBase(FQueue).Count;
end;

destructor TQueue.Destroy;
begin
  Clear;

  FreeAndNil(FQueue);

  inherited;
end;

{$IFDEF DEBUG}
var
  Q : TQueue;
  P : pointer;
{$ENDIF}

initialization
  // Test Codes
  {$IFDEF DEBUG}
  Q := TQueue.Create(16);

  Q.Push(Pointer(1));
  Q.Push(Pointer(2));
  Q.Push(Pointer(3));

  Assert(Q.Pop(P) = true, 'Q.Pop <> true');
  Assert(Integer(P) = 1, 'Q.Pop <> 1');

  Assert(Q.Pop(P) = true, 'Q.Pop <> true');
  Assert(Integer(P) = 2, 'Q.Pop <> 2');

  Assert(Q.Pop(P) = true, 'Q.Pop <> true');
  Assert(Integer(P) = 3, 'Q.Pop <> 3');

  Q.Push(Pointer(1));
  Q.Push(Pointer(2));

  Assert(Q.Pop(P) = true, 'Q.Pop <> true');
  Assert(Integer(P) = 1, 'Q.Pop <> 1');

  Assert(Q.Pop(P) = true, 'Q.Pop <> true');
  Assert(Integer(P) = 2, 'Q.Pop <> 2');
  {$ENDIF}
end.
