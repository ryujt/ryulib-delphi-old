unit DynamicQueue;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TSimpleIterateProcedure = reference to procedure(AItem:pointer);
  TIterateProcedure = reference to procedure(AItem:pointer; var ANeedStop:boolean);

  {*
    A Queue class that provides dynamic capacity.
    It expands it's room for archiving items when if the Queue coomes to the full status.
  }
  TDynamicQueue = class
  private
    FQueue : TObject;
    function GetCount:integer;
    function GetIsEmpty: boolean;
  public
    constructor Create(ANeedThreadSafe:boolean); reintroduce;
    destructor Destroy; override;

    var Clear : procedure of object;
    var Push : procedure (AItem:pointer) of object;
    var Pop : function (var AItem:pointer):boolean of object;
    var Peek : function :pointer of object;

    var SimpleIterate : procedure (AProcedure:TSimpleIterateProcedure) of object;
    var Iterate : procedure (AProcedure:TIterateProcedure) of object;

    property Count : integer read GetCount;
    property IsEmpty : boolean read GetIsEmpty;
  end;

implementation

type
  PNode = ^TNode;
  TNode = record
    Next : PNode;
    Data : pointer;
  end;

  TDynamicQueueBase = class abstract
  private
    FCS : TCriticalSection;
    FCount : integer;
    FHead, FTail : PNode;
    function GetCount: integer; virtual; abstract;
    function GetIsEmpty: boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual; abstract;
    procedure Push(AItem:pointer); virtual; abstract;
    function Pop(var AItem:pointer):boolean; virtual; abstract;
    function Peek:pointer; virtual; abstract;

    procedure SimpleIterate(AProcedure:TSimpleIterateProcedure); virtual; abstract;
    procedure Iterate(AProcedure:TIterateProcedure); virtual; abstract;
  end;

  TDynamicQueueForSingleThread = class (TDynamicQueueBase)
  private
    function GetCount: integer; override;
    function GetIsEmpty: boolean; override;
  public
    procedure Clear; override;
    procedure Push(AItem:pointer); override;
    function Pop(var AItem:pointer):boolean; override;
    function Peek:pointer; override;
    procedure SimpleIterate(AProcedure:TSimpleIterateProcedure); override;
    procedure Iterate(AProcedure:TIterateProcedure); override;
  end;

  TDynamicQueueForMultiThread = class (TDynamicQueueBase)
  private
    function GetCount: integer; override;
    function GetIsEmpty: boolean; override;
  public
    procedure Clear; override;
    procedure Push(AItem:pointer); override;
    function Pop(var AItem:pointer):boolean; override;
    function Peek:pointer; override;
    procedure SimpleIterate(AProcedure:TSimpleIterateProcedure); override;
    procedure Iterate(AProcedure:TIterateProcedure); override;
  end;

{ TDynamicQueueBase }

constructor TDynamicQueueBase.Create;
begin
  inherited Create;

  FCount := 0;
  FHead := nil;
  FTail := nil;

  FCS := TCriticalSection.Create;
end;

destructor TDynamicQueueBase.Destroy;
begin
  FreeAndNil(FCS);

  inherited;
end;

{ TDynamicQueueForSingleThread }

procedure TDynamicQueueForSingleThread.Clear;
var
  Item: pointer;
begin
  while Pop(Item) do Dispose(Item);
  FCount := 0;
end;

function TDynamicQueueForSingleThread.GetCount: integer;
begin
  Result := FCount;
end;

function TDynamicQueueForSingleThread.GetIsEmpty: boolean;
begin
  Result := FHead = nil;
end;

procedure TDynamicQueueForSingleThread.Iterate(AProcedure: TIterateProcedure);
var
  pItem : PNode;
  NeedStop : boolean;
begin
  pItem := FHead;
  while pItem <> nil do begin
    AProcedure(pItem^.Data, NeedStop);
    if NeedStop then Break;

    pItem := pItem^.Next;
  end;
end;

function TDynamicQueueForSingleThread.Peek: pointer;
begin
  Result := nil;
  if FHead = nil then Exit;

  Result := FHead^.Data;
end;

function TDynamicQueueForSingleThread.Pop(var AItem: pointer): boolean;
var
  pHead : PNode;
begin
  AItem := nil;
  Result := false;

  if FHead = nil then Exit;

  Dec(FCount);

  AItem := FHead^.Data;
  Result := true;

  pHead := FHead;
  FHead := FHead^.Next;
  Dispose(pHead);
end;

procedure TDynamicQueueForSingleThread.Push(AItem: pointer);
var
  NewNode : PNode;
begin
  Inc(FCount);

  New(NewNode);
  NewNode^.Data := AItem;
  NewNode^.Next := nil;

  if FHead = nil then begin
    FHead := NewNode;
    FTail := NewNode;
  end else begin
    FTail^.Next := NewNode;
    FTail := NewNode;
  end;
end;

procedure TDynamicQueueForSingleThread.SimpleIterate(
  AProcedure: TSimpleIterateProcedure);
var
  pItem : PNode;
begin
  pItem := FHead;
  while pItem <> nil do begin
    AProcedure(pItem^.Data);
    pItem := pItem^.Next;
  end;
end;

{ TDynamicQueueForMultiThread }

procedure TDynamicQueueForMultiThread.Clear;
var
  pHead : PNode;
begin
  FCS.Enter;
  try
    while FHead <> nil do begin
      Dispose(FHead^.Data);

      pHead := FHead;
      FHead := FHead^.Next;

      Dispose(pHead);
    end;

    FCount := 0;
  finally
    FCS.Leave;
  end;
end;

function TDynamicQueueForMultiThread.GetCount: integer;
begin
  Result := FCount;
end;

function TDynamicQueueForMultiThread.GetIsEmpty: boolean;
begin
  Result := FHead = nil;
end;

procedure TDynamicQueueForMultiThread.Iterate(AProcedure: TIterateProcedure);
var
  pItem : PNode;
  NeedStop : boolean;
begin
  FCS.Enter;
  try
    pItem := FHead;
    while pItem <> nil do begin
      AProcedure(pItem^.Data, NeedStop);
      if NeedStop then Break;

      pItem := pItem^.Next;
    end;
  finally
    FCS.Leave;
  end;
end;

function TDynamicQueueForMultiThread.Peek: pointer;
begin
  FCS.Enter;
  try
    Result := nil;
    if FHead = nil then Exit;

    Result := FHead^.Data;
  finally
    FCS.Leave;
  end;
end;

function TDynamicQueueForMultiThread.Pop(var AItem: pointer): boolean;
var
  pHead : PNode;
begin
  FCS.Enter;
  try
    AItem := nil;
    Result := false;

    if FHead = nil then Exit;

    Dec(FCount);

    AItem := FHead^.Data;
    Result := true;

    pHead := FHead;
    FHead := FHead^.Next;

    Dispose(pHead);
  finally
    FCS.Leave;
  end;
end;

procedure TDynamicQueueForMultiThread.Push(AItem: pointer);
var
  NewNode : PNode;
begin
  FCS.Enter;
  try
    Inc(FCount);

    New(NewNode);
    NewNode^.Data := AItem;
    NewNode^.Next := nil;

    if FHead = nil then begin
      FHead := NewNode;
      FTail := NewNode;
    end else begin
      FTail^.Next := NewNode;
      FTail := NewNode;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TDynamicQueueForMultiThread.SimpleIterate(
  AProcedure: TSimpleIterateProcedure);
var
  pItem : PNode;
begin
  FCS.Enter;
  try
    pItem := FHead;
    while pItem <> nil do begin
      AProcedure(pItem^.Data);
      pItem := pItem^.Next;
    end;
  finally
    FCS.Leave;
  end;
end;

{ TDynamicQueue }

constructor TDynamicQueue.Create(ANeedThreadSafe: boolean);
begin
  inherited Create;

  if ANeedThreadSafe then FQueue := TDynamicQueueForMultiThread.Create
  else FQueue := TDynamicQueueForSingleThread.Create;

  // For the best performance.
  Clear := TDynamicQueueBase(FQueue).Clear;
  Push := TDynamicQueueBase(FQueue).Push;
  Pop := TDynamicQueueBase(FQueue).Pop;
  Peek := TDynamicQueueBase(FQueue).Peek;
  SimpleIterate := TDynamicQueueBase(FQueue).SimpleIterate;
  Iterate := TDynamicQueueBase(FQueue).Iterate;
end;

destructor TDynamicQueue.Destroy;
begin
  Clear;

  FreeAndNil(FQueue);

  inherited;
end;

function TDynamicQueue.GetCount: integer;
begin
  Result := TDynamicQueueBase(FQueue).GetCount;
end;

function TDynamicQueue.GetIsEmpty: boolean;
begin
  Result := TDynamicQueueBase(FQueue).GetIsEmpty;
end;

end.

