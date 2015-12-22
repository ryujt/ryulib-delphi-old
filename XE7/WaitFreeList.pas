unit WaitFreeList;

interface

uses
  RyuLibBase, DebugTools, PacketList, SimpleThread,
  Windows, Classes, SysUtils;

type
  IWaitFreeObject = interface
    ['{C9CCDEF3-128D-4649-8D71-78F5ECDC19B0}']

    function IsWaitFreeObjectDeleted:boolean;
    procedure SetWaitFreeObjectDeleted(AVlaue:boolean);

    function CompareWaitFreeObject(AObject:TObject):boolean;

    procedure WaitFreeObjectAdded;
    procedure WaitFreeObjectRemoved;
    procedure WaitFreeObjectDuplicated;

    procedure SetLeftWaitFreeObject(AObject:TObject; AInterface:IWaitFreeObject);
    function GetLeftWaitFreeObject:TObject;
    function GetLeftWaitFreeInterface:IWaitFreeObject;

    procedure SetRightWaitFreeObject(AObject:TObject; AInterface:IWaitFreeObject);
    function GetRightWaitFreeObject:TObject;
    function GetRightWaitFreeInterface:IWaitFreeObject;
  end;

  IWaitFreeSynchronize = interface
    ['{13DD8A6A-5335-41C5-8FEA-BE7D41DC5C3E}']

    procedure Synchronize(AHeader:IWaitFreeObject; AContext:pointer);
  end;

  TWaitFreeObject = class (TInterfaceBase, IWaitFreeObject)
  strict private
    FIsDeleted : boolean;
    function IsWaitFreeObjectDeleted:boolean;
    procedure SetWaitFreeObjectDeleted(AValue:boolean);
  strict private
    FObjectLeft : TObject;
    FObjectRight : TObject;

    FInterfaceLeft : IWaitFreeObject;
    FInterfaceRight : IWaitFreeObject;

    procedure SetLeftWaitFreeObject(AObject:TObject; AInterface:IWaitFreeObject);
    function GetLeftWaitFreeObject:TObject;
    function GetLeftWaitFreeInterface:IWaitFreeObject;

    procedure SetRightWaitFreeObject(AObject:TObject; AInterface:IWaitFreeObject);
    function GetRightWaitFreeObject:TObject;
    function GetRightWaitFreeInterface:IWaitFreeObject;
  protected
    function CompareWaitFreeObject(AObject:TObject):boolean; virtual;
    procedure WaitFreeObjectAdded; virtual;
    procedure WaitFreeObjectRemoved; virtual;
    procedure WaitFreeObjectDuplicated; virtual;
  public
    constructor Create; virtual;
  end;

  TSimpleIterateProcedure = reference to procedure(AWaitFreeObject:IWaitFreeObject);
  TIterateProcedure = reference to procedure(AWaitFreeObject:IWaitFreeObject; var ANeedStop:boolean);

  {*
    TWaitFreeObjectList is not perfect wait-free list.
    TWaitFreeObjectList provides wait-free read(Iterate) function.
  *}
  TWaitFreeObjectList = class
  private
    FAllowDuplicatedObject : boolean;
    FActionThread : TSimpleThread;
    procedure do_Clear;
    procedure do_Add(AObject:IWaitFreeObject);
    procedure do_Remove(AObject:IWaitFreeObject);
    procedure do_Synchronize(AIterator:IWaitFreeSynchronize; AContext:pointer);
    procedure do_Duplicated(AObject:IWaitFreeObject);
    procedure delete_DuplicatedObject(AObject:IWaitFreeObject);
  private
    FHeaderInterface : IWaitFreeObject;
    FCount: integer;
    FName: string;
    function GetIsEmpty: boolean;
    function GetIsBusy: boolean;
    procedure SetName(const Value: string);
  public
    constructor Create(AAllowDuplicatedObject:boolean); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AObject:IWaitFreeObject);
    procedure Remove(AObject:IWaitFreeObject);

    function GetFirstObject:IWaitFreeObject;

    /// 락을 걸지 않고 탐색을 한다. 중간에 멈출 수 없다.
    procedure Iterate(AProcedure:TSimpleIterateProcedure); overload;

    /// 락을 걸지 않고 탐색을 한다.  중간에 멈출 수 있다.
    procedure Iterate(AProcedure:TIterateProcedure); overload;

    {*
      리스트의 변경은 지정된 스레드에서만 진행하여 락없이 처리하고 있다.
      따라서, 동기적인 동작은 해당 스레드에 의존해야 한다.
      Synchronize 메소드를 호출하면 해당 스레드에 의해서,
      IWaitFreeSynchronize.Synchronize()가 콜백으로 실행된다.
    }
    procedure Synchronize(ASynchronize:IWaitFreeSynchronize; AContext:pointer);

    property Name : string read FName write SetName;
    property IsBusy : boolean read GetIsBusy;
    property IsEmpty : boolean read GetIsEmpty;
    property Count : integer read FCount;
  end;

implementation

uses
  TypInfo;

type
  TListAction = (laClear, laAdd, laRemove, laSynchronize);

  TTargetObject = class
  private
  public
    Target : pointer;
    Context : pointer;
    constructor Create(ATarget,AContext:pointer); reintroduce;
  end;

  TActionThread = class (TSimpleThread)
  private
    FList : TWaitFreeObjectList;
    FActionList : TPacketList;
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TWaitFreeObjectList); reintroduce;

    procedure Clear;
    procedure Add(AObject:IWaitFreeObject);
    procedure Remove(AObject:IWaitFreeObject);
    procedure Synchronize(ASynchronize:IWaitFreeSynchronize; AContext:pointer);
  end;

{ TTargetObject }

constructor TTargetObject.Create(ATarget,AContext:pointer);
begin
  Target := ATarget;
  Context := AContext;
end;

{ TActionThread }

procedure TActionThread.Add(AObject: IWaitFreeObject);
begin
  FActionList.Add(Pointer(TTargetObject.Create(Pointer(AObject), nil)), Pointer(laAdd));
  WakeUp;
end;

procedure TActionThread.Clear;
begin
  FActionList.Add(nil, Pointer(laClear));
  WakeUp;
end;

constructor TActionThread.Create(AList: TWaitFreeObjectList);
begin
  Name := 'WaitFreeList.TActionThread';

  FList := AList;
  FActionList := TPacketList.Create;

  FreeOnTerminate := true;

  inherited Create;
end;

procedure TActionThread.Execute;
var
  ListAction : pointer;
  TargetObject : TTargetObject;
begin
  while not Terminated do begin
    try
      while FActionList.Get(Pointer(TargetObject), ListAction) do begin;
        try
          case TListAction(ListAction) of
            laClear:       FList.do_Clear;
            laAdd:         FList.do_Add(IWaitFreeObject(TargetObject.Target));
            laRemove:      FList.do_Remove(IWaitFreeObject(TargetObject.Target));
            laSynchronize: FList.do_Synchronize(IWaitFreeSynchronize(TargetObject.Target), TargetObject.Context);
          end;
        finally
          TargetObject.Free;
        end;
      end;
    except
      on E : Exception do Trace( Format('TActionThread.Execute(%s) - %s', [FList.Name, E.Message]) );
    end;

    SleepTight;
  end;

  FreeAndNil(FActionList);
end;

procedure TActionThread.Synchronize(ASynchronize: IWaitFreeSynchronize; AContext:pointer);
begin
  FActionList.Add(Pointer(TTargetObject.Create(Pointer(ASynchronize), AContext)), Pointer(laSynchronize));
  WakeUp;
end;

procedure TActionThread.Remove(AObject: IWaitFreeObject);
begin
  FActionList.Add(Pointer(TTargetObject.Create(Pointer(AObject), nil)), Pointer(laRemove));
  WakeUp;
end;

{ TWaitFreeObject }

function TWaitFreeObject.CompareWaitFreeObject(AObject: TObject): boolean;
begin
  Result := false;
end;

constructor TWaitFreeObject.Create;
begin
  inherited;

  FIsDeleted := true;

  FObjectLeft     := nil;
  FObjectRight    := nil;
  FInterfaceLeft  := nil;
  FInterfaceRight := nil;
end;

function TWaitFreeObject.GetLeftWaitFreeInterface: IWaitFreeObject;
begin
  Result := FInterfaceLeft;
end;

function TWaitFreeObject.GetLeftWaitFreeObject: TObject;
begin
  Result := FObjectLeft;
end;

function TWaitFreeObject.GetRightWaitFreeInterface: IWaitFreeObject;
begin
  Result := FInterfaceRight;
end;

function TWaitFreeObject.GetRightWaitFreeObject: TObject;
begin
  Result := FObjectRight;
end;

function TWaitFreeObject.IsWaitFreeObjectDeleted: boolean;
begin
  Result := FIsDeleted;
end;

procedure TWaitFreeObject.SetWaitFreeObjectDeleted(AValue: boolean);
begin
  FIsDeleted := AValue;
end;

procedure TWaitFreeObject.SetLeftWaitFreeObject(AObject: TObject;
  AInterface: IWaitFreeObject);
begin
  FObjectLeft    := AObject;
  FInterfaceLeft := AInterface;

  {$IFDEF DEBUG}
//  Trace(Format('TWaitFreeObject.SetLeftWaitFreeObject: Self=%d, Left=%d', [Integer(Self), Integer(AObject)]));
  {$ENDIF}
end;

procedure TWaitFreeObject.SetRightWaitFreeObject(AObject: TObject;
  AInterface: IWaitFreeObject);
begin
  FObjectRight    := AObject;
  FInterfaceRight := AInterface;

  {$IFDEF DEBUG}
//  Trace(Format('TWaitFreeObject.SetRightWaitFreeObject: Self=%d, Right=%d', [Integer(Self), Integer(AObject)]));
  {$ENDIF}
end;

procedure TWaitFreeObject.WaitFreeObjectAdded;
begin
end;

procedure TWaitFreeObject.WaitFreeObjectDuplicated;
begin
end;

procedure TWaitFreeObject.WaitFreeObjectRemoved;
begin
end;

{ TWaitFreeObjectList }

procedure TWaitFreeObjectList.Add(AObject: IWaitFreeObject);
begin
  TActionThread(FActionThread).Add(AObject)
end;

procedure TWaitFreeObjectList.Clear;
begin
  TActionThread(FActionThread).Clear;
end;

constructor TWaitFreeObjectList.Create(AAllowDuplicatedObject: boolean);
begin
  inherited Create;

  FAllowDuplicatedObject := AAllowDuplicatedObject;

  FCount := 0;
  FHeaderInterface := nil;

  FActionThread := TActionThread.Create(Self);
end;

procedure TWaitFreeObjectList.delete_DuplicatedObject(AObject: IWaitFreeObject);
var
  Node : IWaitFreeObject;
begin
  Node := FHeaderInterface;
  while node <> nil do begin
    if Node.CompareWaitFreeObject(AObject as TObject) then do_Duplicated(Node);
    Node := Node.GetRightWaitFreeInterface;
  end;
end;

destructor TWaitFreeObjectList.Destroy;
begin
  FActionThread.Terminate;

  // TODO: 스레드와 충돌 가능성
  do_Clear;

  inherited;
end;

procedure TWaitFreeObjectList.do_Add(AObject: IWaitFreeObject);
begin
  if AObject = nil then Exit;
  if not AObject.IsWaitFreeObjectDeleted then Exit;

  AObject.SetWaitFreeObjectDeleted(false);

  if not FAllowDuplicatedObject then delete_DuplicatedObject(AObject);

  AObject.SetLeftWaitFreeObject(nil, nil);

  if FHeaderInterface <> nil then begin
    FHeaderInterface.SetLeftWaitFreeObject(AObject as TObject, AObject);
    AObject.SetRightWaitFreeObject(FHeaderInterface as TObject, FHeaderInterface);
  end else begin
    AObject.SetRightWaitFreeObject(nil, nil);
  end;

  FHeaderInterface := AObject;

  Inc(FCount);

  AObject.WaitFreeObjectAdded;
end;

procedure TWaitFreeObjectList.do_Clear;
var
  OldHeader : IWaitFreeObject;
begin
  OldHeader := IWaitFreeObject(InterlockedExchangePointer(Pointer(FHeaderInterface), nil));
  FCount := 0;

  while OldHeader <> nil do begin
    OldHeader.WaitFreeObjectRemoved;
    OldHeader := OldHeader.GetRightWaitFreeInterface;
  end;
end;

procedure TWaitFreeObjectList.do_Duplicated(AObject: IWaitFreeObject);
var
  LeftObject, RightObject : TObject;
  LeftNode, RightNode : IWaitFreeObject;
begin
  if AObject = nil then Exit;
  if AObject.IsWaitFreeObjectDeleted then Exit;

  AObject.SetWaitFreeObjectDeleted(true);

  LeftNode  := AObject.GetLeftWaitFreeInterface;
  if LeftNode <> nil then LeftObject := LeftNode as TObject
  else LeftObject := nil;

  RightNode := AObject.GetRightWaitFreeInterface;
  if RightNode <> nil then RightObject := RightNode as TObject
  else RightObject := nil;

  if AObject = FHeaderInterface then FHeaderInterface := RightNode;

  if LeftNode  <> nil then LeftNode .SetRightWaitFreeObject(RightObject, RightNode);
  if RightNode <> nil then RightNode.SetLeftWaitFreeObject (LeftObject,  LeftNode);

  Dec(FCount);

  AObject.WaitFreeObjectDuplicated;
end;

procedure TWaitFreeObjectList.do_Synchronize(AIterator: IWaitFreeSynchronize; AContext:pointer);
begin
  AIterator.Synchronize(FHeaderInterface, AContext);
end;

procedure TWaitFreeObjectList.do_Remove(AObject: IWaitFreeObject);
var
  LeftObject, RightObject : TObject;
  LeftNode, RightNode : IWaitFreeObject;
begin
  if AObject = nil then begin
    {$IFDEF DEBUG}
    Trace('TWaitFreeObjectList.do_Remove: AObject = nil');
    {$ENDIF}

    Exit;
  end;

  if AObject.IsWaitFreeObjectDeleted then begin
    {$IFDEF DEBUG}
    Trace('TWaitFreeObjectList.do_Remove: AObject.IsDeleted');
    {$ENDIF}

    Exit;
  end;

  AObject.SetWaitFreeObjectDeleted(true);

  LeftNode  := AObject.GetLeftWaitFreeInterface;
  if LeftNode <> nil then LeftObject := LeftNode as TObject
  else LeftObject := nil;

  RightNode := AObject.GetRightWaitFreeInterface;
  if RightNode <> nil then RightObject := RightNode as TObject
  else RightObject := nil;

  if AObject = FHeaderInterface then FHeaderInterface := RightNode;

  if LeftNode  <> nil then LeftNode .SetRightWaitFreeObject(RightObject, RightNode);
  if RightNode <> nil then RightNode.SetLeftWaitFreeObject (LeftObject,  LeftNode);

  Dec(FCount);

  AObject.WaitFreeObjectRemoved;
end;

function TWaitFreeObjectList.GetFirstObject: IWaitFreeObject;
begin
  Result := FHeaderInterface;
end;

function TWaitFreeObjectList.GetIsBusy: boolean;
begin
  Result := TActionThread(FActionThread).FActionList.Count > 0;
end;

function TWaitFreeObjectList.GetIsEmpty: boolean;
begin
  Result := FHeaderInterface = nil;
end;

procedure TWaitFreeObjectList.Iterate(AProcedure: TSimpleIterateProcedure);
var
  Node : IWaitFreeObject;
begin
  Node := FHeaderInterface;
  while Node <> nil do begin
    if not Node.IsWaitFreeObjectDeleted then AProcedure(Node);

    Node := Node.GetRightWaitFreeInterface;
  end;
end;

procedure TWaitFreeObjectList.Iterate(AProcedure: TIterateProcedure);
var
  Node : IWaitFreeObject;
  NeedStop : boolean;
begin
  NeedStop := false;

  Node := FHeaderInterface;
  while Node <> nil do begin
    if not Node.IsWaitFreeObjectDeleted then AProcedure(Node, NeedStop);

    if NeedStop then Break;

    Node := Node.GetRightWaitFreeInterface;
  end;
end;

procedure TWaitFreeObjectList.Remove(AObject: IWaitFreeObject);
begin
  TActionThread(FActionThread).Remove(AObject);
end;

procedure TWaitFreeObjectList.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TWaitFreeObjectList.Synchronize(ASynchronize: IWaitFreeSynchronize; AContext:pointer);
begin
  TActionThread(FActionThread).Synchronize(ASynchronize, AContext);
end;

{ TTestNode }

type
  TTestNode = class (TWaitFreeObject)
  private
  public
    Data : integer;
    constructor Create(AData:integer); reintroduce;
  end;

constructor TTestNode.Create(AData: integer);
begin
  inherited Create;

  Data := AData;
end;

{$IFDEF DEBUG}
var
  Str : set of byte;
  List : TWaitFreeObjectList;
{$ENDIF}

initialization
{$IFDEF DEBUG}
  List := TWaitFreeObjectList.Create(false);
  try
    List.Add(TTestNode.Create(1));
    List.Add(TTestNode.Create(2));
    List.Add(TTestNode.Create(3));

    while List.IsBusy do Sleep(10);

    Str := [];
    List.Iterate(
      procedure (WaitFreeObject:IWaitFreeObject; var NeedStop:boolean) begin
        Str := Str + [TTestNode(WaitFreeObject).Data];
      end
    );
    Assert(Str = [1, 2, 3], 'Str = [1, 2, 3]');

  finally
    List.Free;
  end;
{$ENDIF}
end.
