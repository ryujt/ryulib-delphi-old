unit LinkedList;

interface

uses
  DebugTools, RyuLibBase,
  Windows, Classes, SysUtils;

type
  TLinkedListNode = class;

  ILinkedListNode = interface
    ['{F8482FFC-29D6-479E-BDF8-DD23150D37DE}']

    function GetObject:TLinkedListNode;

    procedure LinkedListNodeAdded;
    procedure LinkedListNodeRemoved;

    procedure SetLeftLinkedListNode(AObject:ILinkedListNode);
    function GetLeftLinkedListNode:ILinkedListNode;

    procedure SetRightLinkedListNode(AObject:ILinkedListNode);
    function GetRightLinkedListNode:ILinkedListNode;
  end;

  TLinkedListNode = class (TInterfaceBase, ILinkedListNode)
  private
    FLeft : ILinkedListNode;
    FRight : ILinkedListNode;

    function GetObject:TLinkedListNode;

    procedure SetLeftLinkedListNode(AObject:ILinkedListNode);
    function GetLeftLinkedListNode:ILinkedListNode;

    procedure SetRightLinkedListNode(AObject:ILinkedListNode);
    function GetRightLinkedListNode:ILinkedListNode;
  protected
    procedure LinkedListNodeAdded; virtual;
    procedure LinkedListNodeRemoved; virtual;
  public
    Data : pointer;
    constructor Create; virtual;
  end;

  TSimpleIterateProcedure = reference to procedure(ALinkedListNode:ILinkedListNode);
  TIterateProcedure = reference to procedure(ALinkedListNode:ILinkedListNode; var ANeedStop:boolean);

  TLinkedList = class
  private
    FHead, FTail : TLinkedListNode;
    FCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure InsertBefore(AItem,ATarget:ILinkedListNode);
    procedure InsertAfter(AItem,ATarget:ILinkedListNode);

    function Add(AData:pointer):ILinkedListNode; overload;
    procedure Add(AItem:ILinkedListNode); overload;

    procedure Remove(ANode:ILinkedListNode); overload;

    function RemoveFirst:ILinkedListNode;
    function RemoveLast:ILinkedListNode;

    function GetFirstNode:ILinkedListNode;
    function GetLastNode:ILinkedListNode;

    procedure Iterate(AProcedure:TSimpleIterateProcedure); overload;
    procedure Iterate(AProcedure:TIterateProcedure); overload;

    function IsEmpty:boolean;
    property Count : integer read FCount;
  end;

implementation

{ TLinkedListNode }

constructor TLinkedListNode.Create;
begin
  inherited;

  FLeft := nil;
  FRight := nil;
end;

function TLinkedListNode.GetLeftLinkedListNode: ILinkedListNode;
begin
  Result := FLeft;
end;

function TLinkedListNode.GetObject: TLinkedListNode;
begin
  Result := Self;
end;

function TLinkedListNode.GetRightLinkedListNode: ILinkedListNode;
begin
  Result := FRight;
end;

procedure TLinkedListNode.LinkedListNodeAdded;
begin
end;

procedure TLinkedListNode.LinkedListNodeRemoved;
begin
end;

procedure TLinkedListNode.SetLeftLinkedListNode(AObject: ILinkedListNode);
begin
  FLeft := AObject;
end;

procedure TLinkedListNode.SetRightLinkedListNode(AObject: ILinkedListNode);
begin
  FRight := AObject;
end;

{ TLinkedList }

procedure TLinkedList.Add(AItem: ILinkedListNode);
begin
  InsertBefore(AItem, FTail);
end;

function TLinkedList.Add(AData: pointer): ILinkedListNode;
var
  LinkedListNode : TLinkedListNode;
begin
  LinkedListNode := TLinkedListNode.Create;
  LinkedListNode.Data := AData;

  Add( LinkedListNode );

  Result := LinkedListNode;
end;

procedure TLinkedList.Clear;
begin
  while not IsEmpty do Remove(GetFirstNode);
  FCount := 0;
end;

constructor TLinkedList.Create;
begin
  inherited;

  FCount := 0;

  FHead := TLinkedListNode.Create;
  FTail := TLinkedListNode.Create;

  FHead.SetLeftLinkedListNode(nil);
  FHead.SetRightLinkedListNode(FTail);

  FTail.SetLeftLinkedListNode(FHead);
  FTail.SetRightLinkedListNode(nil);
end;

destructor TLinkedList.Destroy;
begin
  Clear;

  FreeAndNil(FHead);
  FreeAndNil(FTail);

  inherited;
end;

function TLinkedList.GetFirstNode: ILinkedListNode;
begin
  if FHead.GetRightLinkedListNode <> ILinkedListNode(FTail) then Result := FHead.GetRightLinkedListNode
  else Result := nil;
end;

procedure TLinkedList.InsertAfter(AItem, ATarget:ILinkedListNode);
var
  RightNode : ILinkedListNode;
begin
  RightNode := ATarget.GetRightLinkedListNode;

  ATarget.SetRightLinkedListNode(AItem);

  AItem.SetLeftLinkedListNode(ATarget);
  AItem.SetRightLinkedListNode(RightNode);

  RightNode.SetLeftLinkedListNode(AItem);

  FCount := FCount + 1;

  AItem.LinkedListNodeAdded;
end;

procedure TLinkedList.InsertBefore(AItem, ATarget:ILinkedListNode);
var
  LeftNode : ILinkedListNode;
begin
  LeftNode := ATarget.GetLeftLinkedListNode;

  LeftNode.SetRightLinkedListNode(AItem);

  AItem.SetLeftLinkedListNode(LeftNode);
  AItem.SetRightLinkedListNode(ATarget);

  ATarget.SetLeftLinkedListNode(AItem);

  FCount := FCount + 1;

  AItem.LinkedListNodeAdded;
end;

function TLinkedList.IsEmpty: boolean;
begin
  Result := FHead.GetRightLinkedListNode = ILinkedListNode(FTail);
end;

procedure TLinkedList.Iterate(AProcedure: TSimpleIterateProcedure);
var
  Current : ILinkedListNode;
begin
  Current := FHead;
  while Current.GetRightLinkedListNode <> ILinkedListNode(FTail) do begin
    AProcedure(Current.GetRightLinkedListNode);

    Current := Current.GetRightLinkedListNode;
  end;
end;

procedure TLinkedList.Iterate(AProcedure: TIterateProcedure);
var
  NeedStop : boolean;
  Current : ILinkedListNode;
begin
  NeedStop := false;

  Current := FHead;
  while Current.GetRightLinkedListNode <> ILinkedListNode(FTail) do begin
    AProcedure(Current.GetRightLinkedListNode, NeedStop);
    if NeedStop then Break;

    Current := Current.GetRightLinkedListNode;
  end;
end;

function TLinkedList.GetLastNode: ILinkedListNode;
begin
  if FTail.GetLeftLinkedListNode <> ILinkedListNode(FHead) then Result := FTail.GetLeftLinkedListNode
  else Result := nil;
end;

procedure TLinkedList.Remove(ANode: ILinkedListNode);
var
  pLeft, pRight : ILinkedListNode;
begin
  try
    if ANode = nil then raise Exception.Create('TLinkedList.Remove: ANode = nil');
    if ANode.GetLeftLinkedListNode = nil then raise Exception.Create('TLinkedList.Remove: ANode.GetLeftLinkedListNode = nil');
    if ANode.GetRightLinkedListNode = nil then raise Exception.Create('TLinkedList.Remove: ANode.GetRightLinkedListNode = nil');

    pLeft  := ANode.GetLeftLinkedListNode;
    pRight := ANode.GetRightLinkedListNode;

    pLeft.SetRightLinkedListNode(pRight);
    pRight.SetLeftLinkedListNode(pLeft);

    ANode.SetLeftLinkedListNode(nil);
    ANode.SetRightLinkedListNode(nil);

    FCount := FCount - 1;

    ANode.LinkedListNodeRemoved;
  except
    on E : Exception do Trace( E.Message );
  end;
end;

function TLinkedList.RemoveFirst: ILinkedListNode;
begin
  Result := GetFirstNode;
  if Result <> nil then Remove(Result);
end;

function TLinkedList.RemoveLast: ILinkedListNode;
begin
  Result := GetLastNode;
  if Result <> nil then Remove(Result);
end;

{$IFDEF DEBUG}
const
  _ListLength = 1024;
var
  StrSrc, StrDstForward : string;
  Loop : integer;
  Item : TLinkedListNode;
  List : TLinkedList;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  List := TLinkedList.Create;
  try
    StrDstForward := '';
    for Loop := 1 to _ListLength do begin
      Item := TLinkedListNode.Create;
      Item.Data := Pointer(Loop);
      List.Add(Item);
      StrDstForward := StrDstForward + IntToStr(Loop) + ' ';
    end;

    StrSrc := '';
    List.Iterate(
      procedure (ALinkedListNode:ILinkedListNode) begin
        StrSrc := StrSrc + IntToStr(Integer(TLinkedListNode(ALinkedListNode).Data)) + ' ';
      end
    );
    Assert(StrSrc = StrDstForward, 'TLinkedList iterate forward failed.');
  finally
    List.Free;
  end;
  {$ENDIF}
end.
