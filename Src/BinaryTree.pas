unit BinaryTree;

interface

uses
  RyuLibBase, DebugTools,
  Classes, SysUtils, SyncObjs;

type
  TNode = class;

  {*
    트리의 노드에 저장되는 외부 객체이다.
    노드는 구조 정보만을 갖고 있으며, 실제 데이터는 외부 객체에 있다.
  }
  IGusetObject = interface
    ['{7627E48B-7A92-427A-AD5D-41C578C5AEE3}']

    function GetNode:TNode;
    procedure SetNode(AValue:TNode);
  end;

  TNode = class
  {$IFDEF DEBUG}
  public
  {$ELSE}
  private
  {$ENDIF}
    FParent, FLeft, FRight : TNode;
    FGuestObject : IGusetObject;
  public
    constructor Create(AGuestObject:IGusetObject); reintroduce;

    function isFull:boolean;
    function isEmpty:boolean;
  end;

  /// Test용 게스트 클래스
  TGusetObject = class (TInterfaceBase, IGusetObject)
  private
    FName : string;
  private
    function GetNode:TNode;
    procedure SetNode(AValue:TNode);
  public
    Node : TNode;
  public
    constructor Create(AName:string); reintroduce; virtual;
  end;

  {*
    노드 연결 구성이 바뀌었을 경우 자신의 부모 노드가 어떻게 바뀌었는 가를 알려준다.
    만약, 노드가 추가되면 이벤트는 한 번만 발생한다.
    만약, 노드 하나가 삭제되면 노드 밑에 달려 있는 자식 노드 개수 만큼 이벤트가 발생한다.
    @param Sender 이벤트를 발생시킨 객체
    @param AParant 변경 된 부모
    @param AChild 변경 된 부모에 영향 받는 자식 노드
  }
  TNodeChangedEvent = procedure (Sender:TObject; AParent,AChild:pointer) of object;

  {*
    정렬되지 않은 상태의 이진트리 구조이다.
    P2P 노드들의 분산 처리를 위해서 만들어졌다.
    이진트리는 BTree.pas 유닛을 참고하라.
  }
  TBinaryTree = class
  private
    FHead : TNode;
    FList : TList;

    // 자식 노드가 다 채워지지 않은 노드
    FVacancyList : TList;

    FCS : TCriticalSection;

    procedure add_Node(AParent,AChild:TNode);
    procedure remove_Node(ANode:TNode);

    // 자식 노드가 다 채워지지 않은 노드를 찾는다.
    function get_VacancyNode:TNode;

    // 자식 노드가 하나도 없는 노드를 찾는다.
    function get_EmptyNode:TNode;
  private
    FOnChanged: TNodeChangedEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AGusetObject:IGusetObject);
    procedure Remove(AGusetObject:IGusetObject);

  {$IFDEF DEBUG}
    /// Unit Test를 위한 메소드
    function GetText:string;
  {$ENDIF}
  public
    property OnChanged : TNodeChangedEvent read FOnChanged write FOnChanged;
  end;

implementation

{ TNode }

constructor TNode.Create(AGuestObject: IGusetObject);
begin
  inherited Create;

  FParent := nil;
  FLeft   := nil;
  FRight  := nil;

  FGuestObject := AGuestObject;

  if AGuestObject <> nil then AGuestObject.SetNode(Self);
end;

function TNode.isEmpty: boolean;
begin
  Result := (FLeft = nil) and (FRight = nil);
end;

function TNode.isFull: boolean;
begin
  Result := (FLeft <> nil) and (FRight <> nil);
end;

{ TGusetObject }

constructor TGusetObject.Create(AName:string);
begin
  inherited Create;

  FName := AName;
end;

function TGusetObject.GetNode: TNode;
begin
  Result := Node;
end;

procedure TGusetObject.SetNode(AValue: TNode);
begin
  Node := AValue
end;

{ TBinaryTree }

procedure TBinaryTree.Add(AGusetObject: IGusetObject);
var
  NewNode, VacancyNode : TNode;
begin
  FCS.Acquire;
  try
    VacancyNode := get_VacancyNode;

    if VacancyNode = nil then
      raise Exception.Create('TBinaryTree.Add - VacancyNode = nil');

    NewNode := TNode.Create(AGusetObject);

    add_Node(VacancyNode, NewNode);

    if VacancyNode.isFull then FVacancyList.Remove(VacancyNode);

    FList.Add(NewNode);
    FVacancyList.Add(NewNode);
  finally
    FCS.Release;
  end;
end;

procedure TBinaryTree.add_Node(AParent, AChild: TNode);
begin
  AChild.FParent := AParent;

       if AParent.FLeft  = nil then AParent.FLeft  := AChild
  else if AParent.FRight = nil then AParent.FRight := AChild
end;

procedure TBinaryTree.Clear;
var
  Loop: Integer;
begin
  FCS.Acquire;
  try
    FHead.FParent := nil;
    FHead.FLeft := nil;
    FHead.FRight := nil;

    for Loop := FList.Count-1 downto 0 do TObject(FList[Loop]).Free;

    FList.Clear;

    FVacancyList.Clear;
    FVacancyList.Add(FHead);
  finally
    FCS.Release;
  end;
end;

constructor TBinaryTree.Create;
begin
  inherited;

  FHead := TNode.Create(nil);
  FHead.FParent := nil;
  FHead.FLeft := nil;
  FHead.FRight := nil;

  FList := TList.Create;

  FVacancyList := TList.Create;
  FVacancyList.Add(FHead);

  FCS := TCriticalSection.Create;
end;

destructor TBinaryTree.Destroy;
begin
  Clear;

  FreeAndNil(FHead);
  FreeAndNil(FList);
  FreeAndNil(FVacancyList);
  FreeAndNil(FCS);

  inherited;
end;

function TBinaryTree.GetText: string;
var
  Loop: Integer;
  Node : TNode;
  sParentName, sLeftName, sRightName : string;
  Parent, Left, Right, GusetObject : TGusetObject;
begin
  Result := '';

  for Loop := 0 to FList.Count-1 do begin
    Node := FList[Loop];

    GusetObject := Node.FGuestObject as TGusetObject;

    sParentName := '';

    if Node.FParent = FHead then begin
      sParentName := 'Header';
    end else if Node.FParent <> nil then begin
      Parent := Node.FParent.FGuestObject  as TGusetObject;
      sParentName := Parent.FName;
    end;

    sLeftName := '';

    if Node.FLeft  <> nil then begin
      Left := Node.FLeft.FGuestObject  as TGusetObject;
      sLeftName := Left.FName;
    end;

    sRightName := '';

    if Node.FRight <> nil then begin
      Right := Node.FRight.FGuestObject as TGusetObject;
      sRightName := Right.FName;
    end;

    Result := Result + Format('* Name: %s, (Parent: %s), (Left: %s), (Right: %s)', [GusetObject.FName, sParentName, sLeftName, sRightName]) + #13#10;
  end;
end;

function TBinaryTree.get_EmptyNode: TNode;
var
  Loop: Integer;
  EmptyNode : TNode;

  {$IFDEF DEBUG}
  Left, Right, GusetObject : TGusetObject;
  {$ENDIF}
begin
  Result := nil;

  for Loop := 0 to FVacancyList.Count-1 do begin
    EmptyNode := FVacancyList[Loop];

    {$IFDEF DEBUG}
    GusetObject := EmptyNode.FGuestObject as TGusetObject;

    Trace( 'GusetObject.FName = ' + GusetObject.FName );

    if EmptyNode.FLeft  <> nil then begin
      Left := EmptyNode.FLeft.FGuestObject  as TGusetObject;
      Trace( 'Left.FName = ' + Left.FName );
    end;

    if EmptyNode.FRight <> nil then begin
      Right := EmptyNode.FRight.FGuestObject as TGusetObject;
      Trace( 'Right.FName = ' + Right.FName );
    end;
    {$ENDIF}

    if EmptyNode.isEmpty then begin
      Result := EmptyNode;
      Break;
    end;
  end;
end;

function TBinaryTree.get_VacancyNode: TNode;
var
  VacancyNode : TNode;
begin
  Result := nil;

  while FVacancyList.Count > 0 do begin
    VacancyNode := FVacancyList[0];

    if not VacancyNode.isFull then begin
      Result := VacancyNode;
      Break;
    end;

    FVacancyList.Delete(0);
  end;
end;

procedure TBinaryTree.Remove(AGusetObject: IGusetObject);
var
  hasChild : boolean;
  Node, EmptyNode : TNode;
begin
  Node := AGusetObject.GetNode;

  if Node = nil then Exit;

  FCS.Acquire;
  try
    FList.Remove(Node);
    FVacancyList.Remove(Node);

    remove_Node(Node);

    // 자식이 없으면 자신만 삭제하면 된다.
    if Node.isEmpty then begin
      Node.Free;
      Exit;
    end;

    EmptyNode := get_EmptyNode;

    // 대신 할 수 있는 노드가 없다면 자신이 마지막이다.
    if EmptyNode = nil then begin
      hasChild := not Node.isEmpty;
      Node.Free;

      if hasChild then
        raise Exception.Create('TBinaryTree.Remove - hasChild');

      Exit;
    end;

    add_Node(Node.FParent, EmptyNode);

    EmptyNode.FLeft  := nil;
    EmptyNode.FRight := nil;

    if EmptyNode <> Node.FLeft  then add_Node(EmptyNode, Node.FLeft);
    if EmptyNode <> Node.FRight then add_Node(EmptyNode, Node.FRight);

    Node.Free;
  finally
    FCS.Release;
  end;
end;

procedure TBinaryTree.remove_Node(ANode: TNode);
var
  ParentNode : TNode;
begin
  ParentNode := ANode.FParent;

  if ParentNode = nil then
    raise Exception.Create('TBinaryTree.remove_Node - ParentNode = nil');

       if ParentNode.FLeft  = ANode then ParentNode.FLeft  := nil
  else if ParentNode.FRight = ANode then ParentNode.FRight := nil
end;

end.



