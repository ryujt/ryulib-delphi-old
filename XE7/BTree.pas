// Oroginal Source: http://www.ibrtses.com/delphi/binarytree.html
//
// Taken from Nicklaus Wirth :
// Algorithmen und Datenstrukturen ( in Pascal )
// Balanced Binary Trees p 250 ++
//
//
// Fixed By Giacomo Policicchio
// pgiacomo@tiscalinet.it
// 19/05/2000
//

unit BTree;

interface

uses
  SysUtils, Classes;

type
  TBTreeItem = class abstract
  private
    FLeftNode, FRightNode : TBTreeItem;
    FBalance : - 1 .. 1;
  protected
    // a < self :-1  a=self :0  a > self :+1
    function Compare(const ACompareTo:TBTreeItem):integer; virtual; abstract;
    procedure Copy(const ACopyTo:TBTreeItem); virtual; abstract;
  public
    Data : pointer;
  end;

  TSimpleIterateProcedure = reference to procedure(ABTreeItem:TBTreeItem);
  TIterateProcedure = reference to procedure(ABTreeItem:TBTreeItem; var ANeedStop:boolean);

  TBTree = class
  private
    FRoot: TBTreeItem;
  protected
    procedure DeleteNode(var AItem:TBTreeItem);
    procedure Delete(const AItem: TBTreeItem; var p: TBTreeItem; var h: boolean;
              var ok: boolean);
    procedure SearchAndInsert(const AItem: TBTreeItem; var p: TBTreeItem;
              var h: boolean; var Found: boolean; var FoundItem:TBTreeItem);
    procedure BalanceLeft(var p: TBTreeItem; var h: boolean; dl: boolean);
    procedure BalanceRight(var p: TBTreeItem; var h: boolean; dl: boolean);
    function SearchItem(const AItem:TBTreeItem; var ATargetNode:TBTreeItem):TBTreeItem;
  public
    constructor Create;
    destructor Destroy; override;

    function IsEmpty:boolean;

    procedure Clear;
    function Add(const AItem:TBTreeItem):TBTreeItem;
    function Remove(const AItem:TBTreeItem):boolean;
    function Search(const AItem:TBTreeItem):boolean; overload;
    function Search(const AItem:TBTreeItem; var ASearchResult:TBTreeItem):boolean; overload;

    procedure Iterate(const AProcedure:TSimpleIterateProcedure); overload;
    procedure Iterate(AProcedure:TIterateProcedure); overload;

    property Root : TBTreeItem read FRoot;
  end;

implementation

{ TBTree }

procedure TBTree.DeleteNode(var AItem: TBTreeItem);
begin
  if AItem = nil then Exit;

  if (AItem.FLeftNode <> nil) then DeleteNode(AItem.FLeftNode);
  if (AItem.FRightNode <> nil) then DeleteNode(AItem.FRightNode);

  FreeAndNil(AItem);
end;

procedure TBTree.Clear;
begin
  DeleteNode(FRoot);
end;

constructor TBTree.Create;
begin
  inherited;

  FRoot := nil;
end;

destructor TBTree.Destroy;
begin
  while FRoot <> nil do Remove(FRoot);

  inherited;
end;

function TBTree.IsEmpty: boolean;
begin
  Result := FRoot = nil;
end;


procedure ListItems(const ABTreeItem:TBTreeItem; const AProcedure:TIterateProcedure; var ANeedStop:boolean);
begin
  if ABTreeItem = nil then Exit;

  if ANeedStop then Exit;
  if (ABTreeItem.FLeftNode <> nil) then ListItems(ABTreeItem.FLeftNode, AProcedure, ANeedStop);

  if ANeedStop then Exit;
  AProcedure(ABTreeItem, ANeedStop);

  if ANeedStop then Exit;
  if (ABTreeItem.FRightNode <> nil) then ListItems(ABTreeItem.FRightNode, AProcedure, ANeedStop);
end;

procedure TBTree.Iterate(AProcedure: TIterateProcedure);
var
  NeedStop : boolean;
begin
  NeedStop := false;
  ListItems(FRoot, AProcedure, NeedStop);
end;

procedure SimpleListItems(const ABTreeItem:TBTreeItem; const AProcedure:TSimpleIterateProcedure);
begin
  if ABTreeItem = nil then Exit;

  if (ABTreeItem.FLeftNode <> nil) then SimpleListItems(ABTreeItem.FLeftNode, AProcedure);

  AProcedure(ABTreeItem);

  if (ABTreeItem.FRightNode <> nil) then SimpleListItems(ABTreeItem.FRightNode, AProcedure);
end;

procedure TBTree.Iterate(const AProcedure: TSimpleIterateProcedure);
begin
  SimpleListItems(FRoot, AProcedure);
end;

procedure TBTree.SearchAndInsert(const AItem: TBTreeItem; var p: TBTreeItem;
  var h: boolean; var Found: boolean; var FoundItem:TBTreeItem);
begin
  Found := false;
  FoundItem := nil;

  if p = nil then begin // word not in tree, insert it
    p := AItem;
    h := true;
    with p do begin
      if FRoot = nil then FRoot := p;
      FLeftNode := nil;
      FRightNode := nil;
      FBalance := 0;
    end;
  end else if (AItem.Compare(p) > 0) then begin
    SearchAndInsert(AItem, p.FLeftNode, h, Found, FoundItem);
    if h and not Found then BalanceLeft(p, h, false);
  end else if (AItem.Compare(p) < 0) then begin
    SearchAndInsert(AItem, p.FRightNode, h, Found, FoundItem);
    if h and not Found then BalanceRight(p, h, false);
  end else begin
    h := false;
    Found := true;
    FoundItem := p;
  end;
end; // searchAndInsert

// returns true and a pointer to the equal item if found, false otherwise
function TBTree.SearchItem(const AItem: TBTreeItem; var ATargetNode: TBTreeItem): TBTreeItem;
begin
  Result := nil;
  if ATargetNode = nil then Exit;

  if (AItem.Compare(ATargetNode) = 0) then Result := ATargetNode
  else begin
    if (AItem.Compare(ATargetNode) > 0) then Result := SearchItem(AItem, ATargetNode.FLeftNode)
    else begin
      if (AItem.Compare(ATargetNode) < 0) then Result := SearchItem(AItem, ATargetNode.FRightNode)
    end;
  end;
end;

procedure TBTree.BalanceRight(var p: TBTreeItem; var h: boolean;
  dl: boolean);
var
  p1, p2: TBTreeItem;
Begin
  case p.FBalance of
    - 1:
      begin
        p.FBalance := 0;
        if not dl then
          h := false;
      end;
    0:
      begin
        p.FBalance := +1;
        if dl then
          h := false;
      end;
    +1:
      begin // new balancing
        p1 := p.FRightNode;
        if (p1.FBalance = +1) or ((p1.FBalance = 0) and dl) then
        begin // single rr rotation
          p.FRightNode := p1.FLeftNode;
          p1.FLeftNode := p;
          if not dl then
            p.FBalance := 0
          else
          begin
            if p1.FBalance = 0 then
            begin
              p.FBalance := +1;
              p1.FBalance := -1;
              h := false;
            end
            else
            begin
              p.FBalance := 0;
              p1.FBalance := 0;
              (* h:=false; *)
            end;
          end;
          p := p1;
        end
        else
        begin // double rl rotation
          p2 := p1.FLeftNode;
          p1.FLeftNode := p2.FRightNode;
          p2.FRightNode := p1;
          p.FRightNode := p2.FLeftNode;
          p2.FLeftNode := p;
          if p2.FBalance = +1 then
            p.FBalance := -1
          else
            p.FBalance := 0;
          if p2.FBalance = -1 then
            p1.FBalance := +1
          else
            p1.FBalance := 0;
          p := p2;
          if dl then
            p2.FBalance := 0;
        end;
        if not dl then
        begin
          p.FBalance := 0;
          h := false;
        end;
      end;
  end; // case
End;

procedure TBTree.BalanceLeft(var p: TBTreeItem; var h: boolean;
  dl: boolean);
var
  p1, p2: TBTreeItem;
Begin
  case p.FBalance of
    1:
      begin
        p.FBalance := 0;
        if not dl then
          h := false;
      end;
    0:
      begin
        p.FBalance := -1;
        if dl then
          h := false;
      end;
    -1: (* if (p.Left<>nil) or not dl then *)
      begin // new balancing
        p1 := p.FLeftNode;
        if (p1.FBalance = -1) or ((p1.FBalance = 0) and dl) then
        begin // single ll rotation
          p.FLeftNode := p1.FRightNode;
          p1.FRightNode := p;
          if not dl then
            p.FBalance := 0
          else
          begin
            if p1.FBalance = 0 then
            begin
              p.FBalance := -1;
              p1.FBalance := +1;
              h := false;
            end
            else
            begin
              p.FBalance := 0;
              p1.FBalance := 0;
              (* h:=false; *)
            end;
          end;
          p := p1;
        end
        else
        begin // double lr rotation
          p2 := p1.FRightNode;
          p1.FRightNode := p2.FLeftNode;
          p2.FLeftNode := p1;
          p.FLeftNode := p2.FRightNode;
          p2.FRightNode := p;
          if p2.FBalance = -1 then
            p.FBalance := +1
          else
            p.FBalance := 0;
          if p2.FBalance = +1 then
            p1.FBalance := -1
          else
            p1.FBalance := 0;
          p := p2;
          if dl then
            p2.FBalance := 0;
        end;
        if not dl then
        begin
          p.FBalance := 0;
          h := false;
        end;
      end; { -1 }
  end; { case }
End;

procedure TBTree.Delete(const AItem: TBTreeItem; var p: TBTreeItem;
  var h: boolean; var ok: boolean);
var
  q: TBTreeItem; // h=false;

  procedure del(var r: TBTreeItem; var h: boolean);
  begin // h=false
    if r.FRightNode <> nil then
    begin
      del(r.FRightNode, h);
      if h then
        BalanceLeft(r, h, true);
    end
    else
    begin
      r.Copy(q); { q.key:=r.key; }
      q := r;
      r := r.FLeftNode;
      h := true;
    end;
  end;

begin { main of delete }
  ok := true;
  if (p = nil) then
  begin
    ok := false;
    h := false;
  end
  else if (AItem.Compare(p) > 0) { (x < p^.key) } then
  begin
    Delete(AItem, p.FLeftNode, h, ok);
    if h then
      BalanceRight(p, h, true);
  end
  else if (AItem.Compare(p) < 0) { (x > p^.key) } then
  begin
    Delete(AItem, p.FRightNode, h, ok);
    if h then
      BalanceLeft(p, h, true);
  end
  else
  begin // remove q
    q := p;
    if q.FRightNode = nil then
    begin
      p := q.FLeftNode;
      h := true;
    end
    else if (q.FLeftNode = nil) then
    begin
      p := q.FRightNode;
      h := true;
    end
    else
    begin
      del(q.FLeftNode, h);
      if h then
        BalanceRight(p, h, true);
    end;
    q.free; { dispose(q) };
  end;
end;

function TBTree.Add(const AItem: TBTreeItem): TBTreeItem;
var
  h, Found : boolean;
begin
  // "Found = true" or "Result <> nil" means AItem is not inserted.
  SearchAndInsert(AItem, FRoot, h, Found, Result);
end;

function TBTree.Remove(const AItem: TBTreeItem): boolean;
var
  h : boolean;
begin
  Delete(AItem, FRoot, h, Result);
end;

function TBTree.Search(const AItem: TBTreeItem): boolean;
begin
  Result := SearchItem(AItem, FRoot) <> nil;
end;

function TBTree.Search(const AItem:TBTreeItem; var ASearchResult:TBTreeItem):boolean;
begin
  ASearchResult := SearchItem(AItem, FRoot);
  Result := ASearchResult <> nil;
end;

end.

