unit IntegerList;

interface

uses
  Classes, SysUtils;

type
  TIntegerList = class (TPersistent)
  private
    FList : TList;
    function GetCount: integer;
    function GetItems(Index: integer): int64;
    function GetObjects(Index: integer): TObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Value:int64);
    procedure AddObject(Value:int64; Obj:TObject);
    procedure Delete(Index:integer);

    procedure Assign(Source: TPersistent); override;

    property Items[Index:integer] : int64 read GetItems;
    property Objects[Index:integer] : TObject read GetObjects;
    property Count : integer read GetCount; 
  end;

implementation

type
  TItem = class
  private
  public
    Value : int64;
    Obj : TObject;
  end;

{ TIntegerList }

procedure TIntegerList.Add(Value: int64);
var
  Item : TItem;
begin
  Item := TItem.Create;
  Item.Value := Value;
  Item.Obj := nil;

  FList.Add(Item);
end;

procedure TIntegerList.AddObject(Value: int64; Obj: TObject);
var
  Item : TItem;
begin
  Item := TItem.Create;
  Item.Value := Value;
  Item.Obj := Obj;

  FList.Add(Item);
end;

procedure TIntegerList.Assign(Source: TPersistent);
var
  Item : TItem;
  Loop : integer;
begin
  FList.Clear;

  for Loop := 0 to TIntegerList(Source).Count-1 do begin
    Item := Pointer(TIntegerList(Source).FList[Loop]);
    AddObject(Item.Value, Item.Obj);
  end;
end;

procedure TIntegerList.Clear;
var
  Item : TItem;
  Loop : integer;
begin
  for Loop := 0 to FList.Count - 1 do begin
    Item := Pointer(FList[Loop]);
    Item.Free;
  end;

  FList.Clear;
end;

constructor TIntegerList.Create;
begin
  inherited;

  FList := TList.Create;
end;

procedure TIntegerList.Delete(Index: integer);
var
  Item : TItem;
begin
  Item := Pointer(FList[Index]);
  Item.Free;

  FList.Delete(Index);
end;

destructor TIntegerList.Destroy;
begin
  Clear;
  
  FList.Free;

  inherited;
end;

function TIntegerList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TIntegerList.GetItems(Index: integer): int64;
begin
  Result := TItem(FList[Index]).Value;
end;

function TIntegerList.GetObjects(Index: integer): TObject;
begin
  Result := TItem(FList[Index]).Obj;
end;

end.
