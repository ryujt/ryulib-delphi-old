unit DuplicatedList;

interface

uses
  Classes, SysUtils;

type
  TDuplicatedList = class
  strict private
    FItems : TList;
    FChilds : TList;
  private
    FData: pointer;
    function GetChilds(AIndex: integer): TDuplicatedList;
    function GetChildCount: integer;
    function GetItems(AIndex: integer): pointer;
    function GetCount: integer;
    procedure SetData(const Value: pointer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AItem:pointer);
    procedure Remove(AItem:pointer);
    procedure Delete(AIndex:integer);
    function IndexOf(AItem:pointer):integer;
    property Items[AIndex:integer] : pointer read GetItems;
    property Count : integer read GetCount;

    procedure ClearChild;
    procedure AddChild(AChild:TDuplicatedList);
    property Childs[AIndex:integer] : TDuplicatedList read GetChilds;
    property ChildCount : integer read GetChildCount;

    property Data : pointer read FData write SetData;
  end; 

implementation

{ TDuplicatedList }

procedure TDuplicatedList.Add(AItem: pointer);
begin
  FItems.Add(AItem);
end;

procedure TDuplicatedList.AddChild(AChild: TDuplicatedList);
begin
  FChilds.Add(AChild);
end;

procedure TDuplicatedList.Clear;
begin
  FItems.Clear;
end;

procedure TDuplicatedList.ClearChild;
var
  Loop: Integer;
begin
  for Loop := 0 to FChilds.Count-1 do TObject(FChilds[Loop]).Free;
  FChilds.Clear;
end;

constructor TDuplicatedList.Create;
begin
  inherited;

  FItems := TList.Create;
  FChilds := TList.Create;
end;

procedure TDuplicatedList.Delete(AIndex: integer);
begin
  FItems.Delete(AIndex);
end;

destructor TDuplicatedList.Destroy;
begin
  ClearChild;
  Clear;

  FreeAndNil(FChilds);
  FreeAndNil(FItems);

  inherited;
end;

function TDuplicatedList.GetChildCount: integer;
begin
  Result := FChilds.Count;
end;

function TDuplicatedList.GetChilds(AIndex: integer): TDuplicatedList;
begin
  Result := Pointer(FChilds[AIndex]);
end;

function TDuplicatedList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TDuplicatedList.GetItems(AIndex: integer): pointer;
begin
  Result := FItems[AIndex];
end;

function TDuplicatedList.IndexOf(AItem: pointer): integer;
begin
  Result := FItems.IndexOf(AItem)
end;

procedure TDuplicatedList.Remove(AItem: pointer);
begin
  FItems.Remove(AItem);
end;

procedure TDuplicatedList.SetData(const Value: pointer);
begin
  FData := Value;
end;

end.
