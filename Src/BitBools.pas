unit BitBools;

interface

uses
  Classes, SysUtils ;

const
  _PageSize = 1024;

type
  TBitBools = class
  private
    FPages : TList;
    FSmallIndexes : array [0..7] of byte;
    procedure set_Capacity(Value: integer);
  private
    FCount: integer;
    function GetItems(Index: integer): boolean;
    procedure SetCount(const Value: integer);
    procedure SetItems(Index: integer; const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Items[Index:integer] : boolean read GetItems write SetItems;
    property Count : integer read FCount write SetCount;
  end;

implementation

type
  TPageList = class (TList)
  private
  protected
    procedure Notify(Ptr:Pointer; Action:TListNotification); override;
  public
  end;

{ TPageList }

procedure TPageList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    FreeMem(Ptr);
  end;
end;

{ TBitBools }

procedure TBitBools.Clear;
var
  Page : pointer;
  Loop : Integer;
begin
  for Loop := 0 to FPages.Count-1 do begin
    Page := FPages[Loop];
    FillChar(Page^, _PageSize, 0);
  end;
end;

constructor TBitBools.Create;
var
  Loop : integer;
begin
  inherited;

  FSmallIndexes[0] := 1;
  for Loop := 1 to 7 do FSmallIndexes[Loop] := FSmallIndexes[Loop-1] shl 1;

  FPages := TPageList.Create;
end;

destructor TBitBools.Destroy;
begin
  FreeAndNil(FPages);

  inherited;
end;

function TBitBools.GetItems(Index: integer): boolean;
var
  Item : byte;
  Page : PByteArray; 
  PageIndex, ItemIndex, SmallIndex : integer;
begin
  PageIndex := Index div _PageSize;
  Page := FPages[PageIndex];

  ItemIndex := Index mod _PageSize;
  Item := Page^[ItemIndex];

  SmallIndex := ItemIndex mod 8;

  Result := (Item and FSmallIndexes[SmallIndex]) = FSmallIndexes[SmallIndex];
end;

procedure TBitBools.set_Capacity(Value: integer);
var
  Page : pointer;
  Loop, iPageSize : integer;
begin
  // 항상 _PageSize 단위로 메모리 할당을 한다.
  iPageSize := Value div _PageSize;
  if (Value mod _PageSize) <> 0 then Inc(iPageSize);

  if iPageSize = FPages.Count then Exit;

  if iPageSize < FPages.Count then begin
    while FPages.Count > iPageSize do begin
      FPages.Delete(FPages.Count-FPages.Count);
    end;
    Exit;
  end;

  for Loop := FPages.Count to iPageSize - 1 do begin
    GetMem(Page, _PageSize);
    FillChar(Page^, _PageSize, 0);
    FPages.Add(Page);
  end;
end;

procedure TBitBools.SetCount(const Value: integer);
begin
  FCount := Value;
  set_Capacity(Value);
end;

procedure TBitBools.SetItems(Index: integer; const Value: boolean);
var
  Page : PByteArray;
  PageIndex, ItemIndex, SmallIndex : integer;
begin
  PageIndex := Index div _PageSize;
  Page := FPages[PageIndex];

  ItemIndex := Index mod _PageSize;

  SmallIndex := ItemIndex mod 8;

  if Value then
    Page^[ItemIndex] := Page^[ItemIndex] or (FSmallIndexes[SmallIndex])
  else
    Page^[ItemIndex] := Page^[ItemIndex] and (not FSmallIndexes[SmallIndex]);
end;

end.
