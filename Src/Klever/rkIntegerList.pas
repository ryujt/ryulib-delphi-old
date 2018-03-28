unit rkIntegerList;

interface

uses
  Classes, SysUtils;

type
  { TIntList class }
  TIntList = class;

  PIntItemList = ^TIntItemList;
  TIntItemList = array[0..MaxListSize - 1] of Int64;
  TIntListSortCompare = function(List: TIntList; Index1, Index2: Integer): Integer;

  TIntList = class(TPersistent)
  private
    FList: PIntItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TIntListSortCompare);
    procedure InsertItem(Index: Integer; const S: int64);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): int64;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; const S: int64);
    procedure SetCapacity(NewCapacity: Integer);
  public
    destructor Destroy; override;
    function Add(const S: int64): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Find(const S: int64; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: int64): Integer;
    procedure Insert(Index: Integer; const S: int64);
    procedure Sort; virtual;
    procedure CustomSort(Compare: TIntListSortCompare); virtual;

    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream);

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property Integers[Index: Integer]: int64 read Get write Put; default;
    property Count: Integer read GetCount;
  end;

implementation

{ TIntList }

destructor TIntList.Destroy;
begin
  inherited destroy;
  FCount := 0;
  SetCapacity(0);
end;

procedure TIntList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

const
  sDuplicateInt: string = 'Cannot add integer because it already exists';
  sListIndexError = 'List index Error';
  SSortedListError = 'Cannont insert to sorted list';

function TIntList.Add(const S: Int64): Integer;
begin
  if not Sorted then
    Result := FCount
  else if Find(S, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(SDuplicateInt, 0);
    end;
  InsertItem(Result, S);
end;

procedure TIntList.Clear;
begin
  if FCount <> 0 then begin
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TIntList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Int64));
end;

procedure TIntList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

procedure TIntList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Int64;
  Item1, Item2: ^Int64;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^);
  Item1^ := Item2^;
  Item2^ := Temp;
end;

function TIntList.Find(const S: Int64; var Index: Integer): Boolean;
var
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    if Flist^[I] < S then L := L + 1
    else begin
      H := I - 1;
      if FList^[I] = S then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TIntList.Get(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index];
end;

function TIntList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TIntList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TIntList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then  Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TIntList.IndexOf(const S: Int64): Integer;
begin
  if not Sorted then begin
    for Result := 0 to GetCount - 1 do if Get(Result) = s then Exit;
    Result := -1;
  end
  else if not Find(S, Result) then
    Result := -1;
end;

procedure TIntList.Insert(Index: Integer; const S: Int64);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TIntList.InsertItem(Index: Integer; const S: Int64);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Int64));
  FList^[Index] := s;
  Inc(FCount);
end;

procedure TIntList.Put(Index: Integer; const S: Int64);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  FList^[Index] := S;
end;

procedure TIntList.QuickSort(L, R: Integer; SCompare: TIntListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then begin
        ExchangeItems(I, J);
        if P = I then P := J
        else if P = J then P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TIntList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Int64));
  FCapacity := NewCapacity;
end;

procedure TIntList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

function IntListCompare(List: TIntList; Index1, Index2: Integer): Integer;
begin
  if List.FList^[Index1] > List.FList^[Index2] then result := +1
  else if List.FList^[Index1] < List.FList^[Index2] then result := -1
  else result := 0;
end;

procedure TIntList.Sort;
begin
  CustomSort(IntListCompare);
end;

procedure TIntList.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIntList.SaveToStream(Stream: TStream);
var
  i: integer;
  N: integer;
  Val: int64;
begin
  N := count;
  Stream.WriteBuffer(N, sizeof(N));
  for i := 0 to count - 1 do begin
    val := integers[i];
    stream.writebuffer(val, sizeof(val));
  end;
end;

procedure TIntList.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIntList.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  i: integer;
  N: int64;
begin
  try
    clear;
    Stream.readbuffer(size, sizeof(size));
    for i := 0 to size - 1 do begin
      Stream.Read(N, sizeof(N));
      add(N);
    end;
  finally
  end;
end;

procedure TIntList.CustomSort(Compare: TIntListSortCompare);
begin
  if not Sorted and (FCount > 1) then QuickSort(0, FCount - 1, Compare);
end;

end.

