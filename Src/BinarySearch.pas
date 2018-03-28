unit BinarySearch;

interface

uses
  RyuLibBase,
  SysUtils, Classes;

type
  TSortCompareFunction = reference to function(A,B:pointer):integer;

  TBinarySearchInterface = interface
    ['{5087394E-E6DA-465B-9F81-EF6B78EB2CEC}']

    function GetCount: integer;
    function GetItems(AIndex: integer): pointer;

    property Count : integer read GetCount;
    property Items[Index:integer] : pointer read GetItems;
  end;

  TBinarySearch = class (TInterfaceBase, TBinarySearchInterface)
  private
    FList : TList;
    FSortCompareFunction : TSortCompareFunction;
  private
    function GetCount: integer;
    function GetItems(AIndex: integer): pointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetCompareFunction(ASortCompareFunction:TSortCompareFunction);

    function Insert(AItem:pointer):pointer;

    function Find(AItem:pointer):integer; overload;
    function Find(AItem:pointer; ASortCompareFunction:TSortCompareFunction):integer; overload;

    function FindNear(AItem:pointer):integer; overload;
    function FindNear(AItem:pointer; ASortCompareFunction:TSortCompareFunction):integer; overload;

    procedure Clear;
    procedure Remove(AItem:pointer);
    procedure Delete(AIndex:integer);
  public
    property Count : integer read GetCount;
    property Items[Index:integer] : pointer read GetItems;
  end;

implementation

{ TBinarySearch }

procedure TBinarySearch.Clear;
begin
  FList.Clear;
end;

constructor TBinarySearch.Create;
begin
  inherited;

  FList := TList.Create;
end;

procedure TBinarySearch.Delete(AIndex: integer);
begin
  FList.Delete(AIndex);
end;

destructor TBinarySearch.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

function TBinarySearch.Find(AItem: pointer): integer;
var
  pItem: Pointer;
  iStart, iEnd, iMiddle, iResult: Integer;
begin
  Result := -1;

  iStart := 0;
  iEnd := FList.Count - 1;

  while iStart <= iEnd do begin
    iMiddle := (iStart + iEnd) div 2;
    pItem := FList.Items[iMiddle];
    iResult := FSortCompareFunction(AItem, pItem);

    case iResult of
       1: iStart := iMiddle + 1;
      -1: iEnd := iMiddle - 1;

       else begin
         Result := iMiddle;
         Exit;
       end;
    end;
  end;

  if iStart = iEnd then Result := iEnd;
end;

function TBinarySearch.FindNear(AItem: pointer): integer;
var
  pItem: Pointer;
  iStart, iEnd, iMiddle, iResult: Integer;
begin
  iStart := 0;
  iEnd := FList.Count - 1;

  while iStart <= iEnd do begin
    iMiddle := (iStart + iEnd) div 2;
    pItem := FList.Items[iMiddle];
    iResult := FSortCompareFunction(AItem, pItem);

    case iResult of
       1: iStart := iMiddle + 1;
      -1: iEnd := iMiddle - 1;

       else begin
         Result := iMiddle;
         Exit;
       end;
    end;
  end;

  if iEnd < 0 then iEnd := 0;
  if iStart > iEnd then iStart := iEnd;

  Result := iStart;
end;

function TBinarySearch.GetCount: integer;
begin
  Result := FList.Count;
end;

function TBinarySearch.GetItems(AIndex: integer): pointer;
begin
  Result := FList[AIndex];
end;

function TBinarySearch.Insert(AItem: pointer): pointer;
var
  pItem : Pointer;
  iIndex : integer;
begin
  Result := nil;

  if FList.Count = 0 then begin
    Result :=  AItem;
    FList.Add(AItem);
    Exit;
  end;

  iIndex := FindNear(AItem);

  pItem := FList.Items[iIndex];

  case FSortCompareFunction(AItem, pItem) of
    0 : Result := pItem;

    1 : begin
      Result :=  AItem;
     if iIndex >= FList.Count-1 then begin
       FList.Add(AItem);
     end else begin
       FList.Insert(iIndex+1, AItem);
     end;
   end;

   -1 : begin
     Result :=  AItem;
     if iIndex <= 0 then begin
      FList.Insert(0, AItem);
    end else begin
      FList.Insert(iIndex-1, AItem);
    end;
   end;
  end;
end;

procedure TBinarySearch.Remove(AItem: pointer);
var
  iIndex : integer;
begin
  iIndex := Find(AItem);
  if iIndex <> -1 then FList.Delete(iIndex);
end;

procedure TBinarySearch.SetCompareFunction(
  ASortCompareFunction: TSortCompareFunction);
begin
  FSortCompareFunction := ASortCompareFunction;
end;

function TBinarySearch.Find(AItem: pointer;
  ASortCompareFunction: TSortCompareFunction): integer;
var
  pItem: Pointer;
  iStart, iEnd, iMiddle, iResult: Integer;
begin
  Result := -1;

  iStart := 0;
  iEnd := FList.Count - 1;

  while iStart <= iEnd do begin
    iMiddle := (iStart + iEnd) div 2;
    pItem := FList.Items[iMiddle];
    iResult := ASortCompareFunction(AItem, pItem);

    case iResult of
       1: iStart := iMiddle + 1;
      -1: iEnd := iMiddle - 1;

       else begin
         Result := iMiddle;
         Exit;
       end;
    end;
  end;

  if iStart = iEnd then Result := iEnd;
end;

function TBinarySearch.FindNear(AItem: pointer;
  ASortCompareFunction: TSortCompareFunction): integer;
var
  pItem: Pointer;
  iStart, iEnd, iMiddle, iResult: Integer;
begin
  iStart := 0;
  iEnd := FList.Count - 1;

  while iStart <= iEnd do begin
    iMiddle := (iStart + iEnd) div 2;
    pItem := FList.Items[iMiddle];
    iResult := ASortCompareFunction(AItem, pItem);

    case iResult of
       1: iStart := iMiddle + 1;
      -1: iEnd := iMiddle - 1;

       else begin
         Result := iMiddle;
         Exit;
       end;
    end;
  end;

  if iEnd < 0 then iEnd := 0;
  if iStart > iEnd then iStart := iEnd;

  Result := iStart;
end;

end.
