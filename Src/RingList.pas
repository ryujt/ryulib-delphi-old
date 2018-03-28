unit RingList;

interface

uses
  Windows, Classes, SysUtils;

type
  TRingList = class
  private
    FList : array of Pointer;
    FIndex : integer;
    FCapacity : integer;
    function GetItems(Index: integer): pointer;
    procedure SetItems(Index: integer; const Value: pointer);
    function GetCurrentIndex: integer;
    function GetNextIndex: integer;
  public
    constructor Create(ACapacity:integer); reintroduce;

    function CurrentItem:pointer;
    function NextItem:pointer;

    procedure IncIndex;

    property Items[Index:integer] : pointer read GetItems write SetItems;
    property CurrentIndex : integer read GetCurrentIndex;
    property NextIndex : integer read GetNextIndex;
    property Capacity : integer read FCapacity;
  end;

implementation

{ TRingList }

constructor TRingList.Create(ACapacity: integer);
begin
  inherited Create;

  FIndex := 0;
  FCapacity := ACapacity;

  SetLength(FList, ACapacity);
  FillChar(FList[0], SizeOf(FList), 0);
end;

function TRingList.CurrentItem: pointer;
begin
  Result := FList[FIndex mod Capacity];
end;

function TRingList.GetCurrentIndex: integer;
begin
  Result := FIndex mod Capacity;
end;

function TRingList.GetItems(Index: integer): pointer;
begin
  Result := FList[Index];
end;

function TRingList.GetNextIndex: integer;
begin
  Result := (FIndex + 1) mod FCapacity;
end;

procedure TRingList.IncIndex;
var
  iIndex : integer;
begin
  iIndex := InterlockedIncrement(FIndex);
  if iIndex >= FCapacity then
    InterlockedCompareExchange(FIndex, iIndex mod FCapacity, iIndex);
end;

function TRingList.NextItem: pointer;
var
  iIndex, iMod : integer;
begin
  iIndex := InterlockedIncrement(FIndex);

  iMod := iIndex mod FCapacity;

  if iIndex >= FCapacity then
    InterlockedCompareExchange(FIndex, iMod, iIndex);

  Result := FList[iMod];
end;

procedure TRingList.SetItems(Index: integer; const Value: pointer);
begin
  FList[Index] := Value;
end;

end.

