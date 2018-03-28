unit BooleanList;

interface

uses
  Classes, SysUtils;

type
  TBooleanList = class
  strict private
    FItems : array of boolean;
  private
    FCountOfTrue: integer;
    function GetItems(Index: integer): boolean;
    procedure SetItems(Index: integer; const Value: boolean);
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetCountOfFalse: integer;
    function GetIsAllFalse: boolean;
    function GetIsAllTrue: boolean;
  public
    constructor Create;

    procedure Clear;
  
    property Items[Index:integer] : boolean read GetItems write SetItems;
    property Count : integer read GetCount write SetCount;
    property CountOfTrue : integer read FCountOfTrue;
    property CountOfFalse : integer read GetCountOfFalse;
    property IsAllTrue : boolean read GetIsAllTrue;
    property IsAllFalse : boolean read GetIsAllFalse;
  end;

implementation

{ TBooleanList }

procedure TBooleanList.Clear;
begin
  Count := 0;
  FCountOfTrue := 0;
end;

constructor TBooleanList.Create;
begin
  inherited;

  FCountOfTrue := 0;
end;

function TBooleanList.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TBooleanList.GetCountOfFalse: integer;
begin
  Result := Count - FCountOfTrue;
end;

function TBooleanList.GetIsAllFalse: boolean;
begin
  Result := CountOfFalse = Count;
end;

function TBooleanList.GetIsAllTrue: boolean;
begin
  Result := CountOfTrue = Count;
end;

function TBooleanList.GetItems(Index: integer): boolean;
begin
  Result := FItems[Index];
end;

procedure TBooleanList.SetCount(const Value: integer);
var
  Loop, OldCount : integer;
begin
  OldCount := Count;
  
  SetLength(FItems, Value);

  for Loop := OldCount to Count-1 do Items[Loop] := false;
end;

procedure TBooleanList.SetItems(Index: integer; const Value: boolean);
begin
  if FItems[Index] = Value then Exit;

  FItems[Index] := Value;

  if Value then FCountOfTrue := FCountOfTrue + 1
  else FCountOfTrue := FCountOfTrue - 1;  
end;

end.
