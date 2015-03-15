unit KeyList;

interface

uses
  BinaryList, 
  Windows, Classes, SysUtils, SyncObjs;

type
  TKeyList = class
  private
    FList : TBinaryList;
    FCS : TCriticalSection;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function CheckDuplicate(AKey:DWord):boolean;

    property Count : integer read GetCount;
  end;

implementation

function CompareLogic(Item1, Item2: Pointer): Integer;
begin
  if DWord(Item1) > DWord(Item2) then Result := 1
  else if DWord(Item1) < DWord(Item2) then Result := -1
  else Result := 0;
end;

{ TKeyList }

function TKeyList.CheckDuplicate(AKey: DWord): boolean;
var
  pItem : pointer;
begin
  if AKey = 0 then begin
    Result := false;
    Exit;
  end;

  FCS.Enter;
  try
    pItem := Pointer(AKey);
    Result := FList.InsertByOrder(pItem, CompareLogic, true) = -1;
  finally
    FCS.Leave;
  end;
end;

procedure TKeyList.Clear;
begin
  FCS.Enter;
  try
    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TKeyList.Create;
begin
  inherited;

  FList := TBinaryList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TKeyList.Destroy;
begin
  Clear;

  FreeAndNil(FList);
  FreeAndNil(FCS);

  inherited;
end;

function TKeyList.GetCount: integer;
begin
  FCS.Enter;
  try
    Result := FList.Count;
  finally
    FCS.Leave;
  end;
end;

end.
