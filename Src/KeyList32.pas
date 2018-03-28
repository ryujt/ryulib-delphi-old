unit KeyList32;

interface

uses
  BinaryList, 
  Windows, Classes, SysUtils, SyncObjs;

const
  _KeyCountLimit = 16*1024;

type
  TKeyList32 = class
  private
    FList : TList;
    FCS : TCriticalSection;
    procedure check_NeedAnotherKeys;
    function get_LastKeys:TBinaryList;
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

{ TKeyList32 }

function TKeyList32.CheckDuplicate(AKey: DWord): boolean;
var
  Loop : Integer;
  pItem : pointer;
  BinaryList : TBinaryList;
begin
  if AKey = 0 then begin
    Result := false;
    Exit;
  end;

  FCS.Enter;
  try
    pItem := Pointer(AKey);

    if FList.Count = 0 then FList.Add(TBinaryList.Create);

    // 기존에 저장된 키 리스트에서 중복되었는 지 검사
    for Loop := 0 to FList.Count-1 do begin
      BinaryList := Pointer(FList[Loop]);
      if BinaryList.Search(pItem, CompareLogic) <> -1 then begin
        Result := true;
        Exit;
      end;
    end;

    Result := get_LastKeys.InsertByOrder(pItem, CompareLogic, true) = -1;

    check_NeedAnotherKeys;
  finally
    FCS.Leave;
  end;
end;

procedure TKeyList32.check_NeedAnotherKeys;
begin
  if get_LastKeys.Count >= _KeyCountLimit then FList.Add(TBinaryList.Create);  
end;

procedure TKeyList32.Clear;
var
  Loop: Integer;
  BinaryList : TBinaryList;
begin
  FCS.Enter;
  try
    for Loop := 0 to FList.Count-1 do begin
      BinaryList := Pointer(FList[Loop]);
      BinaryList.Clear;
      BinaryList.Free;
    end;

    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TKeyList32.Create;
begin
  inherited;

  FList := TList.Create;
  FList.Add(TBinaryList.Create);
  
  FCS := TCriticalSection.Create;
end;

destructor TKeyList32.Destroy;
begin
  Clear;

  FreeAndNil(FList);
  FreeAndNil(FCS);

  inherited;
end;

function TKeyList32.GetCount: integer;
var
  Loop: Integer;
begin
  FCS.Enter;
  try
    Result := 0;

    for Loop := 0 to FList.Count-1 do
      Result := Result + TBinaryList(FList[Loop]).Count;
  finally
    FCS.Leave;
  end;
end;

function TKeyList32.get_LastKeys: TBinaryList;
begin
  if FList.Count = 0 then begin
    Result := nil;
    Exit;
  end;

  Result := Pointer(FList[FList.Count-1]);
end;

end.
