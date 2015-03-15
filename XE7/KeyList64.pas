unit KeyList64;

interface

uses
  BinaryList, 
  Windows, Classes, SysUtils, SyncObjs;

type
  TKeyList64 = class
  private
    FList : TBinaryList;
    FCS : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function CheckDuplicate(AKey:int64):boolean;
  end;

implementation

function CompareLogic(Item1, Item2: Pointer): Integer;
var
  pItem1 : ^int64 absolute Item1;
  pItem2 : ^int64 absolute Item2;
begin
  if pItem1^ > pItem2^ then Result := 1
  else if pItem1^ < pItem2^ then Result := -1
  else Result := 0;
end;

{ TKeyList64 }

function TKeyList64.CheckDuplicate(AKey: int64): boolean;
var
  pItem : ^int64;
begin
  if AKey = 0 then begin
    Result := false;
    Exit;
  end;

  FCS.Enter;
  try
    // Todo : 생각보다 메모리 잠식이 심각하다.
    // 크게 한 거번에 메모리를 할당 받고, 그 안에서 나눠서 써야 한다.
    New(pItem);
    pItem^ := AKey;

    Result := FList.InsertByOrder(pItem, CompareLogic, true) = -1;

    if Result then Dispose(pItem);
  finally
    FCS.Leave;
  end;
end;

procedure TKeyList64.Clear;
var
  Loop : integer;
  pItem : ^int64;
begin
  FCS.Enter;
  try
    for Loop := 0 to FList.Count-1 do begin
      pItem := FList.Items[Loop];
      Dispose(pItem);
    end;

    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TKeyList64.Create;
begin
  inherited;

  FList := TBinaryList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TKeyList64.Destroy;
begin
  Clear;

  FreeAndNil(FList);
  FreeAndNil(FCS);

  inherited;
end;

end.
