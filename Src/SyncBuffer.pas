unit SyncBuffer;

interface

uses
  TickCount,
  Classes, SysUtils, SyncObjs;

type
  TSyncBuffer = class
  private
    FTick : int64;
    FCS : TCriticalSection;
  private
    FList : TList;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AData:pointer; ASize:integer);
    function Get(var AData:pointer; var ASize:integer):boolean;
    procedure Sync(ATick:int64);

    property Count : integer read GetCount;
  end;

implementation

type
  TItem = class
  private
  public
    Tick : int64;
    Data : pointer;
    Size : integer;
    constructor Create(AData:pointer; ASize:integer); reintroduce;
  end;

{ TItem }

constructor TItem.Create(AData:pointer; ASize:integer);
begin
  inherited Create;

  Tick := TTickCount.Obj.Get;

  Size := ASize;

  if Size <= 0 then begin
    Size := 0;
    Data := nil;
  end else begin
    GetMem(Data, Size);
    Move(AData^, Data^, Size);
  end;
end;

{ TSyncBuffer }

procedure TSyncBuffer.Add(AData: pointer; ASize: integer);
begin
  FCS.Enter;
  try
    if ASize <= 0 then Exit;
    FList.Add(TItem.Create(AData, ASize));
  finally
    FCS.Leave;
  end;
end;

procedure TSyncBuffer.Clear;
var
  Item : TItem;
  Loop : Integer;
begin
  FCS.Enter;
  try
    for Loop := FList.Count-1 downto 0 do begin
      Item := Pointer(FList[Loop]);
      if Item.Data <> nil then FreeMem(Item.Data);
      Item.Free;
    end;

    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TSyncBuffer.Create;
begin
  inherited;

  FTick := -1;
  FList := TList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TSyncBuffer.Destroy;
begin
  Clear;

  FList.Free;
  FCS.Free;

  inherited;
end;

function TSyncBuffer.Get(var AData: pointer; var ASize: integer): boolean;
var
  Item : TItem;
begin
  FCS.Enter;
  try
    AData := nil;
    ASize := 0;
    Result := false;
    if FList.Count = 0 then Exit;

    Item := Pointer(FList[0]);
    if Item.Tick > FTick then Exit;

    try
      FList.Delete(0);

      ASize := Item.Size;
      AData := Item.Data;
    finally
      Item.Free;
    end;

    Result := true;
  finally
    FCS.Leave;
  end;
end;

function TSyncBuffer.GetCount: integer;
begin
  FCS.Enter;
  try
    Result := FList.Count;
  finally
    FCS.Leave;
  end;
end;

procedure TSyncBuffer.Sync(ATick: int64);
begin
  FCS.Enter;
  try
    FTick := ATick;
  finally
    FCS.Leave;
  end;
end;

end.

