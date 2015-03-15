unit DataCache.MemoryBuffer;

interface

uses
  Queue, 
  Windows, Classes, SysUtils;

type
  TMemoryBuffer = class
  strict private
    FQueue : TQueue;
  public
    constructor Create(ACapacity:integer); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AKey:int64; AData:pointer; ASize:integer);
    function Get(AKey:int64; var AData:pointer; var ASize:integer):boolean;
  end;

implementation

type
  TItem = class
  private
  public
    Key : int64;
    Data : pointer;
    Size : integer;
    constructor Create(AKey:int64; AData:pointer; ASize:integer); reintroduce;
    destructor Destroy; override;

    procedure Copy(var AData:pointer; var ASize:integer);
  end;

{ TItem }

procedure TItem.Copy(var AData: pointer; var ASize: integer);
begin
  if Data = nil then begin
    AData := nil;
  end else begin
    GetMem(AData, Size);
    Move(Data^, AData^, Size);
  end;

  ASize := Size;
end;

constructor TItem.Create(AKey: int64; AData: pointer; ASize: integer);
begin
  inherited Create;

  Key := AKey;

  if ASize = 0 then begin
    Data := nil
  end else begin
    GetMem(Data, ASize);
    Move(AData^, Data^, ASize);
  end;

  Size := ASize;
end;

destructor TItem.Destroy;
begin
  if Data <> nil then FreeMem(Data);

  inherited;
end;

{ TMemoryBuffer }

procedure TMemoryBuffer.Add(AKey: int64; AData: pointer; ASize: integer);
var
  Item : TItem;
begin
  if FQueue.IsFull then begin
    if FQueue.Pop(Pointer(Item)) then Item.Free;
  end;
  
  FQueue.Push(TItem.Create(AKey, AData, ASize));
end;

procedure TMemoryBuffer.Clear;
var
  Loop : integer;
begin
  for Loop := 0 to FQueue.Count-1 do TObject(FQueue.Items[Loop]).Free;
  FQueue.Clear;
end;

constructor TMemoryBuffer.Create(ACapacity: integer);
begin
  inherited Create;

  FQueue := TQueue.Create(ACapacity);
end;

destructor TMemoryBuffer.Destroy;
begin
  FreeAndNil(FQueue);

  inherited;
end;

function TMemoryBuffer.Get(AKey: int64; var AData: pointer;
  var ASize: integer): boolean;
var
  Item : TItem;
  Loop : integer;
begin
  AData := nil;
  ASize := 0;
  Result := false;

  // Todo : 해싱 또는 바이너리 서치로 변경
  for Loop := 0 to FQueue.Count-1 do begin
    Item := Pointer(FQueue.Items[Loop]);
    if Item.Key = AKey then begin
      Item.Copy(AData, ASize);

      Result := true;
      Break;
    end;
  end;
end;

end.
