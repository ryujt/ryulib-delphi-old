unit DataCache.FileBuffer;

interface

uses
  SystemFolder, Disk, BinaryList,
  Windows, Classes, SysUtils;

type
  TFileBuffer = class
  strict private
    FFileName : string;
    FStream : TFileStream;
    FList : TBinaryList;
    procedure read_Data(APosition:int64; var AData:pointer; ASize:integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AKey:int64; AData:pointer; ASize:integer);
    function Get(AKey:int64; var AData:pointer; var ASize:integer):boolean;
  end;

implementation

type
  TItem = class
  public
    Key : int64;
    Position : int64;
    Size : integer;
    constructor Create(AKey:int64; APosition:int64; ASize:integer); reintroduce;
  end;

{ TItem }

constructor TItem.Create(AKey:int64; APosition:int64; ASize:integer);
begin
  inherited Create;

  Key := AKey;
  Position := APosition;
  Size := ASize;
end;

function CompareLogic(Item1, Item2: Pointer): Integer;
var
  pItem1 : TItem absolute Item1;
  pItem2 : TItem absolute Item2;
begin
  if pItem1.Key > pItem2.Key then Result := 1
  else if pItem1.Key < pItem2.Key then Result := -1
  else Result := 0;
end;

{ TFileBuffer }

procedure TFileBuffer.Add(AKey: int64; AData: pointer; ASize: integer);
var
  Item : TItem;
begin
  Item := TItem.Create(AKey, FStream.Position, ASize);
  if FList.InsertByOrder(Item, CompareLogic, true) = -1 then begin
    Item.Free;
  end else begin
    FStream.Write(AData^, ASize);
  end;
end;

procedure TFileBuffer.Clear;
var
  Loop : integer;
begin
  FStream.Position := 0;

  for Loop := 0 to FList.Count-1 do TObject(FList.Items[Loop]).Free;
  FList.Clear;
end;

constructor TFileBuffer.Create;
begin
  inherited Create;

  FFileName := GetRandomTempFileName;

  FList := TBinaryList.Create;
  FStream := TFileStream.Create(FFileName, fmCreate or fmOpenReadWrite);
end;

destructor TFileBuffer.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FList);

  EraseFile(FFileName);

  inherited;
end;

function TFileBuffer.Get(AKey: int64; var AData: pointer;
  var ASize: integer): boolean;
var
  Item : TItem;
  iIndex : integer;
begin
  Item := TItem.Create(AKey, 0, 0);
  try
    iIndex := FList.Search(Item, CompareLogic);
  finally
    Item.Free;
  end;

  Result := iIndex >= 0;

  if Result then begin
    Item := Pointer(FList.Items[iIndex]);

    if Item.Size = 0 then begin
      AData := nil;
    end else begin
      read_Data(Item.Position, AData, Item.Size);
    end;

    ASize := Item.Size;
  end;
end;

procedure TFileBuffer.read_Data(APosition: int64; var AData: pointer;
  ASize: integer);
var
  OldPosition : int64;
begin
  if ASize = 0 then begin
    AData := nil;
    Exit;
  end;

  GetMem(AData, ASize);

  OldPosition := FStream.Position;
  try
    FStream.Position := APosition;
    FStream.Read(AData^, ASize);
  finally
    FStream.Position := OldPosition;
  end;
end;

end.
