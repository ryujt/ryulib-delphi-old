unit MemoryReader;

interface

uses
  MemoryBuffer,
  Classes, SysUtils;

type
  TMemoryReader = class
  private
    FBuffer : TMemoryBuffer;
    function GetSize: int64;
    function GetMemory: pointer;
    function GetCanRead: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Write(AData:pointer; ASize:integer);
    function Remove(ASize:integer):boolean;
    function Read(var AData:pointer; ASize:integer):boolean;

    property CanRead : boolean read GetCanRead;
    property Memory : pointer read GetMemory;
    property Size : int64 read GetSize;
  end;

implementation

{ TMemoryReader }

procedure TMemoryReader.Clear;
begin
  FBuffer.Clear;
end;

constructor TMemoryReader.Create;
begin
  inherited;

  FBuffer := TMemoryBuffer.Create;
end;

destructor TMemoryReader.Destroy;
begin
  FreeAndNil(FBuffer);

  inherited;
end;

function TMemoryReader.GetCanRead: boolean;
var
  pSize : ^Integer;
begin
  Result := false;
  if FBuffer.Size < SizeOf(Integer) then Exit;

  pSize := FBuffer.Memory;
  Result := FBuffer.Size >= (SizeOf(Integer) + pSize^);
end;

function TMemoryReader.GetMemory: pointer;
begin
  Result := FBuffer.Memory;
end;

function TMemoryReader.GetSize: int64;
begin
  Result := FBuffer.Size;
end;

function TMemoryReader.Read(var AData: pointer; ASize: integer): boolean;
var
  iRemain : integer;
  pBuffer : PByte;
  Temp : pointer;
begin
  AData := nil;

  Result := (ASize > 0) and (FBuffer.Size >= ASize);
  if not Result then Exit;

  GetMem(AData, ASize);
  Move(FBuffer.Memory^, AData^, ASize);

  iRemain := FBuffer.Size - ASize;
  if iRemain <= 0 then begin
    FBuffer.Clear;
    Exit;
  end;

  pBuffer := FBuffer.Memory;
  Inc(pBuffer, ASize);

  GetMem(Temp, iRemain);
  try
    Move(pBuffer^, Temp^, iRemain);
    FBuffer.Clear;
    FBuffer.Write(Temp, iRemain);
  finally
    FreeMem(Temp);
  end;
end;

function TMemoryReader.Remove(ASize: integer): boolean;
var
  iRemain : integer;
  pBuffer : PByte;
  Temp : pointer;
begin
  Result := (ASize > 0) and (FBuffer.Size >= ASize);
  if not Result then Exit;

  iRemain := FBuffer.Size - ASize;
  if iRemain <= 0 then begin
    FBuffer.Clear;
    Exit;
  end;

  pBuffer := FBuffer.Memory;
  Inc(pBuffer, ASize);

  GetMem(Temp, iRemain);
  try
    Move(pBuffer^, Temp^, iRemain);
    FBuffer.Clear;
    FBuffer.Write(Temp, iRemain);
  finally
    FreeMem(Temp);
  end;
end;

procedure TMemoryReader.Write(AData: pointer; ASize: integer);
begin
  FBuffer.Write(AData, ASize);
end;

end.
