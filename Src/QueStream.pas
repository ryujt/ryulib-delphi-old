unit QueStream;

interface

uses
  SysUtils, Classes, Dialogs;

type
  TQueStream = class
  private
    FBuffer : TMemoryStream;
    procedure do_Packing;
    function GetSize: int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(AData:pointer; ASize:integer);
    function Read(var AData:pointer; ASize:integer):boolean;

    property Size : int64 read GetSize;
  end;

implementation

{ TQueStream }

constructor TQueStream.Create;
begin
  inherited;

  FBuffer := TMemoryStream.Create;
end;

destructor TQueStream.Destroy;
begin
  FBuffer.Free;

  inherited;
end;

procedure TQueStream.do_Packing;
var
  PackingSize : int64;
  msData : TMemoryStream;
begin
  PackingSize := FBuffer.Size - FBuffer.Position;
  if PackingSize <= 0 then Exit;  

  msData := TMemoryStream.Create;
  try
  try
    msData.CopyFrom(FBuffer, PackingSize);
    msData.Position := 0;

    FBuffer.Clear;
    FBuffer.CopyFrom(msData, msData.Size);
  except on E: Exception do
     ShowMessage(Format('PackingSize: %d, FBuffer.Size: %d, FBuffer.Position: %d', [PackingSize, FBuffer.Size, FBuffer.Position]));
  end;
  finally
    msData.Free;
  end;
end;

function TQueStream.GetSize: int64;
begin
  Result := FBuffer.Size;
end;

function TQueStream.Read(var AData: pointer; ASize: integer): boolean;
begin
  Result := FBuffer.Size >= ASize;
  if not Result then Exit;

  FBuffer.Position := 0;

  GetMem(AData, ASize);
  FBuffer.Read(AData^, ASize);

  do_Packing;
end;

procedure TQueStream.Write(AData: pointer; ASize: integer);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.Write(AData^, ASize);
end;

end.
