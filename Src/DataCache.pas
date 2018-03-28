unit DataCache;

interface

uses
  DataCache.MemoryBuffer,
  DataCache.FileBuffer,
  Windows, Classes, SysUtils;

const
  _MemoryBufferSize = 16 * 1024;

type
  TDataCache = class
  strict private
    FMemoryBuffer : TMemoryBuffer;
    FFileBuffer : TFileBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AKey:int64; AData:pointer; ASize:integer);
    function Get(AKey:int64; var AData:pointer; var ASize:integer):boolean;
  end;

implementation

{ TDataCache }

procedure TDataCache.Add(AKey: int64; AData: pointer; ASize: integer);
begin
  if (AKey = 0) or (ASize = 0) then Exit;
  
  FMemoryBuffer.Add(AKey, AData, ASize);
  FFileBuffer.Add(AKey, AData, ASize);
end;

procedure TDataCache.Clear;
begin
  FMemoryBuffer.Clear;
  FFileBuffer.Clear;
end;

constructor TDataCache.Create;
begin
  inherited;

  FMemoryBuffer := TMemoryBuffer.Create(_MemoryBufferSize);
  FFileBuffer := TFileBuffer.Create;
end;

destructor TDataCache.Destroy;
begin
  FreeAndNil(FMemoryBuffer);
  FreeAndNil(FFileBuffer);

  inherited;
end;

function TDataCache.Get(AKey: int64; var AData: pointer; var ASize: integer): boolean;
begin
  Result := FMemoryBuffer.Get(AKey, AData, ASize);
  if not Result then begin
    Result := FFileBuffer.Get(AKey, AData, ASize);
    if Result then FMemoryBuffer.Add(AKey, AData, ASize);
  end;
end;

end.
