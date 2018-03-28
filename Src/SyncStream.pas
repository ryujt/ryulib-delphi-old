unit SyncStream;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TSyncStream = class
  private
    FBuffer : pointer;
    FSize : integer;
    FCS : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Read(var AData:pointer; var ASize:integer);
    procedure ReadAndClear(var AData:pointer; var ASize:integer);
    procedure Write(AData:pointer; ASize:integer);
  end;

implementation

{ TSyncStream }

procedure TSyncStream.Clear;
begin
  FCS.Enter;
  try
    if FBuffer = nil then Exit;

    FreeMem(FBuffer);
    FBuffer := nil;
  finally
    FCS.Leave;
  end;
end;

constructor TSyncStream.Create;
begin
  inherited;

  FBuffer := nil;
  FSize := 0;

  FCS := TCriticalSection.Create;
end;

destructor TSyncStream.Destroy;
begin
  Clear;

  FCS.Free;

  inherited;
end;

procedure TSyncStream.Read(var AData:pointer; var ASize:integer);
begin
  FCS.Enter;
  try
    if FBuffer = nil then begin
      AData := nil;
      ASize := 0;
      Exit;
    end;

    ASize := FSize;

    GetMem(AData, ASize);
    Move(FBuffer^, AData^, ASize);
  finally
    FCS.Leave;
  end;
end;

procedure TSyncStream.ReadAndClear(var AData: pointer; var ASize: integer);
begin
  FCS.Enter;
  try
    if FBuffer = nil then begin
      AData := nil;
      ASize := 0;
      Exit;
    end;

    ASize := FSize;

    GetMem(AData, ASize);
    Move(FBuffer^, AData^, ASize);

    FreeMem(FBuffer);
    FBuffer := nil;
  finally
    FCS.Leave;
  end;
end;

procedure TSyncStream.Write(AData:pointer; ASize:integer);
begin
  FCS.Enter;
  try
    if ASize <= 0 then Exit;
    if FBuffer <> nil then begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end;

    GetMem(FBuffer, ASize);
    Move(AData^, FBuffer^, ASize);

    FSize := ASize;
  finally
    FCS.Leave;
  end;
end;

end.

