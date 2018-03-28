unit RingBuffer;

interface

uses
  Windows, Classes, SysUtils;

type
  TRingBuffer = class
  private
    FBuffer : pointer;
    FIndex : integer;
    FItemSize, FCapacity : integer;
  public
    constructor Create(AItemSize,ACapacity:integer); reintroduce; virtual;
    destructor Destroy; override;

    function Get:pointer;
  end;

implementation

{ TRingBuffer }

constructor TRingBuffer.Create(AItemSize, ACapacity: integer);
begin
  inherited Create;

  FIndex := 0;
  FItemSize := AItemSize;
  FCapacity := ACapacity;

  // TODO: Array ÀÌ¿כ
  GetMem(FBuffer, AItemSize*ACapacity);
end;

destructor TRingBuffer.Destroy;
begin
  FreeMem(FBuffer);

  inherited;
end;

function TRingBuffer.Get: pointer;
var
  pResult : PByte;
  iIndex : integer;
begin
  iIndex := InterlockedIncrement(FIndex);

  if iIndex >= FCapacity then begin
    InterlockedCompareExchange(FIndex, iIndex mod FCapacity, iIndex);
    iIndex := iIndex mod FCapacity;
  end;

  pResult := FBuffer;
  Inc(pResult, iIndex * FItemSize);

  Result := pResult;
end;

end.
