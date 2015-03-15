unit MemoryBuffer;

interface

uses
  SysUtils, Classes;

const
  DEFAULT_CAPACITY = 16 * 1024;

type
  /// TMemoryStream을 이용해서 패킷을 수신 처리 할 때 알 수 없는 에러가 나서 대체 클래스를 제작하였다.
  TMemoryBuffer = class
  private
    FCapacity : integer;
  private
    FMemory: pointer;
    FSize: integer;
  public
    constructor Create(ACapacity:integer=0); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure Write(AData:pointer; ASize:integer);
  public
    property Size : integer read FSize;
    property Memory : pointer read FMemory;
  end;

implementation

{ TMemoryBuffer }

procedure TMemoryBuffer.Clear;
begin
  FSize := 0;
end;

constructor TMemoryBuffer.Create(ACapacity:integer);
begin
  inherited Create;

  if ACapacity < DEFAULT_CAPACITY then FCapacity := DEFAULT_CAPACITY
  else FCapacity := ACapacity;

  GetMem( FMemory, FCapacity );

  Clear;
end;

destructor TMemoryBuffer.Destroy;
begin
  FreeMem( FMemory );

  inherited;
end;

procedure TMemoryBuffer.Write(AData: pointer; ASize: integer);
var
  iNewSize : integer;
  pCurrent : PByte;
  NewMemory, Temp : pointer;
begin
  iNewSize := FSize + ASize;

  if iNewSize > FCapacity then begin
    FCapacity := FCapacity + DEFAULT_CAPACITY * ( 1 + (ASize div DEFAULT_CAPACITY));
    GetMem( NewMemory, FCapacity );
    Move( FMemory^, NewMemory^, FSize );

    Temp := FMemory;
    FMemory := NewMemory;
    FreeMem( Temp );
  end;

  pCurrent := FMemory;  Inc( pCurrent, FSize );

  Move( AData^, pCurrent^, ASize);

  FSize := iNewSize;
end;

end.
