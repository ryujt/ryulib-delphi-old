unit MemoryPool;

interface

uses
  DebugTools, LazyRelease, Interlocked,
  Windows, SysUtils, Classes;

const
  /// 메모리 풀 크기가 상수를 넘으면 상수 단위로 페이징 한다.
  POOL_UNIT_SIZE = 1024 * 1024 * 32;

  {*
    경계 조건에서 실수가 있더라도 A.V. 에러가 나지 않도록 여유를 둔다.
    2014.10.04 RyuSocket에서는 2KB 단위로 페이징을 한다.  그보다 작은 수치 때문에 에러가 발생했었다.
    현재의 SAFE_ZONE 메모리 할당하는 최대 크기보다 작으면 곤란해 진다.
  }
  SAFE_ZONE = 32 * 1024;

type
  TMemoryPool = class abstract
  private
  public
    procedure GetMem(var AData:pointer; ASize:integer); overload; virtual; abstract;
    function GetMem(ASize:integer):pointer; overload; virtual; abstract;
  end;

  TMemoryPool64 = class (TMemoryPool)
  private
    FPoolSize : int64;
    FPools : array of pointer;
    FIndex : int64;

    // FIndex가 한계를 넘어서 마이너스로 가지 않도록 수정
    procedure do_ResetIndex;
  public
    constructor Create(APoolSize:int64); reintroduce;
    destructor Destroy; override;

    procedure GetMem(var AData:pointer; ASize:integer); overload; override;
    function GetMem(ASize:integer):pointer; overload; override;
  end;

  TMemoryPool32 = class (TMemoryPool)
  private
    FPoolSize : integer;
    FPools : array of pointer;
    FIndex : integer;

    // FIndex가 한계를 넘어서 마이너스로 가지 않도록 수정
    procedure do_ResetIndex;
  public
    constructor Create(APoolSize:integer); reintroduce;
    destructor Destroy; override;

    procedure GetMem(var AData:pointer; ASize:integer); overload; override;
    function GetMem(ASize:integer):pointer; overload; override;
  end;

/// 전역에서 사용 할 수 있는 메모리 풀 생성
procedure CreateMemoryPool(APoolSize:int64);

function GetMemory(ASize:integer):pointer; overload;
procedure GetMemory(var AData:pointer; ASize:integer); overload;

function CloneMemory(AData:pointer; ASize:integer):pointer;

implementation

var
  MemoryPoolObject : TMemoryPool = nil;

procedure CreateMemoryPool(APoolSize:int64);
begin
  {$IFDEF CPUX86}
    MemoryPoolObject := TMemoryPool32.Create(APoolSize);
  {$ENDIF}

  {$IFDEF CPUX64}
    MemoryPoolObject := TMemoryPool64.Create(APoolSize);
  {$ENDIF}
end;

function GetMemory(ASize:integer):pointer; overload;
begin
  Result := MemoryPoolObject.GetMem(ASize);
end;

procedure GetMemory(var AData:pointer; ASize:integer); overload;
begin
  MemoryPoolObject.GetMem(AData, ASize);
end;

function CloneMemory(AData:pointer; ASize:integer):pointer;
begin
  Result := MemoryPoolObject.GetMem(ASize);
  Move(AData^, Result^, ASize);
end;

{ TMemoryPool64 }

constructor TMemoryPool64.Create(APoolSize:int64);
var
  Loop: Integer;
begin
  inherited Create;

  FPoolSize := APoolSize;

  if APoolSize <= POOL_UNIT_SIZE then begin
    SetLength( FPools, 1 );
    System.GetMem( FPools[0], POOL_UNIT_SIZE + SAFE_ZONE );
  end else begin
    SetLength( FPools, ((APoolSize-1) div POOL_UNIT_SIZE) + 1 );
    for Loop := Low(FPools) to High(FPools) do System.GetMem( FPools[Loop], POOL_UNIT_SIZE + SAFE_ZONE );
  end;

  FIndex := 0;
end;

destructor TMemoryPool64.Destroy;
var
  Loop: Integer;
begin
  for Loop := Low(FPools) to High(FPools) do System.FreeMem( FPools[Loop] );

  inherited;
end;

procedure TMemoryPool64.do_ResetIndex;
var
  iIndex, iDiv, iMod : int64;
begin
  iIndex := 0;
  InterlockedCompareExchange64(iIndex, FIndex, 0);

  if iIndex > (FPoolSize  * 2) then begin
    iDiv := (iIndex div POOL_UNIT_SIZE);
    iDiv := (iDiv mod Length(FPools));

    iMod := iIndex mod POOL_UNIT_SIZE;

    // "iMod = iIndex mod FPoolSize" 처럼 계산했으나,
    // 이경우 같은 Pool Unit에 다시 배정되어 방금 전 메모리를 덮어 쓸 위험이 있다.
    InterlockedCompareExchange64(FIndex, (iDiv * POOL_UNIT_SIZE) + iMod, iIndex);

    {$IFDEF DEBUG}
    Trace( Format('TBasicMemoryPool.do_ResetIndex - FIndex: %d, iMod: %d, iIndex: %d', [FIndex, iMod, iIndex]) );
    {$ENDIF}
  end;
end;

function TMemoryPool64.GetMem(ASize: integer): pointer;
begin
  Self.GetMem( Result, ASize );
end;

procedure TMemoryPool64.GetMem(var AData: pointer; ASize: integer);
var
  iIndex, iDiv, iMod : int64;
begin
  AData := nil;

  if ASize <= 0 then Exit;

  if ASize > SAFE_ZONE then
    raise Exception.Create( Format('TBasicMemoryPool.GetMem - ASize > %d KB', [SAFE_ZONE div 1024]) );

  iIndex := InterlockedExchangeAdd64( FIndex, ASize );

  iDiv := iIndex div POOL_UNIT_SIZE;
  iMod := iIndex mod POOL_UNIT_SIZE;

  AData := FPools[iDiv mod Length(FPools)];

  Inc( PByte(AData), iMod );

  do_ResetIndex;
end;

{ TMemoryPool32 }

constructor TMemoryPool32.Create(APoolSize:integer);
var
  Loop: Integer;
begin
  inherited Create;

  FPoolSize := APoolSize;

  if APoolSize <= POOL_UNIT_SIZE then begin
    SetLength( FPools, 1 );
    System.GetMem( FPools[0], POOL_UNIT_SIZE + SAFE_ZONE );
  end else begin
    SetLength( FPools, ((APoolSize-1) div POOL_UNIT_SIZE) + 1 );
    for Loop := Low(FPools) to High(FPools) do System.GetMem( FPools[Loop], POOL_UNIT_SIZE + SAFE_ZONE );
  end;

  FIndex := 0;
end;

destructor TMemoryPool32.Destroy;
var
  Loop: Integer;
begin
  for Loop := Low(FPools) to High(FPools) do System.FreeMem( FPools[Loop] );

  inherited;
end;

procedure TMemoryPool32.do_ResetIndex;
var
  iIndex, iDiv, iMod : integer;
begin
  iIndex := FIndex;

  if iIndex > (FPoolSize  * 2) then begin
    iDiv := (iIndex div POOL_UNIT_SIZE);
    iDiv := (iDiv mod Length(FPools));

    iMod := iIndex mod POOL_UNIT_SIZE;

    // "iMod = iIndex mod FPoolSize" 처럼 계산했으나,
    // 이경우 같은 Pool Unit에 다시 배정되어 방금 전 메모리를 덮어 쓸 위험이 있다.
    InterlockedCompareExchange(FIndex, (iDiv * POOL_UNIT_SIZE) + iMod, iIndex);

    {$IFDEF DEBUG}
    Trace( Format('TBasicMemoryPool.do_ResetIndex - FIndex: %d, iMod: %d, iIndex: %d', [FIndex, iMod, iIndex]) );
    {$ENDIF}
  end;
end;

function TMemoryPool32.GetMem(ASize: integer): pointer;
begin
  Self.GetMem( Result, ASize );
end;

procedure TMemoryPool32.GetMem(var AData: pointer; ASize: integer);
var
  iIndex, iDiv, iMod : integer;
begin
  AData := nil;

  if ASize <= 0 then Exit;

  if ASize > SAFE_ZONE then
    raise Exception.Create( Format('TBasicMemoryPool.GetMem - ASize > %d KB', [SAFE_ZONE div 1024]) );

  iIndex := InterlockedExchangeAdd( FIndex, ASize );

  iDiv := iIndex div POOL_UNIT_SIZE;
  iMod := iIndex mod POOL_UNIT_SIZE;

  AData := FPools[iDiv mod Length(FPools)];

  Inc( PByte(AData), iMod );

  do_ResetIndex;
end;

end.
