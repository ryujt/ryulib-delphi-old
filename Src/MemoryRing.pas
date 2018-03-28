unit MemoryRing;

interface

uses
  LazyRelease, MemoryPool,
  Classes, SysUtils;

type
  {*
    지정된 크기의 원형 큐를 이용해서 메모리를 확보하고 사용한 이후
    바로 삭제하지 않고, 큐의 크기만큼 기다렸다가 삭제한다.  멀티 스레드 상황에서
    메모리의 라이프 사이클이 스레드들의 사용시간 보다 휠씬 길게하고, 임계영역을
    사용하지 않음으로, 성능을 개선한다.
  }
  TMemoryRing = class
  private
    FQueueSize : integer;
    FMemoryPool : TMemoryPool;
  private
    FLazyRelease : TLazyRelease;
    procedure on_Release(Sender:TObject; AObject:pointer);
  private
    function GetSize: integer;
  public
    constructor Create(AQueueSize:integer; ANeedThreadSafe:boolean); reintroduce;
    destructor Destroy; override;

    function GetMem(ASize:integer):TMemoryPage;

    property Size : integer read GetSize;
  end;

implementation

{ TMemoryRing }

constructor TMemoryRing.Create(AQueueSize:integer; ANeedThreadSafe:boolean);
begin
  inherited Create;

  FQueueSize := AQueueSize;

  FLazyRelease := TLazyRelease.Create(AQueueSize);
  FLazyRelease.OnRelease := on_Release;

  FMemoryPool := TMemoryPool.Create(ANeedThreadSafe);
end;

destructor TMemoryRing.Destroy;
begin
  FreeAndNil(FLazyRelease);
  FreeAndNil(FMemoryPool);

  inherited;
end;

function TMemoryRing.GetMem(ASize: integer): TMemoryPage;
begin
  Result := FMemoryPool.GetMem(ASize);
  FLazyRelease.Release(Result);
end;

function TMemoryRing.GetSize: integer;
begin
  Result := FMemoryPool.Size;
end;

procedure TMemoryRing.on_Release(Sender: TObject; AObject: pointer);
begin
  FMemoryPool.FreeMem(Pointer(AObject));
end;

end.
