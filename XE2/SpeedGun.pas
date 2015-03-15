unit SpeedGun;

interface

uses
  Windows, Classes, SysUtils, SyncObjs;

const
  // 어느 정도 시간 동안의 데이터를 평균 낼 것 인가?
  _DefaultTimeLength = 60 * 1000;

type
  TSpeedGun = class
  private
    FTickBuffer : TObject;
    FTickQueue : TObject;
    FCS : TCriticalSection;
  private
    function GetSpeed: int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(ACapacity:int64=_DefaultTimeLength);
    procedure Stop;

    procedure IncSize(ASize:int64);

    property Speed : int64 read GetSpeed;
  end;

implementation

uses
  Queue;

const
  _TickBufferTimeLimit = 10;

var
  Frequency : int64;

type
  TTickBuffer = class
  strict private
    FOldPCount : int64;
    FTick : int64;
    FSize : int64;
    function GetTick: int64;
  public
    constructor Create;

    procedure Clear;
    procedure AddSize(ASize:int64);

    procedure Get(var ATick:Cardinal; var ASize:int64);

    property Tick : int64 read GetTick;
  end;

  TSpeedData = class
  strict private
    FTick: int64;
    FSize: int64;
  public
    constructor Create(ATick:Cardinal; ASize:integer); reintroduce;

    property Tick : int64 read FTick;
    property Size : int64 read FSize;
  end;

  TTickQueue = class
  strict private
    FCapacity : int64;
    FQueue : TQueue;
    procedure do_Clear;
  strict private
    FTime: int64;
    FSize: int64;
  public
    constructor Create(ACapacity:int64); reintroduce;
    destructor Destroy; override;

    procedure AddSize(ATick:Cardinal; ASize:int64);

    property Time : int64 read FTime;
    property Size : int64 read FSize;
  end;

{ TSpeedGun }

function getTickBuffer(Obj:TObject):TTickBuffer;
begin
  Result := Pointer(Obj);
end;

function getTickQueue(Obj:TObject):TTickQueue;
begin
  Result := Pointer(Obj);
end;

procedure TSpeedGun.Start(ACapacity:int64=_DefaultTimeLength);
begin
  FCS.Enter;
  try
    if FTickQueue = nil then FTickQueue := TTickQueue.Create(ACapacity);
    getTickBuffer(FTickBuffer).Clear;
  finally
    FCS.Leave;
  end;
end;

procedure TSpeedGun.Stop;
begin
  FCS.Enter;
  try
    if FTickQueue <> nil then FreeAndNil(FTickQueue);
    getTickBuffer(FTickBuffer).Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TSpeedGun.Create;
begin
  inherited;

  FTickQueue := nil;

  FCS := TCriticalSection.Create;
  FTickBuffer := TTickBuffer.Create;
end;

destructor TSpeedGun.Destroy;
begin
  Stop;

  FreeAndNil(FTickBuffer);
  FreeAndNil(FCS);

  inherited;
end;

function TSpeedGun.GetSpeed: int64;
begin
  FCS.Enter;
  try
    if FTickQueue = nil then begin
      Result := 0;
      Exit;
    end;

    Result := getTickQueue(FTickQueue).Time;
    if Result = 0 then Exit;

    // 초당 AddSize로 입력된 평균 값을 구한다.
    Result := (getTickQueue(FTickQueue).Size * 1000) div getTickQueue(FTickQueue).Time;
  finally
    FCS.Leave;
  end;
end;

procedure TSpeedGun.IncSize(ASize: int64);
var
  Tick : Cardinal;
  Size : int64;
begin
  FCS.Enter;
  try
    if FTickQueue = nil then
      raise Exception.Create(ClassName+'.IncSize: Start를 먼저 하고 사용하시기 바랍니다.');

    getTickBuffer(FTickBuffer).AddSize(ASize);

    if getTickBuffer(FTickBuffer).Tick > _TickBufferTimeLimit then begin
      getTickBuffer(FTickBuffer).Get(Tick, Size);
      getTickQueue(FTickQueue).AddSize(Tick, Size);
    end;
    
  finally
    FCS.Leave;
  end;
end;

//    if FTickQueue = nil then Exit;
//    getTickQueue(FTickQueue).AddSize(getTickBuffer(FTickBuffer).GetTotal);

{ TTickBuffer }

procedure TTickBuffer.AddSize(ASize: int64);
var
  PCount, Term : int64;
begin
  QueryPerformanceCounter(PCount);

  if PCount <= FOldPCount then Term := 0
  else Term := PCount - FOldPCount;

  FTick := FTick + (1000000 * Term div Frequency);

  FOldPCount := PCount;

  FSize := FSize + ASize;
end;

procedure TTickBuffer.Clear;
begin
  FTick := 0;
  FSize := 0;
  QueryPerformanceCounter(FOldPCount);
end;

constructor TTickBuffer.Create;
begin
  inherited;

  Clear;
end;

procedure TTickBuffer.Get(var ATick: Cardinal; var ASize: int64);
begin
  ATick := FTick div 1000;
  FTick := FTick - (ATick * 1000);
  if FTick < 0 then FTick := 0;

  ASize := FSize;
  FSize := 0;
end;

function TTickBuffer.GetTick: int64;
begin
  Result := FTick div 1000;
end;

{ TSpeedData }

constructor TSpeedData.Create(ATick: Cardinal; ASize: integer);
begin
  FTick := ATick;
  FSize := ASize;
end;

{ TTickQueue }

procedure TTickQueue.AddSize(ATick: Cardinal; ASize: int64);
var
  Item : TSpeedData;
begin
  if FQueue.IsFull then begin
     if FQueue.Pop(Pointer(Item)) then begin
       FTime := FTime - Item.Tick;
       FSize := FSize - Item.Size;
       Item.Free;
     end;
  end;

  // 지정된 시간 이상의 데이터는 삭제한다.
  while FTime > FCapacity do begin
     if not FQueue.Pop(Pointer(Item)) then Break;

     FTime := FTime - Item.Tick;
     FSize := FSize - Item.Size;
     Item.Free;
  end;

  FTime := FTime + ATick;
  FSize := FSize + ASize;

  FQueue.Push(TSpeedData.Create(ATick, ASize));
end;

constructor TTickQueue.Create(ACapacity:int64);
begin
  inherited Create;

  FCapacity := ACapacity;
  
  FTime := 0;
  FSize := 0;
  
  // _TickBufferTimeLimit 이상 간격을 두고 작동하기 때문에
  // _TickBufferTimeLimit으로 나눈 정도의 크기로만으로 충분하다.
  FQueue := TQueue.Create(ACapacity div _TickBufferTimeLimit);
end;

destructor TTickQueue.Destroy;
begin
  do_Clear;
  
  FreeAndNil(FQueue);

  inherited;
end;

procedure TTickQueue.do_Clear;
var
  Item : TSpeedData;
begin
  while not FQueue.IsEmpty do begin
    FQueue.Pop(Pointer(Item));
    Item.Free;
  end;
end;

initialization
  QueryPerformanceFrequency(Frequency);
end.
