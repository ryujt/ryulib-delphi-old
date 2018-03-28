unit SpeedGun;

interface

uses
  QueryPerformance, DynamicQueue,
  Windows, Classes, SysUtils;

type
  TSpeedGun = class
  private
    FCapacity : integer;
    FSpeed : integer;
    FTime : integer;
    FOldTick : int64;
    FDynamicQueue : TDynamicQueue;
    procedure do_Clear;
    procedure do_RemoveOldData;
  private
    function GetSpeed: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(ACapacity:integer);
    procedure IncSpeed(ASpeed:integer);
  public
    property Capacity : integer read FCapacity;
    property Speed : integer read GetSpeed;
  end;

implementation

type
  TSpeedInfo = class
    Time : integer;
    Speed : integer;
    constructor Create(ATime:integer; ASpeed:integer); reintroduce;
  end;

{ TSpeedInfo }

constructor TSpeedInfo.Create(ATime: integer; ASpeed: integer);
begin
  Time := ATime;
  Speed := ASpeed;
end;

{ TSpeedGun }

constructor TSpeedGun.Create;
begin
  inherited;

  FTime  := 0;
  FSpeed := 0;
  FOldTick := 0;

  FDynamicQueue := TDynamicQueue.Create(true);
end;

destructor TSpeedGun.Destroy;
begin
  do_Clear;

  FreeAndNil(FDynamicQueue);

  inherited;
end;

procedure TSpeedGun.do_Clear;
var
  SpeedInfo : TSpeedInfo;
begin
  while FDynamicQueue.Pop( Pointer(SpeedInfo) ) do SpeedInfo.Free;
end;

procedure TSpeedGun.do_RemoveOldData;
var
  SpeedInfo : TSpeedInfo;
begin
  while FTime > FCapacity do begin
    if not FDynamicQueue.Pop( Pointer(SpeedInfo) ) then Break;

    try
      FTime  := FTime  - SpeedInfo.Time;
      FSpeed := FSpeed - SpeedInfo.Speed;
    finally
      SpeedInfo.Free;
    end;
  end;
end;

function TSpeedGun.GetSpeed: integer;
begin
  if FTime > 1000 then Result := FSpeed div (FTime div 1000)
  else Result := 0;

  do_RemoveOldData;
end;

procedure TSpeedGun.IncSpeed(ASpeed: integer);
var
  Tick : int64;
  Time : integer;
begin
  Tick := GetTick;
  Time := Tick - FOldTick;

  if Time < 0 then begin
    FOldTick := Tick;
    Exit;
  end;

  FTime  := FTime  + Time;
  FSpeed := FSpeed + ASpeed;

  FOldTick := Tick;

  FDynamicQueue.Push( TSpeedInfo.Create(Time, ASpeed) );
end;

procedure TSpeedGun.Start(ACapacity: integer);
begin
  do_Clear;

  FCapacity := ACapacity;

  FTime  := 0;
  FSpeed := 0;
  FOldTick := GetTick;
end;

end.
