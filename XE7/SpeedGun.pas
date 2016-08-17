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
    Tick : int64;
    Speed : integer;
    constructor Create(ATick:int64; ASpeed:integer); reintroduce;
  end;

{ TSpeedInfo }

constructor TSpeedInfo.Create(ATick: int64; ASpeed: integer);
begin
  Tick := ATick;
  Speed := ASpeed;
end;

{ TSpeedGun }

constructor TSpeedGun.Create;
begin
  inherited;

  FSpeed := 0;

  FDynamicQueue := TDynamicQueue.Create(true);
end;

destructor TSpeedGun.Destroy;
var
  SpeedInfo : TSpeedInfo;
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
  Tick : int64;
  SpeedInfo : TSpeedInfo;
begin
  Tick := GetTick;

  while true do begin
    SpeedInfo := Pointer( FDynamicQueue.Peek );
    if SpeedInfo = nil then Break;

    if SpeedInfo.Tick > Tick then Break;
    if (Tick-SpeedInfo.Tick) <= FCapacity then Break;

    if FDynamicQueue.Pop( Pointer(SpeedInfo) ) then begin
      try
        FSpeed := FSpeed - SpeedInfo.Speed;
      finally
        SpeedInfo.Free;
      end;
    end;
  end;
end;

function TSpeedGun.GetSpeed: integer;
begin
  do_RemoveOldData;
  Result := FSpeed;
end;

procedure TSpeedGun.IncSpeed(ASpeed: integer);
begin
  FSpeed := FSpeed + ASpeed;
  FDynamicQueue.Push( TSpeedInfo.Create(GetTick, ASpeed) );
end;

procedure TSpeedGun.Start(ACapacity: integer);
begin
  FCapacity := ACapacity;
  FSpeed := 0;
  do_Clear;
end;

end.
