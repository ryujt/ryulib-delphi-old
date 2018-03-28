unit TickCounter;

interface

uses
  Windows, Classes, SysUtils;

type
  TTickCounter = class
  private
    FOldTick : cardinal;
    FDuration : int64;
  public
    constructor Create;

    procedure Start;

    /// 방금 전 호출과의 시간 간격을 리턴한다.
    function Get:cardinal;

    /// Start 이후 지난 시간을 리턴한다.
    function GetDuration:int64;
  end;

implementation

{ TTickCounter }

constructor TTickCounter.Create;
begin
  inherited;

  Start;
end;

function TTickCounter.Get: cardinal;
var
  Tick : Cardinal;
begin
  Tick := GetTickCount;
  if Tick < FOldTick then begin
    // TickCount가 두 번 최고치에 도달 할 때까지 Get을 안하면?  그냥, 쓰지마!
    Result := ($FFFFFFFF - FOldTick) + Tick;
  end else begin
    Result := Tick - FOldTick;
  end;

  FDuration := FDuration + Result;

  FOldTick := Tick;
end;

function TTickCounter.GetDuration: int64;
var
  Tick, Temp : Cardinal;
begin
  Tick := GetTickCount;
  if Tick < FOldTick then begin
    // TickCount가 두 번 최고치에 도달 할 때까지 Get을 안하면?  그냥, 쓰지마!
    Temp := ($FFFFFFFF - FOldTick) + Tick;
  end else begin
    Temp := Tick - FOldTick;
  end;

  FDuration := FDuration + Temp;

  FOldTick := Tick;

  Result := FDuration;
end;

procedure TTickCounter.Start;
begin
  FOldTick := GetTickCount;
  FDuration := 0;
end;

end.
