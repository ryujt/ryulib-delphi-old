unit StopWatch;

interface

uses
  Windows;

type
  TStopWatch = Class
  private
    FEstimated : integer;
    FOldTick : Cardinal;
    function GetEstimated: Integer;
  public
    procedure Clear;
    property Estimated : integer read GetEstimated;
  end;

implementation

{ TStopWatch }

procedure TStopWatch.Clear;
begin
  FEstimated := 0;
  FOldTick := GetTickCount;
end;

function TStopWatch.GetEstimated: Integer;
var
  iTick : Cardinal;
begin
  Result := 0;

  iTick := GetTickCount;
  if iTick < FOldTick then begin
    FOldTick := iTick;
    Exit;
  end;

  FEstimated := FEstimated + (iTick - FOldTick);
  FOldTick := iTick;

  Result := FEstimated;
end;

end.
