unit IntervalChecker;

interface

uses
  Windows, SysUtils, Classes;

type
  TIntervalChecker = class
  private
    FFrequency, FOldCount : int64;
    FInterval : integer;
    FIntervalCount : int64;
    procedure SetInterval(const Value: integer);
  public
    constructor Create;

    procedure Start;

    function Check:boolean;
  public
    property Interval : integer read FInterval write SetInterval;
  end;

implementation

{ TIntervalChecker }

function TIntervalChecker.Check: boolean;
var
  Count : int64;
begin
  QueryPerformanceCounter( Count );

  Result :=
    ( Count < FOldCount ) or
    ( (Count-FOldCount) >= FIntervalCount );

  if Result then FOldCount := Count;
end;

constructor TIntervalChecker.Create;
begin
  inherited;

  QueryPerformanceFrequency(FFrequency);

  Start;
end;

procedure TIntervalChecker.SetInterval(const Value: integer);
begin
  FInterval := Value;
  FIntervalCount := FFrequency * Value div 1000;

  Start;
end;

procedure TIntervalChecker.Start;
begin
  QueryPerformanceCounter( FOldCount );
end;

end.
