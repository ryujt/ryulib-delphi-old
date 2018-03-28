unit QueryPerformance;

interface

uses
  Windows, SysUtils, Classes;

type
  TQueryPerformance = class
  private
    FFrequency, FStart, FStop : int64;
  private
    function GetCount: int64;
    function GetTime: double;
  public
    constructor Create;

    class function Obj:TQueryPerformance;

    procedure Start;
    procedure Stop;
  public
    property Frequency : int64 read FFrequency;
    property Count : int64 read GetCount;
    property Time : double read GetTime;
  end;

/// GetTickCount 보다 적확하며 동일한 기능을 가진 함수
function GetTick:int64;

implementation

var
  Frequency : int64;

function GetTick:int64;
begin
  QueryPerformanceCounter( Result );
  Result := Result * 1000 div Frequency;
end;

{ TQueryPerformance }

var
  MyObject : TQueryPerformance = nil;

constructor TQueryPerformance.Create;
begin
  inherited;

  QueryPerformanceFrequency(FFrequency);

  FStart := 0;
  FStop := 0;
end;

function TQueryPerformance.GetCount: int64;
begin
  Result := FStop - FStart;
end;

function TQueryPerformance.GetTime: double;
begin
  Result := (FStop - FStart) / FFrequency;
end;

class function TQueryPerformance.Obj: TQueryPerformance;
begin
  if MyObject = nil then MyObject := TQueryPerformance.Create;
  Result := MyObject;
end;

procedure TQueryPerformance.Start;
begin
  QueryPerformanceCounter( FStart );
end;

procedure TQueryPerformance.Stop;
begin
  QueryPerformanceCounter( FStop );
end;

initialization
  QueryPerformanceFrequency(Frequency);
end.
