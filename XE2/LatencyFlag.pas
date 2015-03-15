unit LatencyFlag;

interface

uses
  Windows, Classes, SysUtils;

type
  TLatencyFlag = class
  private
    FOldTick : cardinal;
    FTickCount : integer;
    FValueTarget : boolean;
  private
    FValueSource : boolean;
    FInterval: integer;
    procedure SetValue(const AValue: boolean);
    function GetValue: boolean;
  public
    constructor Create;

    procedure SetValueNow(AValue:boolean);
  public
    property Interval : integer read FInterval write FInterval;
    property Value : boolean read GetValue write SetValue;
  end;

implementation

{ TLatencyFlag }

constructor TLatencyFlag.Create;
begin
  inherited;

  FValueTarget := false;
  FValueSource := false;

  FInterval := 2000;
end;

function TLatencyFlag.GetValue: boolean;
var
  Tick : Cardinal;
begin
  if FValueTarget <> FValueSource then begin
    Tick := GetTickCount;
    if Tick > FOldTick then FTickCount := FTickCount - (Tick-FOldTick);
    if FTickCount <= 0 then FValueSource := FValueTarget;
    FOldTick := Tick;
  end;

  Result := FValueSource;
end;

procedure TLatencyFlag.SetValue(const AValue: boolean);
begin
  FValueTarget := AValue;
  FOldTick := GetTickCount;
  FTickCount := FInterval;
end;

procedure TLatencyFlag.SetValueNow(AValue: boolean);
begin
  FValueTarget := AValue;
  FValueSource := AValue;
end;

end.
