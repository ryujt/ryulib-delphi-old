unit VitalChecker;

interface

uses
  Windows, Classes, SysUtils;

const
  DEFAULT_INTERVAL = 1000;

type
 TVitalChecker = class
 private
   FOldTick : Cardinal;
   FTickCount : Cardinal;
 private
    FInterval: integer;
    FActive: boolean;
    procedure SetActive(const Value: boolean);
    function GetIsAlive: boolean;
 public
   constructor Create;

   procedure KeepAlive;

   property Active : boolean read FActive write SetActive;
   property Interval : integer read FInterval write FInterval;
   property IsAlive : boolean read GetIsAlive;
 end;

implementation

{ TVitalChecker }

constructor TVitalChecker.Create;
begin
  inherited;

  FActive := false;
  FInterval := DEFAULT_INTERVAL;
  FTickCount := DEFAULT_INTERVAL * 2;
end;

function TVitalChecker.GetIsAlive: boolean;
var
  Tick : Cardinal;
begin
  Result := false;
  if not FActive then Exit;

  Tick := GetTickCount;

  if Tick > FOldTick then FTickCount := FTickCount + Tick - FOldTick;

  FOldTick := Tick;

  Result := FTickCount <= FInterval;
end;

procedure TVitalChecker.KeepAlive;
begin
  FOldTick := GetTickCount;
  FTickCount := 0;
end;

procedure TVitalChecker.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

end.
