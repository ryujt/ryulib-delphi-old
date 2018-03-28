unit Bandwidth;

interface

uses
  Windows, Classes, SysUtils;

const
  _Interval = 5;
  _QueueTime = 1;
  _QueueSize = (_QueueTime * 1000) div _Interval;

type
  {*
    지정된 Bandwidth에 최대한 맞춰서 데이터를 전송하고자 할 경우 사용된다.
  *}
  TBandwidth = class
  private
    FOldTick : Cardinal;
    FTickCount : Cardinal;
    FSize : integer;
    FHead : integer;
    FQueue : array [0.._QueueSize-1] of int64;
    procedure erase_Past;
  private
    FBandwidth: integer;
    function GetIsBusy: boolean;
    procedure SetBandwidth(const Value: integer);
  public
    constructor Create;

    procedure Clear;
    procedure AddBytes(ABytes:integer);

    property IsBusy : boolean read GetIsBusy;
    property Size : integer read FSize;

    property Bandwidth : integer read FBandwidth write SetBandwidth;
  end;

implementation

{ TBandwidth }

procedure TBandwidth.AddBytes(ABytes: integer);
begin
  FQueue[FHead] := FQueue[FHead] + ABytes;
  FSize := FSize + ABytes;

  erase_Past;
end;

procedure TBandwidth.Clear;
begin
  FSize := 0;
  FHead := 0;
  FillChar(FQueue, SizeOf(FQueue), 0);

  FOldTick := GetTickCount;
  FTickCount := 0;
end;

constructor TBandwidth.Create;
begin
  inherited;

  FBandwidth := 0;

  Clear;
end;

procedure TBandwidth.erase_Past;
var
  Loop : integer;
  Tick, Term : Cardinal;
begin
  Tick := GetTickCount;

  if FOldTick >= Tick then begin
    FOldTick := Tick;
    Exit;
  end;

  FTickCount := FTickCount + (Tick - FOldTick);
  FOldTick := Tick;

  if FTickCount >= _Interval then begin
    Term := FTickCount div _Interval;

    FTickCount := FTickCount - (_Interval * Term);

    for Loop := 1 to Term do begin
      Inc(FHead);
      FHead := FHead mod _QueueSize;

      FSize := FSize - FQueue[FHead];
      if FSize < 0 then raise Exception.Create('TBandwidth.erase_Past: ');

      FQueue[FHead] := 0;
    end;
  end;
end;

function TBandwidth.GetIsBusy: boolean;
begin
  erase_Past;
  Result := (FBandwidth > 0) and ((FSize div _QueueTime) > FBandwidth); // and (FQueue[FHead] > (FBandwidth div _QueueSize));
end;

procedure TBandwidth.SetBandwidth(const Value: integer);
begin
  FBandwidth := Value;
end;

end.
