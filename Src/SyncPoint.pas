unit SyncPoint;

interface

uses
  Windows, Classes, SysUtils;

type
  TSyncPoint = class
  private
    FValue : TPoint;
    FCS : TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetXY(var AX,AY:integer);
    procedure SetXY(AX,AY:integer);

    function GetPoint:TPoint;
    procedure SetPoint(APoint:TPoint);
  end;

implementation

{ TSyncPoint }

constructor TSyncPoint.Create;
begin
  inherited;

  FValue.X := 0;
  FValue.Y := 0;

  FCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TSyncPoint.Destroy;
begin
  FreeAndNil(FCS);

  inherited;
end;

function TSyncPoint.GetPoint: TPoint;
begin
  FCS.BeginRead;
  try
    Result := FValue;
  finally
    FCS.EndRead;
  end;
end;

procedure TSyncPoint.GetXY(var AX, AY: integer);
begin
  FCS.BeginRead;
  try
    AX := FValue.X;
    AY := FValue.Y;
  finally
    FCS.EndRead;
  end;
end;

procedure TSyncPoint.SetPoint(APoint: TPoint);
begin
  FCS.BeginWrite;
  try
    FValue := APoint;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSyncPoint.SetXY(AX, AY: integer);
begin
  FCS.BeginWrite;
  try
    FValue.X := AX;
    FValue.Y := AY;
  finally
    FCS.EndWrite;
  end;
end;

end.
