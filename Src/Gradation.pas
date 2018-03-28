unit Gradation;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TDirection = (drUp, drDown);

  TGradation = class(TGraphicControl)
  private
    FDirection : TDirection;
    FColor : TColor;
    procedure SetDirection(Value : TDirection);
    procedure SetColor(Value : TColor);
  protected
    procedure Paint; override;
//    Procedure Resize; Override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Align;
    property Canvas;
    property Visible;
    property Direction : TDirection read FDirection write SetDirection;
    property Color : TColor read FColor write SetColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TGradation]);
end;

constructor TGradation.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  FColor := clBlue;
  FDirection := drUp;
  Width := 100;
  Height := 100;
end;

procedure GradationDown(Canvas : TCanvas; Width, Height : Integer; Color : TColor);
var
  Loop, iR, iG, iB, R, G, B : Integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  iB := (Color and $FF0000) shr 16;
  iG := (Color and $00FF00) shr 8;
  iR := (Color and $0000FF);
  for Loop := 0 to Height do
  begin
    R := iR * Loop div Height;
    G := iG * Loop div Height;
    B := iB * Loop div Height;
    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(0, Loop);
    Canvas.LineTo(Width, Loop);
  end;
end;

procedure GradationUp(Canvas : TCanvas; Width, Height : Integer; Color : TColor);
var
  Loop, iR, iG, iB, R, G, B : Integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  iB := (Color and $FF0000) shr 16;
  iG := (Color and $00FF00) shr 8;
  iR := (Color and $0000FF);
  for Loop := 0 to Height do
  begin
    R := iR * (Height - Loop) div Height;
    G := iG * (Height - Loop) div Height;
    B := iB * (Height - Loop) div Height;
    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(0, Loop);
    Canvas.LineTo(Width, Loop);
  end;
end;

procedure TGradation.SetDirection(Value : TDirection);
begin
  FDirection := Value;
  Paint;
end;

procedure TGradation.SetColor(Value : TColor);
begin
  FColor := Value;
  Paint;
end;

procedure TGradation.Paint;
begin
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  with Self do
    case FDirection of
      drUp : GradationUp(Canvas, Width, Height, FColor);
      drDown : GradationDown(Canvas, Width, Height, FColor);
    end;
end;

end.

