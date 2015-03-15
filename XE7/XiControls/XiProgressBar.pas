{================================================================

    XiProgressBar 1.03
    Written by Eugene Genev

=================================================================}


unit XiProgressBar;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, Forms, Dialogs,
  Math, SysUtils, ComCtrls;

type
  TColorScheme = (csCustom, csDesert, csGrass, csSilver, csSky, csRose, csSun,
                  csHackers, csNight, csVelvet, csMetal, csViolet, csToxic);
  TFillDirection = (fdHorizontal, fdVertical, fdDiagonal);

  TXiProgressBar = class(TGraphicControl)
  private
    FColorBorder: TColor;
    FBackColorFace: TColor;
    FBackColorGrad: TColor;
    FForeColorFace: TColor;
    FForeColorGrad: TColor;
    FOrientation: TProgressBarOrientation;
    FColorScheme: TColorScheme;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FStep: Integer;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetPosition(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetColorScheme(Value: TColorScheme);
    procedure SetOrientation(Value: TProgressBarOrientation);
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt;
  published
    property ColorBorder: TColor index 0 read FColorBorder write SetColors;
    property BackColorFace: TColor index 10 read FBackColorFace write SetColors;
    property BackColorGrad: TColor index 11 read FBackColorGrad write SetColors;
    property ForeColorFace: TColor index 20 read FForeColorFace write SetColors;
    property ForeColorGrad: TColor index 21 read FForeColorGrad write SetColors;
    property ColorScheme: TColorScheme read FColorScheme write SetColorScheme;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Position: Integer read FPosition write SetPosition;
    property Step: Integer read FStep write SetStep;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //property HelpContext;
    //property HelpKeyword;
    property Hint;
    property ShowHint;
    property Visible;
    //property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;
    property OnClick;
  end;

procedure Register;

{//$R XiProgressBar.res}

implementation

procedure Register;
begin
  RegisterComponents('XiControls', [TXiProgressBar]);
end;

constructor TXiProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Width:= 200;
  Height:= 16;
  FMin:= 0;
  FMax:= 100;
  FStep:= 10;
  FPosition:= 0;
  FOrientation:= pbHorizontal;
  ColorScheme:= csDesert;
end;

procedure TXiProgressBar.Paint;
var
  ScrBmp: TBitmap;
  Pos: Integer;
begin
  if not Visible then Exit;
  
  ScrBmp:= TBitmap.Create;
  ScrBmp.Width:= ClientWidth;
  ScrBmp.Height:= ClientHeight;

  case FOrientation of
    pbHorizontal: begin
         Pos:= Round((ScrBmp.Width -2) * (FPosition-FMin) / (FMax-FMin));

         GradientFillRect(ScrBmp.Canvas,
                   Rect(1, 1, ScrBmp.Width -1, ScrBmp.Height -1),
                   FBackColorFace, FBackColorGrad, fdVertical);

         GradientFillRect(ScrBmp.Canvas,
                   Rect(0, 0, Pos, ScrBmp.Height),
                   FForeColorFace, FForeColorGrad, fdVertical);

         ScrBmp.Canvas.Pen.Color:= ScrBmp.Canvas.Pixels[1, 1];
         ScrBmp.Canvas.MoveTo(1, 1);
         ScrBmp.Canvas.LineTo(1, Height);
         ScrBmp.Canvas.Pen.Color:= ScrBmp.Canvas.Pixels[2, Height-2];
         ScrBmp.Canvas.MoveTo(Pos, 1);
         ScrBmp.Canvas.LineTo(Pos, Height);
    end;
    pbVertical: begin
         Pos:= ScrBmp.Height-Round((ScrBmp.Height-2) * (FPosition - FMin) / abs(FMax-FMin)+1);

         GradientFillRect(ScrBmp.Canvas,
                   Rect(1, 1, ScrBmp.Width -1, ScrBmp.Height),
                   FBackColorFace, FBackColorGrad, fdHorizontal);

         GradientFillRect(ScrBmp.Canvas,
                   Rect(0, Pos, ScrBmp.Width, ScrBmp.Height),
                   FForeColorFace, FForeColorGrad, fdHorizontal);

         ScrBmp.Canvas.Pen.Color:= ScrBmp.Canvas.Pixels[2, Height-2];
         ScrBmp.Canvas.MoveTo(1, ScrBmp.Height-2);
         ScrBmp.Canvas.LineTo(ScrBmp.Width, ScrBmp.Height-2);
         ScrBmp.Canvas.Pen.Color:= ScrBmp.Canvas.Pixels[Width-3, Height-3];
         ScrBmp.Canvas.MoveTo(1, Pos);
         ScrBmp.Canvas.LineTo(ScrBmp.Width, Pos);
    end;
  end;


  ScrBmp.Canvas.Pen.Color:= FColorBorder;
  ScrBmp.Canvas.Brush.Style:= bsClear;
  ScrBmp.Canvas.Rectangle(0, 0, ScrBmp.Width, ScrBmp.Height);

  case FOrientation of
    pbHorizontal: Canvas.Draw(0, 0, ScrBmp);
    pbVertical:   begin
                    // Rotate the image;
                    Canvas.Draw(0, 0, ScrBmp);
                  end;
  end;
  ScrBmp.Free;
end;

procedure TXiProgressBar.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FColorBorder:= Value;
    10: FBackColorFace:= Value;
    11: FBackColorGrad:= Value;
    20: FForeColorFace:= Value;
    21: FForeColorGrad:= Value;
  end;
  FColorScheme:= csCustom;
  invalidate;
end;

procedure TXiProgressBar.StepIt;
begin
  Inc(FPosition, FStep);
  Paint;
end;

procedure TXiProgressBar.SetPosition(Value: Integer);
begin
  if Value < FMin then Exit;
  FPosition:= Value;
  Paint;
end;

procedure TXiProgressBar.SetMin(Value: Integer);
begin
  if Value > FMax then Exit;
  FMin:= Value;
  Paint;
end;

procedure TXiProgressBar.SetMax(Value: Integer);
begin
  if Value < FMin then Exit;
  FMax:= Value;
  Paint;
end;

procedure TXiProgressBar.SetStep(Value: Integer);
begin
  FStep:= Value;
  Paint;
end;

procedure TXiProgressBar.SetOrientation(Value: TProgressBarOrientation);
begin
  FOrientation:= Value;
  Invalidate;
end;

procedure TXiProgressBar.SetColorScheme(Value: TColorScheme);
begin
  FColorScheme:= Value;
  case FColorScheme of
    csDesert: begin
                FColorBorder:= $00005680;
                FBackColorFace:= $00C6ECFF;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $009BDEFF;
                FForeColorGrad:= $00007BB7;
              end;
    csGrass:  begin
                FColorBorder:= $00156F37;
                FBackColorFace:= $00CBF5DC;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $00C9F5DB;
                FForeColorGrad:= $0028C162;
              end;
    csRose:   begin
                FColorBorder:= $0000009D;
                FBackColorFace:= $00D7D7FF;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $00E8E8FF;
                FForeColorGrad:= $008080FF;
              end;
    csSilver: begin
                FColorBorder:= $006B6B6B;
                FBackColorFace:= $00E0E0E0;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $00F0F0F0;
                FForeColorGrad:= $00ADADAD;
              end;
    csSky:    begin
                FColorBorder:= $00B35900;
                FBackColorFace:= $00FFEAD5;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $00FFEEDD;
                FForeColorGrad:= $00FFAE5E;
              end;
    csSun:    begin
                FColorBorder:= $00006464;
                FBackColorFace:= $00CEFFFF;
                FBackColorGrad:= clWhite;
                FForeColorFace:= $00DFFFFF;
                FForeColorGrad:= $0005BCDC;
              end;

    csHackers: begin
                FColorBorder:= clBlack;
                FBackColorFace:= $00005500;
                FBackColorGrad:= clGreen;
                FForeColorFace:= $0091ECAE;
                FForeColorGrad:= $0000AA00;
              end;
    csNight:  begin
                FColorBorder:= clBlack;
                FBackColorFace:= $00804000;
                FBackColorGrad:= $00A85400;
                FForeColorFace:= $00FFCB97;
                FForeColorGrad:= $00EC7600;
              end;
    csVelvet: begin
                FColorBorder:= clBlack;
                FBackColorFace:= $00000059;
                FBackColorGrad:= $0046468C;
                FForeColorFace:= $009F9FF2;
                FForeColorGrad:= $004848BB;
              end;
    csMetal:  begin
                FColorBorder:= clBlack;
                FBackColorFace:= $00464646;
                FBackColorGrad:= $006F6F6F;
                FForeColorFace:= $00CECECE;
                FForeColorGrad:= $00737373;
              end;
    csViolet: begin
                FColorBorder:= clBlack;
                FBackColorFace:= $00800040;
                FBackColorGrad:= $00CA0065;
                FForeColorFace:= $00FFB7DB;
                FForeColorGrad:= $00FF3399;
              end;
    csToxic:  begin
                FColorBorder:= clBlack;
                FBackColorFace:= $002C5656;
                FBackColorGrad:= $003E7B7B;
                FForeColorFace:= $00BCDEDE;
                FForeColorGrad:= $00468E8E;
              end;
  end;
  invalidate;
end;

procedure TXiProgressBar.GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
var
  Steps: Integer;
  StartR, StartG, StartB, EndR, EndG, EndB: Byte;
  CrrR, CrrG, CrrB: Double;
  IncR, IncG, incB: Double;
  i: integer;
begin
  case Direction of
    fdVertical:   Steps:= Rect.Bottom - Rect.Top;
    fdHorizontal: Steps:= Rect.Right - Rect.Left;
    fdDiagonal:   Steps:= Rect.Bottom - Rect.Top + Rect.Right - Rect.Left;
  end;

  StartR:= GetRValue(StartColor);  EndR:= GetRValue(EndColor);
  StartG:= GetGValue(StartColor);  EndG:= GetGValue(EndColor);
  StartB:= GetBValue(StartColor);  EndB:= GetBValue(EndColor);

  IncR:= (EndR - StartR) / steps;
  IncG:= (EndG - StartG) / steps;
  IncB:= (EndB - StartB) / steps;

  CrrR:= StartR;
  CrrG:= StartG;
  CrrB:= StartB;

  for i:= 0 to Steps do begin
    Canvas.Pen.Color:= RGB(Round(CrrR), Round(CrrG), Round(CrrB));
    case Direction of
      fdVertical:   begin
                      Canvas.MoveTo(Rect.Left, i);
                      Canvas.LineTo(Rect.Right + Rect.Left, i);
                    end;
      fdHorizontal: begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(i, Rect.Top + Rect.Bottom);
                    end;
      fdDiagonal:   begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(Rect.Left, i);
                    end;
    end;
    CrrR:= CrrR + IncR;
    CrrG:= CrrG + IncG;
    CrrB:= CrrB + IncB;
  end;
end;

end.
