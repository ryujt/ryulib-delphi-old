{================================================================

    XiPanel 1.01
    Written by Eugene Genev

=================================================================}

unit XiPanel;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, Forms, Dialogs,
  Math, SysUtils, ExtCtrls;

type
  TColorScheme = (csCustom, csDesert, csGrass, csSilver, csSky, csRose, csSun);

  TFillDirection = (fdHorizontal, fdVertical, fdDiagonal);

  TXiPanel = class(TCustomPanel)
  private
    FColorFace: TColor;
    FColorGrad: TColor;
    FColorLight: TColor;
    FColorDark: TColor;
    FFillDirection: TFillDirection;
    FColorScheme: TColorScheme;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetColorScheme(Value: TColorScheme);
    procedure SetFillDirection(Value: TFillDirection);
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
      StartColor, EndColor: TColor; Direction: TFillDirection);
    procedure DrawBevel(Canvas: TCanvas; Rect: TRect; LightColor, DarkColor: TColor);
    procedure DrawFrame(Canvas: TCanvas; Offset, FrameWidth: Integer; LightColor, DarkColor: TColor);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property ColorFace: TColor index 0 read FColorFace write SetColors;
    property ColorGrad: TColor index 1 read FColorGrad write SetColors;
    property ColorLight: TColor index 2 read FColorLight write SetColors;
    property ColorDark: TColor index 3 read FColorDark write SetColors;
    property ColorScheme: TColorScheme read FColorScheme write SetColorScheme;
    property FillDirection: TFillDirection read FFillDirection write SetFillDirection;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Caption;
    property Cursor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HelpContext;
   // property HelpKeyword;
  //  property HelpType;
    property Hint;
    property Locked;
    property ParentFont;
    property ShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Tag;
    property UseDockManager;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
  //  property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

{//$R XiPanel.res}

implementation

procedure Register;
begin
  RegisterComponents('XiControls', [TXiPanel]);
end;

constructor TXiPanel.Create(AOwner: TComponent);
begin
  inherited;
  FColorDark:= clBlack;
  FColorLight:= clSilver;
  FFillDirection:= fdVertical;
  ColorScheme:= csDesert;
end;


procedure TXiPanel.Paint;
var
  ScrBmp: TBitmap;
  i: integer;
begin
  ScrBmp:= TBitmap.Create;
  ScrBmp.Width:= ClientWidth;
  ScrBmp.Height:= ClientHeight;

  GradientFillRect(ScrBmp.Canvas,
                   Rect(0, 0, ClientWidth, ClientHeight),
                   FColorFace, FColorGrad, FFillDirection);

  ScrBmp.Canvas.Font:= Font;

  case Alignment of
    taLeftJustify: i:= BevelWidth + 2;
    taRightJustify: i:= ClientWidth - BevelWidth - 2 -ScrBmp.Canvas.TextWidth(Caption);
    taCenter: i:= (ClientWidth - ScrBmp.Canvas.TextWidth(Caption)) div 2;
  end;

  if (BevelInner <> bvNone) and (BevelOuter <> bvNone) then
    case Alignment of
      taLeftJustify: i:= i + BevelWidth;
      taRightJustify: i:= i - BevelWidth;
    end;

  ScrBmp.Canvas.Brush.Style:= bsClear;
  ScrBmp.Canvas.TextOut(i, (ClientHeight - ScrBmp.Canvas.TextHeight('AaBbCcDd')) div 2, Caption);


  case BevelOuter of
    bvRaised: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorLight, FColorDark);
    bvLowered: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorDark, FColorLight);
    bvSpace: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorLight, FColorDark);
  end;

  if BevelOuter <> bvNone then
    case BevelInner of
      bvRaised: DrawFrame(ScrBmp.Canvas, BevelWidth, BevelWidth, FColorLight, FColorDark);
      bvLowered: DrawFrame(ScrBmp.Canvas, BevelWidth, BevelWidth, FColorDark, FColorLight);
      bvSpace: DrawFrame(ScrBmp.Canvas, BevelWidth, BevelWidth, FColorLight, FColorDark);
    end
  else
    case BevelInner of
      bvRaised: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorLight, FColorDark);
      bvLowered: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorDark, FColorLight);
      bvSpace: DrawFrame(ScrBmp.Canvas, 0, BevelWidth, FColorLight, FColorDark);
    end;

  Canvas.Draw(0, 0, ScrBmp);
  ScrBmp.Free;
end;

procedure TXiPanel.DrawBevel(Canvas: TCanvas; Rect: TRect; LightColor, DarkColor: TColor);
begin
  Canvas.Pen.Width:= 1;
  Canvas.Pen.Color:= LightColor;
  Canvas.MoveTo(Rect.Left, Rect.Top + Rect.Bottom);
  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Left + Rect.Right, Rect.Top);
  Canvas.Pen.Color:= DarkColor;
  Canvas.LineTo(Rect.Left + Rect.Right, Rect.Top + Rect.Bottom);
  Canvas.LineTo(Rect.Left, Rect.Top + Rect.Bottom);
end;

procedure TXiPanel.DrawFrame(Canvas: TCanvas; Offset, FrameWidth: Integer; LightColor, DarkColor: TColor);
var
  i: integer;
begin
  for i:= Offset to Offset+FrameWidth-1 do begin
      DrawBevel(Canvas,
                Rect(i, i, ClientWidth - 2*i - 1, ClientHeight - 2*i - 1),
                LightColor, DarkColor);
  end;
end;

procedure TXiPanel.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FColorFace:= Value;
    1: FColorGrad:= Value;
    2: FColorLight:= Value;
    3: FColorDark:= Value;
  end;
  FColorScheme:= csCustom;
  Invalidate;
end;

procedure TXiPanel.SetFillDirection(Value: TFillDirection);
begin
  FFillDirection:= Value;
  Invalidate;
end;

procedure TXiPanel.SetColorScheme(Value: TColorScheme);
begin
  FColorScheme:= Value;
  case FColorScheme of
    csDesert:  begin
                 FColorDark:= $0000699B;
                 FColorFace:= clWhite;
                 FColorGrad:= $00A4E1FF;
                 FColorLight:= $00008FD5;
               end;
    csGrass:   begin
                 FColorDark:= $00156F37;
                 FColorFace:= clWhite;
                 FColorGrad:= $00B0F2CB;
                 FColorLight:= $003EB56A;
               end;
    csRose:    begin
                 FColorDark:= $0000009D;
                 FColorFace:= clWhite;
                 FColorGrad:= $00CCCCFF;
                 FColorLight:= $000651CC;
               end;
    csSky:     begin
                 FColorDark:= $00B35900;
                 FColorFace:= clWhite;
                 FColorGrad:= $00FFE0C1;
                 FColorLight:= $00F0964D;
               end;
    csSilver:  begin
                 FColorDark:= $00626262;
                 FColorFace:= clWhite;
                 FColorGrad:= $00DAE0DE;
                 FColorLight:= $007D7D7D;
               end;
    csSun:     begin
                 FColorDark:= $00006464;
                 FColorFace:= clWhite;
                 FColorGrad:= $00BFFFFF;
                 FColorLight:= $00009595;
               end;
  end;
  Invalidate;
end;

procedure TXiPanel.GradientFillRect(Canvas: TCanvas; Rect: TRect;
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
