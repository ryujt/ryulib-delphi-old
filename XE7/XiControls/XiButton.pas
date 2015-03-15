{================================================================

    XiButton 1.13
    Written by Eugene Genev
    
=================================================================}

unit XiButton;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, Forms, Dialogs,
  Math, SysUtils;

type
  TBtnState = (bsNormal, bsOver, bsDown);
  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TColorScheme = (csNeoDesert, csNeoSky, csNeoGrass, csNeoSilver,
       csNeoRose, csNeoSun,
       csDesert, csGrass, csSky, csSun, csRose, csSilver, csCustom);

  TXiButton = class(TCustomControl)
  private
    FColorFace: TColor;
    FColorGrad: TColor;
    FColorBorder: TColor;
    FColorLight: TColor;
    FColorDark: TColor;
    FColorText: TColor;
    FOverColorFace: TColor;
    FOverColorGrad: TColor;
    FOverColorBorder: TColor;
    FOverColorLight: TColor;
    FOverColorDark: TColor;
    FOverColorText: TColor;
    FDownColorFace: TColor;
    FDownColorGrad: TColor;
    FDownColorBorder: TColor;
    FDownColorLight: TColor;
    FDownColorDark: TColor;
    FDownColorText: TColor;
    FDisabledColorFace: TColor;
    FDisabledColorGrad: TColor;
    FDisabledColorBorder: TColor;
    FDisabledColorLight: TColor;
    FDisabledColorDark: TColor;
    FDisabledColorText: TColor;
    FColorFocusRect: TColor;
    FFocused: Boolean;
    FColorScheme: TColorScheme;
    FCtl3D: boolean;
    FLayout: TButtonLayout;
    FGlyph: TBitmap;
    FTransparentGlyph: Boolean;
    FGradient: Boolean;
    FSpacing: integer;
    FModalResult: TModalResult;
    FCancel: Boolean;
    FDefault: Boolean;
    FHotTrack: Boolean;
    FClicked: Boolean;
    procedure SetColors(Index: integer; Value: TColor);
    procedure SetColorScheme(Value: TColorScheme);
    procedure SetCtl3D(Value: Boolean);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetGlyph(Value: TBitmap);
    procedure SetTransparentGlyph(Value: Boolean);
    procedure SetGradient(Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure SetModalResult(Value: TModalResult);
    procedure SetCancel(Value: Boolean);
    procedure SetDefault(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
                   StartColor, EndColor: TColor);
  protected
    FBtnState: TBtnState;
    procedure Paint; override;
    procedure Click; Override;
    procedure MouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure WMSetFocus(var msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMKeyUp(var msg: TWMKeyUp); message WM_KEYUP;
    procedure WMKeyDown(var msg: TWMKeyDown); message WM_KEYDOWN;
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged (var msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var msg: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged (var msg: TMessage); message CM_ENABLEDCHANGED;
    function GetColorScheme: TStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorFace: TColor index 0 read FColorFace write SetColors;
    property ColorGrad: TColor index 1 read FColorGrad write SetColors;
    property ColorDark: TColor index 2 read FColorDark write SetColors;
    property ColorLight: TColor index 3 read FColorLight write SetColors;
    property ColorBorder: TColor index 4 read FColorBorder write SetColors;
    property ColorText: TColor index 5 read FColorText write SetColors;
    property OverColorFace: TColor index 6 read FOverColorFace write SetColors;
    property OverColorGrad: TColor index 7 read FOverColorGrad write SetColors;
    property OverColorDark: TColor index 8 read FOverColorDark write SetColors;
    property OverColorLight: TColor index 9 read FOverColorLight write SetColors;
    property OverColorBorder: TColor index 10 read FOverColorBorder write SetColors;
    property OverColorText: TColor index 11 read FOverColorText write SetColors;
    property DownColorFace: TColor index 12 read FDownColorFace write SetColors;
    property DownColorGrad: TColor index 13 read FDownColorGrad write SetColors;
    property DownColorDark: TColor index 14 read FDownColorDark write SetColors;
    property DownColorLight: TColor index 15 read FDownColorLight write SetColors;
    property DownColorBorder: TColor index 16 read FDownColorBorder write SetColors;
    property DownColorText: TColor index 17 read FDownColorText write SetColors;
    property DisabledColorFace: TColor index 18 read FDisabledColorFace write SetColors;
    property DisabledColorGrad: TColor index 19 read FDisabledColorGrad write SetColors;
    property DisabledColorDark: TColor index 20 read FDisabledColorDark write SetColors;
    property DisabledColorLight: TColor index 21 read FDisabledColorLight write SetColors;
    property DisabledColorBorder: TColor index 22 read FDisabledColorBorder write SetColors;
    property DisabledColorText: TColor index 23 read FDisabledColorText write SetColors;
    property ColorFocusRect: TColor index 24 read FColorFocusRect write SetColors;
    property ColorScheme: TColorScheme read FColorScheme write SetColorScheme;
    property Ctl3D: Boolean read FCtl3D write SetCtl3D;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Spacing: integer read FSpacing write SetSpacing;
    property TransparentGlyph: Boolean read FTransparentGlyph write SetTransparentGlyph;
    property Gradient: Boolean read FGradient write SetGradient;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Caption;
    property Constraints;
    property Default: Boolean read FDefault write SetDefault default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult: TModalResult read FModalResult write SetModalResult default 0;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
   // property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

{//$R XiButton.res}

implementation

procedure Register;
begin
  RegisterComponents('XiControls', [TXiButton]);
end;

constructor TXiButton.Create(AOwner: TComponent);
begin
  inherited;
  Width:= 75;
  Height:= 25;
  FCtl3D:= True;
  FGlyph:= TBitmap.Create;
  TransparentGlyph:= True;
  FGradient:= False;
  TabStop:= True;
  FSpacing:= 4;
  FCancel:= False;
  FDefault:= False;
  FHotTrack:= True;
  ColorScheme:= csNeoDesert;
  FClicked:= False;
  FOverColorGrad:= clWhite;
  FDownColorGrad:= clWhite;
  FDisabledColorGrad:= clWhite;
end;

destructor TXiButton.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TXiButton.Paint;
var
  BtnBmp: TBitmap;
  CaptionRect: TRect;
  GlyphLeft, GlyphTop, TextTop, TextLeft, TextWidth, TextHeight: integer;
  FaceColor, GradColor, LightColor, DarkColor, BorderColor, TextColor: TColor;
begin
  BtnBmp:= TBitmap.Create;
  BtnBmp.Width:= Width;
  BtnBmp.Height:= Height;

  case FBtnState of
    bsNormal: begin
                FaceColor:= FColorFace;
                GradColor:= FColorGrad;
                LightColor:= FColorLight;
                DarkColor:= FColorDark;
                BorderColor:= FColorBorder;
                TextColor:= FColorText;
              end;

    bsOver:   begin
                FaceColor:= FOverColorFace;
                GradColor:= FOverColorGrad;
                LightColor:= FOverColorLight;
                DarkColor:= FOverColorDark;
                BorderColor:= FOverColorBorder;
                TextColor:= FOverColorText;
              end;

    bsDown:   begin
                FaceColor:= FDownColorFace;
                GradColor:= FDownColorGrad;
                LightColor:= FDownColorLight;
                DarkColor:= FDownColorDark;
                BorderColor:= FDownColorBorder;
                TextColor:= FDownColorText;
              end;
  end;
  if not Enabled then begin
    FaceColor:= FDisabledColorFace;
    GradColor:= FDisabledColorGrad;
    LightColor:= FDisabledColorLight;
    DarkColor:= FDisabledColorDark;
    BorderColor:= FDisabledColorBorder;
    TextColor:= FDisabledColorText;
  end;

  with BtnBmp.Canvas do begin
    Brush.Color:= FaceColor;
    Brush.Style:= bsSolid;
    Rectangle(0, 0, Width, Height);
  end;

  if FGradient then begin
    GradientFillRect(BtnBmp.Canvas, Rect(0, 0, Width, Height), FaceColor, GradColor);
  end;

  BtnBmp.Canvas.Font:= Font;
  BtnBmp.Canvas.Font.Color:= TextColor;
  TextWidth:= BtnBmp.Canvas.TextWidth(Caption);
  TextHeight:= BtnBmp.Canvas.TextHeight(Caption);
  TextTop:= (Height - TextHeight) div 2;
  TextLeft:= (Width - TextWidth) div 2;

  if not Glyph.Empty then begin
    GlyphLeft:= 0;
    case FLayout of
      blGlyphLeft:   begin
                       GlyphTop:= (Height - FGlyph.Height) div 2;
                       GlyphLeft:= TextLeft - FGlyph.Width div 2;
                       inc(TextLeft, FGlyph.Width div 2);
                       if not (Caption = '') then begin
                         GlyphLeft:= GlyphLeft - FSpacing div 2 - FSpacing mod 2;
                         inc(TextLeft, FSpacing div 2);
                       end;
                     end;
      blGlyphRight:  begin
                       GlyphTop:= (Height - FGlyph.Height) div 2;
                       GlyphLeft:= TextLeft + TextWidth - FGlyph.Width div 2;
                       inc(TextLeft, - FGlyph.Width div 2);
                       if not (Caption = '') then begin
                         GlyphLeft:= GlyphLeft + FSpacing div 2 + FSpacing mod 2;
                         inc(TextLeft, - FSpacing div 2);
                       end;
                     end;
      blGlyphTop:    begin
                       GlyphLeft:= (Width - FGlyph.Width) div 2;
                       GlyphTop:= TextTop - FGlyph.Height div 2 - FGlyph.Height mod 2;
                       inc(TextTop, FGlyph.Height div 2);
                       if not (Caption = '') then begin
                         GlyphTop:= GlyphTop - FSpacing div 2 - FSpacing mod 2;
                         inc(TextTop, + FSpacing div 2);
                       end;
                     end;
      blGlyphBottom: begin
                       GlyphLeft:= (Width - FGlyph.Width) div 2;
                       GlyphTop:= TextTop + TextHeight - Glyph.Height div 2;
                       inc(TextTop, - FGlyph.Height div 2);
                       if not (Caption = '') then begin
                         GlyphTop:= GlyphTop + FSpacing div 2 + FSpacing mod 2;
                         inc(TextTop, - FSpacing div 2);
                       end;
                     end;
    end;
    
    if FBtnState = bsDown then begin
      inc(GlyphTop, 1);
      inc(GlyphLeft, 1);
    end;
    FGlyph.TransparentColor:= FGlyph.Canvas.Pixels[0, 0];
    FGlyph.Transparent:= FTransparentGlyph;
    BtnBmp.Canvas.Draw(GlyphLeft, GlyphTop, FGlyph);
  end;
  if FBtnState = bsDown then begin
    inc(TextTop);
    inc(TextLeft);
  end;
  with CaptionRect do begin
    Top:= TextTop;
    Left:=TextLeft;
    Right:= Left + TextWidth;
    Bottom:= Top + TextHeight;
  end;

  if Caption <> '' then begin
    BtnBmp.Canvas.Brush.Style:= bsClear;
    DrawText(BtnBmp.Canvas.Handle,
             PChar(Caption),
             length(Caption),
             CaptionRect,
             DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
  end;

  with BtnBmp.Canvas do begin
    Pen.Style:= psSolid;
    Brush.Color:= FaceColor;
    Pen.Color:= BorderColor;
    Brush.Style:= bsClear;
    Rectangle(0, 0, Width, Height);

    if Ctl3D then begin
      Pen.Color:= LightColor;
      MoveTo(1, Height-2);
      LineTo(1, 1);
      LineTo(Width -1 , 1);

      Pen.Color:= DarkColor;
      MoveTo(Width-2, 1);
      LineTo(Width-2, Height-2);
      LineTo(1, Height-2);
    end;
  end;

  if FFocused then begin
    BtnBmp.Canvas.Pen.Color:= FColorFocusRect;
    BtnBmp.Canvas.Brush.Style:= bsClear;
    BtnBmp.Canvas.Rectangle(3, 3, Width-3, Height-3)
  end;

  Canvas.Draw(0, 0, BtnBmp);
  BtnBmp.Free;
end;

procedure TXiButton.Click;
begin
  if Parent <> nil then
    GetParentForm(self).ModalResult:= ModalResult;
  FBtnState:= bsNormal;
  Paint;
  inherited;
end;

procedure TXiButton.MouseEnter(var msg: TMessage);
begin
  if csDesigning in ComponentState then exit;
  if not FHotTrack then exit;
  if FClicked then
    FBtnState:= bsDown
  else
    FBtnState:= bsOver;
  Paint;
end;

procedure TXiButton.MouseLeave(var msg: TMessage);
begin
  inherited;
  FBtnState:= bsNormal;
  Paint;
end;

procedure TXiButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  FClicked:= True;
  FBtnState:= bsDown;
  if TabStop then SetFocus;
  Paint;
end;

procedure TXiButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FClicked:= False;
  if (x>0) and (y>0) and (x<width) and (y<height) then
    if FHotTrack then FBtnState:= bsOver
  else
    FBtnState:= bsNormal;
  Paint;
end;

procedure TXiButton.MouseMove (Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TXiButton.WMSetFocus(var msg: TWMSetFocus);
begin
  FFocused:= true;
  Paint;
end;

procedure TXiButton.WMKillFocus(var msg: TWMKillFocus);
begin
  FFocused:= false;
  FBtnState:= bsNormal;
  Paint;
end;

procedure TXiButton.WMKeyDown(var msg: TWMKeyDown);
begin
  if msg.CharCode = VK_SPACE then FBtnState:= bsDown;
  if msg.CharCode = VK_RETURN then Click;
  Paint;
end;

procedure TXiButton.WMKeyUp(var msg: TWMKeyUp);
begin
  if (msg.CharCode = VK_SPACE) then begin
    FBtnState:= bsNormal;
    Paint;
    Click;
  end;
end;

procedure TXiButton.CMTextChanged (var msg: TMessage);
begin
  Invalidate;
end;

procedure TXiButton.SetCtl3D(Value: Boolean);
begin
  FCtl3D:= Value;
  Invalidate;
end;

procedure TXiButton.SetLayout(Value: TButtonLayout);
begin
  FLayout:= Value;
  Invalidate;
end;

procedure TXiButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  Invalidate;
end;

procedure TXiButton.SetSpacing(Value: integer);
begin
  FSpacing:= Value;
  Invalidate;
end;

procedure TXiButton.SetTransparentGlyph(Value: Boolean);
begin
  FTransparentGlyph:= Value;
  Invalidate;
end;

procedure TXiButton.SetGradient(Value: Boolean);
begin
  FGradient:= Value;
  Invalidate;
end;

procedure TXiButton.CMFontChanged(var msg: TMessage);
begin
  Invalidate;
end;

procedure TXiButton.CMDialogKey(var msg: TCMDialogKey);
begin
  with msg do begin
    if  (((CharCode = VK_RETURN) and FFocused) or
         ((CharCode = VK_ESCAPE) and FCancel)) and
         (KeyDataToShiftState(KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end else if (FDefault and (CharCode = VK_RETURN) and CanFocus) then begin
      Click;
      Result := 1;
    end
    else inherited;
  end;
end;

procedure TXiButton.CMEnabledChanged(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TXiButton.CMDialogChar(var msg: TCMDialogChar);
begin
  with msg do
    if IsAccel(CharCode, Caption) and Enabled then begin
      Click;
      Result := 1;
    end;
end;

procedure TXiButton.SetModalResult(Value: TModalResult);
begin
  FModalResult:= Value;
end;

procedure TXiButton.SetCancel(Value: Boolean);
begin
  FCancel:= Value;
end;

procedure TXiButton.SetDefault(Value: Boolean);
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
end;

procedure TXiButton.SetHotTrack(Value: Boolean);
begin
  FHotTrack:= Value;
  Invalidate;
end;

procedure TXiButton.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: FColorFace:= Value;
    1: FColorGrad:= Value;
    2: FColorDark:= Value;
    3: FColorLight:= Value;
    4: FColorBorder:= Value;
    5: FColorText:= Value;
    6: FOverColorFace:= Value;
    7: FOverColorGrad:= Value;
    8: FOverColorDark:= Value;
    9: FOverColorLight:= Value;
    10: FOverColorBorder:= Value;
    11: FOverColorText:= Value;
    12: FDownColorFace:= Value;
    13: FDownColorGrad:= Value;
    14: FDownColorDark:= Value;
    15: FDownColorLight:= Value;
    16: FDownColorBorder:= Value;
    17: FDownColorText:= Value;
    18: FDisabledColorFace:= Value;
    19: FDisabledColorGrad:= Value;
    20: FDisabledColorDark:= Value;
    21: FDisabledColorLight:= Value;
    22: FDisabledColorBorder:= Value;
    23: FDisabledColorText:= Value;
    24: FColorFocusRect:= Value;
  end;
  ColorScheme:= csCustom;
  Invalidate;
end;

procedure TXiButton.SetColorScheme(Value: TColorScheme);
begin
  FColorScheme:= Value;
  case FColorScheme of
  csDesert:      begin
                   ColorFace:=$0095DDFF;
                   ColorLight:=$00B9E7FF;
                   ColorDark:=$00009CE8;
                   ColorBorder:=$00005680;
                   ColorText:=clBlack;
                   OverColorFace:=$006FD0FF;
                   OverColorLight:=$0095DAFF;
                   OverColorDark:=$00008ED2;
                   OverColorBorder:=$00005680;
                   OverColorText:=clBlack;
                   DownColorFace:=$006FD0FF;
                   DownColorLight:=$000077B7;
                   DownColorDark:=$008AD9FF;
                   DownColorBorder:=$000070A6;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $004080FF;
                   Gradient:= False;
                 end;
  csGrass:       begin
                   ColorFace:=$0098EBB7;
                   ColorLight:=$00CBF5DB;
                   ColorDark:=$0024B95C;
                   ColorBorder:=$00156F37;
                   ColorText:=clBlack;
                   OverColorFace:=$0068E196;
                   OverColorLight:=$00B5F0CB;
                   OverColorDark:=$0023B459;
                   OverColorBorder:=$0017793D;
                   OverColorText:=clBlack;
                   DownColorFace:=$004EDC83;
                   DownColorLight:=$00177D3E;
                   DownColorDark:=$0089E7AC;
                   DownColorBorder:=$00167439;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $0000A421;
                   Gradient:= False;
                 end;
   csSky:        begin
                   ColorFace:=$00FFE0C1;
                   ColorLight:=$00FFECD9;
                   ColorDark:=$00FFA953;
                   ColorBorder:=$00B35900;
                   ColorText:=clBlack;
                   OverColorFace:=$00FFCD9B;
                   OverColorLight:=$00FFE4CA;
                   OverColorDark:=$00FFB164;
                   OverColorBorder:=$00B35900;
                   OverColorText:=clBlack;
                   DownColorFace:=$00FFC082;
                   DownColorLight:=$00FF9122;
                   DownColorDark:=$00FFD3A8;
                   DownColorBorder:=$00B35900;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $00DC9B14;
                   Gradient:= False;
                 end;
   csRose:  begin
                   ColorFace:=$00C6C6FF;
                   ColorLight:=$00DDDDFF;
                   ColorDark:=$008282FF;
                   ColorBorder:=$0000009D;
                   ColorText:=clBlack;
                   OverColorFace:=$00B0B0FF;
                   OverColorLight:=$00D7D7FF;
                   OverColorDark:=$006A6AFF;
                   OverColorBorder:=$0000009D;
                   OverColorText:=clBlack;
                   DownColorFace:=$009F9FFF;
                   DownColorLight:=$005E5EFF;
                   DownColorDark:=$008888FF;
                   DownColorBorder:=$0000009D;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $005E5EFF;
                   Gradient:= False;
                 end;
  csSun:         begin
                   ColorFace:=$00A8FFFF;
                   ColorLight:=$00F2FFFF;
                   ColorDark:=$0000BBBB;
                   ColorBorder:=$00006464;
                   ColorText:=clBlack;
                   OverColorFace:=$0066F3FF;
                   OverColorLight:=$00CCFFFF;
                   OverColorDark:=$0000A6A6;
                   OverColorBorder:=$00006464;
                   OverColorText:=clBlack;
                   DownColorFace:=$0022EEFF;
                   DownColorLight:=$00008484;
                   DownColorDark:=$0066F3FF;
                   DownColorBorder:=$00006464;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $00008CF4;
                   Gradient:= False;
                 end;
  csSilver:      begin
                   ColorFace:=$00E0E0E0;
                   ColorLight:=$00F7F7F7;
                   ColorDark:=$00AEAEAE;
                   ColorBorder:=$00626262;
                   ColorText:=clBlack;
                   OverColorFace:=$00CFCFCF;
                   OverColorLight:=$00EEEEEE;
                   OverColorDark:=$00797979;
                   OverColorBorder:=$00757575;
                   OverColorText:=clBlack;
                   DownColorFace:=$00D3D3D3;
                   DownColorLight:=$007C7C7C;
                   DownColorDark:=$00E9E9E9;
                   DownColorBorder:=$004E4E4E;
                   DownColorText:=clBlack;
                   DisabledColorFace:=$00E2E2E2;
                   DisabledColorLight:=$00EAEAEA;
                   DisabledColorDark:=$00D8D8D8;
                   DisabledColorBorder:=$00C4C4C4;
                   DisabledColorText:=clGray;
                   ColorFocusRect:= $008A8A8A;
                   Gradient:= False;
                 end;

  csNeoDesert:   begin
                   ColorFace:= $00C6ECFF;
                   ColorGrad:= $0037BEFF;
                   ColorLight:= $00B9E7FF;
                   ColorDark:= $00009CE8;
                   ColorBorder:= $00005680;
                   ColorText:= clBlack;
                   OverColorFace:= $00B3E7FF;
                   OverColorGrad:= $0000A3F0;
                   OverColorLight:= $0095DAFF;
                   OverColorDark:= $00008ED2;
                   OverColorBorder:= $00005680;
                   OverColorText:= clBlack;
                   DownColorFace:= $002BBAFF;
                   DownColorGrad:= $0077D2FF;
                   DownColorLight:= $000077B7;
                   DownColorDark:= $008AD9FF;
                   DownColorBorder:= $000070A6;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $004080FF;
                   Gradient:= true;
                 end;
  csNeoSky:      begin
                   ColorFace:= $00FFEEDD;
                   ColorGrad:= $00FFB66C;
                   ColorLight:= $00FFECD9;
                   ColorDark:= $00FFA851;
                   ColorBorder:= $00B35900;
                   ColorText:= clBlack;
                   OverColorFace:= $00FFEBD7;
                   OverColorGrad:= $00FFA346;
                   OverColorLight:= $00FFE4CA;
                   OverColorDark:= $00FF9E3E;
                   OverColorBorder:= $00B35900;
                   OverColorText:= clBlack;
                   DownColorFace:= $00FFB366;
                   DownColorGrad:= $00FFCE9D;
                   DownColorLight:= $00FF9E3E;
                   DownColorDark:= $00FFD3A8;
                   DownColorBorder:= $00B35900;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $00FFA953;
                   Gradient:= true;
                 end;
  csNeoGrass:    begin
                   ColorFace:= $00DDF9E8;
                   ColorGrad:= $005EDF8E;
                   ColorLight:= $00CBF5DB;
                   ColorDark:= $0024B95C;
                   ColorBorder:= $00156F37;
                   ColorText:= clBlack;
                   OverColorFace:= $00BFF2D2;
                   OverColorGrad:= $003DD877;
                   OverColorLight:= $00B5F0CB;
                   OverColorDark:= $0023B459;
                   OverColorBorder:= $0017793D;
                   OverColorText:= clBlack;
                   DownColorFace:= $004EDC83;
                   DownColorGrad:= $0080E6A6;
                   DownColorLight:= $00177D3E;
                   DownColorDark:= $0089E7AC;
                   DownColorBorder:= $00167439;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $0024B95C;
                   Gradient:= true;
                 end;
  csNeoSilver:   begin
                   ColorFace:= $00F3F3F3;
                   ColorGrad:= $00BCBCBC;
                   ColorLight:= $00F7F7F7;
                   ColorDark:= $00A7A7A7;
                   ColorBorder:= $00626262;
                   ColorText:= clBlack;
                   OverColorFace:= $00F0F0F0;
                   OverColorGrad:= $00A6A6A6;
                   OverColorLight:= $00EEEEEE;
                   OverColorDark:= $00A2A2A2;
                   OverColorBorder:= $00757575;
                   OverColorText:= clBlack;
                   DownColorFace:= $00CACACA;
                   DownColorGrad:= $00DADADA;
                   DownColorLight:= $007C7C7C;
                   DownColorDark:= $00E9E9E9;
                   DownColorBorder:= $004E4E4E;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $00ADADAD;
                   Gradient:= true;
                 end;
  csNeoRose:     begin
                   ColorFace:= $00E8E8FF;
                   ColorGrad:= $009595FF;
                   ColorLight:= $00DDDDFF;
                   ColorDark:= $008282FF;
                   ColorBorder:= $0000009D;
                   ColorText:= clBlack;
                   OverColorFace:= $00DFDFFF;
                   OverColorGrad:= $007777FF;
                   OverColorLight:= $00D7D7FF;
                   OverColorDark:= $006A6AFF;
                   OverColorBorder:= $0000009D;
                   OverColorText:= clBlack;
                   DownColorFace:= $00A6A6FF;
                   DownColorGrad:= $00B9B9FF;
                   DownColorLight:= $005E5EFF;
                   DownColorDark:= $00CECEFF;
                   DownColorBorder:= $0000009D;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $005E5EFF;
                   Gradient:= true;
                 end;
  csNeoSun:      begin
                   ColorFace:= $00F0FFFF;
                   ColorGrad:= $0020D8F9;
                   ColorLight:= $00F2FFFF;
                   ColorDark:= $0000BBBB;
                   ColorBorder:= $00006464;
                   ColorText:= clBlack;
                   OverColorFace:= $00D5FCFF;
                   OverColorGrad:= $0005BCDC;
                   OverColorLight:= $00CCFFFF;
                   OverColorDark:= $0000A6A6;
                   OverColorBorder:= $00006464;
                   OverColorText:= clBlack;
                   DownColorFace:= $0005D1F5;
                   DownColorGrad:= $0066F0FB;
                   DownColorLight:= $00008484;
                   DownColorDark:= $0066F3FF;
                   DownColorBorder:= $00006464;
                   DownColorText:= clBlack;
                   DisabledColorFace:= $00EEEEEE;
                   DisabledColorGrad:= clWhite;
                   DisabledColorLight:= clWhite;
                   DisabledColorDark:= $00D2D2D2;
                   DisabledColorBorder:= clGray;
                   DisabledColorText:= clGray;
                   ColorFocusRect:=  $0000BBBB;
                   Gradient:= true;
                 end;
  end;
  Invalidate;
  FColorScheme:= Value;
end;



procedure TXiButton.GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor);
var
  Steps: Integer;
  StartR, StartG, StartB, EndR, EndG, EndB: Byte;
  CrrR, CrrG, CrrB: Double;
  IncR, IncG, incB: Double;
  i: integer;
begin
  Steps:= Rect.Bottom - Rect.Top;

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
    Canvas.MoveTo(Rect.Left, i);
    Canvas.LineTo(Rect.Right + Rect.Left, i);
    CrrR:= CrrR + IncR;
    CrrG:= CrrG + IncG;
    CrrB:= CrrB + IncB;
  end;
end;


function TXiButton.GetColorScheme: TStringList;
begin
  Result:= TStringList.Create;
  with Result do begin
    Add('ColorFace:= '+ ColorToString(ColorFace) + ';');
    Add('ColorGrad:= '+ ColorToString(ColorGrad) + ';');
    Add('ColorLight:= '+ ColorToString(ColorLight) + ';');
    Add('ColorDark:= '+ ColorToString(ColorDark) + ';');
    Add('ColorBorder:= '+ ColorToString(ColorBorder) + ';');
    Add('ColorText:= '+ ColorToString(ColorText) + ';');
    Add('OverColorFace:= '+ ColorToString(OverColorFace) + ';');
    Add('OverColorGrad:= '+ ColorToString(OverColorGrad) + ';');
    Add('OverColorLight:= '+ ColorToString(OverColorLight) + ';');
    Add('OverColorDark:= '+ ColorToString(OverColorDark) + ';');
    Add('OverColorBorder:= '+ ColorToString(OverColorBorder) + ';');
    Add('OverColorText:= '+ ColorToString(OverColorText) + ';');
    Add('DownColorFace:= '+ ColorToString(DownColorFace) + ';');
    Add('DownColorGrad:= '+ ColorToString(DownColorGrad) + ';');
    Add('DownColorLight:= '+ ColorToString(DownColorLight) + ';');
    Add('DownColorDark:= '+ ColorToString(DownColorDark) + ';');
    Add('DownColorBorder:= '+ ColorToString(DownColorBorder) + ';');
    Add('DownColorText:= '+ ColorToString(DownColorText) + ';');
    Add('DisabledColorFace:= '+ ColorToString(DisabledColorFace) + ';');
    Add('DisabledColorGrad:= '+ ColorToString(DisabledColorGrad) + ';');
    Add('DisabledColorLight:= '+ ColorToString(DisabledColorLight) + ';');
    Add('DisabledColorDark:= '+ ColorToString(DisabledColorDark) + ';');
    Add('DisabledColorBorder:= '+ ColorToString(DisabledColorBorder) + ';');
    Add('DisabledColorText:= '+ ColorToString(DisabledColorText) + ';');
    Add('ColorFocusRect:=  '+ ColorToString(ColorFocusRect) + ';');
  end;
end;

end.
