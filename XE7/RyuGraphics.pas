unit RyuGraphics;

interface

uses
  DebugTools, Math,
  SysUtils, Classes, WinTypes, Types, Vcl.Graphics, Vcl.Controls,
  Vcl.Imaging.JPeg, Vcl.Imaging.PNGImage, Vcl.Imaging.GIFImg,
  Vcl.StdCtrls, Vcl.Forms;

type
  TRyuJpegImage = class(TJPEGImage)
  private
  protected
  public
    procedure SaveToBitmap(Bitmap:TBitmap);
  published
  end;

procedure AssignBitmap(ASrc,ADst:TBitmap);

procedure PngToBitmap(APng:TPngImage; ABitmap:TBitmap);

procedure ImageFileToBitmap(AFileName:string; ABitmap:TBitmap);

function CheckCollision(A,B:TRect):boolean;

Procedure GradationDown(Canvas:TCanvas; Width,Height:Integer; Color:TColor);
Procedure GradationUp(Canvas:TCanvas; Width,Height:Integer; Color:TColor);
Function  BrightColor(Color:TColor; Weight:Integer):TColor;
Function  DarkColor(Color:TColor; Weight:Integer):TColor;

Procedure SaveImageToJPG(Graphic:TPersistent; FileName:String; Quality:Integer);
procedure GetDarkBitmap(Bitmap:TBitmap; Weight:integer);

function PixelToColor(Pixel:DWord):TColor;
function RGBToGray(RGBColor:TColor):TColor;
procedure ColorToHSB(RGBColor:TColor; var H,S,B:integer);

Procedure TransparentTextOut(Canvas:TCanvas; X,Y:Integer; stText:String);
Procedure RotateText(Canvas:TCanvas; stText:String; X,Y,Angle:Integer);

Procedure ScrollCanvasX(Canvas:TCanvas; SRect:TRect; Pixel:Integer);
Procedure ScrollCanvasY(Canvas:TCanvas; SRect:TRect; Pixel:Integer);
function RatioSize(Source,Target:TPoint):TPoint; overload;
Procedure RatioSize(Source,Target:TControl); overload;
Procedure SetClipRect(Canvas:TCanvas; R:TRect);

function GetStringOfWidth(Canvas:TCanvas; Width:integer; var Line: string):WideString;

function Brightness(Color: TColor): integer;
function BrightnessOfBitmap(ABitmap:TBitmap):integer;

procedure SmoothResize(Src,Dst:TBitmap; AKeepRatio:boolean=false);

procedure MakeOpaque(AControl:TControl);

function FindMonitorRect(AX,AY:integer):TRect;
function FindMonitorNo(AX,AY:integer):integer;

function IsWindowMaximized(ALeft,ATop,AWidth,AHeight:integer):boolean;
function IsWindowInMonitorAreas(ALeft,ATop,AWidth,AHeight:integer):boolean;

procedure FalshWindow(AHandle:HWND; ACount,ATimeOut:integer);

function SetBright(AColor:TColor; APercent:integer):TColor;

procedure Bitmap32to24(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
procedure Bitmap24to32(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
procedure Bitmap24to16(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
procedure Bitmap16to24(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);

function LoadGraphicFromFile(const AFileName:string):TGraphic;

function PrintWindow(AHwnd:HWND; ABitmap:TBitmap): Boolean;

procedure ClearWinControl(AControl: TWinControl);

procedure FitFormSizeToMonitorSize(AWinControl:TWinControl);

function GetRealLeft(AParent:TWinControl; ACurrent:integer):integer;
function GetRealTop(AParent:TWinControl; ACurrent:integer):integer;

procedure DrawBitmapOn(AX,AY:integer; ADst,ASrc:TBitmap); overload;
procedure DrawBitmapOn(AX,AY:integer; ADst,ASrc:TBitmap; ATransparentColor:TColor); overload;


{*
  Bitmap 속의 Alpha 값을 토대로 컨트롤을 투명으로 변경해준다.

  * 사용법
    procedure TfmMain.FormCreate(Sender: TObject);
    begin
      SetWindowLong( Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED );
      FBitmap := TBitmap.Create;
      PngToBitmap( TPngImage(Image.Picture.Graphic), FBitmap );
      UpdateLayeredControl( Self, FBitmap );
    end;
}
procedure UpdateLayeredControl(AControl:TWinControl; ABitmap:TBitmap);

procedure SetAlpha(ABitmap:TBitmap; AAplpha:byte); overload;
procedure SetAlpha(ABitmap:TBitmap; AAplpha:byte; AColor:TColor); overload;

procedure BitmapScrollUp(ABitmap:TBitmap; ASize:integer);

/// 현재 모니터 해상도보다 큰 컨트롤을 모니터에 맞게 줄여 준다.
procedure FitFormSize(AControl:TWinControl);

procedure FlipBitmapHorizontal(ABitmap:TBitmap);

implementation

procedure AssignBitmap(ASrc,ADst:TBitmap);
var
  iPixelSize : integer;
begin
  if (ASrc.Width * ASrc.Height) = 0 then Exit;

  case ASrc.PixelFormat of
    pf8bit:  iPixelSize := 1;
    pf16bit: iPixelSize := 2;
    pf24bit: iPixelSize := 3;
    pf32bit: iPixelSize := 4;
    else
      raise Exception.Create('RyuGraphics.AssignBitmap - 사용 할 수 없는 PixelFormat 입니다.');
  end;

  try
    if ASrc.PixelFormat <> ADst.PixelFormat then ADst.PixelFormat := ASrc.PixelFormat;

    if ASrc.Width  <> ADst.Width  then ADst.Width  := ASrc.Width;
    if ASrc.Height <> ADst.Height then ADst.Height := ASrc.Height;

    Move(
      ASrc.ScanLine[ASrc.Height-1]^,
      ADst.ScanLine[ADst.Height-1]^,
      ASrc.Width * ASrc.Height * iPixelSize
    );
  except
    on E : Exception do raise Exception.Create('RyuGraphics.AssignBitmap - ' + E.Message );
  end;
end;

procedure PngFileToBitmap(AFileName:string; ABitmap:TBitmap);
var
  img : TPngImage;
begin
  img := TPngImage.Create;
  try
    img.LoadFromFile( AFileName);

    PngToBitmap( img, ABitmap );
  finally
    img.Free;
  end;
end;

procedure GifFileToBitmap(AFileName:string; ABitmap:TBitmap);
var
  img : TGifImage;
begin
  img := TGifImage.Create;
  try
    img.LoadFromFile( AFileName);

    ABitmap.Width  := img.Width;
    ABitmap.Height := img.Height;

    ABitmap.Canvas.Draw( 0, 0, img );
  finally
    img.Free;
  end;
end;

procedure JpgFileToBitmap(AFileName:string; ABitmap:TBitmap);
var
  img : TJPEGImage;
begin
  img := TJPEGImage.Create;
  try
    img.LoadFromFile( AFileName);

    ABitmap.Width  := img.Width;
    ABitmap.Height := img.Height;

    ABitmap.Canvas.Draw( 0, 0, img );
  finally
    img.Free;
  end;
end;

procedure ImageFileToBitmap(AFileName:string; ABitmap:TBitmap);
var
  sExt : string;
begin
  sExt := LowerCase( ExtractFileExt(AFileName) );

       if sExt = '.bmp' then ABitmap.LoadFromFile( AFileName )
  else if sExt = '.png' then PngFileToBitmap( AFileName, ABitmap )
  else if sExt = '.gif' then GifFileToBitmap( AFileName, ABitmap )
  else if Pos(sExt, '.jpg; .jpeg;') > 0 then JpgFileToBitmap( AFileName, ABitmap )
  else raise Exception.Create('RyuGraphics.ImageFileToBitmap - 지원하지 않는 이미지 포멧 입니다.');
end;

function CheckLineCollision(S1,E1,S2,E2:integer):boolean;
begin
  Result :=
    ((S1 <= S2) and (S2 <= E1)) or
    ((S1 <= E2) and (E2 <= E1)) or
    ((S2 <= S1) and (S1 <= E2)) or
    ((S2 <= E1) and (E1 <= E2));
end;

function CheckCollision(A,B:TRect):boolean;
begin
  Result :=
    CheckLineCollision(A.Left, A.Right,  B.Left, B.Right) and
    CheckLineCollision(A.Top,  A.Bottom, B.Top,  B.Bottom);
end;

Procedure GradationDown(Canvas:TCanvas; Width,Height:Integer; Color:TColor);
Var
   bTemp : TBrushStyle;
   pTemp : TPenStyle;
   Loop, iR, iG, iB, R, G, B : Integer;
Begin
  bTemp:= Canvas.Brush.Style;
  pTemp:= Canvas.Pen.Style;
  Canvas.Brush.Style:= bsSolid;
  Canvas.Pen.Style:=   psSolid;
  iB:= (Color and $FF0000) shr 16;
  iG:= (Color and $00FF00) shr  8;
  iR:= (Color and $0000FF);
  For Loop:= 0 to Height do
      Begin
        R:= iR*Loop div Height;
        G:= iG*Loop div Height;
        B:= iB*Loop div Height;
        Canvas.Pen.Color:= RGB(R, G, B);
        Canvas.MoveTo(0, Loop);
        Canvas.LineTo(Width, Loop);
      End;
  Canvas.Pen.Style:=   pTemp;
  Canvas.Brush.Style:= bTemp;
End;

Procedure GradationUp(Canvas:TCanvas; Width,Height:Integer; Color:TColor);
Var
   bTemp : TBrushStyle;
   pTemp : TPenStyle;
   Loop, iR, iG, iB, R, G, B : Integer;
Begin
  bTemp:= Canvas.Brush.Style;
  pTemp:= Canvas.Pen.Style;
  Canvas.Brush.Style:= bsSolid;
  Canvas.Pen.Style:=   psSolid;
  iB:= (Color and $FF0000) shr 16;
  iG:= (Color and $00FF00) shr  8;
  iR:= (Color and $0000FF);
  For Loop:= 0 to Height do
      Begin
        R:= iR*(Height-Loop) div Height;
        G:= iG*(Height-Loop) div Height;
        B:= iB*(Height-Loop) div Height;
        Canvas.Pen.Color:= RGB(R, G, B);
        Canvas.MoveTo(0, Loop);
        Canvas.LineTo(Width, Loop);
      End;
  Canvas.Pen.Style:=   pTemp;
  Canvas.Brush.Style:= bTemp;
End;

Function  BrightColor(Color:TColor; Weight:Integer):TColor;
Var
   iR, iG, iB : Integer;
Begin
  iR:= (GetRValue(Color)*(100+Weight)) div 100;
  iG:= (GetGValue(Color)*(100+Weight)) div 100;
  iB:= (GetBValue(Color)*(100+Weight)) div 100;
  Result:= RGB(iR, iG, iB);
End;

Function  DarkColor(Color:TColor; Weight:Integer):TColor;
Var
   iR, iG, iB : Integer;
Begin
  iR:= (GetRValue(Color)*(100-Weight)) div 100;
  iG:= (GetGValue(Color)*(100-Weight)) div 100;
  iB:= (GetBValue(Color)*(100-Weight)) div 100;
  Result:= RGB(iR, iG, iB);
End;

procedure SaveImageToJPG(Graphic:TPersistent; FileName:string; Quality:integer);
var
   JP : TJPEGImage;
begin
  JP := TJPEGImage.Create;
  try
    JP.CompressionQuality := Quality;
    JP.Performance := jpBestQuality;
    JP.Assign(Graphic);
    JP.PixelFormat := jf24Bit;
    JP.Compress;
    JP.SaveToFile(FileName);
  finally
    JP.Free;
  end;
end;

procedure GetDarkBitmap(Bitmap:TBitmap; Weight:Integer);
var
  LoopX, LoopY : integer;
begin
  for LoopY := 0 to Bitmap.Height - 1 do
  for LoopX := 0 to Bitmap.Width - 1 do
    Bitmap.Canvas.Pixels[LoopX, LoopY]:= DarkColor(Bitmap.Canvas.Pixels[LoopX, LoopY], Weight);
end;

function PixelToColor(Pixel:DWord):TColor;
var
  R, G, B : integer;
begin
  B := (Pixel and $FF);
  G := ((Pixel and $FF00) shr 8);
  R := ((Pixel and $FF0000) shr 16);

  Result := RGB(R, G, B);
end;

function  RGBToGray(RGBColor:TColor):TColor;
var
   Gray : Byte;
begin
  Gray := Round((0.30*GetRValue(RGBColor)) +
                (0.59*GetGValue(RGBColor)) +
                (0.11*GetBValue(RGBColor )));
  Result := RGB(Gray, Gray, Gray);
end;

procedure ColorToHSB(RGBColor:TColor; var H,S,B:integer);
var
  Red, Green, Blue : byte;
  dH, dS, dB,
  minRGB, maxRGB, Delta : Double;
begin
  Red := GetRValue(RGBColor);
  Green := GetGValue(RGBColor);
  Blue := GetBValue(RGBColor);

  dH := 0.0;

  minRGB := Min(Min(Red, Green), Blue);
  maxRGB := Max(Max(Red, Green), Blue);
  dB := maxRGB;

  Delta := ( maxRGB - minRGB ) ;
  if (maxRGB <> 0.0) then dS := 255.0 * Delta / maxRGB
  else dS := 0.0;

  if (dS <> 0.0) then begin
    if Red = maxRGB then dH := (Green - Blue) / Delta
    else if Green = maxRGB then dH := 2.0 + (Blue - Red) / Delta
    else if Blue = maxRGB then dH := 4.0 + (Red - Green) / Delta
  end else dH := -1.0;

  dH := dH * 60;
  if dH < 0.0 then dH := dH + 360.0
  else if dH > 360.0 then dH := 360.0;           

  H := Round(dH);
  S := Round(dS * 100 / 255);
  B := Round(dB * 100 / 255);
end;

Procedure TransparentTextOut(Canvas:TCanvas; X,Y:Integer; stText:String);
Var
   OldBkMode : Integer;
Begin
  OldBkMode:= SetBkMode(Canvas.Handle, TRANSPARENT);
  Canvas.TextOut(X, Y, stText);
  SetBkMode(Canvas.Handle, OldBkMode);
End;

Procedure RotateText(Canvas:TCanvas; stText:String; X,Y,Angle:Integer);
Var
   LogFont : TLogFont;
Begin
  GetObject(Canvas.Handle, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement:= Angle*10;
  Canvas.Font.Handle := CreateFontIndirect(LogFont);
  Canvas.TextOut(X, Y, stText);
End;

Procedure ScrollCanvasX(Canvas:TCanvas; SRect:TRect; Pixel:Integer);
Var
   DRect : TRect;
Begin
  DRect:= Rect(SRect.Left+Pixel, SRect.Top, SRect.Right+Pixel, SRect.Bottom);
  Canvas.CopyRect(DRect, Canvas, SRect);
End;

Procedure ScrollCanvasY(Canvas:TCanvas; SRect:TRect; Pixel:Integer);
Var
   DRect : TRect;
Begin
  DRect:= Rect(SRect.Left, SRect.Top+Pixel, SRect.Right, SRect.Bottom+Pixel);
  Canvas.CopyRect(DRect, Canvas, SRect);
End;

function RGBChange(A: TColor; Rx, Gx, Bx: SmallInt): TColor;
var
  RValue, GValue, BValue : byte;
begin
  RValue := GetRValue(A);
  GValue := GetGValue(A);
  BValue := GetBValue(A);
  // Change Red Part
  If Rx > 0 Then
    If RValue + Rx < $FF Then Inc(RValue, Rx) else RValue := $FF;
  If Rx < 0 Then
    If RValue > Abs(Rx) Then Dec(RValue, Abs(Rx)) else RValue := 0;
  // Change Green Part
  If Gx > 0 Then
    If GValue + Gx < $FF Then Inc(GValue, Gx) else GValue := $FF;
  If Gx < 0 Then
    If GValue > Abs(Gx) Then Dec(GValue, Abs(Gx)) else GValue := 0;
  // Change Blue Part
  If Bx > 0 Then
    If BValue + Bx < $FF Then Inc(BValue, Bx) else BValue := $FF;
  If Bx < 0 Then
    If BValue > Abs(Bx) Then Dec(BValue, Abs(Bx)) else BValue := 0;
  Result := RGB(RValue, GValue, BValue);
end;

function RatioSize(Source,Target:TPoint):TPoint;
var
  rSRatio, rTRatio, rRatio : double;
begin
  if (Source.X = 0) or (Target.X = 0) or (Source.Y = 0) or (Target.Y = 0) then begin
    Result := Source;
    Exit;
  end;

  rSRatio:= Source.Y / Source.X;
  rTRatio:= Target.Y / Target.X;

  if rSRatio > rTRatio then
    // 원본의 세로축의 비율이 크다.  길쭉한 모양으로
    rRatio:= Source.Y / Target.Y

  else
    //원본의 가로축의 비율이 크다.  넓적한 모양으로
    rRatio:= Source.X / Target.X;

  Result.X:= Trunc(Source.X / rRatio);
  Result.Y:= Trunc(Source.Y / rRatio);
end;

Procedure RatioSize(Source,Target:TControl);
Var
  rSRatio, rTRatio, rRatio : Real;
Begin
  if (Source.Width = 0) or (Target.Width = 0) or (Source.Width = 0) or (Target.Width = 0) then begin
    Target.Width:=  Source.Width;
    Target.Height:= Source.Height;
    Exit;
  end;

  rSRatio:= Source.Height / Source.Width;
  rTRatio:= Target.Height / Target.Width;

  If rSRatio > rTRatio then
    // 원본의 세로축의 비율이 크다.  길쭉한 모양으로
    rRatio:= Source.Height / Target.Height

  Else
    //원본의 가로축의 비율이 크다.  넓적한 모양으로
    rRatio:= Source.Width / Target.Width;

  Target.Width:=  Trunc(Source.Width / rRatio);
  Target.Height:= Trunc(Source.Height / rRatio);
End;

Procedure SetClipRect(Canvas:TCanvas; R:TRect);
var
  Rgn : HRGN;
Begin
  Rgn:= CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
  ExtSelectClipRgn(Canvas.Handle, Rgn, RGN_AND);
  DeleteObject(Rgn);
End;

function GetStringOfWidth(Canvas:TCanvas; Width:integer; var Line: string):WideString;
var
  Loop : Integer;
  sTemp : string;
  sCopy : WideString;
begin
  if Canvas.TextWidth(Line) <= Width then begin
    Result := Line;
    Line := '';
    Exit;
  end;

  Result := '';
  sCopy := Line;
  for Loop := 1 to Length(sCopy) do begin
    Result := Result + sCopy[Loop];
    if Canvas.TextWidth(Result) > Width then begin
      Delete(Result, Length(Result), 1);

      // 짤라낸 만큼 원본도 짤라낸다.
      sTemp := Result;
      Delete(Line, 1, Length(sTemp));

      Exit;
    end;
  end;
end;

function Brightness(Color: TColor): integer;
var
  RGB : TRGBQuad absolute Color;
begin
  Result := ((30*RGB.rgbRed) + (59*RGB.rgbGreen) + (11*RGB.rgbBlue)) div 100;
end;

function BrightnessOfBitmap(ABitmap:TBitmap):integer;
var
  Count : integer;
  Bright : double;
  StartPos, EndPos : ^TColor;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.BrightnessOfBitmap: 32bit 포멧만 지원합니다.');

  StartPos := ABitmap.ScanLine[ABitmap.Height-1];

  EndPos := StartPos;
  Inc(EndPos, ABitmap.Width * ABitmap.Height);

  Count := 0;
  Bright := 0;
  while StartPos <> EndPos do begin
    Inc(Count);
    Bright := (Bright*(Count-1)) / Count;
    Bright := Bright + (Brightness(StartPos^) / Count);

    Inc(StartPos);
  end;

  Result := Round(Bright);
end;

procedure SmoothResize(Src, Dst: TBitmap; AKeepRatio:boolean);
type
  TRGBArray = array[Word] of TRGBQuad;
  pRGBArray = ^TRGBArray;
var
  ptSrc, ptDst, ptResult : TPoint;
  x, y: Integer;
  xP, yP: Integer;
  xP2, yP2: Integer;
  t3: Integer;
  z, z2, iz2: Integer;
  DstGap: Integer;
  w1, w2, w3, w4: Integer;
  SrcLine1, SrcLine2, DstLine: pRGBArray;
begin
  Assert(Src.PixelFormat = pf32Bit, 'Invalid PixelFormat');
  Assert(Dst.PixelFormat = pf32Bit, 'Invalid PixelFormat');

  if (Src.Width = 0) or (Src.Height = 0) or (Dst.Width = 0) or (Dst.Height = 0) then begin
    {$IFDEF DEBUG}
    Trace( 'RyuGraphics.SmoothResize - (Src.Width = 0) or (Src.Height = 0) or (Dst.Width = 0) or (Dst.Height = 0)' );
    {$ENDIF}

    Exit;
  end;

  if AKeepRatio then begin
    ptSrc.X := Src.Width;
    ptSrc.Y := Src.Height;

    ptDst.X := Dst.Width;
    ptDst.Y := Dst.Height;

    ptResult := RatioSize( ptSrc, ptDst );

    Dst.Width  := ptResult.X;
    Dst.Height := ptResult.Y;
  end;

  try
    if (Src.Width = Dst.Width) and (Src.Height = Dst.Height) then begin
      Move(
        Src.ScanLine[Src.Height-1]^,
        Dst.ScanLine[Dst.Height-1]^,
        Dst.Width * Dst.Height * 4
      );

      Exit;
    end;

    DstLine := Dst.ScanLine[0];
    DstGap  := Integer(Dst.ScanLine[1]) - Integer(DstLine);

    xP2 := MulDiv(pred(Src.Width), $10000, Dst.Width);
    yP2 := MulDiv(pred(Src.Height), $10000, Dst.Height);
    yP  := 0;

    for y := 0 to pred(Dst.Height) do begin
      xP := 0;
      SrcLine1 := Src.ScanLine[yP shr 16];

      if (yP shr 16 < pred(Src.Height)) then SrcLine2 := Src.ScanLine[succ(yP shr 16)]
      else SrcLine2 := Src.ScanLine[yP shr 16];

      z2  := succ(yP and $FFFF);
      iz2 := succ((not yp) and $FFFF);
      for x := 0 to pred(Dst.Width) do begin
        t3 := xP shr 16;
        z  := xP and $FFFF;
        w2 := MulDiv(z, iz2, $10000);
        w1 := iz2 - w2;
        w4 := MulDiv(z, z2, $10000);
        w3 := z2 - w4;
        DstLine[x].rgbRed := (SrcLine1[t3].rgbRed * w1 +
          SrcLine1[t3 + 1].rgbRed * w2 +
          SrcLine2[t3].rgbRed * w3 + SrcLine2[t3 + 1].rgbRed * w4) shr 16;
        DstLine[x].rgbGreen :=
          (SrcLine1[t3].rgbGreen * w1 + SrcLine1[t3 + 1].rgbGreen * w2 +

          SrcLine2[t3].rgbGreen * w3 + SrcLine2[t3 + 1].rgbGreen * w4) shr 16;
        DstLine[x].rgbBlue := (SrcLine1[t3].rgbBlue * w1 +
          SrcLine1[t3 + 1].rgbBlue * w2 +
          SrcLine2[t3].rgbBlue * w3 +
          SrcLine2[t3 + 1].rgbBlue * w4) shr 16;
        Inc(xP, xP2);
      end;
      Inc(yP, yP2);
      DstLine := pRGBArray(Integer(DstLine) + DstGap);
    end;
  except
    on E : Exception do
      raise Exception.Create('RyuGraphics.SmoothResize - ' + E.Message);
  end;
end;

type
  TWinControlCopy = class (TWinControl) end;

procedure MakeOpaque(AControl:TControl);
var
  Loop : integer;
  WinControl : TWinControlCopy;
begin
  if AControl is TLabel then Exit;

  if AControl is TWinControl then begin
    WinControl := Pointer(AControl);

    if (AControl.Tag <> 99999) and (not WinControl.ParentBackground) then begin
      AControl.ControlStyle := AControl.ControlStyle + [csOpaque];
      WinControl.DoubleBuffered := true;
    end;

    for Loop := 0 to WinControl.ControlCount-1 do MakeOpaque(WinControl.Controls[Loop]);
  end else begin
    if AControl.Tag <> 99999 then AControl.ControlStyle := AControl.ControlStyle + [csOpaque];
  end;
end;

function FindMonitorRect(AX,AY:integer):TRect;
var
  Monitor : TMonitor;
begin
  Monitor := Screen.MonitorFromPoint(Point(AX, AY));
  if Monitor = nil then Monitor := Screen.Monitors[0];

  Result :=
    Rect(
      Monitor.Left,
      Monitor.Top,
      Monitor.Left + Monitor.Width,
      Monitor.Top  + Monitor.Height
    );
end;

function FindMonitorNo(AX,AY:integer):integer;
var
  Monitor : TMonitor;
  Loop: Integer;
  isEqual : boolean;
begin
  Result := 0;

  Monitor := Screen.MonitorFromPoint(Point(AX, AY));
  if Monitor = nil then Exit;

  for Loop := 0 to Screen.MonitorCount-1 do begin
    isEqual :=
      (Monitor.Left   = Screen.Monitors[Loop].Left  ) and
      (Monitor.Top    = Screen.Monitors[Loop].Top   ) and
      (Monitor.Width  = Screen.Monitors[Loop].Width ) and
      (Monitor.Height = Screen.Monitors[Loop].Height);
    if isEqual then begin
      Result := Loop;
      Break;
    end;
  end;
end;

function IsWindowMaximized(ALeft,ATop,AWidth,AHeight:integer):boolean;
var
  WindowCenter : TPoint;
  MonitorRect : TRect;
begin
  WindowCenter := Point( ALeft + (AWidth div 2), ATop + (AHeight div 2)  );

  MonitorRect := FindMonitorRect( WindowCenter.X, WindowCenter.Y );

  Result := (AWidth >= MonitorRect.Width) and (AHeight >= MonitorRect.Height);

  Result := Result or
    (ALeft   = MonitorRect.Left) and
    (ATop    = MonitorRect.Top) and
    (AWidth  = MonitorRect.Width) and
    (AHeight = MonitorRect.Height);
end;

function IsWindowInMonitorAreas(ALeft,ATop,AWidth,AHeight:integer):boolean;
var
  A, B : TRect;
  Loop: Integer;
  Monitor : TMonitor;
begin
  Result := false;

  A := Rect( ALeft, ATop, ALeft + AWidth, ATop + AHeight );

  for Loop := 0 to Screen.MonitorCount-1 do begin
    Monitor := Screen.Monitors[Loop];

    B :=
      Rect(
        Monitor.Left,
        Monitor.Top,
        Monitor.Left + Monitor.Width,
        Monitor.Top  + Monitor.Height
      );

    if CheckCollision(A, B) then begin
      Result := true;
      Break;
    end;
  end;
end;

procedure FalshWindow(AHandle:HWND; ACount,ATimeOut:integer);
var
  pfwi : FLASHWINFO;
begin
  pfwi.cbSize    := SizeOf(pfwi);
  pfwi.hWnd      := AHandle;
  pfwi.dwFlags   := FLASHW_ALL;
  pfwi.uCount    := ACount;
  pfwi.dwTimeOut := ATimeOut;
  FlashWindowEx(pfwi);
end;

function SetBright(AColor:TColor; APercent:integer):TColor;
var
  iR, iG, iB : integer;
begin
  iB:= (AColor and $FF0000) shr 16;
  iG:= (AColor and $00FF00) shr  8;
  iR:= (AColor and $0000FF);

  Result := RGB(
    iR * APercent div 100,
    iG * APercent div 100,
    iB * APercent div 100
  );
end;

procedure Bitmap32to24(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : PBYTE;
  pDst : PBYTE;
  Loop : integer;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth*AHeight-1 do begin
    Move(pSrc^, pDst^, 3);

    Inc(pSrc, 4);
    Inc(pDst, 3);
  end;
end;

procedure Bitmap24to32(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : PBYTE;
  pDst : PBYTE;
  Loop : integer;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth * AHeight-1 do begin
    FillChar(pDst^, 4, 0);

    Move(pSrc^, pDst^, 3);

    Inc(pSrc, 3);
    Inc(pDst, 4);
  end;
end;

procedure Bitmap24to16(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : pByte;
  pDst : pWord;
  Loop : integer;

  R, G, B : Byte;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth * AHeight-1 do begin
    R := pSrc^;
    Inc(pSrc);
    G := pSrc^;
    Inc(pSrc);
    B := pSrc^;
    Inc(pSrc);

    pDst^ := (((B and $F8) shr 3) shl 11) or
             (((G and $FC) shr 2) shl 5) or
             ((R and $F8) shr 3);
    Inc(pDst);
  end;
end;

procedure Bitmap24to08(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : pByte;
  pDst : pByte;
  Loop : integer;

  R, G, B : Byte;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth * AHeight-1 do begin
    R := pSrc^;
    Inc(pSrc);
    G := pSrc^;
    Inc(pSrc);
    B := pSrc^;
    Inc(pSrc);

    pDst^ := (R shl 5) or (G shl 3) or (B);

    Inc(pDst);
  end;
end;

procedure Bitmap16to24(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : pWord;
  pDst : pByte;
  Loop : integer;

  R, G, B : Byte;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth * AHeight-1 do begin
    R := (pSrc^ and $1f) shl 3;
    G := ((pSrc^ shr 5) and $3f) shl 2;
    B := ((pSrc^ shr 11) and $1f) shl 3;

    pDst^ := R;
    Inc(pDst);
    pDst^ := G;
    Inc(pDst);
    pDst^ := B;
    Inc(pDst);
    Inc(pSrc);
  end;
end;

procedure Bitmap08to24(ASrc:pointer; ADst:pointer; AWidth, AHeight : Integer);
var
  pSrc : pByte;
  pDst : pByte;
  Loop : integer;

  R, G, B : Byte;
begin
  pSrc := ASrc;
  pDst := ADst;

  for Loop := 0 to AWidth * AHeight-1 do begin
    R := (pSrc^ and $E0) shr 5;
    G := (pSrc^ and $18) shr 3;
    B := (pSrc^ and $07);

    pDst^ := R;
    Inc(pDst);
    pDst^ := G;
    Inc(pDst);
    pDst^ := B;
    Inc(pDst);
    Inc(pSrc);
  end;
end;

function LoadGraphicFromFile(const AFileName:string):TGraphic;
const
  JPEG_SOI_Marker : word = $D8FF;
var
  pMarker : PWord;
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    msData.LoadFromFile(AFileName);

    pMarker := msData.Memory;

    if pMarker^ = JPEG_SOI_Marker then Result := TJPEGImage.Create
    else Result := TPNGImage.Create;

    msData.Position := 0;
    Result.LoadFromStream(msData);
  finally
    msData.Free;
  end;
end;

function PrintWindow(AHwnd:HWND; ABitmap:TBitmap): Boolean;
var
  rc: TRect;
  huser32: THandle;
  Func: function(Hnd: HWND; HdcBlt: HDC; nFlags: UINT): BOOL; stdcall;
begin
  Result := False;
  huser32 := GetModuleHandle(user32);
  if huser32 <> 0 then begin
    @Func := GetProcAddress(huser32, 'PrintWindow');
    if @Func <> nil then begin
      GetWindowRect(AHwnd, rc);
      ABitmap.Width := rc.Right - rc.Left;
      ABitmap.Height := rc.Bottom - rc.Top;
      ABitmap.PixelFormat := pf8bit;
      ABitmap.Canvas.Lock;
      try
        Result := Func(AHwnd, ABitmap.Canvas.Handle, 0);
      finally
        ABitmap.Canvas.Unlock;
      end;
    end;
  end;
end;

procedure ClearWinControl(AControl: TWinControl);
var
  Loop: Integer;
  FullRgn, ClientRgn, ControlRgn: THandle;
  Margin, MarginX, MarginY, X, Y: Integer;
begin
  Margin := (AControl.Width - AControl.ClientWidth) div 2;
  FullRgn := CreateRectRgn(0, 0, AControl.Width, AControl.Height);
  MarginX := Margin;
  MarginY := AControl.Height - AControl.ClientHeight - Margin;
  ClientRgn := CreateRectRgn(MarginX, MarginY, MarginX + AControl.ClientWidth,
    MarginY + AControl.ClientHeight);
  CombineRgn(FullRgn, FullRgn, ClientRgn, RGN_DIFF);
  for Loop := 0 to AControl.ControlCount - 1 do
  begin
    X := MarginX + AControl.Controls[Loop].Left;
    Y := MarginY + AControl.Controls[Loop].Top;
    ControlRgn := CreateRectRgn(X, Y, X + AControl.Controls[Loop].Width,
      Y + AControl.Controls[Loop].Height);
    CombineRgn(FullRgn, FullRgn, ControlRgn, RGN_OR);
  end;
  SetWindowRgn(AControl.Handle, FullRgn, True);
end;

procedure FitFormSizeToMonitorSize(AWinControl:TWinControl);
var
  MonitorRect : TRect;
begin
  MonitorRect := FindMonitorRect(AWinControl.Left, AWinControl.Top);
  if (MonitorRect.Width <= AWinControl.Width) or (MonitorRect.Height <= AWinControl.Height) then begin
    AWinControl.Left := 0;
    AWinControl.Top  := 0;
  end else begin
    AWinControl.Left := (MonitorRect.Width  div 2) - (AWinControl.Width  div 2);
    AWinControl.Top  := (MonitorRect.Height div 2) - (AWinControl.Height div 2);
  end;
end;

function GetRealLeft(AParent:TWinControl; ACurrent:integer):integer;
begin
  Result := ACurrent;
  if AParent <> nil then begin
    Result := ACurrent + AParent.Left;
    Result := Result + GetRealLeft(AParent.Parent, Result);
  end;
end;

function GetRealTop(AParent:TWinControl; ACurrent:integer):integer;
begin
  Result := ACurrent;
  if AParent <> nil then begin
    Result := ACurrent + AParent.Top;
    Result := Result + GetRealLeft(AParent.Parent, Result);
  end;
end;

procedure DrawBitmapOn(AX,AY:integer; ADst,ASrc:TBitmap);
var
  iX, iY: Integer;
  pSrc, pDst : PDWord;
begin
  if (ASrc.PixelFormat <> pf32bit) or (ADst.PixelFormat <> pf32bit) then
    raise Exception.Create('RyuGraphics.DrawBitmapOn - PixelFormat <> pf32bit');

  if (ASrc.Width * ASrc.Height) = 0 then Exit;

  if (AX < 0) or (AY < 0) then Exit;

  if (AX >= ADst.Width) or (AY >= ADst.Height) then Exit;

  try
    for iY := 0 to ASrc.Height-1 do begin
      if (AY + iY) >= ADst.Height then Continue;

      pSrc := ASrc.ScanLine[iY];

      pDst := ADst.ScanLine[AY + iY];
      Inc( pDst, AX );

      for iX := 0 to ASrc.Width-1 do begin
        if (AX + iX) >= ADst.Width then Continue;

        pDst^ := pSrc^;

        Inc(pSrc);
        Inc(pDst);
      end;
    end;
  except
    on E : Exception do
      Trace('RyuGraphics.DrawBitmapOn - ' + E.Message);
  end;
end;

procedure DrawBitmapOn(AX,AY:integer; ADst,ASrc:TBitmap; ATransparentColor:TColor);
var
  iX, iY: Integer;
  pSrc, pDst : PDWord;
begin
  if (ASrc.PixelFormat <> pf32bit) or (ADst.PixelFormat <> pf32bit) then
    raise Exception.Create('RyuGraphics.DrawBitmapOn - PixelFormat <> pf32bit');

  if (ASrc.Width * ASrc.Height) = 0 then Exit;

  if (AX < 0) or (AY < 0) then Exit;

  if (AX >= ADst.Width) or (AY >= ADst.Height) then Exit;

  try
    for iY := 0 to ASrc.Height-1 do begin
      if (AY + iY) >= ADst.Height then Continue;

      pSrc := ASrc.ScanLine[iY];

      pDst := ADst.ScanLine[AY + iY];
      Inc( pDst, AX );

      for iX := 0 to ASrc.Width-1 do begin
        if (AX + iX) >= ADst.Width then Continue;

        if pSrc^ <> ATransparentColor then pDst^ := pSrc^;

        Inc(pSrc);
        Inc(pDst);
      end;
    end;
  except
    on E : Exception do
      Trace('RyuGraphics.DrawBitmapOn - ' + E.Message);
  end;
end;

procedure UpdateLayeredControl(AControl:TWinControl; ABitmap:TBitmap);
var
  DestPoint, SourcePoint: TPoint;
  BlendFunction: TBlendFunction;
  Size: TSize;
  DC : HDC;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.UpdateLayeredControl - ABitmap.PixelFormat <> pf32bit');

  Size.cx := ABitmap.Width;
  Size.cy := ABitmap.Height;

  DestPoint := AControl.BoundsRect.TopLeft;
  SourcePoint := Point( 0, 0 );

  DC := GetDC( 0 );
  try
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    UpdateLayeredWindow( AControl.Handle, DC, @DestPoint, @Size, ABitmap.Canvas.Handle, @SourcePoint, clBlack, @BlendFunction, ULW_ALPHA);
  finally
    ReleaseDC( 0, DC );
  end;
end;

procedure SetAlpha(ABitmap:TBitmap; AAplpha:byte);
var
  pPixels : ^TRGBQuad;
  Loop: Integer;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.SetAlpha - ABitmap.PixelFormat <> pf32bit');

  pPixels := ABitmap.ScanLine[ABitmap.Height-1];

  for Loop := 1 to ABitmap.Width * ABitmap.Height do begin
    pPixels^.rgbReserved := AAplpha;
    Inc( pPixels );
  end;
end;

procedure SetAlpha(ABitmap:TBitmap; AAplpha:byte; AColor:TColor); overload;
var
  pPixels : ^TRGBQuad;
  Loop: Integer;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.SetAlpha - ABitmap.PixelFormat <> pf32bit');

  pPixels := ABitmap.ScanLine[ABitmap.Height-1];

  for Loop := 1 to ABitmap.Width * ABitmap.Height do begin
    if CompareMem(pPixels, @AColor, 3) then pPixels^.rgbReserved := AAplpha;
    Inc( pPixels );
  end;
end;

{ TRyuJpegImage }

procedure TRyuJpegImage.SaveToBitmap(Bitmap: TBitmap);
begin
  Bitmap.Width  :=  Width;
  Bitmap.Height := Height;
  Bitmap.Canvas.Draw(0, 0, Self);
end;

procedure PngToBitmap(APng:TPngImage; ABitmap:TBitmap);
type
  TPixels = array [0 .. 0] of TRGBQuad;
  TPixels2 = array [0 .. 0] of TRGBTriple;
var
  Pixels: ^TPixels;
  Pixels2: ^TPixels2;
  Col, Row: Integer;
  Alpha: single;
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.Width  := APng.Width;
  ABitmap.Height := APng.Height;

  for Row := 0 to ABitmap.Height - 1 do begin
    Pixels := ABitmap.ScanLine[Row];
    Pixels2 := APng.ScanLine[Row];

    for Col := 0 to ABitmap.Width - 1 do begin
      Pixels^[Col].rgbReserved := APng.AlphaScanline[Row]^[Col];

      if APng.AlphaScanline[Row]^[Col] < 1 then begin
        Pixels^[Col].rgbRed := 0;
        Pixels^[Col].rgbGreen := 0;
        Pixels^[Col].rgbBlue := 0;
      end else begin
        Alpha := Pixels^[Col].rgbReserved / 255;
        Pixels^[Col].rgbRed := Round(Pixels2^[Col].rgbtRed * Alpha);
        Pixels^[Col].rgbGreen := Round(Pixels2^[Col].rgbtGreen * Alpha);
        Pixels^[Col].rgbBlue := Round(Pixels2^[Col].rgbtBlue * Alpha);
      end;
    end;
  end;
end;

procedure BitmapScrollUp(ABitmap:TBitmap; ASize:integer);
const
  PIXELSIZE = 4;
var
  Loop, iSrc, iDst : Integer;
  pSrc, pDst : PByte;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.ScrollBitmap -Invalid PixelFormat');

  for Loop := 0 to ABitmap.Height-1 do begin
    iSrc := Loop;
    iDst := Loop - ASize;

    if iDst < 0 then Continue;

    pSrc := ABitmap.ScanLine[iSrc];
    pDst := ABitmap.ScanLine[iDst];

    Move( pSrc^, pDst^, ABitmap.Width * PIXELSIZE );
  end;

  ABitmap.Canvas.FillRect( Rect(0, ABitmap.Height-ASize-1, ABitmap.Width, ABitmap.Height) );
end;

procedure FitFormSize(AControl:TWinControl);
var
  MonitorRect : TRect;
begin
  MonitorRect := FindMonitorRect( AControl.Left, AControl.Top );
  if (MonitorRect.Width <= AControl.Width) or (MonitorRect.Height <= AControl.Height) then begin
    AControl.Left   := 0;
    AControl.Top    := 0;
    AControl.Width  := MonitorRect.Width;
    AControl.Height := MonitorRect.Height;
  end;
end;

procedure FlipBitmapHorizontal(ABitmap:TBitmap);
var
  pSrc, pDst : PDWord;
  iTemp : DWord;
  X, Y : integer;
begin
  if (ABitmap.Width * ABitmap.Height) = 0 then Exit;

  if ABitmap.PixelFormat <> pf32bit then
      raise Exception.Create('RyuGraphics.AssignBitmap - 사용 할 수 없는 PixelFormat 입니다.');

  for Y := 0 to ABitmap.Height-1 do begin
    pSrc := ABitmap.ScanLine[Y];

    pDst := ABitmap.ScanLine[Y];
    Inc( pDst, ABitmap.Width - 1 );

    for X := 0 to (ABitmap.Width div 2) - 1 do begin
      iTemp := pSrc^;
      pSrc^ := pDst^;
      pDst^ := iTemp;

      Inc( pSrc );
      Dec( pDst );
    end;
  end;
end;

end.

