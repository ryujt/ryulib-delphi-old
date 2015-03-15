unit rmkUtil;

interface

uses
  Windows, Graphics, ExtCtrls;

const
  cHot: TColor = $00FDDE99;
  cgHotStart: TColor = $00FDF5E6;
  cGHotEnd: TColor = $00FDFBF6;
  cSelected: TColor = $00FDCE99;
  cGSelectedStart: TColor = $00FCEFC4;
  cGSelectedEnd: TColor = $00FDF8EF;
  cShadeSelect: TColor = $00F8F3EA;
  cDisabled: TColor = $00D9D9D9;
  cGDisabledStart: TColor = $00EAE9E9;
  cGDisabledEnd: TColor = $00FCFBFB;
  cShadeDisabled: TColor = $00F6F5F5;
  cGHeaderStart: TColor = $00F9F9F9;
  cGHeaderEnd: TColor = $00FEFEFE;
  cGHeaderHotStart: TColor = $00FFEDBD;
  cGHeaderHotEnd: TColor = $00FFF7E3;
  cGHeaderSelStart: TColor = $00FCEABA;
  cGHeaderSelEnd: TColor = $00FCF4E0;
  cBackground: TColor = clWindow;
  cLineHighLight: TColor = $00FDFAF4; //$00F8F0EA;

  function BlendCol(Color1, Color2: TColor; Value: Byte): TColor;
  procedure WinGradient(DC: HDC; ARect: TRect; AColor2, AColor1: TColor);
  procedure VistaGradient(const Img: TImage; const ARect: TRect;
    const c1, c2: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);
  procedure ShadeGradient(Img: TImage; Back, Color: TColor; Reverse: Boolean);
  procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
    const StartColor, EndColor: TColor);

implementation


function BlendCol(Color1, Color2: TColor; Value: Byte): TColor;
var
  i, j: LongInt;
begin
  Value := Round(2.56 * Value);
  i := ColorToRGB(Color1);
  j := ColorToRGB(Color2);
  Result := Byte((Value * (Byte(i) - Byte(j))) shr 8 + Byte(j));
  Result := Result + (Byte((Value * (Byte(i shr 8) - Byte(j shr 8))) shr 8 +
    Byte(j shr 8)) shl 8);
  Result := Result + (Byte((Value * (Byte(i shr 16) - Byte(j shr 16))) shr 8 +
    Byte(j shr 16)) shl 16);
end;

procedure WinGradient(DC: HDC; ARect: TRect; AColor2, AColor1: TColor);
var
  Vertexs: array[0..1] of TTriVertex;
  GRect: TGradientRect;
begin
  Vertexs[0].X := ARect.Left;
  Vertexs[0].Y := ARect.Top;
  Vertexs[0].Red := (AColor1 and $000000FF) shl 8;
  Vertexs[0].Green := (AColor1 and $0000FF00);
  Vertexs[0].Blue := (AColor1 and $00FF0000) shr 8;
  Vertexs[0].Alpha := 0;
  Vertexs[1].X := ARect.Right;
  Vertexs[1].Y := ARect.Bottom;
  Vertexs[1].Red := (AColor2 and $000000FF) shl 8;
  Vertexs[1].Green := (AColor2 and $0000FF00);
  Vertexs[1].Blue := (AColor2 and $00FF0000) shr 8;
  Vertexs[1].Alpha := 0;
  GRect.UpperLeft := 0;
  GRect.LowerRight := 1;
  GradientFill(DC, @Vertexs, 2, @GRect, 1, GRADIENT_FILL_RECT_V);
end;

procedure VistaGradient(const Img: TImage; const ARect: TRect;
  const c1, c2: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);
type
  PRGB = ^TRGB;
  TRGB = record b, g, r: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBARRAY = array[0..65536] of TRGB;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3: Integer;
  x, y, w, h, j: Integer;
  i, w1: Integer;
  Row: PRGBArray;
  C: TRGB;
  slMain, slSize, slPtr: Integer;
  Color: Integer;
  QCol: array of TRGB;
  g: Byte;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Width := (ARect.Right - ARect.Left) - 1;
  bmp.Height := (ARect.Bottom - ARect.Top) - 1;
  h := bmp.Height;
  w := bmp.Width;
  Color := ColorToRGB(c3);
  rc1 := Byte(Color);
  gc1 := Byte(Color shr 8);
  bc1 := Byte(Color shr 16);
  Color := ColorToRGB(c4);
  rc2 := Byte(Color);
  gc2 := Byte(Color shr 8);
  bc2 := Byte(Color shr 16);
  SetLength(QCol, h);
  for i := 0 to h - 1 do
  begin
    if Reverse then
    begin
      QCol[i].r := Byte(rc1 + (((rc2 - rc1) * i) div h));
      QCol[i].g := Byte(gc1 + (((gc2 - gc1) * i) div h));
      QCol[i].b := Byte(bc1 + (((bc2 - bc1) * i) div h));
    end
    else
    begin
      QCol[i].r := Byte(rc2 + (((rc1 - rc2) * i) div h));
      QCol[i].g := Byte(gc2 + (((gc1 - gc2) * i) div h));
      QCol[i].b := Byte(bc2 + (((bc1 - bc2) * i) div h));
    end;
  end;
  w1 := w - 1;
  if Center then
    w := (w shr 1) + (w and 1);
  slMain := Integer(bmp.ScanLine[0]); // Init scanline accsess
  slSize := Integer(bmp.ScanLine[1]) - slMain;
  for x := 0 to w - 1 do
  begin // Paint gradient
    j := (255 - c1) + MulDiv(c1, x, w);
    slPtr := slMain;
    for y := 0 to h - 1 do
    begin
      Row := PRGBArray(slPtr);
      Row[x].r := Byte(j * (QCol[y].r - rc1) shr 8 + rc1);
      Row[x].g := Byte(j * (QCol[y].g - gc1) shr 8 + gc1);
      Row[x].b := Byte(j * (QCol[y].b - bc1) shr 8 + bc1);
      if (Center) and (x < (w1 - x)) then
        Row[w1 - x] := Row[x];
      slPtr := slPtr + slSize;
    end;
  end;
  QCol := nil;
  Img.Picture.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure ShadeGradient(Img: TImage; Back, Color: TColor; Reverse: Boolean);
type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 0] of TRGB24;
var
  X, Y, w, h, j, w1: Integer;
  Row: PRGBArray;
  slMain, slSize: Integer;
  R, G, B, a: Byte;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Canvas.Brush.Color:= Back;
  w := Img.Width;
  h := Img.Height;
  bmp.Width := w;
  bmp.Height := h;
  Color := ColorToRGB(Color);
  R := Byte(Color);
  G := Byte(Color shr 8);
  B := Byte(Color shr 16);
  w1 := w - 1;
  w := (w shr 1) + (w and 1);
  slMain := Integer(bmp.ScanLine[0]);
  slSize := Integer(bmp.ScanLine[1]) - slMain;
  for Y := 0 to h - 1 do
  begin
    if Reverse then
      j := 255 - MulDiv(255, Y, h)
    else
      j := MulDiv(255, Y + 1, h);
    for X := 0 to w - 1 do
    begin
      a := 255 - MulDiv(j, X, w);
      Row := PRGBArray(slMain);
      Row[X].R := a * (Row[X].R - R) shr 8 + R;
      Row[X].G := a * (Row[X].G - G) shr 8 + G;
      Row[X].B := a * (Row[X].B - B) shr 8 + B;
      if (X < (w1 - X)) then
      begin
        Row[w1 - X].R := a * (Row[w1 - X].R - R) shr 8 + R;
        Row[w1 - X].G := a * (Row[w1 - X].G - G) shr 8 + G;
        Row[w1 - X].B := a * (Row[w1 - X].B - B) shr 8 + B;
      end;
    end;
    slMain := slMain + slSize;
  end;
  Img.Picture.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;
  TGradientColors = array[0..255] of TRGBTriple;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3, y1, i, GSize: Integer;
  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
  GradientBmp: TBitmap;
begin
  rc2 := GetRValue(ColorToRGB(StartColor));
  gc2 := GetGValue(ColorToRGB(StartColor));
  bc2 := GetBValue(ColorToRGB(StartColor));
  rc1 := GetRValue(ColorToRGB(EndColor));
  gc1 := GetGValue(ColorToRGB(EndColor));
  bc1 := GetBValue(ColorToRGB(EndColor));
  rc3 := rc1 + (((rc2 - rc1) * 15) div 9);
  gc3 := gc1 + (((gc2 - gc1) * 15) div 9);
  bc3 := bc1 + (((bc2 - bc1) * 15) div 9);
  if rc3 < 0 then
    rc3 := 0
  else if rc3 > 255 then
    rc3 := 255;
  if gc3 < 0 then
    gc3 := 0
  else if gc3 > 255 then
    gc3 := 255;
  if bc3 < 0 then
    bc3 := 0
  else if bc3 > 255 then
    bc3 := 255;
  GradientBMP := TBitmap.Create;
  GradientBmp.PixelFormat := pf24bit;
  GradientBmp.Width := 1;
  GradientBmp.Height := (ARect.Bottom - ARect.Top) - 1;
  GSize := GradientBmp.Height;
  y1 := GSize div 2;
  for i := 0 to y1 - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    GradCol.rgbtRed := Byte(rc1 + (((rc2 - rc1) * (i)) div y1));
    GradCol.rgbtGreen := Byte(gc1 + (((gc2 - gc1) * (i)) div y1));
    GradCol.rgbtBlue := Byte(bc1 + (((bc2 - bc1) * (i)) div y1));
    Row[0] := GradCol;
  end;
  for i := y1 to GSize - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    GradCol.rgbtRed := Byte(rc3 + (((rc2 - rc3) * (i)) div GSize));
    GradCol.rgbtGreen := Byte(gc3 + (((gc2 - gc3) * (i)) div GSize));
    GradCol.rgbtBlue := Byte(bc3 + (((bc2 - bc3) * (i)) div GSize));
    Row[0] := GradCol;
  end;
  Canvas.StretchDraw(ARect, GradientBmp);
  GradientBmp.Free;
end;

end.
