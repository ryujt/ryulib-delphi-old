{**************************************************************************************************}
{                                                                                                  }
{  YUVConverts                                                                                     }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YUVConverts.pas, part of CapDemoC.dpr.                                    }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$ALIGN ON, $BOOLEVAL OFF, $LONGSTRINGS ON, $IOCHECKS ON, $WRITEABLECONST OFF, $OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF, $TYPEDADDRESS ON, $MINENUMSIZE 1}

unit yuvconverts;

interface
uses
  Windows;
  
type
  TVideoCodec = (vcUnknown, vcRGB, vcYUY2, vcUYVY, vcBTYUV, vcYVU9, vcYUV12, vcY8, vcY211);

const
  BI_YUY2  = $32595559;  // 'YUY2'
  BI_UYVY  = $59565955;  // 'UYVY'
  BI_BTYUV = $50313459;  // 'Y41P'
  BI_YVU9  = $39555659;  // 'YVU9'  planar
  BI_YUV12 = $30323449;  // 'I420'  planar
  BI_Y8    = $20203859;  // 'Y8  '
  BI_Y211  = $31313259;  // 'Y211'

function BICompressionToVideoCodec(Value: DWord): TVideoCodec;

function ConvertCodecToRGB(Codec: TVideoCodec; Src, Dst: Pointer; AWidth, AHeight: Integer): Boolean;

implementation

function BICompressionToVideoCodec(Value: DWord): TVideoCodec;
begin
  case Value of
    BI_RGB, BI_BITFIELDS: Result := vcRGB;   // no RLE
    BI_YUY2:              Result := vcYUY2 ;
    BI_UYVY:              Result := vcUYVY ;
    BI_BTYUV:             Result := vcBTYUV;
    BI_YVU9:              Result := vcYVU9;
    BI_YUV12:             Result := vcYUV12;
    BI_Y8:                Result := vcY8;
    BI_Y211:              Result := vcY211;
  else
    Result := vcUnknown;
  end;
end;

const
  // RGB255 ColorFAQ
  fY  =  298.082 / 256;
  fRU =  0;
  fGU = -100.291 / 256;
  fBU =  516.411 / 256;
  fRV =  408.583 / 256;
  fGV = -208.120 / 256;
  fBV =  0;
  
{  // RGB219 ColorFAQ           too dark
  fY  =  256 / 256;
  fRU =  0;
  fGU =  -86.132 / 256;
  fBU =  443.506 / 256;
  fRV =  350.901 / 256;
  fGV = -178.738 / 256;
  fBV =  0; }

{  // Earl            same like RGB255
  fY  =  1.164;
  fRU =  0;
  fGU = -0.392;
  fBU =  2.017;
  fRV =  1.596;
  fGV = -0.813;
  fBV =  0;
}

// |R|   |fY fRU fRV|   |Y|   | 16|
// |G| = |fY fGU fGV| * |U| - |128|
// |B|   |fY fBU fBV|   |V|   |128|

type
  TYUV = packed record
    Y, U, V, F1: Byte;
  end;

  PBGR32 = ^TBGR32;
  TBGR32 = packed record
    B, G, R, A: Byte;
  end;

function YUVtoBGRAPixel(AYUV: DWord): DWord;
var
  ValueY, ValueU, ValueV: Integer;
  ValueB, ValueG, ValueR: Integer;
begin
  ValueY := TYUV(AYUV).Y - 16;
  ValueU := TYUV(AYUV).U - 128;
  ValueV := TYUV(AYUV).V - 128;

  ValueB := Trunc(fY * ValueY + fBU * ValueU);  // fBV = 0
  if ValueB > 255 then
    ValueB := 255;
  if ValueB <   0 then
    ValueB :=   0;

  ValueG := Trunc(fY * ValueY + fGU * ValueU + fGV * ValueV);
  if ValueG > 255 then
    ValueG := 255;
  if ValueG <   0 then
    ValueG :=   0;

  ValueR := Trunc(fY * ValueY + fRV * ValueV);  // fRU = 0
  if ValueR > 255 then
    ValueR := 255;
  if ValueR <   0 then
    ValueR :=   0;

  with TBGR32(Result) do begin
    B := ValueB;
    G := ValueG;
    R := ValueR;
    A := 0;
  end;
end;

type
  TDWordRec = packed record
  case Integer of
    0: (B0, B1, B2, B3: Byte);
    1: (W0, W1: Word);
  end;

// UYVY
// YUV 4:2:2 (Y sample at every pixel, U and V sampled at every second pixel
// horizontally on each line). A macropixel contains 2 pixels in 1 DWord.
// 16 Bits per Pixel, 4 Byte Macropixel
// U0 Y0 V0 Y1
procedure UYVYtoRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
type
  PUYVY = ^TUYVY;
  TUYVY = packed record
    U, Y0, V, Y1: Byte;
  end;

var
  x, y: Integer;
  w: Integer;
  SrcPtr: PDWord;
  DstPtr: PDWord;
  SrcLineSize: Integer;
  DstLineSize: Integer;
  YUV: DWord;
  b: Byte;
begin
  SrcLineSize := AWidth * 2;
  DstLineSize := AWidth * 4;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  w := (AWidth div 2) - 1;      { TODO : bei ungeraden Breiten fehlt letztes Pixel }
  for y := 0 to AHeight - 1 do begin
    SrcPtr := Src;
    DstPtr := Dst;
    for x := 0 to w do begin
      YUV := SrcPtr^;
      // First Pixel
      b := TDWordRec(YUV).B0;
      TDWordRec(YUV).B0 := TDWordRec(YUV).B1;
      TDWordRec(YUV).B1 := b;

      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      // Second Pixel
      TDWordRec(YUV).B0 := TDWordRec(YUV).B3;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Inc(SrcPtr);
    end;
    Dec(PByte(Dst), DstLineSize);
    Inc(PByte(Src), SrcLineSize);
  end;
end;

// YUY2, YUNV, V422
// YUV 4:2:2 as for UYVY but with different component ordering within the DWord
// macropixel.
// 16 Bits per Pixel, 4 Byte Macropixel
// Y0 U0 Y1 V0
procedure YUY2toRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
var
  x, y: Integer;
  w: Integer;
  SrcPtr: PDWord;
  DstPtr: PDWord;
  SrcLineSize: Integer;
  DstLineSize: Integer;
  YUV: DWord;
  b: Byte;
begin
  SrcLineSize := AWidth * 2;
  DstLineSize := AWidth * 4;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  w := (AWidth div 2) - 1;      { TODO : bei ungeraden Breiten fehlt letztes Pixel }
  for y := 0 to AHeight - 1 do begin
    SrcPtr := Src;
    DstPtr := Dst;
    for x := 0 to w do begin
      YUV := SrcPtr^;
      // First Pixel
      b := TDWordRec(YUV).B2;                  //  Y0 U Y1 V -> Y0 U V Y1
      TDWordRec(YUV).B2 := TDWordRec(YUV).B3;
      TDWordRec(YUV).B3 := b;

      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      // Second Pixel
      TDWordRec(YUV).B0 := TDWordRec(YUV).B3;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Inc(SrcPtr);
    end;
    Dec(PByte(Dst), DstLineSize);
    Inc(PByte(Src), SrcLineSize);
  end;
end;

// BTYUV, I42P
// YUV 4:1:1 (Y sample at every pixel, U and V sampled at every fourth pixel
// horizontally on each line). A macropixel contains 8 pixels in 3 DWords.
// 16 Bits per Pixel, 12 Byte Macropixel
// U0 Y0 V0 Y1 U4 Y2 V4 Y3 Y4 Y5 Y6 Y7
procedure BTYUVtoRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
type
  PBTYUVPixel = ^TBTYUVPixel;
  TBTYUVPixel = packed record
    U0, Y0, V0, Y1, U4, Y2, V4, Y3, Y4, Y5, Y6, Y7: Byte;
  end;

var
  x, y: Integer;
  w: Integer;
  SrcPtr: PBTYUVPixel;
  DstPtr: PDWord;
  SrcLineSize: Integer;
  DstLineSize: Integer;
  YUV: DWord;
  SrcPixel: TBTYUVPixel;
begin
  SrcLineSize := ((AWidth + 7) div 8) * (3 * 4);
  DstLineSize := AWidth * 4;

  w := AWidth - 1;
  for y := 0 to AHeight - 1 do begin
    SrcPtr := Src;
    DstPtr := Dst;
    x := w;
    while x > 0 do begin
      // read macropixel
      SrcPixel := SrcPtr^;
      // First 4 Pixel
      TYUV(YUV).U := SrcPixel.U0;
      TYUV(YUV).V := SrcPixel.V0;

      TYUV(YUV).Y := SrcPixel.Y0;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y1;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y2;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y3;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      // Second 4 Pixel
      TYUV(YUV).U := SrcPixel.U4;
      TYUV(YUV).V := SrcPixel.V4;

      TYUV(YUV).Y := SrcPixel.Y4;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y5;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y6;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);
      Dec(x);
      if x <= 0 then
        Break;

      TYUV(YUV).Y := SrcPixel.Y7;
      DstPtr^ := YUVtoBGRAPixel(YUV);
      Inc(DstPtr);

      Inc(SrcPtr);
    end;
    Inc(PByte(Dst), DstLineSize);
    Inc(PByte(Src), SrcLineSize);
  end;
end;

// YVU9
// 8 bit Y plane followed by 8 bit 4x4 subsampled V and U planes.
// 9 Bits per Pixel, planar format
procedure YVU9toRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
var
  x, y, r, l: Integer;
  w: Integer;
  SrcYPtr: PByte;
  SrcUPtr: PByte;
  SrcVPtr: PByte;
  DstPtr: PDWord;
  SrcYLineSize: Integer;
  SrcUVLineSize: Integer;
  DstLineSize: Integer;
  YUV: DWord;
begin
  DstLineSize := AWidth * 4;

  SrcYLineSize := AWidth;
  SrcUVLineSize := (AWidth + 3) div 4;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  SrcYPtr := Src;
  SrcVPtr := PByte(LongInt(SrcYPtr) + SrcYLineSize * AHeight);
  SrcUPtr := PByte(LongInt(SrcVPtr) + SrcUVLineSize * ((AHeight + 3) div 4));

  w := (AWidth div 4) - 1;      { TODO : bei ungeraden Breiten fehlt letztes Pixel }
  for y := 0 to (AHeight div 4) - 1 do begin  { TODO : bei ungeraden H枚hen fehlt letzte Reihe }
    for l := 0 to 3 do begin
      DstPtr := Dst;
      for x := 0 to w do begin
        // U and V
        YUV := (SrcUPtr^ shl 8) or (SrcVPtr^ shl 16);
        for r := 0 to 3 do begin
          YUV := (YUV and $00FFFF00) or SrcYPtr^;
          DstPtr^ := YUVtoBGRAPixel(YUV);
          Inc(DstPtr);
          Inc(SrcYPtr);
        end;
        Inc(SrcUPtr);
        Inc(SrcVPtr);
      end;
      Dec(PByte(Dst), DstLineSize);
      if l < 3 then begin
        Dec(SrcUPtr, SrcUVLineSize);
        Dec(SrcVPtr, SrcUVLineSize);
      end;
    end;
  end;
end;

// YUV12, I420, IYUV
// 8 bit Y plane followed by 8 bit 2x2 subsampled U and V planes.
// 12 Bits per Pixel, planar format
procedure YUV12toRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);  // I420, IYUV
var
  x, y, l: Integer;
  w: Integer;
  SrcYPtr: PByte;
  SrcUPtr: PByte;
  SrcVPtr: PByte;
  DstPtr: PDWord;
  SrcYLineSize: Integer;
  SrcUVLineSize: Integer;
  DstLineSize: Integer;
  YUV: DWord;
begin
  DstLineSize := AWidth * 4;

  SrcYLineSize := AWidth;
  SrcUVLineSize := (AWidth + 1) div 2;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  SrcYPtr := Src;
  SrcUPtr := PByte(LongInt(SrcYPtr) + SrcYLineSize * AHeight);
  SrcVPtr := PByte(LongInt(SrcUPtr) + SrcUVLineSize * ((AHeight + 1) div 2));

  w := (AWidth div 2) - 1;      { TODO : bei ungeraden Breiten fehlt letztes Pixel }
  for y := 0 to (AHeight div 2) - 1 do begin  { TODO : bei ungeraden H枚hen fehlt letzte Reihe }
    for l := 0 to 1 do begin
      DstPtr := Dst;
      for x := 0 to w do begin
        // First Pixel
        YUV := SrcYPtr^ or (SrcUPtr^ shl 8) or (SrcVPtr^ shl 16);
        DstPtr^ := YUVtoBGRAPixel(YUV);
        Inc(DstPtr);
        Inc(SrcYPtr);
        // Second Pixel
        YUV := (YUV and $00FFFF00) or SrcYPtr^;
        DstPtr^ := YUVtoBGRAPixel(YUV);
        Inc(DstPtr);
        Inc(SrcYPtr);
        Inc(SrcUPtr);
        Inc(SrcVPtr);
      end;
      Dec(PByte(Dst), DstLineSize);
      if l = 0 then begin
        Dec(SrcUPtr, SrcUVLineSize);
        Dec(SrcVPtr, SrcUVLineSize);
      end;
    end;
  end;
end;

// Y8, Y800
// Simple, single Y plane for monochrome images.
// 8 Bits per Pixel, planar format
procedure Y8toRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
var
  x, y: Integer;
  w: Integer;
  SrcPtr: PByte;
  DstPtr: PDWord;
  SrcLineSize: Integer;
  DstLineSize: Integer;
  Pixel: DWord;
begin
  SrcLineSize := AWidth;
  DstLineSize := AWidth * 4;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  w := (AWidth) - 1;
  for y := 0 to AHeight - 1 do begin
    SrcPtr := Src;
    DstPtr := Dst;
    for x := 0 to w do begin
      Pixel := SrcPtr^;
      TDWordRec(Pixel).B1 := TDWordRec(Pixel).B0;
      TDWordRec(Pixel).B2 := TDWordRec(Pixel).B0;
      TDWordRec(Pixel).B3 := 0;
      DstPtr^ := Pixel;
      Inc(DstPtr);
      Inc(SrcPtr);
    end;
    Dec(PByte(Dst), DstLineSize);
    Inc(PByte(Src), SrcLineSize);
  end;
end;

// Y211
// Packed YUV format with Y sampled at every second pixel across each line
// and U and V sampled at every fourth pixel.
// 8 Bits per Pixel, 4 Byte Macropixel
// Y0, U0, Y2, V0
procedure Y211toRGB(Src, Dst: Pointer; AWidth, AHeight: Integer);
type
  PYUYV = ^TYUYV;
  TYUYV = packed record
    Y0, U, Y2, V: Byte;
  end;

var
  x, y: Integer;
  w : Integer;
  SrcPtr : PDWord;
  DstPtr : PDWord;
  SrcLineSize : Integer;
  DstLineSize : Integer;
  YUV: DWord;
  BGR: DWord;
  b: Byte;
begin
  SrcLineSize := ((AWidth + 3) div 4) * 4;
  DstLineSize := AWidth * 4;

  // Dst is Bottom Top Bitmap
  Inc(PByte(Dst), (AHeight - 1) * DstLineSize);

  w := (AWidth div 4) - 1;      { TODO : bei ungeraden Breiten fehlt letztes Pixel }
  for y := 0 to AHeight - 1 do begin
    SrcPtr := Src;
    DstPtr := Dst;
    for x := 0 to w do begin
      // Y0 U Y2 V
      YUV := SrcPtr^;
      // First and second Pixel
      b := TDWordRec(YUV).B2;                   // Y0 U Y2 V -> Y0 U V Y2
      TDWordRec(YUV).B2 := TDWordRec(YUV).B3;
      TDWordRec(YUV).B3 := b;
      BGR := YUVtoBGRAPixel(YUV);
      DstPtr^ := BGR;
      Inc(DstPtr);
      DstPtr^ := BGR;
      Inc(DstPtr);

      // third and fourth
      TDWordRec(YUV).B0 := TDWordRec(YUV).B3;   // Y0 U V Y2 -> Y2 U V Y2
      BGR := YUVtoBGRAPixel(YUV);
      DstPtr^ := BGR;
      Inc(DstPtr);
      DstPtr^ := BGR;
      Inc(DstPtr);

      Inc(SrcPtr);
    end;
    Dec(PByte(Dst), DstLineSize);
    Inc(PByte(Src), SrcLineSize);
  end;
end;

function ConvertCodecToRGB(Codec: TVideoCodec; Src, Dst: Pointer; AWidth, AHeight: Integer): Boolean;
begin
  Result := True;
  case Codec of
    vcYUY2:  YUY2toRGB (Src, Dst, AWidth, AHeight);
    vcUYVY:  UYVYtoRGB (Src, Dst, AWidth, AHeight);
    vcBTYUV: BTYUVtoRGB(Src, Dst, AWidth, AHeight);
    vcYVU9:  YVU9toRGB (Src, Dst, AWidth, AHeight);
    vcYUV12: YUV12toRGB(Src, Dst, AWidth, AHeight);
    vcY8:    Y8toRGB   (Src, Dst, AWidth, AHeight);
    vcY211:  Y211toRGB (Src, Dst, AWidth, AHeight);
  else
    Result := False;
  end;
end;

//  History:
//  2005-02-12, Peter J. Haas
//
//  2002-02-22, Peter J. Haas
//   - add YVU9, YUV12 (I420)
//   - add Y211 (untested)
//
//  2001-06-14, Peter J. Haas
//   - First public version
//   - YUY2, UYVY, BTYUV (Y41P), Y8

end.
