unit OpenCV;

interface

uses
  DebugTools, RyuGraphics,
  Windows, Classes, SysUtils, Graphics;

procedure ResizeBitmap32(ABitmapSrc,ABitmapDst:TBitmap; AKeepRatio:boolean=false);

implementation

procedure resize32bit(pSrc,pDst:pointer; widthSrc,heightSrc,widthDst,heightDst:integer); cdecl;
          external 'libImageResize.dll' delayed;

procedure ResizeBitmap32(ABitmapSrc,ABitmapDst:TBitmap; AKeepRatio:boolean=false);
var
  ptSrc, ptDst, ptResult : TPoint;
begin
  Assert(ABitmapSrc.PixelFormat = pf32Bit, 'Invalid PixelFormat');
  Assert(ABitmapDst.PixelFormat = pf32Bit, 'Invalid PixelFormat');

  if (ABitmapSrc.Width = 0) or (ABitmapSrc.Height = 0) or (ABitmapDst.Width = 0) or (ABitmapDst.Height = 0) then begin
    {$IFDEF DEBUG}
    Trace( 'RyuGraphics.SmoothResize - (Src.Width = 0) or (Src.Height = 0) or (Dst.Width = 0) or (Dst.Height = 0)' );
    {$ENDIF}

    Exit;
  end;

  if AKeepRatio then begin
    ptSrc.X := ABitmapSrc.Width;
    ptSrc.Y := ABitmapSrc.Height;

    ptDst.X := ABitmapDst.Width;
    ptDst.Y := ABitmapDst.Height;

    ptResult := RatioSize(ptSrc, ptDst);

    ABitmapDst.Width  := ptResult.X;
    ABitmapDst.Height := ptResult.Y;
  end;

  if (ABitmapSrc.Width = ABitmapDst.Width) and (ABitmapSrc.Height = ABitmapDst.Height) then begin
    Move(
      ABitmapSrc.ScanLine[ABitmapSrc.Height-1]^,
      ABitmapDst.ScanLine[ABitmapDst.Height-1]^,
      ABitmapDst.Width * ABitmapDst.Height * 4
    );

    Exit;
  end;

  resize32bit(ABitmapSrc.ScanLine[ABitmapSrc.Height-1], ABitmapDst.ScanLine[ABitmapDst.Height-1], ABitmapSrc.Width, ABitmapSrc.Height, ABitmapDst.Width, ABitmapDst.Height);
end;

end.
