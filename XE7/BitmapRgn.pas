unit BitmapRgn;

(*
  * 양병규님이 공개해주신 소스를 조금 수정한 버전입니다.
  * 제거할 컬러의 위치 또는 컬러를 직접 지정할 수 있도록 하였습니다.
    procedure TfmSelectWhiteBoardTool.FormCreate(Sender: TObject);
    var
      Rgn : HRGN;
    begin
      Image.Picture.Bitmap.PixelFormat := pf32bit;

      Rgn := CreateBitmapRgn32(Image.Picture.Bitmap, clBlack);
      try
        SetWindowRgn(Handle, Rgn, true);
      finally
        DeleteObject(Rgn);
      end;
    end;
*)

interface

uses
  Windows, Classes, SysUtils, Graphics;


function CreateBitmapRgn(Bitmap:TBitmap; TransparentColorX,TransparentColorY:integer):HRGN;
function CreateBitmapRgn32(Bitmap:TBitmap; TransparentColor:TColor):HRGN;

implementation

const
  DEFAULTRECTCOUNT = 50;

procedure GetTransparentPixel(var ATransparentPixel:pointer; ABitmap:TBitmap; AX,AY:integer);
var
  pByte : ^byte;
  PicxelSize : integer;
begin
  GetMem(ATransparentPixel, 4);
  
  case ABitmap.PixelFormat of
    pf8Bit : PicxelSize := 1;
    pf16Bit: PicxelSize := 2;
    pf24Bit: PicxelSize := 3;
    else PicxelSize := 4;
  end;

  pByte := ABitmap.ScanLine[0];
  Dec(pByte, (AY*ABitmap.Width + AX)*PicxelSize);

  Move(pByte^, ATransparentPixel^, PicxelSize);
end;

function CreateBitmapRgn(Bitmap: TBitmap; TransparentColorX,TransparentColorY:integer): HRGN;
type
  TPixels08 = array[0..0] of Byte;
  TPixels16 = array[0..0] of Word;
  TPixels24 = array[0..0] of TRGBTriple;
  TPixels32 = array[0..0] of Cardinal;
var
  PixelFormat: TPixelFormat;
  Pixels: Pointer;
  TransparentPixel: Pointer;
  RegionData: PRgnData;
  RegionBufferSize: Integer;
  RectCount, NewRectLeft: Integer;
  X, Y: Integer;
function IsTransparent(X: Integer): Boolean;
  begin
    case PixelFormat of
    pf8Bit : Result := TPixels08( Pixels^ )[ X ] = PByte( TransparentPixel )^;
    pf16Bit: Result := TPixels16( Pixels^ )[ X ] = PWord( TransparentPixel )^;
    pf24Bit: Result := ( TPixels24( Pixels^ )[ X ].rgbtRed = PRGBTriple( TransparentPixel )^.rgbtRed ) and
                       ( TPixels24( Pixels^ )[ X ].rgbtGreen = PRGBTriple( TransparentPixel )^.rgbtGreen ) and
                       ( TPixels24( Pixels^ )[ X ].rgbtBlue = PRGBTriple( TransparentPixel )^.rgbtBlue );
    pf32Bit: Result := TPixels32( Pixels^ )[ X ] = PCardinal( TransparentPixel )^;
    else Result := False;
    end;
  end;
procedure AddRect(LastCol: Boolean = False);
  type
    PRectBuffer = ^TRectBuffer;
    TRectBuffer = array[0..0] of TRect;
  begin
    if ( RegionBufferSize div SizeOf( TRect ) ) = RectCount then
     begin
       Inc( RegionBufferSize, SizeOf( TRect ) * DEFAULTRECTCOUNT );
       ReallocMem( RegionData, SizeOf( TRgnDataHeader ) + RegionBufferSize + 3 );
     end;

    if LastCol then Inc( X );

    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Left := NewRectLeft;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Top := Y;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Right := X;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Bottom := Y + 1;

    Inc( RectCount );
    NewRectLeft := -1;
  end;
begin
  PixelFormat := Bitmap.PixelFormat;
  Pixels := Bitmap.ScanLine[ 0 ];
  GetTransparentPixel(TransparentPixel, Bitmap, TransparentColorX, TransparentColorY);

  RectCount := 0;
  RegionBufferSize := SizeOf( TRect ) * DEFAULTRECTCOUNT;
  GetMem( RegionData, SizeOf( TRgnDataHeader ) + RegionBufferSize + 3 );
  try

    for Y := 0 to Bitmap.Height - 1 do
     begin
       Pixels := Bitmap.ScanLine[ Y ];
       NewRectLeft := -1;
       for X := 0 to Bitmap.Width - 1 do
        if IsTransparent( X ) then
         begin
           if NewRectLeft >= 0 then AddRect;
         end
        else
         begin
           if NewRectLeft = -1 then NewRectLeft := X;
           if ( X = Bitmap.Width - 1 ) and ( NewRectLeft >= 0 ) then AddRect( True );
         end;
     end;

    RegionData^.rdh.dwSize := SizeOf( TRgnDataHeader );
    RegionData^.rdh.iType := RDH_RECTANGLES;
    RegionData^.rdh.nCount := RectCount;
    RegionData^.rdh.nRgnSize := RectCount * SizeOf( TRect );
    Result := ExtCreateRegion( nil, RegionData^.rdh.dwSize + RegionData^.rdh.nRgnSize, RegionData^ );

  finally
    FreeMem( RegionData );
    FreeMem( TransparentPixel );
  end;
end;

function CreateBitmapRgn32(Bitmap:TBitmap; TransparentColor:TColor):HRGN;
var
  pPixel : PCardinal;
  TransparentPixel : Cardinal;
  RegionData: PRgnData;
  RegionBufferSize: Integer;
  RectCount, NewRectLeft: Integer;
  X, Y: Integer;

procedure AddRect(LastCol: Boolean = False);
  type
    PRectBuffer = ^TRectBuffer;
    TRectBuffer = array[0..0] of TRect;
  begin
    if ( RegionBufferSize div SizeOf( TRect ) ) = RectCount then
     begin
       Inc( RegionBufferSize, SizeOf( TRect ) * DEFAULTRECTCOUNT );
       ReallocMem( RegionData, SizeOf( TRgnDataHeader ) + RegionBufferSize + 3 );
     end;

    if LastCol then Inc( X );

    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Left := NewRectLeft;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Top := Y;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Right := X;
    PRectBuffer( @RegionData^.Buffer )^[ RectCount ].Bottom := Y + 1;

    Inc( RectCount );
    NewRectLeft := -1;
  end;
  
begin
  if Bitmap.PixelFormat <> pf32bit then
    raise Exception.Create('CreateBitmapRgn32: pf32bit 포멧만 지원합니다.');

  TransparentPixel :=
    GetRValue(TransparentColor) shl 16 +
    GetGValue(TransparentColor) shl  8 +
    GetBValue(TransparentColor);

  RectCount := 0;
  RegionBufferSize := SizeOf(TRect) * DEFAULTRECTCOUNT;
  GetMem(RegionData, SizeOf(TRgnDataHeader) + RegionBufferSize + 3);
  try
    for Y := 0 to Bitmap.Height - 1 do begin
      pPixel := Bitmap.ScanLine[Y];
      NewRectLeft := -1;
      for X := 0 to Bitmap.Width - 1 do begin
        if pPixel^ = TransparentPixel then begin
          if NewRectLeft >= 0 then AddRect;
        end else begin
          if NewRectLeft = -1 then NewRectLeft := X;
          if ( X = Bitmap.Width - 1 ) and ( NewRectLeft >= 0 ) then AddRect( True );
        end;
        
        Inc(pPixel);
       end
    end;

    RegionData^.rdh.dwSize := SizeOf( TRgnDataHeader );
    RegionData^.rdh.iType := RDH_RECTANGLES;
    RegionData^.rdh.nCount := RectCount;
    RegionData^.rdh.nRgnSize := RectCount * SizeOf( TRect );
    Result := ExtCreateRegion( nil, RegionData^.rdh.dwSize + RegionData^.rdh.nRgnSize, RegionData^ );
  finally
    FreeMem( RegionData );
  end;
end;

end.
