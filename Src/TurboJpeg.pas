unit TurboJpeg;

interface

uses
  Windows;

const
  TJFLAG_BOTTOMUP = 2;
  TJFLAG_FASTUPSAMPLE = 256;
  TJFLAG_NOREALLOC = 1024;
  TJFLAG_FASTDCT = 2048;
  TJFLAG_ACCURATEDCT = 4096;

type
  TtjPF = (
    TJPF_RGB, TJPF_BGR, TJPF_RGBX, TJPF_BGRX, TJPF_XBGR, TJPF_XRGB, TJPF_GRAY,
    TJPF_RGBA, TJPF_BGRA, TJPF_ABGR, TJPF_ARGB, TJPF_CMYK
  );

  TtjSAMP = (
    TJSAMP_444, TJSAMP_422, TJSAMP_420, TJSAMP_GRAY, TJSAMP_440, TJSAMP_411
  );

function tjInitCompress:pointer; cdecl;
         external 'turbojpeg.dll' delayed;

function tjInitDecompress:pointer; cdecl;
         external 'turbojpeg.dll' delayed;

function tjCompress2(handle:pointer;
         srcBuf:pointer; width,pitch,height,pixelFormat:integer;
         var jpegBuf:pointer; var jpegSize:DWord;
         jpegSubsamp,jpegQual,flags:integer):integer; cdecl;
         external 'turbojpeg.dll' delayed;

function tjDecompress2(handle:pointer;
         jpegBuf:pointer; jpegSize:Dword;
         dstBuf:pointer; width,pitch,height,pixelFormat:integer;
         flags:integer):integer; cdecl;
         external 'turbojpeg.dll' delayed;

function tjDestroy(handle:pointer):integer; cdecl;
         external 'turbojpeg.dll' delayed;

function tjAlloc(bytes:integer):pointer; cdecl;
         external 'turbojpeg.dll' delayed;

procedure tjFree(buffer:pointer); cdecl;
         external 'turbojpeg.dll' delayed;

function CreateEncoderHandle:pointer;
function CreateDecoderHandle:pointer;

procedure DestroyHandle(AHandle:pointer);

function BitmapToJpeg24(AHandle:pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer=100):boolean;
function JpegToBitmap24(AHandle:pointer; ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer):boolean;

function BitmapToJpeg32(AHandle:pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer=100):boolean;
function JpegToBitmap32(AHandle:pointer; ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer):boolean;

implementation

function CreateEncoderHandle:pointer;
begin
  Result := tjInitCompress;
end;

function CreateDecoderHandle:pointer;
begin
  Result := tjInitDecompress;
end;

procedure DestroyHandle(AHandle:pointer);
begin
  tjDestroy(AHandle);
end;

function BitmapToJpeg24(AHandle:pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer=100):boolean;
var
  pJpeg : pointer;
  jpeg_size : DWord;
begin
  pJpeg := nil;
  jpeg_size := 0;
  ADstSize := 0;

  Result := 0 = tjCompress2(
    AHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGR),
    pJpeg, jpeg_size,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  if not Result then Exit;

  try
    Move( pJpeg^, ADst^, jpeg_size );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpeg_size;
end;

function JpegToBitmap24(AHandle:pointer; ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer):boolean;
begin
  Result := 0 = tjDecompress2(
    AHandle,
    ASrc, ASrcSize,
    ADst, AWidth, 0, AHeight, Integer(TJPF_BGR), TJFLAG_BOTTOMUP
  );
end;

function BitmapToJpeg32(AHandle:pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer=100):boolean;
var
  pJpeg : pointer;
  jpeg_size : DWord;
begin
  pJpeg := nil;
  jpeg_size := 0;
  ADstSize := 0;

  Result := 0 = tjCompress2(
    AHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpeg_size,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  if not Result then Exit;

  try
    Move( pJpeg^, ADst^, jpeg_size );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpeg_size;
end;

function JpegToBitmap32(AHandle:pointer; ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer):boolean;
begin
  Result := 0 = tjDecompress2(
    AHandle,
    ASrc, ASrcSize,
    ADst, AWidth, 0, AHeight, Integer(TJPF_BGRA), TJFLAG_BOTTOMUP
  );
end;

end.
