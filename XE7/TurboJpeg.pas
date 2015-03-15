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

  TTurboJpeg = class
  private
    FEncoderHandle : pointer;
    FDecoderHandle : pointer;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure GetBuffer(var AData:pointer; ASize:integer);
    class procedure FreeBuffer(AData:pointer);

    {*
      ADst는 BitmapToJpeg() 호출하면 새로 메모리가 할당 된다.  사용 후에 삭제해야 한다.
    }
    procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);

    {*
      ADst는 BitmapToJpegCopy() 호출 전에 메모리가 확보되어 있는 상태여야 한다.
      따라서, ADst는 한 번 할당 받은 이후 계속 재사용 할 수 있다.
    }
    procedure BitmapToJpegCopy(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);

    procedure JpegToBitmap(ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer);
  end;

  TTurboJpegEncoder = class
  private
    FEncoderHandle : pointer;
  public
    constructor Create;
    destructor Destroy; override;

    {*
      ADst는 BitmapToJpeg() 호출하면 새로 메모리가 할당 된다.  사용 후에 삭제해야 한다.
    }
    procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);

    {*
      ADst는 BitmapToJpegCopy() 호출 전에 메모리가 확보되어 있는 상태여야 한다.
      따라서, ADst는 한 번 할당 받은 이후 계속 재사용 할 수 있다.
    }
    procedure BitmapToJpegCopy(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
  end;

// TODO: 모든 함수 접근을 싱크 할 필요 있는 지 검토
procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
procedure BitmapToJpegCopy(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
procedure JpegToBitmap(ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer);

implementation

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

var
  TurboJpegEncoderHandle : pointer = nil;
  TurboJpegDecoderHandle : pointer = nil;

procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
var
  pJpeg : pointer;
  jpegSize : DWord;
begin
  pJpeg := nil;
  jpegSize := 0;

  tjCompress2(
    TurboJpegEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpegSize,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  GetMem( ADst, jpegSize );

  try
    Move( pJpeg^, ADst^, jpegSize );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpegSize;
end;

procedure BitmapToJpegCopy(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
var
  pJpeg : pointer;
  jpegSize : DWord;
begin
  pJpeg := nil;
  jpegSize := 0;

  tjCompress2(
    TurboJpegEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpegSize,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  try
    Move( pJpeg^, ADst^, jpegSize );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpegSize;
end;

procedure JpegToBitmap(ASrc:pointer; ASrcSize:integer; ADst:pointer; AWidth,AHeight:integer);
begin
  tjDecompress2(
    TurboJpegDecoderHandle,
    ASrc, ASrcSize,
    ADst, AWidth, 0, AHeight, Integer(TJPF_BGRA), TJFLAG_BOTTOMUP
  );
end;

{ TTurboJpeg }

procedure TTurboJpeg.BitmapToJpeg(ASrc: pointer; AWidth, AHeight: integer;
  var ADst: pointer; var ADstSize: integer; AQuality: integer);
var
  pJpeg : pointer;
  jpegSize : DWord;
begin
  pJpeg := nil;
  jpegSize := 0;

  tjCompress2(
    FEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpegSize,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  GetMem( ADst, jpegSize );

  try
    Move( pJpeg^, ADst^, jpegSize );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpegSize;
end;

procedure TTurboJpeg.BitmapToJpegCopy(ASrc: pointer; AWidth,
  AHeight: integer; var ADst: pointer; var ADstSize: integer; AQuality: integer);
var
  jpegSize : DWord;
begin
  jpegSize := 0;

  tjCompress2(
    FEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    ADst, jpegSize,
    Integer(TJSAMP_444), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  ADstSize := jpegSize;
end;

constructor TTurboJpeg.Create;
begin
  inherited;

  FEncoderHandle := tjInitCompress;
  FDecoderHandle := tjInitDecompress;
end;

destructor TTurboJpeg.Destroy;
begin
  tjDestroy(FEncoderHandle);
  tjDestroy(FDecoderHandle);

  inherited;
end;

class procedure TTurboJpeg.FreeBuffer(AData: pointer);
begin
  tjFree( AData );
end;

class procedure TTurboJpeg.GetBuffer(var AData: pointer; ASize: integer);
begin
  AData := tjAlloc( ASize );
end;

procedure TTurboJpeg.JpegToBitmap(ASrc: pointer; ASrcSize: integer;
  ADst: pointer; AWidth, AHeight: integer);
begin
  tjDecompress2(
    FEncoderHandle,
    ASrc, ASrcSize,
    ADst, AWidth, 0, AHeight, Integer(TJPF_BGRA), TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );
end;

{ TTurboJpegEncoder }

procedure TTurboJpegEncoder.BitmapToJpeg(ASrc: pointer; AWidth,
  AHeight: integer; var ADst: pointer; var ADstSize: integer;
  AQuality: integer);
var
  pJpeg : pointer;
  jpegSize : DWord;
begin
  pJpeg := nil;
  jpegSize := 0;

  tjCompress2(
    FEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpegSize,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  GetMem( ADst, jpegSize );

  try
    Move( pJpeg^, ADst^, jpegSize );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpegSize;
end;

procedure TTurboJpegEncoder.BitmapToJpegCopy(ASrc: pointer; AWidth,
  AHeight: integer; var ADst: pointer; var ADstSize: integer;
  AQuality: integer);
var
  pJpeg : pointer;
  jpegSize : DWord;
begin
  pJpeg := nil;
  jpegSize := 0;

  tjCompress2(
    FEncoderHandle,
    ASrc, AWidth, 0, AHeight, Integer(TJPF_BGRA),
    pJpeg, jpegSize,
    Integer(TJSAMP_422), AQuality, TJFLAG_FASTDCT + TJFLAG_BOTTOMUP
  );

  try
    Move( pJpeg^, ADst^, jpegSize );
  finally
    tjFree( pJpeg );
  end;

  ADstSize := jpegSize;
end;

constructor TTurboJpegEncoder.Create;
begin
  inherited;

  FEncoderHandle := tjInitCompress;
end;

destructor TTurboJpegEncoder.Destroy;
begin
  tjDestroy(FEncoderHandle);

  inherited;
end;

initialization
  TurboJpegEncoderHandle := tjInitCompress;
  TurboJpegDecoderHandle := tjInitDecompress;
finalization
  tjDestroy(TurboJpegEncoderHandle);
  tjDestroy(tjInitDecompress);
end.
