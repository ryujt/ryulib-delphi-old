unit LZMA;

interface

uses
  Windows, Classes, SysUtils;

function CompressSlow(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
function CompressDefault(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
function CompressFast(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;

function SourceSize(AData:pointer):integer;

function Uncompress(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;

//function LzmaCompress(dest: PByte; destLen: PDWORD; const src: PByte; srcLen: DWORD;
//  outProps: PByte; outPropsSize: PDWORD; level: Integer; dictSize: DWORD;
//  lc, lp, pb, fb, numThreads:integer): Integer;
//  stdcall;
//
//function LzmaUncompress(dest: PByte; destLen: PDWORD; const src: PByte;
//    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer;
//    stdcall;

type
  TLzmaCompressFunc = function(dest: PByte; destLen: PDWORD; const src: PByte; srcLen: DWORD;
  outProps: PByte; outPropsSize: PDWORD; level: Integer; dictSize: DWORD;
  lc, lp, pb, fb, numThreads:integer): Integer;
  stdcall;
  TLzmaUncompress = function(dest: PByte; destLen: PDWORD; const src: PByte;
    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer;
    stdcall;

var
  LzmaCompress: TLzmaCompressFunc;
  LzmaUncompress: TLzmaUncompress;

implementation

//function LzmaCompress(dest: PByte; destLen: PDWORD; const src: PByte; srcLen: DWORD;
//  outProps: PByte; outPropsSize: PDWORD; level: Integer; dictSize: DWORD;
//  lc, lp, pb, fb, numThreads:integer): Integer;
//  stdcall; external 'LZMA.dll' delayed;
//
//function LzmaUncompress(dest: PByte; destLen: PDWORD; const src: PByte;
//    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer;
//    stdcall; external 'LZMA.dll' delayed;

const
  SPEED_SLOW = 0;
  SPEED_DEFAULT = 1;
  SPEED_FAST = 2;

type
  TProp = array [0 .. 4] of Byte;
  PProp = ^TProp;

function Compress(ASpeed:integer; ADataIn:pointer; ASizeIn:DWord; ADataOut:pointer; ASizeOut:PDWord):integer;
const
  LZMA_PROPS_SIZE = 5;
var
  pDst : PByte;
  prop : PProp;
  pSizeIn : PDWORD;
  propSize: DWORD;
begin
  propSize := LZMA_PROPS_SIZE;

  pDst := ADataOut;

  Move(ASizeIn, pDst^, SizeOf(DWORD));
  Inc(pDSt, SizeOf(DWORD));

  prop := PProp(pDst);
  Inc(pDSt, SizeOf(TProp));

  FillChar(prop^, SizeOf(TProp), 0);

  case ASpeed of
    SPEED_SLOW:
      Result := LzmaCompress(pDSt, ASizeOut, ADataIn, ASizeIn, PByte(prop), PDWORD(@propSize),
        9, 1 shl 24, 3, 0, 2, 273, 2
      );

    SPEED_DEFAULT:
      Result := LzmaCompress(pDSt, ASizeOut, ADataIn, ASizeIn, PByte(prop), PDWORD(@propSize),
        5, 1 shl 24, 3, 0, 2, 128, 2
      );

    SPEED_FAST:
      Result := LzmaCompress(pDSt, ASizeOut, ADataIn, ASizeIn, PByte(prop), PDWORD(@propSize),
        0, 1 shl 24, 3, 0, 2, 32, 2
      );

    else
      Result := -1;
  end;

  // 압축 후 크기가 더 크다면, 압축하지 않고 저장 함
  if ASizeOut^ >= ASizeIn then begin
    pSizeIn := PDWORD(ADataOut);

    pSizeIn^ := 0;
    Inc(pSizeIn);

    pSizeIn^ := ASizeIn;
    Inc(pSizeIn);

    Move(ADataIn^, pSizeIn^, ASizeIn);

    ASizeOut^ := ASizeIn + SizeOf(DWORD) * 2;
  end else begin
    ASizeOut^ := ASizeOut^ + SizeOf(DWORD) + SizeOf(TProp);
  end;
end;

function CompressSlow(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
begin
  Result := Compress(SPEED_SLOW, ADataIn, ASizeIn, ADataOut, @ASizeOut);
end;

function CompressDefault(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
begin
  Result := Compress(SPEED_DEFAULT, ADataIn, ASizeIn, ADataOut, @ASizeOut);
end;

function CompressFast(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
begin
  Result := Compress(SPEED_FAST, ADataIn, ASizeIn, ADataOut, @ASizeOut);
end;

function SourceSize(AData:pointer):integer;
var
  pSizeIn : PDWORD;
begin
  // 데이터가 압축되지 않고 저장 됨
  pSizeIn := AData;
  if pSizeIn^ = 0 then begin
    Inc(pSizeIn);
    Result := pSizeIn^;
  end else begin
    Result := pSizeIn^;
  end;
end;

function Uncompress(ADataIn:pointer; ASizeIn:integer; ADataOut:pointer; var ASizeOut:integer):integer;
const
  LZMA_PROPS_SIZE = 5;
var
  pSrc : PByte;
  prop : PProp;
  pSizeIn : PDWORD;
  propSize: DWORD;
begin
  propSize := LZMA_PROPS_SIZE;

  // 데이터가 압축되지 않고 저장 됨
  pSizeIn := ADataIn;
  if pSizeIn^ = 0 then begin
    Inc(pSizeIn);

    ASizeOut := pSizeIn^;
    Inc(pSizeIn);

    Move(pSizeIn^, ADataOut^, ASizeOut);

    Result := 0;

    Exit;
  end;

  pSrc := ADataIn;

  Move(pSrc^, ASizeOut, SizeOf(DWORD));
  Inc(pSrc, SizeOf(DWORD));

  prop := PProp(pSrc);
  Inc(pSrc, SizeOf(TProp));

  ASizeIn := ASizeIn - SizeOf(DWORD) - SizeOf(TProp);

  Result := LzmaUncompress(ADataOut, @ASizeOut, pSrc, @ASizeIn, PByte(prop), propSize);
end;

var
  dllHandle: Cardinal;
  FileName: array[0..MAX_PATH] of char;

initialization
  FillChar(FileName, sizeof(FileName), #0);
  GetModuleFileName(HInstance, FileName, SizeOf(FileName));
  dllHandle:= LoadLibrary(pchar(ExtractFilePath(FileName) + 'LZMA.dll'));
  if dllHandle <> 0 then begin
    @LzmaCompress:= GetProcAddress(dllHandle, 'LzmaCompress');
    @LzmaUncompress:= GetProcAddress(dllHandle, 'LzmaUncompress');
  end;
finalization
  FreeLibrary(dllHandle);
end.
