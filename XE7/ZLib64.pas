unit ZLib64;

interface

uses
  System.SysUtils, System.Classes;

type
  TAlloc = function (AppData: Pointer; Items, Size: Integer): Pointer;
  TFree = procedure (AppData, Block: Pointer);

  TZStreamRec = packed record
    next_in: PAnsiChar;   // next input byte
    avail_in: Integer;    // number of bytes available at next_in
    total_in: Integer;    // total nb of input bytes read so far

    next_out: PAnsiChar;      // next output byte should be put here
    avail_out: Integer;   // remaining free space at next_out
    total_out: Integer;   // total nb of bytes output so far

    msg: PAnsiChar;           // last error message, NULL if no error
    internal: Pointer;    // not visible by applications

    zalloc: TAlloc;       // used to allocate the internal state
    zfree: TFree;         // used to free the internal state
    AppData: Pointer;     // private data object passed to zalloc and zfree

    data_type: Integer;   //  best guess about the data type: ascii or binary
    adler: Integer;       // adler32 value of the uncompressed data
    reserved: Integer;    // reserved for future use
    padding : Integer;    // 8byte align
  end;

  // Abstract ancestor class
  TCustomZlibStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Integer;
    FOnProgress: TNotifyEvent;
    FZRec: TZStreamRec;
    FBuffer: array [Word] of AnsiChar;
  protected
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    constructor Create(Strm: TStream);
  end;
  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TCompressionStream = class(TCustomZlibStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  TDecompressionStream = class(TCustomZlibStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property OnProgress;
  end;



{ CompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
                      out OutBuf: Pointer; out OutBytes: Integer);


{ DecompressBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
       OutEstimate = zero, or est. size of the decompressed data
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
 OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);

{ DecompressToUserBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to user-allocated buffer to contain decompressed data
       BufSize = number of bytes in OutBuf   }
procedure DecompressToUserBuf(const InBuf: Pointer; InBytes: Integer;
  const OutBuf: Pointer; BufSize: Integer);

const
  zlib_Version : AnsiString = '1.2.5';
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

const
  zlibso = 'zlib64.dll';

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

// Direct ZLib access

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer;
procedure zlibFreeMem(AppData, Block: Pointer);
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PAnsiChar; recsize: Integer): Integer;
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
function deflateEnd(var strm: TZStreamRec): Integer;
function inflateInit_(var strm: TZStreamRec; version: PAnsiChar; recsize: Integer): Integer;
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
function inflateEnd(var strm: TZStreamRec): Integer;
function inflateReset(var strm: TZStreamRec): Integer;

implementation

uses
  ZLib64Const;

procedure _memset(P: Pointer; B: Byte; count: Integer);
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer);
begin
  Move(source^, dest^, count);
end;

// deflate compresses data
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PAnsiChar;
  recsize: Integer): Integer; external zlibso name 'deflateInit_';
function deflate(var strm: TZStreamRec; flush: Integer): Integer; external zlibso name 'deflate';
function deflateEnd(var strm: TZStreamRec): Integer; external zlibso name 'deflateEnd';

// inflate decompresses data
function inflateInit_(var strm: TZStreamRec; version: PAnsiChar; recsize: Integer): Integer; external zlibso name 'inflateInit_';
function inflate(var strm: TZStreamRec; flush: Integer): Integer; external zlibso name 'inflate';
function inflateEnd(var strm: TZStreamRec): Integer; external zlibso name 'inflateEnd';
function inflateReset(var strm: TZStreamRec): Integer; external zlibso name 'inflateReset';

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer;
begin
  Result := AllocMem(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create(sError); //!!
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create(sError);  //!!
end;

procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
                      out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TZStreamRec;
  P: Pointer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    CCheck(deflateInit_(strm, Z_BEST_COMPRESSION, PAnsiChar(zlib_version), sizeof(strm)));
    try
      while CCheck(deflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PAnsiChar(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := 256;
      end;
    finally
      CCheck(deflateEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;


procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TZStreamRec;
  P: Pointer;
  BufInc: Integer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  BufInc := (InBytes + 255) and not 255;
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    DCheck(inflateInit_(strm, PAnsiChar(zlib_version), sizeof(strm)));
    try
      while DCheck(inflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PAnsiChar(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := BufInc;
      end;
    finally
      DCheck(inflateEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

procedure DecompressToUserBuf(const InBuf: Pointer; InBytes: Integer;
  const OutBuf: Pointer; BufSize: Integer);
var
  strm: TZStreamRec;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  strm.next_in := InBuf;
  strm.avail_in := InBytes;
  strm.next_out := OutBuf;
  strm.avail_out := BufSize;
  DCheck(inflateInit_(strm, PAnsiChar(zlib_version), sizeof(strm)));
  try
    if DCheck(inflate(strm, Z_FINISH)) <> Z_STREAM_END then
      raise EZlibError.CreateRes(@sTargetBufferTooSmall);
  finally
    DCheck(inflateEnd(strm));
  end;
end;

// TCustomZlibStream

constructor TCustomZLibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
  FZRec.zalloc := zlibAllocMem;
  FZRec.zfree := zlibFreeMem;
end;

procedure TCustomZLibStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;


// TCompressionStream

constructor TCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Dest: TStream);
const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Dest);
  FZRec.next_out := FBuffer;
  FZRec.avail_out := sizeof(FBuffer);
  CCheck(deflateInit_(FZRec, Levels[CompressionLevel], PAnsiChar(zlib_version), sizeof(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.next_in := nil;
  FZRec.avail_in := 0;
  try
    if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
    while (CCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END)
      and (FZRec.avail_out = 0) do
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
    end;
    if FZRec.avail_out < sizeof(FBuffer) then
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer) - FZRec.avail_out);
  finally
    deflateEnd(FZRec);
  end;
  inherited Destroy;
end;

function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ECompressionError.CreateRes(@sInvalidStreamOp);
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FZRec.next_in := @Buffer;
  FZRec.avail_in := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_in > 0) do
  begin
    CCheck(deflate(FZRec, 0));
    if FZRec.avail_out = 0 then
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
  end;
  Result := Count;
end;

function TCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FZRec.total_in
  else
    raise ECompressionError.CreateRes(@sInvalidStreamOp);
end;

function TCompressionStream.GetCompressionRate: Single;
begin
  if FZRec.total_in = 0 then
    Result := 0
  else
    Result := (1.0 - (FZRec.total_out / FZRec.total_in)) * 100.0;
end;


// TDecompressionStream

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZRec.next_in := FBuffer;
  FZRec.avail_in := 0;
  DCheck(inflateInit_(FZRec, PAnsiChar(zlib_version), sizeof(FZRec)));
end;

destructor TDecompressionStream.Destroy;
begin
  FStrm.Seek(-FZRec.avail_in, 1);
  inflateEnd(FZRec);
  inherited Destroy;
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FZRec.next_out := @Buffer;
  FZRec.avail_out := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FZRec.avail_out > 0) do
  begin
    if FZRec.avail_in = 0 then
    begin
      FZRec.avail_in := FStrm.Read(FBuffer, sizeof(FBuffer));
      if FZRec.avail_in = 0 then
      begin
        Result := Count - FZRec.avail_out;
        Exit;
      end;
	  FZRec.next_in := FBuffer;
      FStrmPos := FStrm.Position;
      Progress(Self);
    end;
    CCheck(inflate(FZRec, 0));
  end;
  Result := Count;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EDecompressionError.CreateRes(@sInvalidStreamOp);
end;

function TDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  I: Integer;
  Buf: array [0..4095] of AnsiChar;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    DCheck(inflateReset(FZRec));
    FZRec.next_in := FBuffer;
    FZRec.avail_in := 0;
    FStrm.Position := 0;
    FStrmPos := 0;
  end
  else if ( (Offset >= 0) and (Origin = soFromCurrent)) or
          ( ((Offset - FZRec.total_out) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then Dec(Offset, FZRec.total_out);
    if Offset > 0 then
    begin
      for I := 1 to Offset div sizeof(Buf) do
        ReadBuffer(Buf, sizeof(Buf));
      ReadBuffer(Buf, Offset mod sizeof(Buf));
    end;
  end
  else
    raise EDecompressionError.CreateRes(@sInvalidStreamOp);
  Result := FZRec.total_out;
end;

end.

end.
