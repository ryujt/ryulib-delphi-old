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
  DebugTools, HandleComponent, SimpleThread, RyuGraphics,
  Generics.Collections,
  Windows, Messages, Classes, SysUtils, Graphics, SyncObjs;

type
  TBitmapRgn32 = class
  private
    FHandle : THandle;
    FBitmap : TBitmap;
    FTransparentPixel : Cardinal;
  private
    FRGN: THandle;
    FRegionData: PRgnData;
    FRegionBufferSize: Integer;
    FRectCount, NewRectLeft: Integer;
    procedure do_AddRect(LastCol:boolean; X,Y:integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Prepare(AHandle:THandle; ABitmap:TBitmap; ATransparentColor:TColor);
    procedure ExecuteBitmap;
    procedure ApplyRegion;
  end;

  TBitmapRgn = class (THandleComponent)
  private
    procedure on_RGN(var msg:TMessage); message WM_USER;
  private
    FCS : TCriticalSection;
    FList : TList<TBitmapRgn32>;
    procedure do_Clear;
    function GetBitmapRgn32:TBitmapRgn32;
  private
    FSimpleThread : TSimpleThread;
    procedure on_FSimpleThread_Repeat(ASimpleThread:TSimpleThread);
  private
    FOnFinished: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure CreateRgn(AHandle:THandle; ABitmap:TBitmap; ATransparentColor:TColor);
  published
    property OnFinished : TNotifyEvent read FOnFinished write FOnFinished;
  end;

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

procedure AddRect(LastCol:boolean);
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
           if NewRectLeft >= 0 then AddRect(false);
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

procedure AddRect(LastCol:boolean);
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
          if NewRectLeft >= 0 then AddRect(false);
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

{ TBitmapRgn32 }

procedure TBitmapRgn32.ApplyRegion;
begin
  FRegionData^.rdh.dwSize := SizeOf( TRgnDataHeader );
  FRegionData^.rdh.iType := RDH_RECTANGLES;
  FRegionData^.rdh.nCount := FRectCount;
  FRegionData^.rdh.nRgnSize := FRectCount * SizeOf( TRect );

  FRGN := ExtCreateRegion( nil, FRegionData^.rdh.dwSize + FRegionData^.rdh.nRgnSize, FRegionData^ );
  try
    SetWindowRgn(FHandle, FRGN, true);
  finally
    DeleteObject(FRGN);
  end;
end;

constructor TBitmapRgn32.Create;
begin
  inherited;

  FRectCount := 0;
  FRegionBufferSize := SizeOf(TRect) * DEFAULTRECTCOUNT;
  GetMem(FRegionData, SizeOf(TRgnDataHeader) + FRegionBufferSize + 3);

  FBitmap := TBitmap.Create;
end;

procedure TBitmapRgn32.Prepare(AHandle:THandle; ABitmap: TBitmap; ATransparentColor: TColor);
begin
  FHandle := AHandle;

  AssignBitmap(ABitmap, FBitmap);

  // 완전히 빈 Bitmap을 사용하면 구멍 뚫기가 실패한다.
  FBitmap.Canvas.Pixels[0, 0] := clBlack;

  FTransparentPixel :=
    GetRValue(ATransparentColor) shl 16 +
    GetGValue(ATransparentColor) shl  8 +
    GetBValue(ATransparentColor);
end;

destructor TBitmapRgn32.Destroy;
begin
  FreeMem(FRegionData);

  FreeAndNil(FBitmap);

  inherited;
end;

procedure TBitmapRgn32.do_AddRect(LastCol:boolean; X,Y:integer);
type
  PRectBuffer = ^TRectBuffer;
  TRectBuffer = array[0..0] of TRect;
begin
  if (FRegionBufferSize div SizeOf( TRect )) = FRectCount then begin
    Inc( FRegionBufferSize, SizeOf( TRect ) * DEFAULTRECTCOUNT );
    ReallocMem(FRegionData, SizeOf( TRgnDataHeader ) + FRegionBufferSize + 3);
  end;

  if LastCol then Inc( X );

  PRectBuffer( @FRegionData^.Buffer )^[ FRectCount ].Left := NewRectLeft;
  PRectBuffer( @FRegionData^.Buffer )^[ FRectCount ].Top := Y;
  PRectBuffer( @FRegionData^.Buffer )^[ FRectCount ].Right := X;
  PRectBuffer( @FRegionData^.Buffer )^[ FRectCount ].Bottom := Y + 1;

  Inc( FRectCount );
  NewRectLeft := -1;
end;

procedure TBitmapRgn32.ExecuteBitmap;
var
  pPixel : PCardinal;
  X, Y : integer;
begin
  for Y := 0 to FBitmap.Height - 1 do begin
    pPixel := FBitmap.ScanLine[Y];
    NewRectLeft := -1;
    for X := 0 to FBitmap.Width - 1 do begin
      if pPixel^ = FTransparentPixel then begin
        if NewRectLeft >= 0 then do_AddRect(false, X, Y);
      end else begin
        if NewRectLeft = -1 then NewRectLeft := X;
        if ( X = FBitmap.Width - 1 ) and ( NewRectLeft >= 0 ) then do_AddRect(true, X, Y);
      end;

      Inc(pPixel);
     end
  end;
end;

{ TBitmapRgn }

procedure TBitmapRgn.Clear;
begin
  FCS.Acquire;
  try
    do_Clear;
  finally
    FCS.Release;
  end;
end;

constructor TBitmapRgn.Create(AOwner: TComponent);
begin
  inherited;

  FCS := TCriticalSection.Create;
  FList := TList<TBitmapRgn32>.Create;

  FSimpleThread := TSimpleThread.Create('TBitmapRgn.Create', on_FSimpleThread_Repeat);
end;

procedure TBitmapRgn.CreateRgn(AHandle:THandle; ABitmap: TBitmap;
  ATransparentColor: TColor);
var
  BitmapRgn32 : TBitmapRgn32;
begin
  FCS.Acquire;
  try
    do_Clear;

    BitmapRgn32 := TBitmapRgn32.Create;
    BitmapRgn32.Prepare(AHandle, ABitmap, ATransparentColor);

    FList.Add(BitmapRgn32);

    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

destructor TBitmapRgn.Destroy;
begin
  Clear;

  FSimpleThread.TerminateNow;

  inherited;
end;

procedure TBitmapRgn.do_Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do FList[Loop].Free;
  FList.Clear;
end;

function TBitmapRgn.GetBitmapRgn32:TBitmapRgn32;
begin
  Result := nil;

  FCS.Acquire;
  try
    if FList.Count = 0 then Exit;

    Result := FList[FList.Count-1];
    FList.Delete(FList.Count-1);

    do_Clear;
  finally
    FCS.Release;
  end;
end;

procedure TBitmapRgn.on_FSimpleThread_Repeat(ASimpleThread:TSimpleThread);
var
  BitmapRgn32 : TBitmapRgn32;
begin
  while not ASimpleThread.Terminated do begin
    BitmapRgn32 := GetBitmapRgn32;
    if BitmapRgn32 <> nil then begin
      BitmapRgn32.ExecuteBitmap;
      PostMessage(Handle, WM_USER, Integer(BitmapRgn32), Integer(BitmapRgn32));
    end;

    ASimpleThread.SleepTight;
  end;

//  FreeAndNil(FCS);
//  FreeAndNil(FBitmaps);
end;

procedure TBitmapRgn.on_RGN(var msg: TMessage);
var
  BitmapRgn32 : TBitmapRgn32;
begin
  BitmapRgn32 := TBitmapRgn32(Pointer(msg.WParam));
  try
    BitmapRgn32.ApplyRegion;
    if Assigned(FOnFinished) then FOnFinished(Self);
  finally
    BitmapRgn32.Free;
  end;
end;

end.
