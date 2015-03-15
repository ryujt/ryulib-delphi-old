unit EasyCam;

interface

uses
  yuvconverts, RyuGraphics,
  Generics.Collections,
  Windows, Messages, SysUtils, Classes, Controls, Graphics, SyncObjs;

const
  WM_CAP_START = WM_USER;
  WM_CAP_DRIVER_CONNECT       = WM_CAP_START+ 10;
  WM_CAP_DRIVER_DISCONNECT    = WM_CAP_START+ 11;

  WM_CAP_SET_CALLBACK_FRAME   = WM_CAP_START+ 5;
  WM_CAP_SET_VIDEOFORMAT      = WM_CAP_START+ 45;
  WM_CAP_SET_PREVIEW          = WM_CAP_START+ 50;
  WM_CAP_SET_OVERLAY          = WM_CAP_START+ 51;
  WM_CAP_SET_PREVIEWRATE      = WM_CAP_START+ 52;
  WM_CAP_SET_SCALE            = WM_CAP_START+ 53;

  WM_CAP_GRAB_FRAME_NOSTOP    = WM_CAP_START+ 61;

  WM_CAP_GET_VIDEOFORMAT      = WM_CAP_START+ 44;

  WM_CAP_DLG_VIDEOFORMAT      = WM_CAP_START+ 41;

type
  TVIDEOHDR= record
    lpData: Pointer; // address of video buffer
    dwBufferLength: DWord; // size, in bytes, of the Data buffer
    dwBytesUsed: DWord; // see below
    dwTimeCaptured: DWord; // see below
    dwUser: DWord; // user-specific data
    dwFlags: DWord; // see below
    dwReserved1, dwReserved2, dwReserved3: DWord; // reserved; do not use
  end;
  TVIDEOHDRPtr= ^TVideoHDR;

  TEasyCam = class (TWinControl)
  private
    // TODO:
    FCS : TCriticalSection;
    FPreviewHandle : THandle;
    FCodec : TVideoCodec;
    FBuffer : pointer;
    FBitmap : TBitmap;
    FIsBitmapReady : boolean;
  private
    FIsActive: boolean;
    FImageSize: integer;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open(AWidth,AHeight:integer);
    procedure Close;

    function GetBitmap(ABitmap:TBitmap):boolean;
  published
    property IsActive : boolean read FIsActive;
    property ImageWidth : integer read GetImageWidth;
    property ImageHeight : integer read GetImageHeight;
    property ImageSize : integer read FImageSize;
  end;

implementation

function capCreateCaptureWindow(lpszWindowName:LPCSTR; dwStyle:DWORD;
         x,y,width,height:integer; hwndParent: HWND; ID: integer):HWND; stdcall;
         external 'AVICAP32.DLL' name 'capCreateCaptureWindowA';

var
  EasyCamList : TList<TEasyCam>;

{ TEasyCam }

procedure TEasyCam.Close;
begin
  SendMessage(FPreviewHandle, WM_CAP_DRIVER_DISCONNECT, 0, 0);
  FIsActive := false;
end;

constructor TEasyCam.Create(AOwner: TComponent);
begin
  inherited;

  FIsActive := false;
  FImageSize := 0;

  FPreviewHandle := 0;
  FBuffer := nil;
  FIsBitmapReady := false;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;

  EasyCamList.Add(Self);
end;

destructor TEasyCam.Destroy;
begin
  EasyCamList.Remove(Self);

  Close;

  if FBuffer <> nil then FreeMem(FBuffer);

  FreeAndNil(FBitmap);

  inherited;
end;

function TEasyCam.GetBitmap(ABitmap: TBitmap): boolean;
begin
  Result := false;

  if not FIsBitmapReady then Exit;

  AssignBitmap( FBitmap, ABitmap );

  Result := true;
end;

function TEasyCam.GetImageHeight: integer;
begin
  Result := FBitmap.Height;
end;

function TEasyCam.GetImageWidth: integer;
begin
  Result := FBitmap.Width;
end;

function FindEasyCamWidthPreviewHandle(AHandle:hWnd):TEasyCam;
var
  Loop: Integer;
begin
  Result := nil;
  for Loop := 0 to EasyCamList.Count-1 do
    if EasyCamList[Loop].FPreviewHandle = AHandle then begin
      Result := EasyCamList[Loop];
      Break;
    end;
end;

function FrameCallbackFunction(AHandle:hWnd; VIDEOHDR:TVideoHDRPtr):bool; stdcall;
var
  EasyCam : TEasyCam;
begin
  EasyCam := FindEasyCamWidthPreviewHandle( AHandle );
  if (EasyCam = nil) or (EasyCam.FIsActive = false) then Exit;

  ConvertCodecToRGB( EasyCam.FCodec, VideoHDR^.lpData, EasyCam.FBuffer, EasyCam.FBitmap.Width, EasyCam.FBitmap.Height );
  Move( EasyCam.FBuffer^, EasyCam.FBitmap.ScanLine[EasyCam.FBitmap.Height-1]^, EasyCam.FImageSize);

  EasyCam.FIsBitmapReady := true;

  Result := true;
end;

procedure TEasyCam.Open(AWidth, AHeight: integer);
var
  BitmapInfo: TBitmapInfo;
begin
  if FIsActive then
    raise Exception.Create('TEasyCam.Open - FIsActive = true');

  FPreviewHandle:= capCreateCaptureWindow('Video', WS_CHILD or WS_VISIBLE, 0, 0, AWidth, AHeight, Handle, 1);
  SendMessage(FPreviewHandle, WM_CAP_DRIVER_CONNECT, 0, 0);
  SendMessage(FPreviewHandle, WM_CAP_SET_PREVIEWRATE, 1000 div 25, 0);
  sendMessage(FPreviewHandle, WM_CAP_SET_OVERLAY, 1, 0);
  SendMessage(FPreviewHandle, WM_CAP_SET_PREVIEW, 1, 0);
  SendMessage(FPreviewHandle, WM_CAP_SET_SCALE, 1, 0);

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  SendMessage(FPreviewHandle, WM_CAP_GET_VIDEOFORMAT, SizeOf(BitmapInfo), Integer(@BitmapInfo));

  BitmapInfo.bmiHeader.biWidth  := AWidth;
  BitmapInfo.bmiHeader.biHeight := AHeight;
  BitmapInfo.bmiHeader.biSizeImage := AWidth * AHeight * BitmapInfo.bmiHeader.biBitCount div 8;
  SendMessage(FPreviewHandle, WM_CAP_SET_VIDEOFORMAT, SizeOf(BitmapInfo), Integer(@BitmapInfo));

  // Make sure the screen size is changed.
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  SendMessage(FPreviewHandle, WM_CAP_GET_VIDEOFORMAT, SizeOf(BitmapInfo), Integer(@BitmapInfo));

  FBitmap.Width  := BitmapInfo.bmiHeader.biWidth;
  FBitmap.Height := BitmapInfo.bmiHeader.biHeight;

  if FBuffer <> nil then FreeMem(FBuffer);

  FImageSize := FBitmap.Width * FBitmap.Height * 4;

  Getmem( FBuffer, FImageSize );

  FCodec:= BICompressionToVideoCodec(bitmapinfo.bmiHeader.biCompression);
  if FCodec = vcUnknown then
    raise Exception.Create('TEasyCam.Open - FCodec = vcUnknown');

  SendMessage(FPreviewHandle, WM_CAP_SET_CALLBACK_FRAME, 0, integer(@FrameCallbackFunction));

  FIsActive := true;
end;

initialization
  EasyCamList := TList<TEasyCam>.Create;
end.
