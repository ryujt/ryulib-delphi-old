unit CursorCaptrue;

interface

uses
  DebugTools, ZLibUtils,
  Generics.Collections,
  Windows, Classes, SysUtils, Vcl.Graphics;

type
  TCursorImage = class (TComponent)
  private
    FIconInfo : TIconInfo;
    FCursorInfo : TCursorInfo;
    procedure init;
    function get_CursorInfo: boolean;
    procedure get_IconInfo;
  private
    FPosition: TPoint;
    FHotSpot: TPoint;
    FBitmapMask: TBitmap;
    FBitmapColor: TBitmap;
    FBitmapInvert: TBitmap;
    FOffset: TPoint;
    function GetX: integer;
    function GetY: integer;
    function GetOffsetX: integer;
    function GetOffsetY: integer;
    procedure SetOffsetX(const Value: integer);
    procedure SetOffsetY(const Value: integer);
    function GetHeight: integer;
    function GetWidth: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Capture;

    procedure Draw(ABitmap:TBitmap);

    function GetImageData(var AData:pointer; var ASize:integer):boolean;
  published
    property Offset : TPoint read FOffset write FOffset;
    property OffsetX : integer read GetOffsetX write SetOffsetX;
    property OffsetY : integer read GetOffsetY write SetOffsetY;

    property X : integer read GetX;
    property Y : integer read GetY;

    property Width : integer read GetWidth;
    property Height : integer read GetHeight;

    property Position : TPoint read FPosition;

    property HotSpot : TPoint read FHotSpot;

    property BitmapMask : TBitmap read FBitmapMask;
    property BitmapColor : TBitmap read FBitmapColor;
    property BitmapInvert : TBitmap read FBitmapInvert;
  end;

  ICursor = interface
    ['{B65A5F33-09AF-4143-A761-FF730B5F1CCE}']

    function GetImageData(var AData:pointer; var ASize:integer):boolean;

    function GetCursorIndex: integer;

    property CursorIndex : integer read GetCursorIndex;

    function GetX: integer;
    function GetY: integer;

    property X : integer read GetX;
    property Y : integer read GetY;

    function GetHeight: integer;
    function GetWidth: integer;

    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
  end;

  TCursorCaptrue = class (TComponent, ICursor)
  private
    FCursorImages : TList<TCursorImage>;
    function find_CursorImage(AHandle:HCURSOR):integer;
  private
    FCursorIndex : integer;
    FOffset: TPoint;
    procedure clear_CursorImages;
    function GetOffsetX: integer;
    function GetOffsetY: integer;
    procedure SetOffsetX(const Value: integer);
    procedure SetOffsetY(const Value: integer);
    function GetX: integer;
    function GetY: integer;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetCursorIndex: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Capture;

    procedure Draw(ABitmap:TBitmap);

    function GetImageData(var AData:pointer; var ASize:integer):boolean;
  published
    property CursorIndex : integer read GetCursorIndex;

    property X : integer read GetX;
    property Y : integer read GetY;

    property Width : integer read GetWidth;
    property Height : integer read GetHeight;

    property Offset : TPoint read FOffset write FOffset;
    property OffsetX : integer read GetOffsetX write SetOffsetX;
    property OffsetY : integer read GetOffsetY write SetOffsetY;
  end;

implementation

{ TCursorImage }

procedure TCursorImage.Capture;
begin
  init;

  if get_CursorInfo = false then Exit;

  get_IconInfo;
end;

constructor TCursorImage.Create(AOwner: TComponent);
begin
  inherited;

  FOffset := Point(0, 0);

  FBitmapMask := TBitmap.Create;
  FBitmapMask.PixelFormat := pf32bit;

  FBitmapColor := TBitmap.Create;
  FBitmapColor.PixelFormat := pf32bit;

  FBitmapInvert := TBitmap.Create;
  FBitmapInvert.PixelFormat := pf32bit;

  init;
end;

destructor TCursorImage.Destroy;
begin
  FreeAndNil(FBitmapMask);
  FreeAndNil(FBitmapColor);
  FreeAndNil(FBitmapInvert);

  inherited;
end;

procedure TCursorImage.Draw(ABitmap: TBitmap);
var
  OldCopyMode : TCopyMode;
begin
  OldCopyMode := ABitmap.Canvas.CopyMode;
  try
    ABitmap.Canvas.CopyMode := cmSrcAnd;
    ABitmap.Canvas.Draw(X, Y, FBitmapMask);

    ABitmap.Canvas.CopyMode := cmSrcPaint;
    ABitmap.Canvas.Draw(X, Y, FBitmapColor);

    if (FBitmapInvert.Width * FBitmapInvert.Width) > 0 then begin
      ABitmap.Canvas.CopyMode := cmSrcInvert;
      ABitmap.Canvas.Draw(X, Y, FBitmapInvert);
    end;
  finally
    ABitmap.Canvas.CopyMode := OldCopyMode;
  end;
end;

function TCursorImage.GetHeight: integer;
begin
  Result := FBitmapMask.Height;
end;

function TCursorImage.GetImageData(var AData: pointer; var ASize: integer): boolean;
var
  iSize, Loop : integer;
  Bitmap: TBitmap;
  pDst, pMask, pColor, pInvert : PDWord;
  isBitmapColorExist : boolean;
  isBitmapInvertExist : boolean;
begin
  Result := false;

  AData := nil;
  ASize := 0;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Width  := FBitmapMask.Width;
    Bitmap.Height := FBitmapMask.Height;

    iSize := Bitmap.Width * Bitmap.Height * 4;

    if iSize = 0 then Exit;

    Result := true;

    pDst   := Bitmap.ScanLine[Bitmap.Height-1];
    pMask  := FBitmapMask.ScanLine[FBitmapMask.Height-1];

    isBitmapColorExist := (FBitmapColor.Width * FBitmapColor.Height) <> 0;

    if isBitmapColorExist then
      pColor := FBitmapColor.ScanLine[FBitmapColor.Height-1]
    else
      pColor := nil;

    isBitmapInvertExist := (FBitmapInvert.Width * FBitmapInvert.Height) <> 0;

    if isBitmapInvertExist then
      pInvert := FBitmapInvert.ScanLine[FBitmapInvert.Height-1]
    else
      pInvert := nil;

    for Loop := 1 to Bitmap.Width * Bitmap.Height do begin
      if (pColor <> nil) and (pMask^ = 0) then pDst^ := pColor^
      else pDst^ := $00563412;

      if pInvert <> nil then begin
        if pInvert^ > 0 then pDst^ := not (pInvert^);
      end;

      Inc(pDst);
      Inc(pMask);

      if pColor <> nil then Inc(pColor);
      if pInvert <> nil then Inc(pInvert);
    end;

    ShrinkDataSlow( Bitmap.ScanLine[Bitmap.Height-1], iSize, AData, ASize );
  finally
    Bitmap.Free;
  end;
end;

function TCursorImage.GetOffsetX: integer;
begin
  Result := FOffset.X;
end;

function TCursorImage.GetOffsetY: integer;
begin
  Result := FOffset.Y;
end;

function TCursorImage.GetWidth: integer;
begin
  Result := FBitmapMask.Width;
end;

function TCursorImage.GetX: integer;
begin
  Result := FPosition.X - FHotSpot.X - FOffset.X;
end;

function TCursorImage.GetY: integer;
begin
  Result := FPosition.Y - FHotSpot.Y - FOffset.Y;
end;

function TCursorImage.get_CursorInfo: boolean;
begin
  Result := false;
  FCursorInfo.cbSize:= SizeOf(FCursorInfo);
  if (GetCursorInfo(FCursorInfo) = false) or (FCursorInfo.Flags <> CURSOR_SHOWING) then Exit;

  FPosition := FCursorInfo.ptScreenPos;

  Result := true;
end;

procedure TCursorImage.get_IconInfo;
var
  Icon : TIcon;
  bmpMask, bmpColor : TBitmap;
begin
  Icon := TIcon.Create;
  bmpMask := TBitmap.Create;
  bmpColor := TBitmap.Create;
  try
    Icon.Handle := CopyIcon(FCursorInfo.hCursor);
    if GetIconInfo(Icon.Handle, FIconInfo) then begin
      FHotSpot := Point(FIconInfo.xHotspot, FIconInfo.yHotspot);
      bmpMask.Handle := FIconInfo.hbmMask;
      bmpColor.Handle := FIconInfo.hbmColor;

      FBitmapMask.Assign(bmpMask);
      FBitmapMask.PixelFormat := pf32bit;

      FBitmapColor.Assign(bmpColor);
      FBitmapColor.PixelFormat := pf32bit;

      if FBitmapMask.Height > FBitmapMask.Width then begin
        FBitmapInvert.Width := FBitmapMask.Width;
        FBitmapInvert.Height := FBitmapMask.Height div 2;
        FBitmapInvert.Canvas.CopyRect(
          Rect(0, 0, FBitmapInvert.Width, FBitmapInvert.Height),
          FBitmapMask.Canvas,
          Rect(0, FBitmapInvert.Height, FBitmapInvert.Width, FBitmapMask.Height)
        );
        FBitmapMask.Height := FBitmapMask.Height div 2;
      end else begin
        FBitmapInvert.Width := 0;
        FBitmapInvert.Height := 0;
      end;
    end;
  finally
    Icon.Free;
    bmpMask.Free;
    bmpColor.Free;
  end;
end;

procedure TCursorImage.init;
begin
  FPosition := Point(0, 0);
  FHotSpot := Point(0, 0);

  FBitmapMask.Width := 0;
  FBitmapColor.Width := 0;
  FBitmapInvert.Width := 0;
end;

procedure TCursorImage.SetOffsetX(const Value: integer);
begin
  FOffset.X := Value;
end;

procedure TCursorImage.SetOffsetY(const Value: integer);
begin
  FOffset.Y := Value;
end;

{ TCursorCaptrue }

procedure TCursorCaptrue.Capture;
var
  iOldIndex : integer;
  NewCursorImage, OldCursorImage : TCursorImage;
begin
  FCursorIndex := -1;

  NewCursorImage := TCursorImage.Create(Self);
  if NewCursorImage.get_CursorInfo = false then begin
    NewCursorImage.Free;
    Exit;
  end;

  iOldIndex := find_CursorImage(NewCursorImage.FCursorInfo.hCursor);

  // 새로운 커서 핸들이라면, 추가하고 IconInfo도 가져온다.
  // 이미 존재하는 커서 핸들이면, 기존 것이 재사용되기 때문에 객체를 삭제한다.
  if iOldIndex > -1 then begin
    OldCursorImage := FCursorImages.Items[iOldIndex];
    OldCursorImage.FPosition := NewCursorImage.FPosition;

    NewCursorImage.Free;

    FCursorIndex := iOldIndex;
  end else begin
    NewCursorImage.get_IconInfo;
    FCursorImages.Add(NewCursorImage);

    FCursorIndex := FCursorImages.Count - 1;
  end;
end;

procedure TCursorCaptrue.clear_CursorImages;
var
  Loop : Integer;
begin
  for Loop := 0 to FCursorImages.Count-1 do FCursorImages.Items[Loop].Free;
  FCursorImages.Clear;
end;

constructor TCursorCaptrue.Create(AOwner: TComponent);
begin
  inherited;

  FOffset := Point(0, 0);

  FCursorImages := TList<TCursorImage>.Create;
end;

destructor TCursorCaptrue.Destroy;
begin
  clear_CursorImages;

  FreeAndNil(FCursorImages);

  inherited;
end;

procedure TCursorCaptrue.Draw(ABitmap: TBitmap);
var
  CursorImage : TCursorImage;
begin
  if FCursorIndex = -1 then Exit;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  CursorImage.Draw(ABitmap);
end;

function TCursorCaptrue.find_CursorImage(AHandle: HCURSOR): integer;
var
  Loop: Integer;
begin
  Result := -1;

  for Loop := 0 to FCursorImages.Count-1 do
    if FCursorImages[Loop].FCursorInfo.hCursor = AHandle then begin
      Result := Loop;
      Break;
    end;
end;

function TCursorCaptrue.GetCursorIndex: integer;
begin
  Result := FCursorIndex;
end;

function TCursorCaptrue.GetHeight: integer;
var
  CursorImage : TCursorImage;
begin
  if FCursorIndex = -1 then begin
    Result := 0;
    Exit;
  end;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  Result := CursorImage.Height;
end;

function TCursorCaptrue.GetImageData(var AData: pointer;
  var ASize: integer): boolean;
var
  CursorImage : TCursorImage;
begin
  AData := nil;
  ASize := 0;

  Result := FCursorIndex > -1;

  if not Result then Exit;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  Result := CursorImage.GetImageData( AData, ASize );
end;

function TCursorCaptrue.GetOffsetX: integer;
begin
  Result := FOffset.X;
end;

function TCursorCaptrue.GetOffsetY: integer;
begin
  Result := FOffset.Y;
end;

function TCursorCaptrue.GetWidth: integer;
var
  CursorImage : TCursorImage;
begin
  if FCursorIndex = -1 then begin
    Result := 0;
    Exit;
  end;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  Result := CursorImage.Width;
end;

function TCursorCaptrue.GetX: integer;
var
  CursorImage : TCursorImage;
begin
  if FCursorIndex = -1 then begin
    Result := 0;
    Exit;
  end;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  Result := CursorImage.X;
end;

function TCursorCaptrue.GetY: integer;
var
  CursorImage : TCursorImage;
begin
  if FCursorIndex = -1 then begin
    Result := 0;
    Exit;
  end;

  CursorImage := FCursorImages.Items[FCursorIndex];
  CursorImage.Offset := Self.Offset;

  Result := CursorImage.Y;
end;

procedure TCursorCaptrue.SetOffsetX(const Value: integer);
begin
  FOffset.X := Value;
end;

procedure TCursorCaptrue.SetOffsetY(const Value: integer);
begin
  FOffset.Y := Value;
end;

end.
