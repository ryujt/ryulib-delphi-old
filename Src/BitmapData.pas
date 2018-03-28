unit BitmapData;

interface

uses
  SysUtils, Classes, Graphics;

type
  TBitmapData = class
  private
    FData : pointer;
  private
    FWidth: integer;
    FHeight: integer;
    FChanged: boolean;
    FPixelFormat: TPixelFormat;
    FSize: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ABitmap:TBitmap);
    function GetBitmap(ABitmap:TBitmap):boolean;
  public
    property Changed : boolean read FChanged;
    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property PixelFormat : TPixelFormat read FPixelFormat;
    property Size : integer read FSize;
  end;

implementation

{ TBitmapData }

procedure TBitmapData.Assign(ABitmap: TBitmap);
var
  iBitmapSize, iPixelSize : integer;
begin
  if ABitmap = nil then begin
    FChanged := false;
    FWidth  := 0;
    FHeight := 0;
    FPixelFormat := pf32bit;
    FSize := 0;

    if FData <> nil then FreeMem(FData);

    FData := nil;

    Exit;
  end;

  FPixelFormat := ABitmap.PixelFormat;

  case FPixelFormat of
    pf8bit:  iPixelSize := 1;
    pf16bit: iPixelSize := 2;
    pf24bit: iPixelSize := 3;
    pf32bit: iPixelSize := 4;

    else begin
      raise Exception.Create('TBitmapData.Assign - 사용 할 수 없는 PixelFormat 입니다.');
      iPixelSize := 0;
    end;
  end;

  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;

  iBitmapSize := iPixelSize * FWidth * FHeight;

  if (FData = nil) or (iBitmapSize <> FSize) then begin
    if FData <> nil then FreeMem(FData);

    GetMem(FData, iBitmapSize);
  end;

  FSize := iBitmapSize;

  Move( ABitmap.ScanLine[ABitmap.Height-1]^, FData^, FSize );

  FChanged := true;
end;

constructor TBitmapData.Create;
begin
  inherited;

  FData := nil;

  FChanged := false;
  FWidth  := 0;
  FHeight := 0;
  FPixelFormat := pf32bit;
  FSize := 0;
end;

destructor TBitmapData.Destroy;
begin
  if FData <> nil then FreeMem(FData);

  FData := nil;

  inherited;
end;

function TBitmapData.GetBitmap(ABitmap: TBitmap): boolean;
begin
  Result := FChanged;
  FChanged := false;
  if not Result then Exit;

  if (FData = nil) or (FSize <= 0) then Exit;

  if ABitmap.PixelFormat <> FPixelFormat then ABitmap.PixelFormat := FPixelFormat;

  if ABitmap.Width  <> FWidth  then ABitmap.Width := FWidth;
  if ABitmap.Height <> FHeight then ABitmap.Height := FHeight;

  Move( FData^, ABitmap.ScanLine[ABitmap.Height-1]^, FSize );
end;

end.
