unit BitmapProgressBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type
  TBitmapProgressBar = class(TCustomPanel)
  private
    FBitmapForegroundCopy: TBitmap;
    FBitmapBackgroundCopy: TBitmap;
    procedure draw_Tile(ASrc,ADst:TBitmap);
  protected
    procedure Resize; override;
    procedure Paint; override;
  private
    FBitmapForeground: TBitmap;
    FBitmapBackground: TBitmap;
    FPercent: integer;
    procedure SetBitmapBackground(const Value: TBitmap);
    procedure SetBitmapForeground(const Value: TBitmap);
    procedure SetPercent(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;
  published
    property Percent : integer read FPercent write SetPercent;
    property BitmapForeground : TBitmap read FBitmapForeground write SetBitmapForeground;
    property BitmapBackground : TBitmap read FBitmapBackground write SetBitmapBackground;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBitmapProgressBar]);
end;

{ TBitmapProgressBar }

constructor TBitmapProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];

  FBitmapForeground := TBitmap.Create;
  FBitmapBackground := TBitmap.Create;

  FBitmapForegroundCopy := TBitmap.Create;
  FBitmapBackgroundCopy := TBitmap.Create;
end;

destructor TBitmapProgressBar.Destroy;
begin
  FreeAndNil(FBitmapForeground);
  FreeAndNil(FBitmapBackground);
  FreeAndNil(FBitmapForegroundCopy);
  FreeAndNil(FBitmapBackgroundCopy);

  inherited;
end;

procedure TBitmapProgressBar.draw_Tile(ASrc, ADst: TBitmap);
var
  Src, Dst : TRect;
  Loop, iPieceSize : Integer;
begin
  if (Width * Height) = 0 then Exit;

  if (ASrc.Width * ASrc.Height) = 0 then Exit;

  iPieceSize := ASrc.Width div 3;
  if iPieceSize < 1 then Exit;

  ADst.Width  := Width;
  ADst.Height := Height;

  // Repeat middle part.
  for Loop := 0 to Width div iPieceSize do begin
    Src := Bounds(iPieceSize, 0, iPieceSize, ASrc.Height);
    Dst := Bounds(iPieceSize * Loop, 0, iPieceSize, ASrc.Height);
    ADst.Canvas.CopyRect(Dst, ASrc.Canvas, Src);
  end;

  // Draw left part.
  Src := Bounds(0, 0, iPieceSize, ASrc.Height);
  Dst := Src;
  ADst.Canvas.CopyRect(Dst, ASrc.Canvas, Src);

  // Draw right part.
  Src := Bounds(ASrc.Width-iPieceSize, 0, iPieceSize, ASrc.Height);
  Dst := Bounds(Width-iPieceSize, 0, iPieceSize, ASrc.Height);
  ADst.Canvas.CopyRect(Dst, ASrc.Canvas, Src);
end;

procedure TBitmapProgressBar.Paint;
var
  Src, Dst : TRect;
begin
  if (Width * Height) = 0 then Exit;

  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  Canvas.Draw(0, 0, FBitmapBackgroundCopy);

  Src := Bounds(0, 0, FBitmapForegroundCopy.Width * FPercent div 100, FBitmapForegroundCopy.Height);
  Dst := Src;
  Canvas.CopyRect(Dst, FBitmapForegroundCopy.Canvas, Src);
end;

procedure TBitmapProgressBar.Resize;
begin
  draw_Tile( FBitmapForeground, FBitmapForegroundCopy );
  draw_Tile( FBitmapBackground, FBitmapBackgroundCopy );
end;

procedure TBitmapProgressBar.SetBitmapBackground(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmapBackground.Width := 0;
    Invalidate;
    Exit;
  end;

  Height := Value.Height;

  FBitmapBackground.Assign( Value );

  Resize;

  Invalidate;
end;

procedure TBitmapProgressBar.SetBitmapForeground(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmapForeground.Width := 0;
    Invalidate;
    Exit;
  end;

  Height := Value.Height;

  FBitmapForeground.Assign( Value );

  Resize;

  Invalidate;
end;

procedure TBitmapProgressBar.SetPercent(const Value: integer);
begin
  FPercent := Value;
  Invalidate;
end;

end.
