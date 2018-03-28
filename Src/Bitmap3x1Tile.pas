unit Bitmap3x1Tile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type
  TBitmap3x1Tile = class(TCustomPanel)
  protected
    FBitmapPreveiw : TBitmap;
    procedure Resize; override;
    procedure Paint; override;
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetBitmap(ABitmap:TBitmap);
  published
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;

    property Bitmap : TBitmap read FBitmap write SetBitmap;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBitmap3x1Tile]);
end;

{ TBitmap3x1Tile }

constructor TBitmap3x1Tile.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];

  FBitmap := TBitmap.Create;
  FBitmapPreveiw := TBitmap.Create;
end;

destructor TBitmap3x1Tile.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBitmapPreveiw);

  inherited;
end;

procedure TBitmap3x1Tile.GetBitmap(ABitmap: TBitmap);
begin
  ABitmap.Assign( FBitmapPreveiw );
end;

procedure TBitmap3x1Tile.Paint;
begin
  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  Canvas.Draw(0, 0, FBitmapPreveiw);
end;

procedure TBitmap3x1Tile.Resize;
var
  Loop: Integer;
  Src, Dst : TRect;
  FBitmapPiece : TBitmap;
begin
  if (Width * Height) = 0 then Exit;
  if (FBitmap.Width * FBitmap.Height) = 0 then Exit;

  FBitmapPiece := TBitmap.Create;
  try
    FBitmapPreveiw.Width := Width;
    FBitmapPreveiw.Height := FBitmap.Height;

    FBitmapPiece.Width := FBitmap.Width div 3;
    FBitmapPiece.Height := FBitmap.Height;

    if FBitmapPiece.Width = 0 then Exit;

    Dst := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);

    // Repeat middle part.
    Src := Bounds(FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    for Loop := 0 to Width div FBitmapPiece.Width do
      FBitmapPreveiw.Canvas.Draw(Loop * FBitmapPiece.Width, 0, FBitmapPiece);

    // Draw left part.
    Src := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(0, 0, FBitmapPiece);

    // Draw right part.
    Src := Bounds(FBitmap.Width-FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(Width-FBitmapPiece.Width, 0, FBitmapPiece);
  finally
    FBitmapPiece.Free;
  end;
end;

procedure TBitmap3x1Tile.SetBitmap(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmap.Width := 0;
    Invalidate;
    Exit;
  end;

  FBitmap.Assign(Value);

  Resize;

  Invalidate;
end;

end.
