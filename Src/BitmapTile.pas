unit BitmapTile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type
  TBitmapTile = class(TCustomPanel)
  private
    FBitmapPreview : TBitmap;
    procedure prepare_BitmapPreview;
  protected
    procedure Resize; override;
    procedure Paint; override;
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;

    property Bitmap : TBitmap read FBitmap write SetBitmap;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBitmapTile]);
end;

{ TBitmapTile }

constructor TBitmapTile.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];

  FBitmap := TBitmap.Create;
  FBitmapPreview := TBitmap.Create;
end;

destructor TBitmapTile.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBitmapPreview);

  inherited;
end;

procedure TBitmapTile.Paint;
begin
  inherited;

  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  prepare_BitmapPreview;

  Canvas.Draw(0, 0, FBitmapPreview);
end;

procedure TBitmapTile.prepare_BitmapPreview;
var
  LoopX, LoopY: Integer;
begin
  if (Width * Height) = 0 then Exit;
  if (FBitmap.Width * FBitmap.Height) = 0 then Exit;

  if (FBitmapPreview.Width = Width) and (FBitmapPreview.Height = Height) then Exit;

  FBitmapPreview.Width := Width;
  FBitmapPreview.Height := Height;

  for LoopY := 0 to Height div FBitmap.Height do
  for LoopX := 0 to Width div FBitmap.Width do
    FBitmapPreview.Canvas.Draw(LoopX * FBitmap.Width, LoopY * FBitmap.Height, FBitmap);
end;

procedure TBitmapTile.Resize;
begin
  Invalidate;
end;

procedure TBitmapTile.SetBitmap(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmap.Width := 0;
    Invalidate;
    Exit;
  end;

  FBitmap.Assign(Value);

  Invalidate;
end;

end.
