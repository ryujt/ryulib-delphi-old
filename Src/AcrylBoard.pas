unit AcrylBoard;

interface

uses
  RyuLibBase,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

const
  DRAWDATA_LIST_COUNT = 256;

type
  TDrawData = packed record
    X,Y : word;
  end;

  TDrawDataList = packed array [0..DRAWDATA_LIST_COUNT-1] of TDrawData;

  TAcrylBoard = class (TCustomControl)
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  private
    FMouseDown : TPoint;
    FOldWindowSize : TSize;
    FDrawDataIndex : integer;
    FDrawDataList : TDrawDataList;
    procedure add_DrawData(AX,AY:integer);
  private
    FBitmap : TBitmap;
    procedure on_Bitmap_Change(Sender:TObject);
  private
    FBitmapLayer : TBitmap;
    procedure do_ResizeBitmapLayer;
  private
    FAutoSize: boolean;
    FOnDrawData: TDataEvent;
    FCanDraw: boolean;
    function GetPenColor: TColor;
    function GetTransparentColor: TColor;
    procedure SetPenColor(const Value: TColor);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetAutoSize(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
  published
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  published
    property CanDraw : boolean read FCanDraw write FCanDraw;
    property AutoSize : boolean read FAutoSize write SetAutoSize;
    property PenColor : TColor read GetPenColor write SetPenColor;
    property TransparentColor : TColor read GetTransparentColor write SetTransparentColor;
    property Bitmap : TBitmap read FBitmap;
    property BitmapLayer : TBitmap read FBitmapLayer;
    property OnDrawData : TDataEvent read FOnDrawData write FOnDrawData;
  end;

implementation

{ TAcrylBoard }

procedure TAcrylBoard.add_DrawData(AX, AY: integer);
begin
  if (Width * Height) = 0 then Exit;

  FDrawDataList[FDrawDataIndex].X := AX * $FFFF div Width;
  FDrawDataList[FDrawDataIndex].Y := AY * $FFFF div Height;

  FDrawDataIndex := FDrawDataIndex + 1;

  if FDrawDataIndex >= DRAWDATA_LIST_COUNT then begin
    if Assigned(FOnDrawData) then FOnDrawData( Self, @FDrawDataList, FDrawDataIndex * SizeOf(TDrawData) );

    FBitmapLayer.Canvas.MoveTo( AX, AY );

    FDrawDataIndex := 0;

    FDrawDataList[FDrawDataIndex].X := AX * $FFFF div Width;
    FDrawDataList[FDrawDataIndex].Y := AY * $FFFF div Height;

    FDrawDataIndex := FDrawDataIndex + 1;
  end;
end;

procedure TAcrylBoard.Clear;
begin
  FDrawDataIndex := 0;
  FBitmapLayer.Canvas.FillRect( Rect(0, 0, Width, Height) );
  Invalidate;
end;

constructor TAcrylBoard.Create(AOwner: TComponent);
const
  DEFAULT_PEN_COLOR = clRed;
  DEFAULT_TRANSPARENT_COLOR = $578390;
begin
  inherited;

  FDrawDataIndex := 0;

  FCanDraw := false;

  DoubleBuffered := true;
  ControlStyle := ControlStyle + [csOpaque];

  FBitmap := TBitmap.Create;
  FBitmap.OnChange := on_Bitmap_Change;

  FBitmapLayer := TBitmap.Create;
  FBitmapLayer.PixelFormat := pf32bit;
  FBitmapLayer.Canvas.Pen.Color := DEFAULT_PEN_COLOR;
  FBitmapLayer.Canvas.Pen.Width := 3;
  FBitmapLayer.Canvas.Brush.Color := DEFAULT_TRANSPARENT_COLOR;
  FBitmapLayer.TransparentColor := DEFAULT_TRANSPARENT_COLOR;
  FBitmapLayer.Transparent := true;

  do_ResizeBitmapLayer;
end;

destructor TAcrylBoard.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBitmapLayer);

  inherited;
end;

procedure TAcrylBoard.do_ResizeBitmapLayer;
begin
  if (FBitmapLayer.Width <> Width) or (FBitmapLayer.Height <> Height) then begin
    FBitmapLayer.Width  := Width;
    FBitmapLayer.Height := Height;
  end;
end;

function TAcrylBoard.GetPenColor: TColor;
begin
  Result := FBitmapLayer.Canvas.Pen.Color;
end;

function TAcrylBoard.GetTransparentColor: TColor;
begin
  Result := FBitmapLayer.TransparentColor;
end;

procedure TAcrylBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FDrawDataIndex := 0;

  if not FCanDraw then Exit;

  add_DrawData( X, Y );

  FMouseDown := Point( X, Y);

  FOldWindowSize.cx := Width;
  FOldWindowSize.cy := Height;

  FBitmapLayer.Canvas.MoveTo( X, Y );
end;

procedure TAcrylBoard.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;

  if not FCanDraw then Exit;

  if Shift = [ssLeft] then begin
    FBitmapLayer.Canvas.LineTo( X, Y );
    Invalidate;

    add_DrawData( X, Y );
  end;
end;

function PointDistance(A,B:TPoint):integer;
begin
  Result := Round( SQRT (SQR(A.X-B.X) + SQR(A.Y-B.Y)) );
end;

procedure TAcrylBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
const
  POINT_DISTANCE_LIMIT = 1;
begin
  inherited;

  if not FCanDraw then Exit;

  // 그리는 동안 컨트롤의 크기가 변한다면 그리기 과정은 취소한다.
  if (FOldWindowSize.cx <> Width) or (FOldWindowSize.cy <> Height) then begin
    FBitmapLayer.Canvas.FillRect( Rect(0, 0, Width, Height) );
    Invalidate;

    Exit;
  end;

  // 그리는 동안 마우스 위치가 한 번 이상 변했거나, 위치가 1 픽셀 이상 변했을 경우만 처리 나머진 무시
  // 더블클릭 등을 그리기 동작으로 오해하는 것을 방지
  if (FDrawDataIndex > 1) or (PointDistance(FMouseDown, Point(X, Y)) > POINT_DISTANCE_LIMIT) then begin
    if Assigned(FOnDrawData) then FOnDrawData( Self, @FDrawDataList, FDrawDataIndex * SizeOf(TDrawData) );
    FDrawDataIndex := 0;
  end;
end;

procedure TAcrylBoard.on_Bitmap_Change(Sender: TObject);
begin
  if not FAutoSize then Exit;

  if (FBitmap.Width <> Width) or (FBitmap.Height <> Height) then begin
    Width  := FBitmap.Width;
    Height := FBitmap.Height;
  end;
end;

procedure TAcrylBoard.Paint;
begin
  inherited;

  Canvas.Brush.Style := bsSolid;
  Canvas.Draw( 0, 0, FBitmap );
  Canvas.Draw( 0, 0, FBitmapLayer );
end;

procedure TAcrylBoard.Resize;
begin
  inherited;

  do_ResizeBitmapLayer;
end;

procedure TAcrylBoard.SetAutoSize(const Value: boolean);
begin
  FAutoSize := Value;
  on_Bitmap_Change( FBitmap );
end;

procedure TAcrylBoard.SetPenColor(const Value: TColor);
begin
  FBitmapLayer.Canvas.Pen.Color := Value;
end;

procedure TAcrylBoard.SetTransparentColor(const Value: TColor);
begin
  FBitmapLayer.TransparentColor := Value;
end;

end.

