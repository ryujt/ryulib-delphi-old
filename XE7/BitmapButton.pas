unit BitmapButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

const
  INDEX_NORMAL = 0;
  INDEX_HOVER = 1;
  INDEX_DOWN = 2;
  INDEX_DISABLED = 3;

  BITMAPS_COUNT = 4;

type
  TBitmapButton = class(TCustomControl)
  private
    FIsMouseOver : boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  private
    FBitmapIsChanged : boolean;
    FBitmap : TBitmap;
    FBitmaps : array [0..BITMAPS_COUNT-1] of TBitmap;
    FBitmapIndex: integer;
    FMenualDisplay: boolean;
    FTransparent: boolean;
    FTransparentColor: TColor;
    FBackgroundColor: TColor;
    FUseBackgroundColor: boolean;
    FInternalBitmap: TBitmap;
    procedure slice_Bitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetBitmapIndex(const Value: integer);
    procedure SetTransparent(const Value: boolean);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetUseBackgroundColor(const Value: boolean);
  protected
    procedure Paint; override;
    procedure SetEnabled(Value: boolean); override;
  protected
    FIsMouseDown : boolean;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReloadBitmap;

    property InternalBitmap : TBitmap read FInternalBitmap;
  published
    property Canvas;
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;
    property OnClick;

    /// 버턴에 쓰이는 Bitmap은 여러 개의 이미지를 가로로 붙여서 사용한다.
    property Bitmap : TBitmap read FBitmap write SetBitmap;

    /// 현재 표시되어야 하는 이미지의 순번, Normal, Hover, Down, Disabled
    property BitmapIndex : integer read FBitmapIndex write SetBitmapIndex;

    /// 마우스 동작이 아닌 SetBitmapIndex을 이용해서만 이미지가 변경된다.
    property MenualDisplay : boolean read FMenualDisplay write FMenualDisplay;

    property Transparent : boolean read FTransparent write SetTransparent;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor;

    property UseBackgroundColor : boolean read FUseBackgroundColor write SetUseBackgroundColor;
    property BackgroundColor : TColor read FBackgroundColor write SetBackgroundColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBitmapButton]);
end;

{ TBitmapButton }

procedure TBitmapButton.CMMouseEnter(var Message: TMessage);
begin
  FIsMouseOver := true;
  Invalidate;
end;

procedure TBitmapButton.CMMouseLeave(var Message: TMessage);
begin
  FIsMouseOver := false;
  Invalidate;
end;

constructor TBitmapButton.Create(AOwner: TComponent);
var
  Loop: Integer;
begin
  inherited;

  FBitmapIsChanged := true;

  FIsMouseOver := false;
  FIsMouseDown := false;

  FBitmapIndex := INDEX_NORMAL;
  FMenualDisplay := false;

  FBitmap := TBitmap.Create;

  FInternalBitmap := TBitmap.Create;
  FInternalBitmap.PixelFormat := pf32bit;

  for Loop := 0 to BITMAPS_COUNT-1 do FBitmaps[Loop] := TBitmap.Create;
end;

destructor TBitmapButton.Destroy;
var
  Loop: Integer;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FInternalBitmap);

  for Loop := 0 to BITMAPS_COUNT-1 do FreeAndNil(FBitmaps[Loop]);

  inherited;
end;

procedure TBitmapButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FIsMouseDown := true;
  Invalidate;

  inherited;
end;

procedure TBitmapButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FIsMouseDown := false;
  Invalidate;

  inherited;
end;

procedure TBitmapButton.Paint;
var
  Loop: Integer;
  BitmapIndex : integer;
begin
  inherited;

  if FBitmap.Width * FBitmap.Height = 0 then Exit;

  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  if Enabled then begin
    if FMenualDisplay then begin
      BitmapIndex := FBitmapIndex;
    end else begin
      if FIsMouseDown then begin
        BitmapIndex := INDEX_DOWN;
      end else begin
        if FIsMouseOver then BitmapIndex := INDEX_HOVER
        else BitmapIndex := INDEX_NORMAL;
      end;
    end;
  end else begin
    BitmapIndex := INDEX_DISABLED;
  end;

  slice_Bitmap;

  for Loop := 0 to BITMAPS_COUNT-1 do begin
    FBitmaps[Loop].TransparentColor := FTransparentColor;
    FBitmaps[Loop].Transparent := FTransparent;
  end;

  if FUseBackgroundColor then begin
    FInternalBitmap.Canvas.Brush.Color := FBackgroundColor;
    FInternalBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  FInternalBitmap.Canvas.Draw( 0, 0, FBitmaps[BitmapIndex] );

  Canvas.Brush.Style := bsSolid;
  Canvas.Draw( 0, 0, FInternalBitmap );
end;

procedure TBitmapButton.ReloadBitmap;
begin
  Width  := FBitmap.Width div BITMAPS_COUNT;
  Height := FBitmap.Height;

  FBitmapIsChanged := true;

  Invalidate;
end;

procedure TBitmapButton.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  Invalidate;
end;

procedure TBitmapButton.SetBitmap(const Value: TBitmap);
var
  Loop: Integer;
begin
  if Value = nil then begin
    FBitmap.Width := 0;
    for Loop := 0 to BITMAPS_COUNT-1 do FBitmaps[Loop].Width := 0;

    Invalidate;

    Exit;
  end;

  FBitmap.Assign(Value);

  ReloadBitmap;
end;

procedure TBitmapButton.SetBitmapIndex(const Value: integer);
begin
  FBitmapIndex := Value;
  if FMenualDisplay then Invalidate;
end;

procedure TBitmapButton.SetEnabled(Value: boolean);
begin
  inherited;

  Invalidate;
end;

procedure TBitmapButton.SetTransparent(const Value: boolean);
var
  Loop: Integer;
begin
  FTransparent := Value;

  FBitmap.Transparent := Value;

  for Loop := 0 to BITMAPS_COUNT-1 do FBitmaps[Loop].Transparent := Value;

  Invalidate;
end;

procedure TBitmapButton.SetTransparentColor(const Value: TColor);
var
  Loop: Integer;
begin
  FTransparentColor := Value;

  FBitmap.TransparentColor := Value;

  for Loop := 0 to BITMAPS_COUNT-1 do FBitmaps[Loop].TransparentColor := Value;

  Invalidate;
end;

procedure TBitmapButton.SetUseBackgroundColor(const Value: boolean);
begin
  FUseBackgroundColor := Value;
  Invalidate;
end;

procedure TBitmapButton.slice_Bitmap;
var
  Loop: Integer;
  SrcRect : TRect;
begin
  if not FBitmapIsChanged then Exit;

  FBitmapIsChanged := false;

  FInternalBitmap.Width  := Width;
  FInternalBitmap.Height := Height;

  for Loop := 0 to BITMAPS_COUNT-1 do begin
    FBitmaps[Loop].Width  := Width;
    FBitmaps[Loop].Height := Height;

    SrcRect := Rect(Loop * Width, 0, (Loop + 1) * Width, Height);
    FBitmaps[Loop].Canvas.CopyRect(Rect(0, 0, Width, Height), FBitmap.Canvas, SrcRect);
  end;
end;

end.
