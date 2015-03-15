unit MusicTrackBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TMusicTrackBar = class(TCustomControl)
  private
    FBackground : TBitmap;
    FPositionBar : TBitmap;
    FDownloader : TBitmap;
  protected
    FBitmapPreveiw : TBitmap;
    procedure Resize; override;
    procedure Paint; override;
  protected
    FIsMouseDown : boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    function GetBitmapBackground: TBitmap;
    function GetBitmapPositionBar: TBitmap;
    function GetBitmapDownloader: TBitmap;
    procedure SetPosition(const Value: integer);
    function GetPosition: integer;
    function GetOnChanged: TNotifyEvent;
    procedure SetOnChanged(const Value: TNotifyEvent);
    function GetDownloaded: integer;
    procedure SetDownloaded(const Value: integer);
    procedure SetBitmapBackground(const Value: TBitmap);
    procedure SetBitmapDownloader(const Value: TBitmap);
    procedure SetBitmapPositionBar(const Value: TBitmap);
    function GetMax: integer;
    function GetMin: integer;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    function GetTransparentColorBackground: TColor;
    function GetTransparentColorDownloader: TColor;
    function GetTransparentColorPositionBar: TColor;
    procedure SetTransparentColorBackground(const Value: TColor);
    procedure SetTransparentColorDownloader(const Value: TColor);
    procedure SetTransparentColorPositionBar(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BitmapBackground : TBitmap read GetBitmapBackground write SetBitmapBackground;
    property BitmapPositionBar : TBitmap read GetBitmapPositionBar write SetBitmapPositionBar;
    property BitmapDownloader : TBitmap read GetBitmapDownloader write SetBitmapDownloader;
    property TransparentColorBackground : TColor read GetTransparentColorBackground write SetTransparentColorBackground;
    property TransparentColorPositionBar : TColor read GetTransparentColorPositionBar write SetTransparentColorPositionBar;
    property TransparentColorDownloader : TColor read GetTransparentColorDownloader write SetTransparentColorDownloader;
  published
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;

    property Min : integer read GetMin write SetMin;
    property Max : integer read GetMax write SetMax;
    property Position : integer read GetPosition write SetPosition;
    property Downloaded : integer read GetDownloaded write SetDownloaded;
  published
    property OnChanged : TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TMusicTrackBar]);
end;

type
  TBitmapBase = class (TBitmap)
  protected
    FBitmapPreveiw : TBitmap;
    FTransparentColor : TColor;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TBackground = class (TBitmapBase)
  private
  public
    procedure Set_Size(AWidth,AHeight:integer);
    procedure PaintOn(APreveiw:TBitmap);
  end;

  TPositionBar = class (TBitmap)
  private
    FMusicTrackBar : TMusicTrackBar;
    FTransparentColor : TColor;
  private
    FMax: integer;
    FMin: integer;
    FPosition: integer;
    FOnChanged: TNotifyEvent;
    procedure SetPosition(Value: integer);
  public
    constructor Create(AMusicTrackBar:TMusicTrackBar); reintroduce;

    procedure MouseMove(AX,AY,AWidth,AMargin:integer);
    procedure PaintOn(APreveiw:TBitmap);
  public
    property Min : integer read FMin write FMin;
    property Max : integer read FMax write FMax;
    property Position : integer read FPosition write SetPosition;
  public
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TDownloader = class (TBitmapBase)
  private
    FWidth : integer;
    FHeight : integer;
    FMusicTrackBar : TMusicTrackBar;
  private
    FDownloaded: integer;
    procedure SetDownloaded(const Value: integer);
  public
    constructor Create(AMusicTrackBar:TMusicTrackBar); reintroduce;

    procedure Set_Size(AWidth,AHeight:integer);
    procedure PaintOn(APreveiw:TBitmap);
  public
    property Downloaded : integer read FDownloaded write SetDownloaded;
  end;

{ TMusicTrackBar }

constructor TMusicTrackBar.Create(AOwner: TComponent);
begin
  inherited;

  FIsMouseDown := false;

  ControlStyle := ControlStyle + [csOpaque];

  FBackground := TBackground.Create;
  FPositionBar := TPositionBar.Create(Self);
  FDownloader := TDownloader.Create(Self);
  FBitmapPreveiw := TBitmap.Create;
end;

destructor TMusicTrackBar.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FPositionBar);
  FreeAndNil(FDownloader);
  FreeAndNil(FBitmapPreveiw);

  inherited;
end;

function TMusicTrackBar.GetBitmapBackground: TBitmap;
begin
  Result := FBackground;
end;

function TMusicTrackBar.GetBitmapDownloader: TBitmap;
begin
  Result := FDownloader;
end;

function TMusicTrackBar.GetBitmapPositionBar: TBitmap;
begin
  Result := FPositionBar;
end;

function TMusicTrackBar.GetDownloaded: integer;
begin
  Result := TDownloader(FDownloader).Downloaded;
end;

function TMusicTrackBar.GetMax: integer;
begin
  Result := TPositionBar(FPositionBar).Max;
end;

function TMusicTrackBar.GetMin: integer;
begin
  Result := TPositionBar(FPositionBar).Min;
end;

function TMusicTrackBar.GetOnChanged: TNotifyEvent;
begin
  Result := TPositionBar(FPositionBar).OnChanged;
end;

function TMusicTrackBar.GetPosition: integer;
begin
  Result := TPositionBar(FPositionBar).Position;
end;

function TMusicTrackBar.GetTransparentColorBackground: TColor;
begin
  Result := TBackground(FBackground).FTransparentColor;
end;

function TMusicTrackBar.GetTransparentColorDownloader: TColor;
begin
  Result := TDownloader(FDownloader).FTransparentColor;
end;

function TMusicTrackBar.GetTransparentColorPositionBar: TColor;
begin
  Result := TPositionBar(FPositionBar).FTransparentColor;
end;

procedure TMusicTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FIsMouseDown := true;
end;

procedure TMusicTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FIsMouseDown then begin
    TPositionBar(FPositionBar).MouseMove(X, Y, Width, FBackground.Width div 3);
    Invalidate;
  end;
end;

procedure TMusicTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FIsMouseDown := false;
end;

procedure TMusicTrackBar.Paint;
begin
  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  TBackground(FBackground).PaintOn(FBitmapPreveiw);
  TDownloader(FDownloader).PaintOn(FBitmapPreveiw);
  TPositionBar(FPositionBar).PaintOn(FBitmapPreveiw);

  Canvas.Draw(0, 0, FBitmapPreveiw);
end;

procedure TMusicTrackBar.Resize;
begin
  if (Width * Height) = 0 then Exit;

  FBitmapPreveiw.Width := Width;
  FBitmapPreveiw.Height := Height;

  TBackground(FBackground).Set_Size(Width, Height);
  TDownloader(FDownloader).Set_Size(Width, Height);
end;

procedure TMusicTrackBar.SetBitmapBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TMusicTrackBar.SetBitmapDownloader(const Value: TBitmap);
begin
  FDownloader.Assign(Value);
  Invalidate;
end;

procedure TMusicTrackBar.SetBitmapPositionBar(const Value: TBitmap);
begin
  FPositionBar.Assign(Value);
  Invalidate;
end;

procedure TMusicTrackBar.SetDownloaded(const Value: integer);
begin
  TDownloader(FDownloader).Downloaded := Value;
end;

procedure TMusicTrackBar.SetMax(const Value: integer);
begin
  TPositionBar(FPositionBar).Max := Value;
end;

procedure TMusicTrackBar.SetMin(const Value: integer);
begin
  TPositionBar(FPositionBar).Min := Value;
end;

procedure TMusicTrackBar.SetOnChanged(const Value: TNotifyEvent);
begin
  TPositionBar(FPositionBar).OnChanged := Value;
end;

procedure TMusicTrackBar.SetPosition(const Value: integer);
begin
  if not FIsMouseDown then begin
    TPositionBar(FPositionBar).FPosition := Value;
    Invalidate;
  end;
end;

procedure TMusicTrackBar.SetTransparentColorBackground(const Value: TColor);
begin
  TBackground(FBackground).FTransparentColor := Value;
end;

procedure TMusicTrackBar.SetTransparentColorDownloader(const Value: TColor);
begin
  TDownloader(FDownloader).FTransparentColor := Value;
end;

procedure TMusicTrackBar.SetTransparentColorPositionBar(const Value: TColor);
begin
  TPositionBar(FPositionBar).FTransparentColor := Value;
end;

{ TBitmapBase }

constructor TBitmapBase.Create;
begin
  inherited;

  FTransparentColor := $11240828;
  FBitmapPreveiw := TBitmap.Create;
end;

destructor TBitmapBase.Destroy;
begin
  FreeAndNil(FBitmapPreveiw);

  inherited;
end;

{ TBackground }

procedure TBackground.PaintOn(APreveiw: TBitmap);
begin
  FBitmapPreveiw.TransparentColor := FTransparentColor;
  FBitmapPreveiw.Transparent := true;
  APreveiw.Canvas.Draw(0, 0, FBitmapPreveiw);
end;

procedure TBackground.Set_Size(AWidth, AHeight: integer);
var
  Loop: Integer;
  Src, Dst : TRect;
  FBitmapPiece : TBitmap;
begin
  if (Width * Height) = 0 then Exit;

  FBitmapPreveiw.Width := AWidth;
  FBitmapPreveiw.Height := Height;

  FBitmapPiece := TBitmap.Create;
  try
    FBitmapPiece.Width := Width div 3;
    FBitmapPiece.Height := Height;

    if FBitmapPiece.Width = 0 then Exit;

    Dst := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);

    // Repeat middle part.
    Src := Bounds(FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    for Loop := 0 to AWidth div FBitmapPiece.Width do
      FBitmapPreveiw.Canvas.Draw(Loop * FBitmapPiece.Width, 0, FBitmapPiece);

    // Draw left part.
    Src := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(0, 0, FBitmapPiece);

    // Draw right part.
    Src := Bounds(Width-FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(AWidth-FBitmapPiece.Width, 0, FBitmapPiece);
  finally
    FBitmapPiece.Free;
  end;
end;

{ TPositionBar }

constructor TPositionBar.Create(AMusicTrackBar:TMusicTrackBar);
begin
  inherited Create;

  FTransparentColor := $11240828;

  FMusicTrackBar := AMusicTrackBar;

  FMin := 0;
  FMax := 100;
  FPosition := 0;
end;

procedure TPositionBar.MouseMove(AX, AY, AWidth, AMargin: integer);
begin
  if (Width * Height) = 0 then Exit;
  if (AWidth - AMargin*2)  <= 0 then Exit;

  AX := (FMax - FMin) * (AX - AMargin) div (AWidth - AMargin*2);
  SetPosition(AX + FMin);
end;

procedure TPositionBar.PaintOn(APreveiw: TBitmap);
var
  BarPosition : integer;
begin
  if (Width * Height) = 0 then Exit;
  if (FMax - FMin)  <= 0 then Exit;

  BarPosition := (APreveiw.Width - Width) * (FPosition - FMin) div (FMax - FMin);
  if BarPosition > APreveiw.Width then BarPosition := APreveiw.Width - Width;

  Self.TransparentColor := FTransparentColor;
  Self.Transparent := true;
  APreveiw.Canvas.Draw(BarPosition, (APreveiw.Height - Height) div 2, Self);
end;

procedure TPositionBar.SetPosition(Value: integer);
begin
  if Value < FMin then Value := FMin
  else if Value > FMax then Value := FMax;

  if FPosition <> Value then begin
    FPosition := Value;
    if Assigned(FOnChanged) then FOnChanged(FMusicTrackBar);
  end;
end;

{ TDownloader }

constructor TDownloader.Create(AMusicTrackBar:TMusicTrackBar);
begin
  inherited Create;

  FMusicTrackBar := AMusicTrackBar;
  FTransparentColor := $11240828;
  FWidth := 0;
end;

procedure TDownloader.PaintOn(APreveiw: TBitmap);
var
  Loop: Integer;
  Src, Dst : TRect;
  FBitmapPiece : TBitmap;
begin
  if (Width * Height) = 0 then Exit;
  if FDownloaded = 0 then Exit;

  FBitmapPreveiw.Width := FWidth * FDownloaded div 100;
  FBitmapPreveiw.Height := Height;

  FBitmapPiece := TBitmap.Create;
  try
    FBitmapPiece.Width := Width div 3;
    FBitmapPiece.Height := Height;

    if FBitmapPiece.Width = 0 then Exit;

    Dst := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);

    // Repeat middle part.
    Src := Bounds(FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    for Loop := 0 to FBitmapPreveiw.Width div FBitmapPiece.Width do
      FBitmapPreveiw.Canvas.Draw(Loop * FBitmapPiece.Width, 0, FBitmapPiece);

    // Draw left part.
    Src := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(0, 0, FBitmapPiece);

    // Draw right part.
    Src := Bounds(Width-FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, Canvas, Src);
    FBitmapPreveiw.Canvas.Draw(FBitmapPreveiw.Width-FBitmapPiece.Width, 0, FBitmapPiece);
  finally
    FBitmapPiece.Free;
  end;

  FBitmapPreveiw.TransparentColor := FTransparentColor;
  FBitmapPreveiw.Transparent := true;
  APreveiw.Canvas.Draw((APreveiw.Width - FWidth) div 2, (APreveiw.Height div 2) - (FBitmapPreveiw.Height div 2), FBitmapPreveiw);
end;

procedure TDownloader.SetDownloaded(const Value: integer);
begin
  if Value in [0..100] then begin
    if Value <> FDownloaded then FMusicTrackBar.Invalidate;
    FDownloaded := Value;
  end;
end;

procedure TDownloader.Set_Size(AWidth,AHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

end.
