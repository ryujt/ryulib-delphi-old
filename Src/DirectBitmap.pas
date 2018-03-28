unit DirectBitmap;

interface

uses
  DebugTools, RyuGraphics, BitmapOutputCanvas, DXResolutionLimit,
  SyncObjs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, DXDraws, DXClass, Vcl.Menus;

type
  TfmDirectBitmap = class(TBitmapOutputCanvas)
    DXDraw: TDXDraw;
    Timer: TTimer;
    ScrollBox: TScrollBox;
    procedure on_Timer(Sender: TObject);
    procedure DXDrawDblClick(Sender: TObject);
  private
    // 화면이 변하지 않더라도 가끔씩 출력하도록 한다.
    FSkipCount : integer;

    FCS : TCriticalSection;

    FIsBitmapChanged : boolean;
    FDXResolutionLimit : TDXResolutionLimit;
    FBitmap : TBitmap;
    FResizedBitmap : TBitmap;
    FStretch: boolean;
    procedure set_DXDrawToCenter;
    procedure draw_Bitmap;
    procedure draw_ResizedBitmap;
    function set_DXDrawSize(ASize:TPoint):boolean;
  private
    FOldFromWidth, FOldFormHeight : integer;
    procedure set_DisplaySize;
  private
    FOldBitmapWidth, FOldBitmapHeight : integer;
    procedure set_BitmapSize;
  protected
    function GetScreenHint: string; override;
    procedure SetScreenHint(const Value: string); override;
  private
    procedure SetStretch(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Terminate; override;

    function GetCanvasWidth:integer; override;
    function GetCanvasHeight:integer; override;

    procedure DrawBitmap(ABitmap:TBitmap); override;
  public
    property ScreenHint : string read GetScreenHint write SetScreenHint;
    property Stretch : boolean read FStretch write SetStretch;
  end;

implementation

{$R *.dfm}

{ TfmDirectBitmap }

constructor TfmDirectBitmap.Create(AOwner: TComponent);
begin
  inherited;

  MakeOpaque(Self);

  FSkipCount := 0;

  FOldFromWidth  := 0;
  FOldFormHeight := 0;

  FOldBitmapWidth  := 0;
  FOldBitmapHeight := 0;

  FIsBitmapChanged := false;
  FStretch := true;

  FCS := TCriticalSection.Create;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;

  FResizedBitmap := TBitmap.Create;
  FResizedBitmap.PixelFormat := pf32bit;

  FDXResolutionLimit := TDXResolutionLimit.Create(DXDraw);
  FDXResolutionLimit.TestLimit;

  Timer.OnTimer := on_Timer;
end;

destructor TfmDirectBitmap.Destroy;
begin
  Timer.OnTimer := nil;

  FreeAndNil(FCS);
  FreeAndNil(FBitmap);
  FreeAndNil(FResizedBitmap);
  FreeAndNil(FDXResolutionLimit);

  inherited;
end;

function TfmDirectBitmap.GetCanvasHeight: integer;
begin
  if Stretch then Result := Height
  else Result := 0;
end;

function TfmDirectBitmap.GetCanvasWidth: integer;
begin
  if Stretch then Result := Width
  else Result := 0;
end;

function TfmDirectBitmap.GetScreenHint: string;
begin
  Result := DXDraw.Hint;
end;

procedure TfmDirectBitmap.DrawBitmap(ABitmap: TBitmap);
begin
  try
    FCS.Acquire;
    try
      AssignBitmap( ABitmap, FBitmap );
    finally
      FCS.Release;
    end;

    FIsBitmapChanged := true;
  except
    on E : Exception do raise Exception.Create('TfmDirectBitmap.DrawBitmap (AssignBitmap) - ' + E.Message);
  end;
end;

procedure TfmDirectBitmap.on_Timer(Sender: TObject);
const
  MAX_SKIPCOUNT = 10;
var
  isImageSmall : boolean;
begin
  Timer.Enabled := false;
  try
    if not FIsBitmapChanged then begin
      if FSkipCount >= MAX_SKIPCOUNT then begin
        FSkipCount := 0;
      end else begin
        FSkipCount := FSkipCount + 1;
        Exit;
      end;
    end;

    FIsBitmapChanged := false;

    if Stretch then set_DisplaySize
    else set_BitmapSize;

    if not DXDraw.CanDraw then Exit;

    if (FBitmap.Width * FBitmap.Height) = 0 then Exit;

    isImageSmall := (FBitmap.Width <= DXDraw.SurfaceWidth) and (FBitmap.Height <= DXDraw.SurfaceHeight);

    if not Stretch then begin
      if DXDraw.Left <> 0 then DXDraw.Left := 0;
      if DXDraw.Top  <> 0 then DXDraw.Top  := 0;

      try
        draw_Bitmap;
      except
        on E : Exception do
          if Assigned(OnErrorMsg) then OnErrorMsg( Self, Format('TfmDirectBitmap.on_Timer - %s', [E.Message]) );
      end;

      Exit;
    end;

    try
      if isImageSmall then draw_Bitmap
      else draw_ResizedBitmap;
    except
      on E : Exception do
        if Assigned(OnErrorMsg) then OnErrorMsg( Self, Format('TfmDirectBitmap.on_Timer - %s', [E.Message]) );
    end;

    set_DXDrawToCenter;
  finally
    Timer.Enabled := true;
  end;
end;

procedure TfmDirectBitmap.SetScreenHint(const Value: string);
begin
  DXDraw.Hint := Value;
end;

procedure TfmDirectBitmap.SetStretch(const Value: boolean);
begin
  FStretch := Value;

  FOldFromWidth  := 0;
  FOldFormHeight := 0;

  FOldBitmapWidth  := 0;
  FOldBitmapHeight := 0;

  if Value then begin
    DXDraw.Parent := Self;

    set_DisplaySize;
    set_DXDrawToCenter;

    ScrollBox.Visible := false;
  end else begin
    ScrollBox.Visible := true;

    set_BitmapSize;

    DXDraw.Parent := ScrollBox;
    DXDraw.Left := 0;
    DXDraw.Top  := 0;
  end;

  if Assigned(OnStretchChanged) then OnStretchChanged(Self);
end;

procedure TfmDirectBitmap.draw_Bitmap;
begin
  FCS.Acquire;
  try
    DXDraw.Surface.Canvas.Draw(
      (DXDraw.Width  div 2) - (FBitmap.Width  div 2),
      (DXDraw.Height div 2) - (FBitmap.Height div 2),
      FBitmap
    );
  finally
    FCS.Release;
  end;

  DXDraw.Surface.Canvas.Release;

  DXDraw.Flip;
end;

procedure TfmDirectBitmap.draw_ResizedBitmap;
var
  Bitmap : TBitmap;
  isNeedResize, isBitmapChanged : boolean;
  ptSrc, ptDst, ptResult : TPoint;
begin
  ptSrc.X := FBitmap.Width;
  ptSrc.Y := FBitmap.Height;

  ptDst.X := DXDraw.Width;
  ptDst.Y := DXDraw.Height;

  ptResult := RatioSize(ptSrc, ptDst);

  isNeedResize := (FBitmap.Width > ptResult.X) or (FBitmap.Height > ptResult.Y);

  if isNeedResize then begin
    FResizedBitmap.Width  := ptResult.X;
    FResizedBitmap.Height := ptResult.Y;

    isBitmapChanged :=
      FIsBitmapChanged or
      ((FBitmap.Width <> FResizedBitmap.Width) or (FBitmap.Height <> FResizedBitmap.Height));

    FCS.Acquire;
    try
      if isBitmapChanged then SmoothResize( FBitmap, FResizedBitmap );
    finally
      FCS.Release;
    end;

    Bitmap := FResizedBitmap;
  end else begin
    Bitmap := FBitmap;
  end;

  FCS.Acquire;
  try
    DXDraw.Surface.Canvas.Draw(
      (DXDraw.Width  div 2) - (Bitmap.Width  div 2),
      (DXDraw.Height div 2) - (Bitmap.Height div 2),
      Bitmap
    );
  finally
    FCS.Release;
  end;

  DXDraw.Surface.Canvas.Release;

  DXDraw.Flip;
end;

procedure TfmDirectBitmap.DXDrawDblClick(Sender: TObject);
begin
  Stretch := not Stretch;
end;

procedure TfmDirectBitmap.set_BitmapSize;
begin
  if (FBitmap.Width = FOldBitmapWidth) and (FBitmap.Height = FOldBitmapHeight) then Exit;

  FOldBitmapWidth  := FBitmap.Width;
  FOldBitmapHeight := FBitmap.Height;

  if set_DXDrawSize( Point(FBitmap.Width, FBitmap.Height) ) then begin
    DXDraw.Width  := FBitmap.Width;
    DXDraw.Height := FBitmap.Height;
  end else begin
    if set_DXDrawSize( FDXResolutionLimit.GetLargerSize(FBitmap.Width, FBitmap.Height) ) then begin
      DXDraw.Width  := DXDraw.SurfaceWidth;
      DXDraw.Height := DXDraw.SurfaceHeight;
    end else begin
      if Assigned(OnErrorMsg) then OnErrorMsg( Self, Format('set_BitmapSize Error - FBitmap.Width: %d, FBitmap.Height: %d', [FBitmap.Width, FBitmap.Height]) );
      Trace('TfmDirectBitmap.set_BitmapSize - Error');
    end;
  end;
end;

procedure TfmDirectBitmap.set_DisplaySize;
begin
  if (Width = FOldFromWidth) and (Height = FOldFormHeight) then Exit;

  FOldFromWidth  := Width;
  FOldFormHeight := Height;

  if not set_DXDrawSize( Point(Width, Height) ) then begin
    if set_DXDrawSize( FDXResolutionLimit.GetSimilarSize(Width, Height) )  then begin
    end else begin
      if Assigned(OnErrorMsg) then OnErrorMsg( Self, Format('set_DisplaySize Error - Width: %d, Height: %d', [Width, Height]) );
      Trace('TfmDirectBitmap.set_DisplaySize - Error');
    end;
  end;

  DXDraw.Width  := DXDraw.SurfaceWidth;
  DXDraw.Height := DXDraw.SurfaceHeight;
end;

function TfmDirectBitmap.set_DXDrawSize(ASize: TPoint): boolean;
begin
  Result := true;

  DXDraw.Finalize;

  DXDraw.SurfaceWidth  := ASize.X;
  DXDraw.SurfaceHeight := ASize.Y;
  DXDraw.Display.BitCount := 32;

  try
    DXDraw.Initialize;
  except
    Result := false;
  end;
end;

procedure TfmDirectBitmap.set_DXDrawToCenter;
var
  iLeft, iTop : integer;
begin
  iLeft := (Width  div 2) - (DXDraw.Width  div 2);
  iTop  := (Height div 2) - (DXDraw.Height div 2);

  if iLeft <> DXDraw.Left then DXDraw.Left := iLeft;
  if iTop  <> DXDraw.Top  then DXDraw.Top  := iTop;
end;

procedure TfmDirectBitmap.Terminate;
begin
  Timer.Enabled := false;
end;

end.
