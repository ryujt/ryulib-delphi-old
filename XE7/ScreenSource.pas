unit ScreenSource;

interface

uses
  Windows, Classes, SysUtils, Graphics, Forms;

type
  TScreenSource = class (TComponent)
  private
    FDeskCanvas : TCanvas;
    procedure do_Capture;
  private
    FBitmap: TBitmap;
    FPosition: TPoint;
    function GetPixelFormat: TPixelFormat;
    procedure SetPixelFormat(const Value: TPixelFormat);
    function GetSize: TSize;
    procedure SetSize(const Value: TSize);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Capture; virtual; abstract;
  published
    property Bitmap : TBitmap read FBitmap;
    property PixelFormat : TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Position : TPoint read FPosition write FPosition;
    property Size : TSize read GetSize write SetSize;
  end;

  TScreenSourceRegion = class (TScreenSource)
  private
  public
    procedure Capture; override;
  end;

  TScreenSourceMonitor = class (TScreenSource)
  private
    FMonitorNo: integer;
    procedure SetMonitorNo(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Capture; override;
  published
    property MonitorNo : integer read FMonitorNo write SetMonitorNo;
  end;

  TScreenSourceWindow = class (TScreenSource)
  private
    FTargetHandle: HWND;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Capture; override;
  published
    property TargetHandle : HWND read FTargetHandle write FTargetHandle;
  end;

implementation

{ TScreenCaptrueSource }

constructor TScreenSource.Create(AOwner: TComponent);
begin
  inherited;

  FPosition := Point(0, 0);

  FBitmap := TBitmap.Create;
  FDeskCanvas := TCanvas.Create;
end;

destructor TScreenSource.Destroy;
begin
  FreeAndNil(FDeskCanvas);
  FreeAndNil(FBitmap);

  inherited;
end;

procedure TScreenSource.do_Capture;
const
  CAPTUREBLT = $40000000;
var
  DC : HDC;
  SRect, DRect : TRect;
begin
  SRect := Rect(Position.X, Position.Y, Position.X+Size.cx, Position.Y+Size.cy);
  DRect := Rect(0, 0, Size.cx, Size.cy);

  DC := GetWindowDC( GetDesktopWindow );
  if DC = 0 then
    raise Exception.Create('TScreenSource.do_Capture - DC = 0');

  FDeskCanvas.Handle := DC;
  try
    FBitmap.Canvas.Brush.Color := clBlack;
    FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
    FBitmap.Canvas.Brush.Color := clWhite;

    FBitmap.Canvas.CopyMode := SRCCOPY or CAPTUREBLT;

    FBitmap.Canvas.CopyRect(DRect, FDeskCanvas, SRect);
  finally
    ReleaseDC(0, DC);
  end;
end;

function TScreenSource.GetPixelFormat: TPixelFormat;
begin
  Result := FBitmap.PixelFormat;
end;

function TScreenSource.GetSize: TSize;
begin
  Result.cx := FBitmap.Width;
  Result.cy := FBitmap.Height;
end;

procedure TScreenSource.SetPixelFormat(const Value: TPixelFormat);
begin
  FBitmap.PixelFormat := Value;
end;

procedure TScreenSource.SetSize(const Value: TSize);
begin
  FBitmap.Width := Value.cx;
  FBitmap.Height := Value.cy;
end;

{ TScreenSourceRegion }

procedure TScreenSourceRegion.Capture;
begin
  do_Capture;
end;

{ TScreenSourceMonitor }

procedure TScreenSourceMonitor.Capture;
begin
  do_Capture;
end;

constructor TScreenSourceMonitor.Create(AOwner: TComponent);
begin
  inherited;

  SetMonitorNo(0);
end;

procedure TScreenSourceMonitor.SetMonitorNo(const Value: integer);
begin
  if (Value < 0) or (Value >= Screen.MonitorCount) then
    raise Exception.Create(ClassName + '.SetMonitorNo: 없는 모니터 번호를 지정하였습니다.');

  FMonitorNo := Value;

  FPosition.X := Screen.Monitors[Value].Left;
  FPosition.Y := Screen.Monitors[Value].Top;
  FBitmap.Width := Screen.Monitors[Value].Width;
  FBitmap.Height := Screen.Monitors[Value].Height;

  FMonitorNo := Value;
end;

{ TScreenSourceWindow }

procedure TScreenSourceWindow.Capture;
var
  SRect : TRect;
begin
  if FTargetHandle = 0 then Exit;
  if GetWindowRect(FTargetHandle, SRect) = false then Exit;

  FPosition.X := SRect.Left;
  FPosition.Y := SRect.Top;
  FBitmap.Width := SRect.Right - SRect.Left;
  FBitmap.Height := SRect.Bottom - SRect.Top;

  do_Capture;
end;

constructor TScreenSourceWindow.Create(AOwner: TComponent);
begin
  inherited;

  FTargetHandle := 0;
end;

end.
