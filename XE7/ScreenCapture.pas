unit ScreenCapture;

interface

uses
  CursorCaptrue, ScreenSource,
  Windows, Classes, SysUtils, Vcl.Graphics;

type
  TScreenSourceType = (ssRegion, ssMonitor, ssWindow);

  TScreenCapture = class (TComponent)
  private
    FCursorCaptrue : TCursorCaptrue;
    FSource, FSourceRegion, FSourceMonitor, FSourceWindow : TScreenSource;
  private
    FWithCursor: boolean;
    FScreenSourceType: TScreenSourceType;
    function GetPixelFormat: TPixelFormat;
    procedure SetPixelFormat(const Value: TPixelFormat);
    function GetHeight: integer;
    function GetWidth: integer;
    function GetX: integer;
    function GetY: integer;
    procedure SetScreenSourceType(const Value: TScreenSourceType);
    function GetBitmap: TBitmap;
    function GetMonitorNo: integer;
    procedure SetMonitorNo(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    function GetTargetHandle: HWND;
    procedure SetTargetHandle(const Value: HWND);
    function GetCursor: ICursor;
    function GetData: pointer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Capture;
  public
    property Data : pointer read GetData;
    property Bitmap : TBitmap read GetBitmap;
  published
    property WithCursor : boolean read FWithCursor write FWithCursor;
    property Cursor : ICursor read GetCursor;

    property PixelFormat : TPixelFormat read GetPixelFormat write SetPixelFormat;

    property X : integer read GetX write SetX;
    property Y : integer read GetY write SetY;

    property Width : integer read GetWidth write SetWidth;
    property Height : integer read GetHeight write SetHeight;

    property ScreenSource : TScreenSource read FSource;

    property ScreenSourceType : TScreenSourceType read FScreenSourceType write SetScreenSourceType;

    property MonitorNo : integer read GetMonitorNo write SetMonitorNo;

    property TargetHandle : HWND read GetTargetHandle write SetTargetHandle;
  end;

implementation

{ TScreenCapture }

procedure TScreenCapture.Capture;
begin
  try
    FSource.Capture;
  except
    on E : Exception do
      raise Exception.Create('TScreenCapture.Capture - ' + E.Message);
  end;

  FCursorCaptrue.Offset := FSource.Position;

  try
    FCursorCaptrue.Capture;
  except
    on E : Exception do
      raise Exception.Create('TScreenCapture.Capture (CursorCaptrue) - ' + E.Message);
  end;

  if WithCursor then FCursorCaptrue.Draw(FSource.Bitmap);
end;

constructor TScreenCapture.Create(AOwner: TComponent);
begin
  inherited;

  FWithCursor := true;
  FScreenSourceType := ssMonitor;

  FSourceRegion := TScreenSourceRegion.Create(Self);
  FSourceMonitor := TScreenSourceMonitor.Create(Self);
  FSourceWindow := TScreenSourceWindow.Create(Self);
  FSource := FSourceMonitor;

  FCursorCaptrue := TCursorCaptrue.Create(Self);
end;

destructor TScreenCapture.Destroy;
begin
  FreeAndNil(FCursorCaptrue);
  FreeAndNil(FSourceRegion);
  FreeAndNil(FSourceMonitor);
  FreeAndNil(FSourceWindow);

  inherited;
end;

function TScreenCapture.GetBitmap: TBitmap;
begin
  Result := FSource.Bitmap;
end;

function TScreenCapture.GetCursor: ICursor;
begin
  Result := FCursorCaptrue as ICursor;
end;

function TScreenCapture.GetData: pointer;
begin
  Result := FSource.Bitmap.ScanLine[FSource.Bitmap.Height-1];
end;

function TScreenCapture.GetHeight: integer;
begin
  Result := FSource.Size.cy;
end;

function TScreenCapture.GetMonitorNo: integer;
begin
  Result := TScreenSourceMonitor(FSourceMonitor).MonitorNo;
end;

function TScreenCapture.GetPixelFormat: TPixelFormat;
begin
  Result := FSource.PixelFormat;
end;

function TScreenCapture.GetTargetHandle: HWND;
begin
  Result := TScreenSourceWindow(FSource).TargetHandle;
end;

function TScreenCapture.GetWidth: integer;
begin
  Result := FSource.Size.cx;
end;

function TScreenCapture.GetX: integer;
begin
  Result := FSource.Position.X;
end;

function TScreenCapture.GetY: integer;
begin
  Result := FSource.Position.Y;
end;

procedure TScreenCapture.SetHeight(const Value: integer);
var
  NewSize : TSize;
begin
  NewSize.cx := FSource.Size.cx;
  NewSize.cy := Value;

  FSource.Size := NewSize;
end;

procedure TScreenCapture.SetMonitorNo(const Value: integer);
begin
  TScreenSourceMonitor(FSourceMonitor).MonitorNo := Value;
end;

procedure TScreenCapture.SetPixelFormat(const Value: TPixelFormat);
begin
  FSourceRegion.PixelFormat := Value;
  FSourceMonitor.PixelFormat := Value;
  FSourceWindow.PixelFormat := Value;
end;

procedure TScreenCapture.SetScreenSourceType(const Value: TScreenSourceType);
begin
  FScreenSourceType := Value;

  case Value of
    ssRegion: FSource := FSourceRegion;
    ssMonitor: FSource := FSourceMonitor;
    ssWindow: FSource := FSourceWindow;

    else raise Exception.Create(ClassName + '.SetScreenSourceType: 알 수 없는 타입입니다.');
  end;
end;

procedure TScreenCapture.SetTargetHandle(const Value: HWND);
begin
  TScreenSourceWindow(FSource).TargetHandle := Value;
end;

procedure TScreenCapture.SetWidth(const Value: integer);
var
  NewSize : TSize;
begin
  NewSize.cx := Value;
  NewSize.cy := FSource.Size.cy;

  FSource.Size := NewSize;
end;

procedure TScreenCapture.SetX(const Value: integer);
var
  NewPos : TPoint;
begin
  NewPos.X := Value;
  NewPos.Y := FSource.Position.Y;

  FSource.Position := NewPos;
end;

procedure TScreenCapture.SetY(const Value: integer);
var
  NewPos : TPoint;
begin
  NewPos.X := FSource.Position.X;
  NewPos.Y := Value;

  FSource.Position := NewPos;
end;

end.
