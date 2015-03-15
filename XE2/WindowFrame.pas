unit WindowFrame;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

const
  FRAME_SIZE = 10;

type
  TfmWindowFrame = class(TForm)
    Timer: TTimer;
    Image: TImage;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure make_A_Hole;
  private
    ExWindowRect : TRect;
    FTargetWindow: THandle;
    procedure SetTargetWindow(const Value: THandle);
  public
  published
    property TargetWindow : THandle read FTargetWindow write SetTargetWindow;
  end;

implementation

{$R *.dfm}

{ TfmWindowFrame }

procedure TfmWindowFrame.FormCreate(Sender: TObject);
begin
  FTargetWindow := 0;
end;

procedure TfmWindowFrame.FormPaint(Sender: TObject);
var
  Loop,
  iLeft, iTop, iWidth, iHeight : integer;
begin
  iLeft   := FRAME_SIZE;
  iTop    := FRAME_SIZE;
  iWidth  := Width  - FRAME_SIZE;
  iHeight := Height - FRAME_SIZE;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clLime;

  for Loop := 1 to FRAME_SIZE do begin
    Canvas.Rectangle(iLeft, iTop, iWidth, iHeight);

    Dec(iLeft);
    Dec(iTop);
    Inc(iWidth);
    Inc(iHeight);

    Canvas.Pen.Color := SetBright(clLime, 100 - Loop * 5);
  end;
end;

procedure TfmWindowFrame.make_A_Hole;
var
  hRect, hDelete, hResult: THandle;
begin
  hResult := CreateRectRgn(0, 0, Width, Height);
  hRect := CreateRectRgn(0, 0, Width, Height);
  hDelete := CreateRectRgn(FRAME_SIZE, FRAME_SIZE, Width - FRAME_SIZE, Height - FRAME_SIZE);

  if CombineRgn(hResult, hRect, hDelete, RGN_XOR) <> ERROR then
    SetWindowRgn(Handle, hResult, True);
end;

procedure TfmWindowFrame.SetTargetWindow(const Value: THandle);
begin
  if Value = Handle then Exit;
  if Value = FindWindow('Shell_TrayWnd',nil) then Exit;

  FTargetWindow := Value;
  Timer.Enabled := Value <> 0;

  if Timer.Enabled then begin
    Show;
    make_A_Hole;
  end else begin
    Hide;
  end;
end;

procedure TfmWindowFrame.TimerTimer(Sender: TObject);
var
  WindRect : TRect;
begin
  if FTargetWindow = 0 then Exit;
  if not IsWindowVisible(FTargetWindow) then begin
    Hide;
    Exit;
  end;

  Winapi.Windows.GetWindowRect(FTargetWindow, WindRect);

  if ExWindowRect = WindRect then Exit;
  ExWindowRect := WindRect;

  Left   := WindRect.Left   - FRAME_SIZE;
  Top    := WindRect.Top    - FRAME_SIZE;
  Width  := WindRect.Width  + FRAME_SIZE * 2;
  Height := WindRect.Height + FRAME_SIZE * 2;

  make_A_Hole;
end;

end.
