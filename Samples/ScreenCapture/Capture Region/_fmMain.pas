unit _fmMain;

interface

uses
  ScreenCapture,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    ScrollBox1: TScrollBox;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := 1024;
  Image.Picture.Bitmap.Height := 768;
  Image.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, 1024, 768));

  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.PixelFormat := pf32bit;
  FScreenCapture.ScreenSourceType := ssRegion;
  FScreenCapture.X := 100;
  FScreenCapture.Y := 100;
  FScreenCapture.Width := 800;
  FScreenCapture.Height := 600;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  FScreenCapture.Capture;
  Image.Picture.Bitmap.Canvas.Draw(0, 0, FScreenCapture.Bitmap);
end;

end.
