unit _fmMain;

interface

uses
  EasyCam,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, YUVConverts, StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Panel1: TPanel;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FEasyCam : TEasyCam;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Picture.Bitmap.Width  := 320;
  Image.Picture.Bitmap.Height := 240;

  FEasyCam := TEasyCam.Create(Self);
  FEasyCam.Align := alClient;
  FEasyCam.Parent := Panel1;

  FEasyCam.Open( 320, 240 );
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  if FEasyCam.GetBitmap(Image.Picture.Bitmap) then Image.Repaint;
end;

end.
