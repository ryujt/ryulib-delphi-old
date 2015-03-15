unit _fmMain;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    btFlipBitmapHorizontal: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btFlipBitmapHorizontalClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btFlipBitmapHorizontalClick(Sender: TObject);
begin
  FlipBitmapHorizontal( Image.Picture.Bitmap );
  Image.Repaint;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;
end;

end.
