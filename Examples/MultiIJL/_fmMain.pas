unit _fmMain;

interface

uses
  MultiIJL,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMultiIJL : TMultiIJL;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  iWidth, iHeight : integer;

  Bitmap24 : pointer;

  JPegData : pointer;
  JPegSize : integer;

  BitmapOut : pointer;
  BitmapOutSize : integer;
begin
  iWidth  := Image1.Picture.Bitmap.Width;
  iHeight := Image1.Picture.Bitmap.Height;

  Image1.Picture.Bitmap.PixelFormat := pf24bit;
  Bitmap24 := Image1.Picture.Bitmap.ScanLine[iHeight-1];

  Image2.Picture.Bitmap.PixelFormat := pf24bit;
  Image2.Picture.Bitmap.Width := iWidth;
  Image2.Picture.Bitmap.Height := iHeight;

  FMultiIJL.BitmapToJpeg(Bitmap24, iWidth, iHeight, JPegData, JPegSize);
  try
    Memo1.Lines.Add(Format('JPegSize: %d kb', [JPegSize div 1024]));

    FMultiIJL.JpegToBitmap(JPegData, JPegSize, Image2.Picture.Bitmap.ScanLine[iHeight-1], iWidth, iHeight);
    Image2.Repaint;
  finally
    FreeMem(JPegData);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FMultiIJL := TMultiIJL.Create('ijl01.dll');
end;

end.
