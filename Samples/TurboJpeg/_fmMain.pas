unit _fmMain;

interface

uses
  DeskCamUtils, TurboJpeg,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

const
  QUALITY = 40;

type
  TfmMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Button1: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FTurboJpeg : TTurboJpeg;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  Disk;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  sLine : string;
  iLineCount : integer;

  DataJpeg : pointer;
  SizeJpeg : integer;

  Loop: Integer;
  pIndex : PByte;

  fsData : TFileStream;

  BitmapOut : TBitmap;
begin
  with Image1.Picture do begin
    TTurboJpeg.GetBuffer( DataJpeg, Bitmap.Width * Bitmap.Height * 4 );

    BitmapToJpegCopy(Bitmap.ScanLine[Bitmap.Height-1], Bitmap.Width, Bitmap.Height, DataJpeg, SizeJpeg, QUALITY);

    fsData := TFileStream.Create(GetExecPath + 'a.jpg', fmCreate);
    try
      fsData.Write(DataJpeg^, SizeJpeg);
    finally
      fsData.Free;
    end;

    // Jpeg --> BMP Test
    BitmapOut := TBitmap.Create;
    try
      BitmapOut.PixelFormat := pf32bit;
      BitmapOut.Width       := Bitmap.Width;
      BitmapOut.Height      := Bitmap.Height;

      JpegToBitmap(DataJpeg, SizeJpeg, BitmapOut.ScanLine[BitmapOut.Height-1], BitmapOut.Width, BitmapOut.Height);

      BitmapOut.SaveToFile('./a.bmp');
    finally
      BitmapOut.Free;
    end;
  end;

  // 헤더 만들기
  pIndex := DataJpeg;
  sLine := '';
  iLineCount := 0;
  for Loop := 0 to _JpegHeaderSize-1 do begin
    iLineCount := iLineCount + 1;
    sLine := sLine + '$' + IntToHex(pIndex^, 2) + ', ';
    if iLineCount >= 16 then begin
      iLineCount := 0;
      sLine := sLine + #13#10;
    end;

    Inc(pIndex);
  end;
  moMsg.Text := sLine;

  with Image2.Picture do begin
    BitmapToJpegCopy(Bitmap.ScanLine[Bitmap.Height-1], Bitmap.Width, Bitmap.Height, DataJpeg, SizeJpeg, QUALITY);

    fsData := TFileStream.Create(GetExecPath + 'b.jpg', fmCreate);
    try
      fsData.Write(DataJpeg^, SizeJpeg);
    finally
      fsData.Free;
    end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  X: Integer;
  Y: Integer;
begin
  FTurboJpeg := TTurboJpeg.Create;

  Image1.Picture.Bitmap.PixelFormat := pf32bit;
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Width := 32;
  Image1.Picture.Bitmap.Height := 32;


  Image2.Picture.Bitmap.PixelFormat := pf32bit;
  Image2.Picture.Bitmap.Canvas.Brush.Color := clRed;
  Image2.Picture.Bitmap.Width := 32;
  Image2.Picture.Bitmap.Height := 32;

  for Y := 0 to 32-1 do
  for X := 0 to 32-1 do begin
//    Image1.Picture.Bitmap.Canvas.Pixels[X, Y] := Random(255) + (Random(255) shl 8) + (Random(255) shl 16);
    Image2.Picture.Bitmap.Canvas.Pixels[X, Y] := Random(255) + (Random(255) shl 8) + (Random(255) shl 16);
  end;
end;

end.
