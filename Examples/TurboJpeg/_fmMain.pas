unit _fmMain;

interface

uses
  TurboJpeg,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

const
  QUALITY = 70;
  _JpegHeaderSize = 16 * 41 + 3;

type
  TfmMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Button1: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBuffer : pointer;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  Disk;

var
  pEncoder : pointer;
  pDecoder : pointer;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  jpeg_size : integer;

  sLine : string;
  iLineCount : integer;

  Loop: Integer;
  pIndex : PByte;

  fsData : TFileStream;

  BitmapOut : TBitmap;
begin
  with Image1.Picture do begin
    BitmapToJpeg24(pEncoder, Bitmap.ScanLine[Bitmap.Height-1], Bitmap.Width, Bitmap.Height, FBuffer, jpeg_size, QUALITY);

    fsData := TFileStream.Create(GetExecPath + 'a.jpg', fmCreate);
    try
      fsData.Write(FBuffer^, jpeg_size);
    finally
      fsData.Free;
    end;

    // Jpeg --> BMP Test
    BitmapOut := TBitmap.Create;
    try
      BitmapOut.PixelFormat := pf24bit;
      BitmapOut.Width       := Bitmap.Width;
      BitmapOut.Height      := Bitmap.Height;

      JpegToBitmap24(pDecoder, FBuffer, jpeg_size, BitmapOut.ScanLine[BitmapOut.Height-1], BitmapOut.Width, BitmapOut.Height);

      BitmapOut.SaveToFile('./a.bmp');
    finally
      BitmapOut.Free;
    end;
  end;

  // 헤더 만들기
  pIndex := FBuffer;
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
    BitmapToJpeg24(pEncoder, Bitmap.ScanLine[Bitmap.Height-1], Bitmap.Width, Bitmap.Height, FBuffer, jpeg_size, QUALITY);

    fsData := TFileStream.Create(GetExecPath + 'b.jpg', fmCreate);
    try
      fsData.Write(FBuffer^, jpeg_size);
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
  GetMem(FBuffer, 32 * 32 * 3);

  pEncoder := CreateEncoderHandle;
  pDecoder := CreateDecoderHandle;

  Image1.Picture.Bitmap.PixelFormat := pf24bit;
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Width := 32;
  Image1.Picture.Bitmap.Height := 32;


  Image2.Picture.Bitmap.PixelFormat := pf24bit;
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
