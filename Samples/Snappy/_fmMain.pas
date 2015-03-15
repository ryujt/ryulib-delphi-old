unit _fmMain;

interface

uses
  Snappy, ZLibUtils, LZMA,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    ImageIn: TImage;
    btTest: TButton;
    moResult: TMemo;
    Panel1: TPanel;
    ScrollBox: TScrollBox;
    ImageOut: TImage;
    procedure btTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFrequency : int64;
    FBitmapIn, FBitmapOut : TBitmap;
    FSourceSize : integer;
    FDataZip : pointer;
    FSizeZipSnappy : DWord;
    FSizeZip : integer;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btTestClick(Sender: TObject);
var
  Loop: Integer;
  iStart, iStop : int64;
  iBufferOutSize, iUncompressedLength : DWord;
begin
  // 이미지 압축 및 시간 측정
  begin
    QueryPerformanceCounter( iStart );

    for Loop := 1 to 1024 do begin
      iBufferOutSize := FSizeZipSnappy;
      compress(
        FBitmapIn.ScanLine[FBitmapIn.Height-1], FSourceSize,
        FDataZip, iBufferOutSize
      );
    end;

//    for Loop := 1 to 1024 do begin
//      iBufferOutSize := FSizeZipSnappy;
//      CompressFast(
//        FBitmapIn.ScanLine[FBitmapIn.Height-1], FSourceSize,
//        FDataZip, iBufferOutSize
//      );
//    end;

    FSizeZip := iBufferOutSize;

//    for Loop := 1 to 1024 do begin
//      ShrinkDataToBufferFast(
//        FBitmapIn.ScanLine[FBitmapIn.Height-1], FSourceSize,
//        FDataZip, FSizeZip
//      );
//    end;

    QueryPerformanceCounter( iStop );

    moResult.Lines.Add(
      Format(
        '* compress --> SizeZip: %d KB (%d%%), QueryPerformanceCounter: %g',
        [FSizeZip div 1024, FSizeZip * 100 div FSourceSize,(iStop-iStart) / FFrequency])
    );
  end;

//  // 압축 전 원래 크기 점검
//  begin
//    uncompressed_length(FDataZip, FSizeZip, iUncompressedLength);
//
//    moResult.Lines.Add( Format('* uncompressed_length: %d KB', [iUncompressedLength div 1024]) );
//  end;
//
//  // 압축 풀기
//  begin
//    uncompress(FDataZip, FSizeZip, ImageOut.Picture.Bitmap.ScanLine[FBitmapOut.Height-1], iUncompressedLength);
//
//    ImageOut.Repaint;
//
//    moResult.Lines.Add( Format('* uncompress --> SizeUnZip: %d KB', [iUncompressedLength div 1024]) );
//  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  QueryPerformanceFrequency(FFrequency);

  FBitmapIn := ImageIn.Picture.Bitmap;
  FBitmapIn.PixelFormat := pf32bit;

  FBitmapOut := ImageOut.Picture.Bitmap;
  FBitmapOut.PixelFormat := pf32bit;
  FBitmapOut.Width := FBitmapIn.Width;
  FBitmapOut.Height := FBitmapIn.Height;

  FSourceSize := FBitmapIn.Width * FBitmapIn.Height * 4;

  FSizeZipSnappy := max_compressed_length(FSourceSize);

  GetMem( FDataZip, FSizeZipSnappy );
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeMem( FDataZip );
end;

end.
