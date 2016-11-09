unit _fmMain;

interface

uses
  LZMA, ScreenCapture,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  DATA_SOURCE_SIZE = 1024 * 1024;
  BUFFER_SIZE = 1024 * 1024 * 32;

type
  TfmMain = class(TForm)
    btLZMA: TButton;
    moMsg: TMemo;
    btTestLZMA: TButton;
    procedure btLZMAClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btTestLZMAClick(Sender: TObject);
  private
    FSizeOutLZMA : integer;
    FBufferSrc, FBufferZip, FBufferUnZip : pointer;
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function BytesStr(ABytes:integer):string;
var
  KB, MB, B : integer;
begin
  B := ABytes mod 1024;

  KB := ABytes div 1024;
  MB := 0;

  if KB >= 1024 then begin
    MB := KB div 1024;
    KB := KB - (MB * 1024);
  end;

  if MB > 0 then Result := Format('%d MB + %d KB + %d', [MB, KB, B])
  else Result := Format('%d KB + %d', [KB, b]);
end;

procedure TfmMain.btLZMAClick(Sender: TObject);
var
  Tick : Cardinal;
  iResult, iSizeIn : integer;
  Loop: Integer;
begin
  iSizeIn  := DATA_SOURCE_SIZE;
  FSizeOutLZMA := BUFFER_SIZE;

  Tick := GetTickCount;

//  for Loop := 1 to 100 do
    iResult := CompressSlow(FBufferSrc, iSizeIn, FBufferZip, FSizeOutLZMA);
//  iResult := CompressDefault(FBufferSrc, iSizeIn, FBufferZip, FSizeOutLZMA);
//  iResult := CompressFast(FBufferSrc, iSizeIn, FBufferZip, FSizeOutLZMA);

  moMsg.Lines.Add(Format('Compress: %d ms, Result=%d, iSizeOut=%s', [(GetTickCount-Tick) div 100, iResult, BytesStr(FSizeOutLZMA)]));
end;

procedure TfmMain.btTestLZMAClick(Sender: TObject);
var
  Tick : Cardinal;
  iResult, iSizeOut : integer;
begin
  Tick := GetTickCount;

  iResult := Uncompress(FBufferZip, FSizeOutLZMA, FBufferUnZip, iSizeOut);

  if not CompareMem(FBufferSrc, FBufferUnZip, DATA_SOURCE_SIZE) then
    MessageDlg('원본과 다름', mtError, [mbOk], 0)
  else
    moMsg.Lines.Add(Format('Uncompress: %dms, Result=%d, iSizeOut=%s', [GetTickCount-Tick, iResult, BytesStr(iSizeOut)]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  pData : PByte;
  Loop, iDataSize: Integer;
begin
  GetMem(FBufferSrc, BUFFER_SIZE);
  GetMem(FBufferZip, BUFFER_SIZE);
  GetMem(FBufferUnZip, BUFFER_SIZE);

  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.PixelFormat := pf32bit;
  FScreenCapture.MonitorNo := 0;
  FScreenCapture.Capture;

  iDataSize := FScreenCapture.Bitmap.Width * FScreenCapture.Bitmap.Height * 4;
  if iDataSize > DATA_SOURCE_SIZE then iDataSize := DATA_SOURCE_SIZE;

  Move(FScreenCapture.Bitmap.ScanLine[FScreenCapture.Bitmap.Height-1]^, FBufferSrc^, iDataSize);

//  pData := FBufferSrc;
//  for Loop := 0 to BUFFER_SIZE-1 do begin
//    pData^ := Random(256);
//    pData^ := Loop mod 256;
//    Inc(pData);
//  end;
end;

end.

