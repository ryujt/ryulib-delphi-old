unit _fmMain;

interface

uses
  RyuGraphics, ScreenCapture, Bitmap256, LZMA, ZLibUtils,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    ScrollBox: TScrollBox;
    Image: TImage;
    Timer: TTimer;
    moMsg: TMemo;
    Panel1: TPanel;
    btStart: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  Loop, Loop1, Loop2, Temp, h, m : Integer;

  // 휘도 순에 대한 인덱스
  Indexs : array [0..255] of byte;
begin
  FillChar(Indexs, SizeOf(Indexs), 0);

  for Loop := Low(PALETTE_256_SORTED) to High(PALETTE_256_SORTED) do begin
    h := PALETTE_256_SORTED[Loop, 3];
    if Indexs[h] <> Loop then Indexs[h] := Loop;
  end;

  m := 0;

  // 빈칸 채우기
  for Loop := Low(Indexs) to High(Indexs) do begin
    if Indexs[Loop] > m then m := Indexs[Loop];
    if Indexs[Loop] = 0 then Indexs[Loop] := m;
  end;

  sLine := '';

  for Loop := Low(Indexs) to High(Indexs) do begin
    sLine := sLine + Format('%d, ', [Indexs[Loop]]);

    if ((Loop + 1) mod 8) = 0 then begin
      moMsg.Lines.Add( sLine );
      sLine := '';
    end;
  end;

  moMsg.Lines.Add( sLine );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;

  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.MonitorNo := 1;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  BufferInSize, BufferOutSize : DWord;
//  BufferInSize, BufferOutSize : integer;
  BufferIn, BufferOut : pointer;
begin
  Timer.Enabled := false;
  try
    FScreenCapture.Capture;

    BufferInSize := FScreenCapture.Bitmap.Width * FScreenCapture.Bitmap.Height;
    BufferOutSize := FScreenCapture.Bitmap.Width * FScreenCapture.Bitmap.Height;

    GetMem(BufferIn, BufferInSize);
    GetMem(BufferOut, BufferOutSize);
    try
      Convert32to8(
        FScreenCapture.Bitmap.ScanLine[FScreenCapture.Bitmap.Height-1],
        BufferIn,
        FScreenCapture.Bitmap.Width, FScreenCapture.Bitmap.Height
      );

//      CompressSlow(BufferIn, BufferInSize, BufferOut, BufferOutSize);
//      ShrinkDataToBufferSlow(BufferIn, BufferInSize, BufferOut, BufferOutSize);

      Caption := Format( 'BufferOutSize: %d KB', [BufferOutSize div 1024] );

      Image.Picture.Bitmap.Width  := FScreenCapture.Bitmap.Width;
      Image.Picture.Bitmap.Height := FScreenCapture.Bitmap.Height;

      Convert8to32(
        BufferIn,
        Image.Picture.Bitmap.ScanLine[Image.Picture.Bitmap.Height-1],
        Image.Picture.Bitmap.Width, Image.Picture.Bitmap.Height
      );
    finally
      FreeMem(BufferIn);
      FreeMem(BufferOut);
    end;

    Image.Repaint;
  finally
    Timer.Enabled := true;
  end;
end;

end.


// 휘도 구하기
procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  Loop, h: Integer;
begin
  sLine := '';

  // 28% (72) Red, 55% (140) Green, and 17% (44) blue
  for Loop := Low(PALETTE_256) to High(PALETTE_256) do begin
    h := ( 72 * PALETTE_256[Loop, 0])  +
         (140 * PALETTE_256[Loop, 1])  +
         ( 44 * PALETTE_256[Loop, 2]);

    h := h div 256;

    if h > 255 then begin
      raise Exception.Create('Error Message');
    end else begin
      sLine := sLine + Format('(%d, %d, %d, %d), ', [PALETTE_256[Loop, 0], PALETTE_256[Loop, 1], PALETTE_256[Loop, 2], h]);

      if ((Loop + 1) mod 8) = 0 then begin
        moMsg.Lines.Add( sLine );
        sLine := '';
      end;
    end;
  end;

  moMsg.Lines.Add( sLine );
end;

// 휘도 순으로 정렬 하기
procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  Loop, Loop1, Loop2, Temp: Integer;
begin
  sLine := '';

  for Loop1 := Low(PALETTE_256) to High(PALETTE_256) do
  for Loop2 := Low(PALETTE_256) to High(PALETTE_256) do begin
    if PALETTE_256[Loop1, 3] < PALETTE_256[Loop2, 3] then begin
      Temp := PALETTE_256[Loop1, 0];
      PALETTE_256[Loop1, 0] := PALETTE_256[Loop2, 0];
      PALETTE_256[Loop2, 0] := Temp;

      Temp := PALETTE_256[Loop1, 1];
      PALETTE_256[Loop1, 1] := PALETTE_256[Loop2, 1];
      PALETTE_256[Loop2, 1] := Temp;

      Temp := PALETTE_256[Loop1, 2];
      PALETTE_256[Loop1, 2] := PALETTE_256[Loop2, 2];
      PALETTE_256[Loop2, 2] := Temp;

      Temp := PALETTE_256[Loop1, 3];
      PALETTE_256[Loop1, 3] := PALETTE_256[Loop2, 3];
      PALETTE_256[Loop2, 3] := Temp;
    end;
  end;

  // 28% (72) Red, 55% (140) Green, and 17% (44) blue
  for Loop := Low(PALETTE_256) to High(PALETTE_256) do begin
    sLine := sLine + Format('(%d, %d, %d, %d), ', [PALETTE_256[Loop, 0], PALETTE_256[Loop, 1], PALETTE_256[Loop, 2], PALETTE_256[Loop, 3]]);

    if ((Loop + 1) mod 8) = 0 then begin
      moMsg.Lines.Add( sLine );
      sLine := '';
    end;
  end;

  moMsg.Lines.Add( sLine );
end;

// 휘도를 인덱스로 바로 접근 하도록
procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  Loop, Loop1, Loop2, Temp, h, m : Integer;

  // 휘도 순에 대한 인덱스
  Indexs : array [0..255] of byte;
begin
  FillChar(Indexs, SizeOf(Indexs), 0);

  for Loop := Low(PALETTE_256_SORTED) to High(PALETTE_256_SORTED) do begin
    h := PALETTE_256_SORTED[Loop, 3];
    if Indexs[h] <> Loop then Indexs[h] := Loop;
  end;

  m := 0;

  // 빈칸 채우기
  for Loop := Low(Indexs) to High(Indexs) do begin
    if Indexs[Loop] > m then m := Indexs[Loop];
    if Indexs[Loop] = 0 then Indexs[Loop] := m;
  end;

  sLine := '';

  for Loop := Low(Indexs) to High(Indexs) do begin
    sLine := sLine + Format('%d, ', [Indexs[Loop]]);

    if ((Loop + 1) mod 8) = 0 then begin
      moMsg.Lines.Add( sLine );
      sLine := '';
    end;
  end;

  moMsg.Lines.Add( sLine );
end;

// 고속으로 근사치 휘도를 찾기
procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  Loop, h: Integer;
begin
  sLine := '';

  // 28% (72) Red, 55% (140) Green, and 17% (44) blue
  for Loop := Low(PALETTE_256) to High(PALETTE_256) do begin
    h := GetLuminance( PALETTE_256[Loop, 0], PALETTE_256[Loop, 1], PALETTE_256[Loop, 2] );

    sLine := sLine + Format('(%d, %d, %d, %d), ', [PALETTE_256[Loop, 0], PALETTE_256[Loop, 1], PALETTE_256[Loop, 2], h]);

    if ((Loop + 1) mod 8) = 0 then begin
      moMsg.Lines.Add( sLine );
      sLine := '';
    end;
  end;

  moMsg.Lines.Add( sLine );
end;

