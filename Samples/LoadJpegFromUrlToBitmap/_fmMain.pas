unit _fmMain;

interface

uses
  LoadImageFromURL,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    btLoadJPEG: TButton;
    moMsg: TMemo;
    btAsync: TButton;
    procedure btLoadJPEGClick(Sender: TObject);
    procedure btAsyncClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btAsyncClick(Sender: TObject);
var
  Tick, OldTick : int64;
begin
  moMsg.Clear;

  QueryPerformanceCounter (OldTick);
  AsyncLoadImageFromUrlToBitmap(
    'http://cfile9.uf.tistory.com/image/196F444B4DE068841DBB1E',
    Image.Picture.Bitmap
  );
  QueryPerformanceCounter (Tick);
  moMsg.Lines.Add(Format('Tick: %d', [Tick - OldTick]));
end;

procedure TfmMain.btLoadJPEGClick(Sender: TObject);
var
  Tick, OldTick : int64;
begin
  moMsg.Clear;

  QueryPerformanceCounter (OldTick);
  LoadImageFromUrlToBitmap(
    'http://cfile9.uf.tistory.com/image/196F444B4DE068841DBB1E',
    Image.Picture.Bitmap
  );
  QueryPerformanceCounter (Tick);
  moMsg.Lines.Add(Format('Tick: %d', [Tick - OldTick]));
end;

end.
