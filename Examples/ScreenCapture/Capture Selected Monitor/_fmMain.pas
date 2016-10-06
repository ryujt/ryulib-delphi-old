unit _fmMain;

interface

uses
  ScreenCapture,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    ScrollBox: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    btCapture: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCaptureClick(Sender: TObject);
  private
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btCaptureClick(Sender: TObject);
begin
  FScreenCapture.Capture;

  Image1.Picture.Bitmap.Assign(FScreenCapture.Bitmap);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.WithCursor := true;
  FScreenCapture.MonitorNo := 0;
end;

end.
