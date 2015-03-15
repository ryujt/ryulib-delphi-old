unit main;

interface

uses
  WebCam,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    btSnapShot: TButton;
    btStart: TButton;
    btStop: TButton;
    plCam: TPanel;
    Image: TImage;
    cbDeviceList: TComboBox;
    cbResolutionList: TComboBox;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure cbResolutionListChange(Sender: TObject);
    procedure cbDeviceListChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FWebCam : TWebCamCtrl;
    procedure on_Captured(Sender:TObject; const ABitmap:TBitmap);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FWebCam.Start;

  cbResolutionList.Items.Text := FWebCam.ResolutionList.Text;
  cbResolutionList.ItemIndex := 0;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FWebCam.Stop;
end;

procedure TfmMain.cbDeviceListChange(Sender: TObject);
begin
  if cbDeviceList.ItemIndex > -1 then
    FWebCam.DeviceNo := cbDeviceList.ItemIndex;
end;

procedure TfmMain.cbResolutionListChange(Sender: TObject);
begin
  if cbResolutionList.ItemIndex > -1 then
    FWebCam.Resolution := cbResolutionList.ItemIndex;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWebCam := TWebCamCtrl.Create(Self);
  FWebCam.Align  := alClient;
  FWebCam.Parent := plCam;
  FWebCam.OnCaptured := on_Captured;

  cbDeviceList.Items.Text := FWebCam.DeviceList.Text;
  cbDeviceList.ItemIndex := 0;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FWebCam.Free;
end;

procedure TfmMain.on_Captured(Sender: TObject; const ABitmap: TBitmap);
begin
  Image.Picture.Bitmap.Assign(ABitmap);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  try
    FWebCam.Capture;
//    if FWebCam.SnapShot(Image.Picture.Bitmap) then ; // do Something
  finally
    Timer.Enabled := true;
  end;
end;

end.
