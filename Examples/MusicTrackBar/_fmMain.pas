unit _fmMain;

interface

uses
  MusicTrackBar,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    imgBackground: TImage;
    imgBar: TImage;
    bt30: TButton;
    bt100: TButton;
    imgDownloader: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bt30Click(Sender: TObject);
    procedure bt100Click(Sender: TObject);
  private
    FTrackBar : TMusicTrackBar;
    procedure on_Changed(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.bt100Click(Sender: TObject);
begin
  FTrackBar.Downloaded := 100;
end;

procedure TfmMain.bt30Click(Sender: TObject);
begin
  FTrackBar.Downloaded := 30;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTrackBar := TMusicTrackBar.Create(Self);
  FTrackBar.BitmapBackground.Assign(imgBackground.Picture.Bitmap);
  FTrackBar.BitmapPositionBar.Assign(imgBar.Picture.Bitmap);
  FTrackBar.BitmapDownloader.Assign(imgDownloader.Picture.Bitmap);
  FTrackBar.Align := alClient;
  FTrackBar.Parent := Panel1;
  FTrackBar.OnChanged := on_Changed;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackBar);
end;

procedure TfmMain.on_Changed(Sender: TObject);
begin
  Caption := Format('Position: %d', [FTrackBar.Position]);
end;

end.
