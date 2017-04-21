unit _fmMain;

interface

uses
  MMD,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    RenderBar: TProgressBar;
    CaptureBar: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  RenderBar.Max := 65535;
  CaptureBar.Max := 65535;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Temp: Single;
begin
  Render.GetPeakValue(Temp);
  RenderBar.position := Round(Temp * 65535);

  Capture.GetPeakValue(Temp);
  CaptureBar.position := Round(Temp * 65535);
end;

end.
