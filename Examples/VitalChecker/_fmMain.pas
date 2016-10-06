unit _fmMain;

interface

uses
  VitalChecker,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FVitalChecker : TVitalChecker;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FVitalChecker.KeepAlive;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FVitalChecker := TVitalChecker.Create;
  FVitalChecker.Active := true;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  Caption := Format('Alive: %s', [BoolToStr(FVitalChecker.IsAlive, true)]);
end;

end.
