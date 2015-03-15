unit _fmMain;

interface

uses
  IntervalChecker,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FOldTick : Cardinal;
    FIntervalChecker : TIntervalChecker;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FIntervalChecker := TIntervalChecker.Create;
  FIntervalChecker.Interval := 100;
  FIntervalChecker.Start;

  FOldTick := GetTickCount;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Tick : Cardinal;
begin
  Tick := GetTickCount;

  if FIntervalChecker.Check then begin
    moMsg.Lines.Add( Format('%d', [Tick - FOldTick]) );
    FOldTick := Tick;
  end;
end;

end.
