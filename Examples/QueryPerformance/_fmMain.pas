unit _fmMain;

interface

uses
  QueryPerformance,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FFrequency, FCount, FOldCount : int64;
    FTick, FOldTick : Cardinal;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FOldCount := GetTick;
  FOldTick := GetTickCount;

//  QueryPerformanceFrequency(FFrequency);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
//var
//  Count, Term : int64;
begin
  FCount := GetTick;
  FTick := GetTickCount;

  Caption := Format( '%d, %d', [FCount - FOldCount, FTick - FOldTick]);

  FOldCount := FCount;
  FOldTick := FTick;

//  QueryPerformanceCounter(Count);
//
//  Term := Count-FOldCount;
//
//  Caption := Format('%d, %d, %d, %d', [Count, FOldCount, Term, Term * 1000 div FFrequency]);
//
//  FOldCount := Count;
end;

end.
