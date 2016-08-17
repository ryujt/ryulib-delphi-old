unit _fmMain;

interface

uses
  SpeedGun,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FSpeedGun : TSpeedGun;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSpeedGun := TSpeedGun.Create;
  FSpeedGun.Start(5000);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  FSpeedGun.IncSpeed(20);
  Caption := Format('Speed = %d', [FSpeedGun.Speed]);
end;

end.
