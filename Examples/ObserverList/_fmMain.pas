unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, _frBar, _frText;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer: TTimer;
    frText: TfrText;
    frBar: TfrBar;
    procedure TimerTimer(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  View;

{$R *.dfm}

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  TView.Obj.sp_CurrentVolume(Random(100));
end;

end.
