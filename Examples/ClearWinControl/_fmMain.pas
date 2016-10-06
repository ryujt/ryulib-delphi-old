unit _fmMain;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button2Click(Sender: TObject);
begin
  ClearWinControl(Panel1);
end;

end.
