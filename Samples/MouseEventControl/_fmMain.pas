unit _fmMain;

interface

uses
  MouseEventControl,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FMouseEventControl : TMouseEventControl;
    procedure on_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FMouseEventControl := TMouseEventControl.Create(Self);
  FMouseEventControl.TargetControl := Self;
  FMouseEventControl.OnMouseMove := on_MouseMove;
end;

procedure TfmMain.on_MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Caption := Format( 'X: %d, Y: %d', [X, Y] );
end;

end.
