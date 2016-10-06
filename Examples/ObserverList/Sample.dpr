program Sample;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _frText in '_frText.pas' {frText: TFrame},
  _frBar in '_frBar.pas' {frBar: TFrame},
  View in 'View.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
