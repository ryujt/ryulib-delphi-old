program Sample;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  VitalChecker in '..\..\VitalChecker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
