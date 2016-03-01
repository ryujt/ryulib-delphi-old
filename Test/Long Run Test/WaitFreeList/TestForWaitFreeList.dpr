program TestForWaitFreeList;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  TestScenario in 'TestScenario.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
