program ClassToDLL;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  FunctionList in 'FunctionList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
