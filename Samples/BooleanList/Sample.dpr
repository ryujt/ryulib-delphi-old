program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  BooleanList in '..\..\BooleanList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
