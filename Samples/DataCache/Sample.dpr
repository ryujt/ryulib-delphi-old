program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  DataCache in '..\..\DataCache.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
