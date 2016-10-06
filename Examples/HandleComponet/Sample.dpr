program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  HandleComponent in '..\..\HandleComponent.pas',
  SyncList in '..\..\SyncList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
