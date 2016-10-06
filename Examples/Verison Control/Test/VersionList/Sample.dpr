program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  VersionList in '..\..\Lib\VersionList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
