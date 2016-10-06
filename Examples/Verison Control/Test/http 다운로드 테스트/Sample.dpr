program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  HttpDownload in '..\..\Lib\HttpDownload.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
