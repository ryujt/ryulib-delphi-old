program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  DuplicatedList in '..\..\DuplicatedList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
