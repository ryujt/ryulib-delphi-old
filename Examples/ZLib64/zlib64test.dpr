program zlib64test;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {Form1},
  ZLib64 in 'ZLib64.pas',
  Zlib64Const in 'Zlib64Const.pas',
  ZLib64Utils in 'ZLib64Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
