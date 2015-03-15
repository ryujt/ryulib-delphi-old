program Sample;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  HtmlScanner in '..\..\HtmlScanner.pas',
  HtmlParser in '..\..\HtmlParser.pas',
  HtmlTagScanner in '..\..\HtmlTagScanner.pas',
  HtmlElement in '..\..\HtmlElement.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
