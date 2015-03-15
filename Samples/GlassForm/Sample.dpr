program Sample;

uses
  Forms,
  _fmChat in '_fmChat.pas' {fmChat};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmChat, fmChat);
  Application.Run;
end.
