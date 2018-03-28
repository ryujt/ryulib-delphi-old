program Sample;

uses
  Vcl.Forms,
  ObserverList in 'ObserverList.pas',
  ThreadRepeater in 'ThreadRepeater.pas',
  ValueList in 'ValueList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
