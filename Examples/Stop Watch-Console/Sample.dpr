program Sample;

{$APPTYPE CONSOLE}

uses
  SysUtils, StopWatch, Windows;

var
  sw : TStopWatch;
begin
  sw := TStopWatch.Create;
  sw.Clear;

  WriteLn(sw.Estimated);
  sleep(100);
  WriteLn(sw.Estimated);
  WriteLn(sw.Estimated);
  sleep(2000);
  WriteLn(sw.Estimated);
  sleep(2500);
  WriteLn(sw.Estimated);
  sleep(3000);
  WriteLn(sw.Estimated);
  sleep(3500);
  WriteLn(sw.Estimated);
  sw.Free;

  readln;
end.
