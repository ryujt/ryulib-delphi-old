program PlayCap;

uses
  Forms,
  main in 'main.pas' {fmMain},
  WebCam in '..\..\WebCam.pas' {WebCamCtrl: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
