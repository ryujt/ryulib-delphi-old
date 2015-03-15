program Sample;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _fmAppBarRight in '_fmAppBarRight.pas' {fmAppBarRight},
  _fmAppBarBottom in '_fmAppBarBottom.pas' {fmAppBarBottom},
  _frCommon in '_frCommon.pas' {frCommon: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
