program IDC;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  SelectPathDlg in 'SelectPathDlg.pas' {fmSelectPathDlg},
  InsertDebugCode in 'InsertDebugCode.pas',
  ScanMethod in 'ScanMethod.pas',
  ScanMgr in 'ScanMgr.pas',
  Config in 'Config.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
