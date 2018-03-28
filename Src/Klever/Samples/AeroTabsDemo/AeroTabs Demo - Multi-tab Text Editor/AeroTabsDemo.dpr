program AeroTabsDemo;

uses
  Forms,
  uMain in 'uMain.pas' {frmMain},
  uEditor in 'uEditor.pas' {frmEditor: TFrame},
  uAbout in 'uAbout.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TrkAeroTabbed Text Editor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
