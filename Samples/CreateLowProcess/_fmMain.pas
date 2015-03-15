unit _fmMain;

interface

uses
  CreateLowProcess, Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
//  ShellExecuteFile( 'C:\ProgramData\Mediawave\Update.exe', '', '' );
//  CreateLowProc('C:\ProgramData\Mediawave\Update.exe');
  CreateLowProc('notepad.exe Test.txt');
end;

end.
