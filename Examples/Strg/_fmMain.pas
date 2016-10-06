unit _fmMain;

interface

uses
  Strg,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    Edit1: TEdit;
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
var
  s : string;
begin
//  ShowMessage(MiddleStr(Edit1.Text, '<', '>', [rfIgnoreCase]));
//  ShowMessage(DeleteLeftPlus(Edit1.Text, ';'));
//  ShowMessage(CopyHanStr(Edit1.Text, 3, 100));

  s := 'Second';

  case IndexOf(S, ['First', 'Second', 'Third', 'x']) of
       0: ShowMessage('1: ' + s);
       1: ShowMessage('2: ' + s);
       2: ShowMessage('3: ' + s);
  else
    ShowMessage('else: ' + s);
  end;
end;

end.

