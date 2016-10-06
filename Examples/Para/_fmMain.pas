unit _fmMain;

interface

uses
  Para,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  // Parameter = 1 -a=3 2 -b=abce -c

  moMsg.Lines.Add(Format('GetParameterCount: %d', [GetParameterCount]));
  moMsg.Lines.Add(Format('GetParameter(0): %s', [GetParameter(0)]));
  moMsg.Lines.Add(Format('GetParameter(1): %s', [GetParameter(1)]));
  moMsg.Lines.Add('');

  moMsg.Lines.Add(Format('GetSwitchCount: %d', [GetSwitchCount]));
  moMsg.Lines.Add(Format('GetSwitchName(0): %s', [GetSwitchName(0)]));
  moMsg.Lines.Add(Format('GetSwitchValue(''a''): %s', [GetSwitchValue('a')]));
  moMsg.Lines.Add(Format('FindSwitchName(''c''): %s', [BoolToStr(FindSwitchName('c'), true)]));
end;

end.
