unit _fmMain;

interface

uses
  JsonData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Panel1: TPanel;
    btTest1: TButton;
    btTest2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTest1Click(Sender: TObject);
    procedure btTest2Click(Sender: TObject);
  private
    FJsonData : TJsonData;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btTest1Click(Sender: TObject);
var
  Child : TJsonData;
  Loop: Integer;
begin
  FJsonData.Text := moMsg.Text;

  moMsg.Lines.Add('');
  for Loop := 0 to FJsonData.Size-1 do moMsg.Lines.Add(FJsonData.Names[Loop]);


  moMsg.Lines.Add('');
  moMsg.Lines.Add(Format('Result: %s', [BoolToStr(FJsonData.Booleans['Result'], true)]));
  moMsg.Lines.Add(Format('Name: %s', [FJsonData.Values['Name']]));
  moMsg.Lines.Add(Format('email: %s', [FJsonData.Values['email']]));

  Child := FJsonData.GetJsonData('Family');
  try
    moMsg.Lines.Add('');
    moMsg.Lines.Add(Format('Family.Wife: %s', [Child.Values['Wife']]));
    moMsg.Lines.Add(Format('Family.Son: %s', [Child.Values['Son']]));
    moMsg.Lines.Add(Format('Family.Daughter: %s', [Child.Values['Daughter']]));
  finally
    Child.Free;
  end;
end;

procedure TfmMain.btTest2Click(Sender: TObject);
begin
  FJsonData.Text := '{ "Item1": ""}';
  FJsonData.SetJsonData('Item1', ' { "Field1": "x", "Field2": "y", "Field3": "z"}');
  moMsg.Text := FJsonData.Text;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FJsonData := TJsonData.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FJsonData);
end;

end.
