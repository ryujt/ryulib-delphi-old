unit _fmMain;

interface

uses
  HtmlParser,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    pnl1: TPanel;
    btn1: TButton;
    moInput: TMemo;
    spl1: TSplitter;
    moResult: TMemo;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHtmlParser : THtmlParser;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btn1Click(Sender: TObject);
begin
  FHtmlParser.Clear;
  FHtmlParser.Add(moInput.Text);
  moResult.Text := FHtmlParser.Root.GetTestString;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHtmlParser := THtmlParser.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHtmlParser);
end;

end.
