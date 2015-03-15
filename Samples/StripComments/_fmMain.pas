unit _fmMain;

interface

uses
  StripComments,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    moInput: TMemo;
    moResult: TMemo;
    btStart: TButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure moInputDblClick(Sender: TObject);
  private
    FStripComments : TStripComments;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  moResult.Text := FStripComments.Execute(moInput.Text);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FStripComments := TStripComments.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FStripComments.Free;
end;

procedure TfmMain.moInputDblClick(Sender: TObject);
begin
  if OpenDialog.Execute then moInput.Lines.LoadFromFile(OpenDialog.FileName);  
end;

end.
