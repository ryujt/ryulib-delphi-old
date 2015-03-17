unit _fmMain;

interface

uses
  Scanner, Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btOpen: TButton;
    OpenDialog: TOpenDialog;
    moSrc: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
  private
    FScanner : TScanner;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
var
  Token : TToken;
  sSrc : string;
begin
  if not OpenDialog.Execute then Exit;

  FScanner.SetText( LoadFileAsText(OpenDialog.FileName) );

  sSrc := '';

  Token := FScanner.GetNextToken;
  while not FScanner.IsEOF do begin
    sSrc := sSrc + Token.OriginalText;

    Token := FScanner.GetNextToken;
  end;

  moSrc.Text := sSrc;

  moSrc.Lines.SaveToFile( OpenDialog.FileName );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScanner := TScanner.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScanner);
end;

end.
