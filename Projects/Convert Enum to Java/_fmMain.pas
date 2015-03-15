unit _fmMain;

interface

uses
  Scanner,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Panel1: TPanel;
    btConvert: TButton;
    procedure btConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScanner : TScanner;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btConvertClick(Sender: TObject);
var
  stlTemp : TStringList;
  Loop: Integer;
begin
  FScanner.SetText( moMsg.Text );

  moMsg.Clear;

  stlTemp := TStringList.Create;
  try
    while not FScanner.IsEOF do begin
      FScanner.GetNextToken;
      if FScanner.Token.TokenType = ttIdentifier then stlTemp.Add( FScanner.Token.Text );
    end;

    for Loop := 0 to stlTemp.Count-1 do
      moMsg.Lines.Add( Format(#9'public static final int %s = %d;', [stlTemp[Loop], Loop]) );

    moMsg.Lines.Add( '' );

    moMsg.Lines.Add( #9'public static String toString(int value) {' );
    moMsg.Lines.Add( #9#9'switch (value) {' );

    for Loop := 0 to stlTemp.Count-1 do
      moMsg.Lines.Add( Format(#9#9#9'case %s: return "%s";', [stlTemp[Loop], stlTemp[Loop]]) );

    moMsg.Lines.Add( #9#9#9'default: return "";' );
    moMsg.Lines.Add( #9#9'}' );
    moMsg.Lines.Add( #9'}' );
  finally
    stlTemp.Free;
  end;
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
