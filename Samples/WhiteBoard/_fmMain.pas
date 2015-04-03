unit _fmMain;

interface

uses
  RyuLib.WhiteBoard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    FWhiteBoard : TWhiteBoard;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btClearClick(Sender: TObject);
begin
  FWhiteBoard.Clear;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWhiteBoard := TWhiteBoard.Create(Self);
  FWhiteBoard.Align := alClient;
  FWhiteBoard.Parent := Self;
  FWhiteBoard.Prepare( FWhiteBoard.Width, FWhiteBoard.Height );
end;

end.
