unit _fmMain;

interface

uses
  TileView,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FTileView : TTileView;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  Item : TPanel;
  Loop: Integer;
begin
  FTileView := TTileView.Create(Self);
  FTileView.Align := alClient;
  FTileView.Parent := Self;
  FTileView.MarginHorizontal := 6;
  FTileView.MarginVertical := 4;

  for Loop := 1 to 23 do begin
    Item := TPanel.Create(Self);
    Item.ParentBackground := false;
    Item.ParentColor := false;
    Item.Color := Random(256) + Random(256) shl 8 + Random(256) shl 16;
    FTileView.Add(Item);
  end;
end;

end.
