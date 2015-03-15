unit _fmMain;

interface

uses
  Bitmap3x1Tile,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTile : TBitmap3x1Tile;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTile := TBitmap3x1Tile.Create(Self);
  FTile.Bitmap.Assign(Image1.Picture.Bitmap);
  FTile.Align := alClient;
  FTile.Parent := Panel1;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTile);
end;

end.
