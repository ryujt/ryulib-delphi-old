unit _fmMain;

interface

uses
  BitmapTile,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTile : TBitmapTile;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTile := TBitmapTile.Create(Self);
  FTile.Bitmap.Assign(Image1.Picture.Bitmap);
  FTile.Align := alClient;
  FTile.Parent := Self;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTile);
end;

end.
