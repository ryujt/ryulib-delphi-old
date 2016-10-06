unit _fmMain;

interface

uses
  TileScrollBox,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTileScrollBox : TTileScrollBox;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTileScrollBox := TTileScrollBox.Create(Self);
  FTileScrollBox.Bitmap.Assign(Image.Picture.Bitmap);
  FTileScrollBox.Align := alClient;
  FTileScrollBox.Parent := Self;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTileScrollBox);
end;

end.
