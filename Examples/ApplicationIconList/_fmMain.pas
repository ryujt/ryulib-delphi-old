unit _fmMain;

interface

uses
  ApplicationIconList, Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    btOpen: TButton;
    Image: TImage;
    OpenDialog: TOpenDialog;
    procedure btOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FApplicationIconList : TApplicationIconList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    if FApplicationIconList.GetBitmap(OpenDialog.FileName, Image.Picture.Bitmap) then Image.Repaint;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FApplicationIconList := TApplicationIconList.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FApplicationIconList);
end;

end.
