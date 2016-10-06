unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BitmapWindow, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmapWindow: TBitmapWindow;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;

  FBitmapWindow := TBitmapWindow.Create(Self);
  FBitmapWindow.Bitmap.Assign(Image1.Picture.Bitmap);
  FBitmapWindow.Align := alClient;
  FBitmapWindow.Parent := Self;
  FBitmapWindow.TargetControl := Self;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBitmapWindow);
end;

end.
