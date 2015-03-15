unit _fmMain;

interface

uses
  BitmapRgn, RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, BitmapWindow;

type
  TfmMain = class(TForm)
    BitmapWindow1: TBitmapWindow;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure create_Hole;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.create_Hole;
var
  Rgn : THandle;
begin
  Rgn := CreateBitmapRgn(BitmapWindow1.InternalBitmap, 40, 40);
  try
    SetWindowRgn(Handle, Rgn, true);
  finally
    DeleteObject(Rgn);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  MakeOpaque(Self);

  create_Hole;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  create_Hole;
end;

end.
