unit _fmMain;

interface

uses
  glCanvas, RyuGraphics,
  OpenGL,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    btDrawBitmap: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btDrawBitmapClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    FglCanvas : TglCanvas;
    procedure on_Error(Sender:TObject; AValue:Integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btClearClick(Sender: TObject);
begin
  FglCanvas.Clear;
end;

procedure TfmMain.btDrawBitmapClick(Sender: TObject);
begin
  FglCanvas.Draw( Image.Picture.Bitmap );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  MakeOpaque( Self );

  Image.Picture.Bitmap.PixelFormat := pf32bit;

  FglCanvas := TglCanvas.Create(Self);
  FglCanvas.Align := alClient;
  FglCanvas.Parent := Self;
  FglCanvas.OnError := on_Error;

  FglCanvas.CanDraw := true;
end;

procedure TfmMain.on_Error(Sender: TObject; AValue: Integer);
begin

end;

end.
