unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SwitchButton, ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    SwitchButton1: TSwitchButton;
    Image1: TImage;
    Image2: TImage;
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Image1Click(Sender: TObject);
begin
  SwitchButton1.BitmapReference := TImage(Sender).Picture.Bitmap;
end;

end.
