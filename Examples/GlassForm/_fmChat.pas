unit _fmChat;

interface

uses
  ValueList, GlassForm, ChatCanvas,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

type
  TfmChat = class(TGlassForm)
    Image: TImage;
    imgClose: TImage;
    edMsg: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgCloseClick(Sender: TObject);
    procedure edMsgKeyPress(Sender: TObject; var Key: Char);
  private
    FChatCanvas : TChatCanvas;
  public
  end;

var
  fmChat: TfmChat;

implementation

{$R *.dfm}

procedure TfmChat.edMsgKeyPress(Sender: TObject; var Key: Char);
begin
  if Trim(edMsg.Text) = '' then Exit;

  if Key = #13 then begin
    Key := #0;
    FChatCanvas.AddText(edMsg.Text);
    edMsg.Text := '';
  end;
end;

procedure TfmChat.FormCreate(Sender: TObject);
begin
  Bitmap := Image.Picture.Bitmap;
  TransparentColor := Bitmap.Canvas.Pixels[Bitmap.Width div 2, Bitmap.Height div 2];
  Crystalize;

  FChatCanvas := TChatCanvas.Create(Self);
  FChatCanvas.Left := 5;
  FChatCanvas.Font.Color := clBlack;
  FChatCanvas.Font.Name := '±¼¸²Ã¼';
  FChatCanvas.Top  := TopMargine + 5;
  FChatCanvas.Width  := Width  - FChatCanvas.Left*2;
  FChatCanvas.Height := Height - FChatCanvas.Top - 32;
  FBackGround.Show;
end;

procedure TfmChat.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FChatCanvas);
end;

procedure TfmChat.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, $F012, 0);
end;

procedure TfmChat.imgCloseClick(Sender: TObject);
begin
  Close;
end;

end.
