unit formPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ieview, imageenview, ExtCtrls, ImageEnProc, hyiedefs,
  Menus,
  // for Audio Playback

  //cbAudioPlay,

  rkIntegerList,
  // Setup dialog
  StdCtrls;

const
  CM_LOAD = WM_USER + 2100; // Custom Message...
  CM_DIALOG = WM_USER + 2101;

type
  TfrmView = class(TForm)
    pnlPreview: TPanel;
    iePreview: TImageEnView;
    ieZoom: TImageEnView;
    popView: TPopupMenu;
    meuFull: TMenuItem;
    labInfo: TLabel;
    N1: TMenuItem;
    popZoom1: TMenuItem;
    popZoom2: TMenuItem;
    popZoom3: TMenuItem;
    popZoom: TMenuItem;
    procedure ToggleView;
    procedure iePreviewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function DynBackCol(Dominante: Boolean): TColor;
    //procedure timerSlideTimer(Sender: TObject);
    procedure popFullScreenClick(Sender: TObject);
    procedure iePreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure iePreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure iePreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure popZoomClick(Sender: TObject);
    procedure popViewPopup(Sender: TObject);
    procedure iePreviewSpecialKey(Sender: TObject; CharCode: Word;
      Shift: TShiftState; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    ImgList: TIntList;
    imageIdx: integer;
    filter: boolean;
    transition: boolean;
    //multiple: boolean;
    first: boolean;
    procedure DoLoadImage(var Message: TMessage); message CM_LOAD;
    procedure DoDialog(var Message: TMessage); message CM_DIALOG;
    //procedure NewLoadImage(fName: string);
    { Private declarations }
  public
    ShowProgress: boolean;
    Multiple: boolean;
    SlideShow: boolean;
    Dialog: boolean;
    UseTransitions: boolean;
    Files: TStringList;
    AudioFile: string;
    ColorMode: boolean;
    ColorDom: boolean;
    Zoom: Byte;
    { Public declarations }
  end;

var
  frmView: TfrmView;
  GotZoom: Boolean;
  Zooming: Boolean;
  lx, ly: Integer;
  timeCount: Integer;

implementation

uses
  main;

{$R *.dfm}

procedure TfrmView.ToggleView;
var
  bool: Boolean;
begin
  bool := iePreview.ZoomFilter <> rfNone;
  if bool then
    iePreview.ZoomFilter := rfNone;
  if iePreview.Tag = 0 then
  begin
    pnlPreview.BevelOuter := bvNone;
    if ColorMode then
      iePreview.Background := DynBackCol(ColorDom)
    else
      iePreview.Background := clBlack;
    frmView.WindowState := wsMaximized;
    iePreview.Tag := 1
  end
  else
  begin
    pnlPreview.BevelOuter := bvRaised;
    if ColorMode then
      iePreview.Background := DynBackCol(ColorDom)
    else
      iePreview.Background := clBtnFace;
    frmView.WindowState := wsNormal;
    iePreview.Tag := 0;
  end;
  if bool then
    iePreview.ZoomFilter := rfFastLinear;
end;

procedure TfrmView.iePreviewDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmView.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ImgList := TIntList.Create;
  first := true;
  imageIdx := 0;
  files := TStringList.Create;
  transition := true;

  frmMain.GetSelection(files);

  for i := 0 to files.Count - 1 do
    ImgList.Add(i);
  multiple := files.Count > 1;
  filter := true;
  if filter then
    iePreview.ZoomFilter := rfFastLinear
  else
    iePreview.ZoomFilter := rfNone;
  ieZoom.SetExternalBitmap(iePreview.IEBitmap);
end;

procedure TfrmView.FormDestroy(Sender: TObject);
begin
  ieZoom.SetExternalBitmap(nil);
  files.Free;
  ImgList.Free;
end;

procedure TfrmView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrOk;
end;

procedure TfrmView.DoDialog;
begin
end;

procedure TfrmView.FormShow(Sender: TObject);
begin
  if dialog then
    Postmessage(handle, CM_DIALOG, 0, 0)
  else
  begin
    if first then
      Postmessage(handle, CM_LOAD, 0, 0);
  end;
end;

function TfrmView.DynBackCol(Dominante: Boolean): TColor;
var
  c: TRGB;
  r, g, b: Cardinal;
  p, o, t, x, y: Integer;
begin
  if Dominante then
  begin
    iePreview.Proc.GetDominantColor(c);
    Result := c.r or (c.g shl 8) or (c.b shl 16);
  end
  else
  begin
    r := 0;
    g := 0;
    b := 0;
    p := Integer(iePreview.IEBitmap.Scanline[0]);
    o := Integer(iePreview.IEBitmap.Scanline[1]) - p;
    for y := 0 to iePreview.IEBitmap.Height - 1 do
    begin
      for x := 0 to iePreview.IEBitmap.Width - 1 do
      begin
        r := r + PRGB(p + x * 3).r;
        g := g + PRGB(p + x * 3).g;
        b := b + PRGB(p + x * 3).b;
      end;
      p := p + o;
    end;
    t := iePreview.IEBitmap.Width * iePreview.IEBitmap.Height;
    if r > 0 then
      r := Trunc(r / t);
    if g > 0 then
      g := Trunc(g / t);
    if b > 0 then
      b := Trunc(b / t);
    Result := r or (g shl 8) or (b shl 16);
  end;
end;

{
procedure TfrmView.timerSlideTimer(Sender: TObject);
var
  i: integer;
begin
  timerSlide.Enabled := false;
  if imageIdx < files.Count - 1 then
    Inc(imageIdx)
  else
    imageIdx := 0;
  iePreview.PrepareTransition;
  iePreview.IO.LoadFromFile(files[ImgList[imageIdx]]);
  if ColorMode then
    iePreview.Background := DynBackCol(ColorDom);
  i := random(162) + 1;
  if UseTransitions then
    iePreview.RunTransition(TIETransitionType(i), 500)
  else
    iePreview.RunTransition(TIETransitionType(0), 0);
  timerSlide.Enabled := true;
  GotZoom := False;
  Zooming := False;
end;
}

procedure TfrmView.DoLoadImage;
var
  i: Integer;
begin
  //timerSlide.Enabled := false;
  Application.ProcessMessages;
  iePreview.PrepareTransition;
  iePreview.IO.LoadFromFile(files[ImgList[imageIdx]]);
  iePreview.OnProgress := nil;
  if ColorMode then
    iePreview.Background := DynBackCol(ColorDom);
  i := random(162) + 1;
  if UseTransitions then
    iePreview.RunTransition(TIETransitionType(i), 250)
  else
    iePreview.RunTransition(TIETransitionType(0), 0);
  iePreview.SetFocus;
  //timerSlide.Enabled := SlideShow;
end;

{
procedure TfrmView.NewLoadImage(fName: string);
begin
  timerSlide.Enabled := false;
  Application.ProcessMessages;
  iePreview.PrepareTransition;
  iePreview.IO.LoadFromFile(fName);
  iePreview.OnProgress := nil;
  if ColorMode then
    iePreview.Background := DynBackCol(ColorDom);
  iePreview.RunTransition(TIETransitionType(0), 0);
  timerSlide.Enabled := SlideShow;
end;
}

procedure TfrmView.popFullScreenClick(Sender: TObject);
begin
  ToggleView;
end;

procedure TfrmView.popViewPopup(Sender: TObject);
begin
  popZoom.Checked:= Zoom = 0;
  popZoom1.Checked:= Zoom = 1;
  popZoom2.Checked:= Zoom = 2;
  popZoom3.Checked:= Zoom = 3;
end;

procedure TfrmView.popZoomClick(Sender: TObject);
begin
  Zoom:= (Sender as TMenuItem).Tag;
end;

procedure TfrmView.iePreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  w2, h2, bx, by: Integer;
  zz: Single;
begin
  if (Button = mbRight) or (ssRight in Shift) then
    Exit;
  zz:= 0;
  case Zoom of
    0: zz:= 1;
    1: zz:= 1.5;
    2: zz:= 2;
    3: zz:= 3;
  end;
  ieZoom.LockPaint;
  ieZoom.Zoom := zz * 100;
  bx := iePreview.XScr2Bmp(x);
  by := iePreview.YScr2Bmp(y);
  w2 := (ieZoom.ClientWidth div 2);
  h2 := (ieZoom.ClientHeight div 2);
  ieZoom.SetViewXY(trunc(bx * zz - w2), trunc(by * zz - h2));
  ieZoom.UnLockPaint;
  ieZoom.BringToFront;
  GotZoom := True;
  LX := BX;
  LY := BY;
  ieZoom.Cursor := 1099;
  ieZoom.SetFocus;
  Zooming := True;
end;

procedure TfrmView.iePreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  bx, by, dx, dy: Integer;
begin
  if Zooming then
  begin
    bx := iePreview.XScr2Bmp(x);
    by := iePreview.YScr2Bmp(y);
    dX := bx - lx;
    dY := by - ly;
    ieZoom.LockPaint;
    ieZoom.ViewX := ieZoom.ViewX + dx;
    ieZoom.ViewY := ieZoom.ViewY + dy;
    ieZoom.UnLockPaint;
    lx := bx;
    ly := by;
  end;
end;

procedure TfrmView.iePreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lx := -1;
  ly := -1;
  Zooming := False;
  iePreview.BringToFront;
end;

procedure TfrmView.iePreviewSpecialKey(Sender: TObject; CharCode: Word;
  Shift: TShiftState; var Handled: Boolean);
var
  i: integer;
begin
  first := false;
  i := imageIdx;
  if CharCode = VK_F7 then
    transition := not transition;
  if CharCode = VK_F8 then
  begin
    filter := not filter;
    if filter then
      iePreview.ZoomFilter := rfFastLinear
    else
      iePreview.ZoomFilter := rfNone;
  end;
  if CharCode = VK_F9 then
  begin
    if iePreview.AutoFit then
    begin
      iePreview.AutoFit := false;
      iePreview.Zoom := 100;
    end
    else
    begin
      iePreview.AutoFit := true;
      iePreview.Fit;
    end;
  end;
  if CharCode = VK_Left then
    if imageIdx > 0 then
      dec(imageIdx);
  if CharCode = VK_Right then
    if imageIdx < files.Count - 1 then
      inc(imageIdx);
  if imageIdx <> i then
  begin
    if transition then
    begin
      iePreview.PrepareTransition;
      iePreview.IO.LoadFromFile(files[imageIdx]);
      if ColorMode then
        iePreview.Background := DynBackCol(ColorDom);
      iePreview.RunTransition(iettCrossDissolve, 250)
    end
    else
    begin
      if ColorMode then
        iePreview.Background := DynBackCol(ColorDom);
      iePreview.IO.LoadFromFile(files[imageIdx]);
    end;
  end;
end;

end.

