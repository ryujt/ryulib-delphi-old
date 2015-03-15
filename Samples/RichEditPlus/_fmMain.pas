unit _fmMain;

interface

uses
  RichEditPlus,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Samples.Spin;

const
  WM_Lock = WM_USER + 1;
  WM_Unlock = WM_USER + 2;

type
  TRichEdit = class(TRichEditPlus);

  TfmMain = class(TForm)
    RichEdit1: TRichEdit;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure WMLock(var Message: TWMVScroll); message WM_Lock;
    procedure WMUnlock(var Message: TWMVScroll); message WM_Unlock;
    procedure on_Scroll(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  Strg;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  RichEdit1.HideSelection := true;
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  RichEdit1.HideSelection := false;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  RichEdit1.OnScroll := on_Scroll;
end;

procedure TfmMain.on_Scroll(Sender: TObject);
const
  SCROOL_MARGIN = 10;
var
  iPos, iMin, iMax : integer;
begin
  iPos := GetScrollPos(RichEdit1.Handle, SB_VERT);
  GetScrollRange(RichEdit1.Handle, SB_VERT, iMin, iMax);

  if (iPos + RichEdit1.Height + SCROOL_MARGIN) > iMax then begin
    Caption := 'Unlock';
    PostMessage(Handle, WM_Unlock, 0, 0);
  end else begin
    Caption := 'Lock';
    PostMessage(Handle, WM_Lock, 0, 0);
  end;
end;

procedure TfmMain.Timer1Timer(Sender: TObject);
begin
  RichEdit1.Lines.Add(RandomStr);
end;

procedure TfmMain.WMLock(var Message: TWMVScroll);
begin
  Timer1.Enabled := false;
  RichEdit1.HideSelection := true;
end;

procedure TfmMain.WMUnlock(var Message: TWMVScroll);
begin
  RichEdit1.HideSelection := false;
  Timer1.Enabled := true;
end;

end.
