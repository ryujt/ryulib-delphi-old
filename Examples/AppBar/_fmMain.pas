unit _fmMain;

interface

uses
  AppBar,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, _frCommon, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btClose: TButton;
    btRight: TButton;
    btBottom: TButton;
    frCommon: TfrCommon;
    procedure btCloseClick(Sender: TObject);
    procedure btRightClick(Sender: TObject);
    procedure btBottomClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure do_WM_ShowAppBar(var Msg:TMessage); message WM_ShowAppBar;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  ShellAPI, _fmAppBarRight, _fmAppBarBottom;

procedure TfmMain.btBottomClick(Sender: TObject);
var
  fmAppBarBottom : TfmAppBarBottom ;
begin
  Self.Hide;
  fmAppBarBottom := TfmAppBarBottom.Create(Self);
  frCommon.Parent := fmAppBarBottom;
end;

procedure TfmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.btRightClick(Sender: TObject);
var
  fmAppBarRight : TfmAppBarRight;
begin
  Self.Hide;
  fmAppBarRight := TfmAppBarRight.Create(nil);
  frCommon.Parent := fmAppBarRight;
end;

procedure TfmMain.do_WM_ShowAppBar(var Msg: TMessage);
begin
  case Msg.WParam of
    ABE_RIGHT: btRightClick(nil);
    ABE_BOTTOM: btBottomClick(nil);
  end;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  frCommon.Parent := Self;
end;

end.
