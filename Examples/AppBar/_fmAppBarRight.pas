unit _fmAppBarRight;

interface

uses
  AppBar,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmAppBarRight = class(TAppBar)
    Panel1: TPanel;
    btClose: TButton;
    btBottom: TButton;
    procedure btCloseClick(Sender: TObject);
    procedure btBottomClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ShellAPI;

{$R *.dfm}

{ TfmAppBarRight }

procedure TfmAppBarRight.btBottomClick(Sender: TObject);
begin
  Close;
  SendMessage(Application.MainForm.Handle, WM_ShowAppBar, ABE_BOTTOM, 0);
end;

procedure TfmAppBarRight.btCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TfmAppBarRight.Create(AOwner: TComponent);
begin
  BarSize := 250;
  Edge := ABE_RIGHT;

  inherited;
end;

end.
