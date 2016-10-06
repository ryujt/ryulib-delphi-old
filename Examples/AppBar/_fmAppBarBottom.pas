unit _fmAppBarBottom;

interface

uses
  AppBar,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmAppBarBottom = class(TAppBar)
    Panel1: TPanel;
    btClose: TButton;
    Button1: TButton;
    procedure btCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TfmAppBarBottom.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmAppBarBottom.Button1Click(Sender: TObject);
begin
  Close;
  SendMessage(Application.MainForm.Handle, WM_ShowAppBar, ABE_RIGHT, 0);
end;

constructor TfmAppBarBottom.Create(AOwner: TComponent);
begin
  BarSize := 250;
  Edge := ABE_BOTTOM;

  inherited;
end;

procedure TfmAppBarBottom.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
