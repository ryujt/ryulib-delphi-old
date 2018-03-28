unit formPro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ieview, imageenview, rkVistaProBar;

type
  TfrmPro = class(TForm)
    ieThumb: TImageEnView;
    proDlg: TVistaProBar;
    btnClose: TButton;
    procedure btnAbortClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    Abort: Boolean;
  end;

var
  frmPro: TfrmPro;

implementation

{$R *.dfm}

procedure TfrmPro.btnAbortClick(Sender: TObject);
begin
  Abort:= True;
end;

procedure TfrmPro.FormShow(Sender: TObject);
begin
  Abort:= False;
end;

procedure TfrmPro.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Abort:= True;
end;

end.
