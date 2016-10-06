unit _fmMain;

interface

uses
  Magnetic,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TMagneticMainForm)
    btShow: TButton;
    btHide: TButton;
    procedure btShowClick(Sender: TObject);
    procedure btHideClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  _fmSub;

procedure TfmMain.btHideClick(Sender: TObject);
begin
  fmSub.Close;
end;

procedure TfmMain.btShowClick(Sender: TObject);
begin
  fmSub.Show;
end;

end.
