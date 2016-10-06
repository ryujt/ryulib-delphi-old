unit _fmMain;

interface

uses
  WinVersion,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  TypInfo;

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := GetEnumName(TypeInfo(TWinVersion), Integer(GetWinVersion));
end;

end.
