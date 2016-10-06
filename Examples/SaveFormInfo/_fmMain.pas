unit _fmMain;

interface

uses
  SaveFormInfo,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSaveFormInfo : TSaveFormInfo;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSaveFormInfo := TSaveFormInfo.Create(Self, 0, 0, 0, 0);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSaveFormInfo);
end;

end.
