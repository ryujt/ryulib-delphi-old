unit _fmMain;

interface

uses
  RWLock,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FRWLock : TRWLock;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FRWLock := TRWLock.Create;
end;

end.
