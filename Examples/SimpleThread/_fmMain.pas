unit _fmMain;

interface

uses
  SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btRun: TButton;
    btStop: TButton;
    procedure btRunClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btRunClick(Sender: TObject);
begin
  TSimpleThread.Create(
    Self,
    procedure (Context:pointer) begin
      while TForm(Context).Tag < 10 do begin
        TForm(Context).Caption := IntToStr( TForm(Context).Tag );
        TForm(Context).Tag := TForm(Context).Tag + 1;

        Sleep(1000);
      end;
    end
  );
end;

end.
