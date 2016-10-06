unit _fmMain;

interface

uses
  AsyncTasks,
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

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  AsyncTask(
    // This will execute task asynchronously by new thread without blocking.
    procedure (AUserData:pointer)
    begin
      Sleep(1000 * 3);
    end,

    // This will execute by main thread after above code completed.
    procedure (AUserData:pointer)
    begin
      fmMain.Caption := 'Done';
    end
  );
end;

end.
