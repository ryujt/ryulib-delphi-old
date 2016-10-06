unit _fmMain;

interface

uses
  WindowFrame,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FWindowFrame : TfmWindowFrame;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWindowFrame := TfmWindowFrame.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWindowFrame);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  hActive : THandle;
  Title : string;
  Len : integer;
begin
  FWindowFrame.TargetWindow := GetForegroundWindow;

  hActive := GetForegroundWindow;
  Len := GetWindowTextLength(hActive) + 1;
  SetLength(Title, Len);
  GetWindowText(hActive, PChar(Title), Len);
  Caption := Title;
end;

end.

