unit _fmMain;

interface

uses
  WaitFreeValues, SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FWaitFreeString : TWaitFreeString;
    FSimpleThread : TSimpleThread;
    procedure on_Execute(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWaitFreeString := TWaitFreeString.Create(32);
  FSimpleThread := TSimpleThread.Create(on_Execute);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FSimpleThread.Terminate;
end;

function GetRandomString:string;
const
  Alphas : string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  Loop: Integer;
begin
  Result := '';
  for Loop := 1 to 32 do Result := Result + Alphas[Random(Length(Alphas) + 1)];
end;

procedure TfmMain.on_Execute(Sender: TObject);
begin
  while not FSimpleThread.Terminated do begin
    FWaitFreeString.Value := GetRandomString;

    Sleep(1);
  end;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  Caption := FWaitFreeString.Value;
end;

end.
