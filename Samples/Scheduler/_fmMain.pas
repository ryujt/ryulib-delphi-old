unit _fmMain;

interface

uses
  Scheduler,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btStart: TButton;
    btStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btStopClick(Sender: TObject);
  private
    FScheduler : TScheduler;
    procedure on_Timer(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FScheduler.Start;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FScheduler.Stop;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FScheduler.Clear;
  FScheduler.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScheduler := TScheduler.Create;
  FScheduler.OnTimer := on_Timer;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScheduler);
end;

procedure TfmMain.on_Timer(Sender: TObject);
begin
  if not FScheduler.IsStarted then Exit;

  Tag := Tag + 1;
  Caption := IntToStr(Tag);
end;

end.
