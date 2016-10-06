unit _fmMain;

interface

uses
  DebugTools, Worker,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    btStart: TButton;
    btStop: TButton;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
  private
    FWorker : TWorker;
    procedure on_Start(Sender:TObject);
    procedure on_Stop(Sender:TObject);
    procedure on_Data(Sender:TObject; AData:pointer; ASize:integer; ATag:pointer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FWorker.Start;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FWorker.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWorker := TWorker.Create(Self);
  FWorker.OnStart := on_Start;
  FWorker.OnStop := on_Stop;
  FWorker.OnData := on_Data;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorker);
end;

procedure TfmMain.on_Data(Sender: TObject; AData: pointer; ASize: integer;
  ATag: pointer);
begin
  Trace( 'TfmMain.on_Data' );
  Sleep(1000);
end;

procedure TfmMain.on_Start(Sender: TObject);
begin
  Trace( 'TfmMain.on_Start' );
  Timer.Enabled := true;
end;

procedure TfmMain.on_Stop(Sender: TObject);
begin
  Timer.Enabled := false;
  Trace( 'TfmMain.on_Stop' );
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  FWorker.Add( nil, 0 );
end;

end.
