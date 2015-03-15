unit _fmMain;

interface

uses
  LatencyBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    tmCreate: TTimer;
    btStartCreate: TButton;
    btStopCreate: TButton;
    Label1: TLabel;
    tmUse: TTimer;
    btStartUse: TButton;
    btStopUse: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btStartCreateClick(Sender: TObject);
    procedure btStopCreateClick(Sender: TObject);
    procedure tmCreateTimer(Sender: TObject);
    procedure btStartUseClick(Sender: TObject);
    procedure btStopUseClick(Sender: TObject);
    procedure tmUseTimer(Sender: TObject);
  private
    FLatencyBuffer : TLatencyBuffer;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartCreateClick(Sender: TObject);
begin
  // OutputDebugString에 의해서 내부 상태를 알 수 있다.
  tmCreate.Enabled := true;
end;

procedure TfmMain.btStartUseClick(Sender: TObject);
begin
  tmUse.Enabled := true;
end;

procedure TfmMain.btStopCreateClick(Sender: TObject);
begin
  tmCreate.Enabled := false;
end;

procedure TfmMain.btStopUseClick(Sender: TObject);
begin
  tmUse.Enabled := false;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FLatencyBuffer := TLatencyBuffer.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLatencyBuffer);
end;

procedure TfmMain.tmCreateTimer(Sender: TObject);
begin
  FLatencyBuffer.Add( nil, 0, 20 );
end;

procedure TfmMain.tmUseTimer(Sender: TObject);
var
  Data : pointer;
  Size : integer;
begin
  FLatencyBuffer.Get( Data, Size );
end;

end.
