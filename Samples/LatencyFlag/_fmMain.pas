unit _fmMain;

interface

uses
  LatencyFlag,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    btFalse: TButton;
    btTrue: TButton;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btFalseClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btTrueClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLatencyFlag : TLatencyFlag;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btFalseClick(Sender: TObject);
begin
  FLatencyFlag.Value := false;
end;

procedure TfmMain.btTrueClick(Sender: TObject);
begin
  FLatencyFlag.Value := true;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FLatencyFlag := TLatencyFlag.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  Timer.Enabled := false;
  FreeAndNil(FLatencyFlag);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  if FLatencyFlag.Value then Caption := 'true'
  else Caption := 'false';
end;

end.
