unit _fmMain;

interface

uses
  ServiceCtrl,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btSrart: TButton;
    btStop: TButton;
    btStatus: TButton;
    procedure btSrartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

const
  ServiceName : string = 'Service Name';

{$R *.dfm}

procedure TfmMain.btSrartClick(Sender: TObject);
begin
  ServiceStart(ServiceName);
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  ServiceStop(ServiceName);
end;

procedure TfmMain.btStatusClick(Sender: TObject);
begin
  Caption := IntToStr(ServiceGetStatus(ServiceName));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := BoolToStr(IsServiceInstalled(ServiceName), true);
end;

end.
