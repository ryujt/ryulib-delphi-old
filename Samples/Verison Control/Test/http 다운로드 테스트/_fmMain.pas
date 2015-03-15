unit _fmMain;

interface

uses
  HttpDownload,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, ComCtrls;

type
  TfmMain = class(TForm)
    ProgressBar1: TProgressBar;
    btStart: TButton;
    edURL: TEdit;
    btStop: TButton;
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FHttpDownload : THttpDownload;
    procedure on_Progress(Sender:TObject; Percent:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FHttpDownload.URL := edURL.Text;
  FHttpDownload.FileName := './Data.zip';
  FHttpDownload.Start;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FHttpDownload.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHttpDownload := THttpDownload.Create(Self);
  FHttpDownload.OnProgress := on_Progress;
end;

procedure TfmMain.on_Progress(Sender: TObject; Percent: integer);
begin
  ProgressBar1.Position := Percent;
end;

end.
