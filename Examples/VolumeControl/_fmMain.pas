unit _fmMain;

interface

uses
  VolumeControl,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Label1: TLabel;
    sbMic: TScrollBar;
    Label2: TLabel;
    sbSpeaker: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure sbMicChange(Sender: TObject);
    procedure sbSpeakerChange(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  sbMic.Position := Round(GetMicVolume * 100);
  sbSpeaker.Position := Round(GetSpeakerVolume * 100);
end;

procedure TfmMain.sbMicChange(Sender: TObject);
begin
  SetMicVolume(sbMic.Position / 100);
end;

procedure TfmMain.sbSpeakerChange(Sender: TObject);
begin
  SetSpeakerVolume(sbSpeaker.Position / 100);
end;

end.
