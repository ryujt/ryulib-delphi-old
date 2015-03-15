unit _fmMain;

interface

uses
  Bandwidth,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FOldTick : Cardinal;
    FSize : int64;
    FBandwidth : TBandwidth;
  public
  end;

var
  fmMain: TfmMain;

implementation

const
  _Bytes = 256;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  iBytes : integer;
begin
  FBandwidth.Clear;
  repeat
    iBytes := Round(_Bytes);

    if not FBandwidth.IsBusy then begin
      FSize := FSize + iBytes;
      FBandwidth.AddBytes(iBytes);
    end;

    Caption := IntToStr(FBandwidth.Size div 1024);

    Application.ProcessMessages;

    Sleep(1);
  until false;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSize := 0;
  FOldTick := GetTickCount;

  FBandwidth := TBandwidth.Create;
  FBandwidth.Bandwidth := 1024 * 128;  // 1 Mbps = 128 KB
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBandwidth);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  iMod, iBytes : integer;
begin
  iBytes := Round(_Bytes);

  if not FBandwidth.IsBusy then begin
    FSize := FSize + iBytes;
    FBandwidth.AddBytes(iBytes);
  end;

  iMod := ((GetTickCount - FOldTick) div 1000);
  if iMod > 0 then Caption := IntToStr(FSize div (iMod * 1024));
end;

end.
