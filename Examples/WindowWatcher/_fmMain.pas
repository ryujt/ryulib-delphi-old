unit _fmMain;

interface

uses
  WindowWatcher,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    btStart: TButton;
    lbMsg: TLabel;
    btStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
  private
    FWindowWatcher : TWindowWatcher;
  public
    procedure on_Show(Sender:TObject);
    procedure on_Hide(Sender:TObject);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FWindowWatcher.Start(FindWindow(nil, '제목 없음 - 메모장'));
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FWindowWatcher.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FWindowWatcher := TWindowWatcher.Create(Self);
  FWindowWatcher.OnShow := on_Show;
  FWindowWatcher.OnHide := on_Hide;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FWindowWatcher.Stop;
  FreeAndNil(FWindowWatcher);
end;

procedure TfmMain.on_Hide(Sender: TObject);
begin
  lbMsg.Caption := 'Hide';
end;

procedure TfmMain.on_Show(Sender: TObject);
begin
  lbMsg.Caption := 'Show';
end;

end.
