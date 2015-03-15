unit _fmMain;

interface

uses
  ThreadRepeater,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    lbCount: TLabel;
    btClose: TButton;
    Timer: TTimer;
    btRun1: TButton;
    btRun2: TButton;
    btRun3: TButton;
    btStop1: TButton;
    btStop2: TButton;
    btStop3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btRun1Click(Sender: TObject);
    procedure btRun2Click(Sender: TObject);
    procedure btRun3Click(Sender: TObject);
    procedure btStop1Click(Sender: TObject);
    procedure btStop2Click(Sender: TObject);
    procedure btStop3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThreadRepeater1 : TThreadRepeater;
    FThreadRepeater2 : TThreadRepeater;
    FThreadRepeater3 : TThreadRepeater;
    procedure do_Execute(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.btRun1Click(Sender: TObject);
begin
  FThreadRepeater1.Execute(do_Execute);
end;

procedure TfmMain.btRun2Click(Sender: TObject);
begin
  FThreadRepeater2.Execute(do_Execute);
end;

procedure TfmMain.btRun3Click(Sender: TObject);
begin
  FThreadRepeater3.Execute(do_Execute);
end;

procedure TfmMain.btStop1Click(Sender: TObject);
begin
  FThreadRepeater1.Stop;
end;

procedure TfmMain.btStop2Click(Sender: TObject);
begin
  FThreadRepeater2.Stop;
end;

procedure TfmMain.btStop3Click(Sender: TObject);
begin
  FThreadRepeater3.Stop;
end;

procedure TfmMain.do_Execute(Sender: TObject);
begin
  Tag := Tag + 1;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FThreadRepeater1 := TThreadRepeater.Create(Self);
  FThreadRepeater2 := TThreadRepeater.Create(Self);
  FThreadRepeater3 := TThreadRepeater.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThreadRepeater1);
  FreeAndNil(FThreadRepeater2);
  FreeAndNil(FThreadRepeater3);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  lbCount.Caption := Format('TThreadRepeater.Count: %d', [TThreadRepeater.Count]);
end;

end.
