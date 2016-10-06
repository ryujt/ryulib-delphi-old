unit _fmMain;

interface

uses
  ThreadRepeater, SyncObjs, SRWLock,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  _LoopCount = 1000;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    btCS_Single: TButton;
    btSL_Single: TButton;
    btCS_Multi: TButton;
    btSL_Multi: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCS_SingleClick(Sender: TObject);
    procedure btSL_SingleClick(Sender: TObject);
    procedure btCS_MultiClick(Sender: TObject);
    procedure btSL_MultiClick(Sender: TObject);
  private
    FTick : Cardinal;
    FCount : integer;
    FCS : TCriticalSection;
    FSL : TSRWLock;
    FRepeater1 : TThreadRepeater;
    FRepeater2 : TThreadRepeater;
    FRepeater3 : TThreadRepeater;
    procedure on_RepeatCS(Sender:TObject);
    procedure on_RepeatSL(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

var
  Temp : integer;

{$R *.dfm}

procedure TfmMain.btCS_MultiClick(Sender: TObject);
begin
  FCount := 0;
  FTick := GetTickCount;

  FRepeater1.Execute(on_RepeatCS);
  FRepeater2.Execute(on_RepeatCS);
  FRepeater3.Execute(on_RepeatCS);
end;

procedure TfmMain.btCS_SingleClick(Sender: TObject);
var
  Loop1, Loop2 : Integer;
begin
  FTick := GetTickCount;

  FCS.Enter;
  try
    for Loop1 := 1 to 10000 do
    for Loop2 := 1 to 10000 do begin
      Tag := Temp;
    end;
  finally
    FCS.Leave;
  end;

  moMsg.Lines.Add(Format('btCS_SingleClick - %d ms', [GetTickCount-FTick]));
end;

procedure TfmMain.btSL_MultiClick(Sender: TObject);
begin
  FCount := 0;
  FTick := GetTickCount;

  FRepeater1.Execute(on_RepeatSL);
  FRepeater2.Execute(on_RepeatSL);
  FRepeater3.Execute(on_RepeatSL);
end;

procedure TfmMain.btSL_SingleClick(Sender: TObject);
var
  Loop1, Loop2 : Integer;
begin
  FTick := GetTickCount;

  FSL.AcquireShared;
  try
    for Loop1 := 1 to 10000 do
    for Loop2 := 1 to 10000 do begin
      Tag := Temp;
    end;
  finally
    FSL.ReleaseShared;
  end;

  moMsg.Lines.Add(Format('btCS_SingleClick - %d ms', [GetTickCount-FTick]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCS := TCriticalSection.Create;
  FSL := TSRWLock.Create;
  FRepeater1 := TThreadRepeater.Create(Self);
  FRepeater2 := TThreadRepeater.Create(Self);
  FRepeater3 := TThreadRepeater.Create(Self);
end;

procedure TfmMain.on_RepeatCS(Sender: TObject);
begin
  FCS.Enter;
  try
    Tag := Temp;
    Sleep(1);
  finally
    FCS.Leave;
  end;

  if InterlockedIncrement(FCount) > _LoopCount then begin
    moMsg.Lines.Add(Format('on_RepeatCS - %d ms', [GetTickCount-FTick]));
    TThreadRepeater(Sender).Terminate;
  end;
end;

procedure TfmMain.on_RepeatSL(Sender: TObject);
begin
  FSL.AcquireShared;
  try
    Tag := Temp;
    Sleep(1);
  finally
    FSL.ReleaseShared;
  end;

  if InterlockedIncrement(FCount) > _LoopCount then begin
    moMsg.Lines.Add(Format('on_RepeatSL - %d ms', [GetTickCount-FTick]));
    TThreadRepeater(Sender).Terminate;
  end;
end;

end.
