unit _fmMain;

interface

uses
  PreventRecording,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPreventRecording : TPreventRecording;
    procedure on_FindRecorder(Sender:TObject; ARecorderList:TStringList);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FPreventRecording := TPreventRecording.Create;
  FPreventRecording.OnFindRecorder := on_FindRecorder;

  FPreventRecording.AddRecorder('', 'TfmMain');

  FPreventRecording.Start;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPreventRecording);
end;

procedure KillProcess(ProcessID:integer);
var
  hProcess : THandle;
  TermSucc : BOOL;
begin
  // 존재하는 process object의 handle을 return한다
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, TRUE, ProcessID);
  if hProcess = NULL then raise Exception.Create('KillProcess: OpenProcess error !');

  // 명시한 process를 강제 종료시킨다
  TermSucc := TerminateProcess(hProcess, 0);
  if TermSucc = false then Exception.Create('KillProcess: TerminateProcess error !');
end;

procedure TfmMain.on_FindRecorder(Sender: TObject; ARecorderList: TStringList);
begin
  moMsg.Text := ARecorderList.Text;

//  try
//  KillProcess( StrToInt(ARecorderList.Names[0]) );
//  except
//    // TODO:
//  end;
end;

end.
