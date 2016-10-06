unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StringListTour;

type
  TfmMain = class(TForm)
    btRun: TButton;
    moMsg: TMemo;
    procedure btRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FList : TStringList;
    procedure on_Error(Sender:TObject; var ErrorAction:integer);
    procedure on_DropPoint(Sender:TObject; Text:string; var Error:boolean);
    procedure on_Cancel(Sender:TObject);
    procedure on_Complete(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btRunClick(Sender: TObject);
var
  FStringListTour : TStringListTour;
begin
  Tag := 0;

  FStringListTour := TStringListTour.Create(FList);
  try
    FStringListTour.OnDropPoint := on_DropPoint;
    FStringListTour.OnError := on_Error;
    FStringListTour.OnCancel := on_Cancel;
    FStringListTour.OnComplete := on_Complete;
    
    FStringListTour.Start;
  finally
    FStringListTour.Free;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FList := TStringList.Create;

  FList.Add('1');
  FList.Add('2');
  FList.Add('3');
  FList.Add('4');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TfmMain.on_Cancel(Sender: TObject);
begin
  moMsg.Clear;
end;

procedure TfmMain.on_Complete(Sender: TObject);
begin
  moMsg.Lines.Add('정상 완료되었습니다.');
end;

procedure TfmMain.on_DropPoint(Sender: TObject; Text: string;
  var Error: boolean);
begin
  if Text = '2' then begin
    Error := Tag = 0;
    Tag := 1;
  end else
    Error := false;      

  if Error = false then moMsg.Lines.Add(Text);  
end;

procedure TfmMain.on_Error(Sender: TObject; var ErrorAction: integer);
begin
  ErrorAction := MessageDlg('에러발생!  다시 시도?', mtError, [mbYes, mbCancel, mbIgnore], 0);
end;

end.
