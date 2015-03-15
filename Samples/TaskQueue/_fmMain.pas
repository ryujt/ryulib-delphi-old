unit _fmMain;

interface

uses
  TaskQueue,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FTaskQueue : TTaskQueue;
    procedure on_Task(Sender:TObject; AData:pointer; ASize:integer; ATag:pointer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FTaskQueue.Add( Pointer(1) );
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  FTaskQueue.Add( Pointer(2) );
end;

procedure TfmMain.Button3Click(Sender: TObject);
begin
  FTaskQueue.Add( Pointer(3) );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTaskQueue := TTaskQueue.Create;
  FTaskQueue.OnTask := on_Task;
end;

procedure TfmMain.on_Task(Sender: TObject; AData: pointer; ASize: integer;
  ATag: pointer);
begin
  moMsg.Lines.Add( IntToStr(Integer(ATag)) );
end;

end.
