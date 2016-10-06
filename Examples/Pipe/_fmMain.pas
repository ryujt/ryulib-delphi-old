unit _fmMain;

interface

uses
  Pipes, ObserverList, ValueList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    edMsg: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FObserverList : TObserverList;
  private
    FPipeReceiver : TPipeReceiver;
    procedure on_ReceiveText(Sender:TObject; const AText:string);
  private
    FPipeSender : TPipeSender;
  public
  published
    procedure rp_Msg(AValueList:TValueList);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FPipeSender.SendText(edMsg.Text);
  edMsg.Text := '';
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FObserverList.Remove(Self);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FObserverList := TObserverList.Create(Self);
  FObserverList.Add(Self);

  FPipeReceiver := TPipeReceiver.Create('Ryu');
  FPipeReceiver.OnReceiveText := on_ReceiveText;

  FPipeSender := TPipeSender.Create('Ryu');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPipeReceiver);
  FreeAndNil(FObserverList);
end;

procedure TfmMain.on_ReceiveText(Sender: TObject; const AText: string);
begin
  FObserverList.AsyncBroadcast('Code=Msg<rYu>' + AText);
end;

procedure TfmMain.rp_Msg(AValueList: TValueList);
begin
  moMsg.Lines.Add(AValueList.Text);
end;

end.
