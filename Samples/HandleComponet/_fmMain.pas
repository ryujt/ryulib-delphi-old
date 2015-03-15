unit _fmMain;

interface

uses
  HandleComponent, ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  WM_Test = WM_USER + 2;

type
  TTest = class (THandleComponent)
  private
    procedure on_Test(var msg:TMessage); message WM_Test;
  public
  published
    procedure rp_Test(ValueList:TValueList);
  end;

  TfmMain = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FHandleComponent : TTest;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  SendMessage(FHandleComponent.Handle, WM_Test, 0, 0);
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  FHandleComponent.Packet.Clear;
  FHandleComponent.Packet.Values['Code'] := 'Test';
  FHandleComponent.Packet.Values['Param1'] := 'Param1';
  FHandleComponent.Packet.Integers['Param2'] := 2;
  FHandleComponent.SendPacket(FHandleComponent.Handle);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHandleComponent := TTest.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FHandleComponent.Free;
end;

{ TTest }

procedure TTest.on_Test(var msg: TMessage);
begin
  fmMain.Memo1.Lines.Add('WM_Test');
  fmMain.Memo1.Lines.Add('');
end;

procedure TTest.rp_Test(ValueList: TValueList);
begin
  fmMain.Memo1.Lines.Add(ValueList.Text);
  fmMain.Memo1.Lines.Add('');
end;

end.
