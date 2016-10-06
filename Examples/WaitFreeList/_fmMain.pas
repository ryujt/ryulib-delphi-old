unit _fmMain;

interface

uses
  WaitFreeList, ThreadRepeater, DynamicQueue,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FMemoryPool : TDynamicQueue;
    FList : TWaitFreeObjectList;
    Repeater1 : TThreadRepeater;
    Repeater2 : TThreadRepeater;
    Repeater3 : TThreadRepeater;
    Repeater4 : TThreadRepeater;
    procedure on_RepeatClear(Sender:TObject);
    procedure on_RepeatAdd(Sender:TObject);
    procedure on_RepeatRemove(Sender:TObject);
    procedure on_RepeatIterate(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

type
  TTestNode = class (TWaitFreeObject)
  private
  public
    Data : integer;
    constructor Create(AData:integer); reintroduce;
  end;

{ TTestNode }

constructor TTestNode.Create(AData: integer);
begin
  inherited Create;

  Data := AData;
end;

{$R *.dfm}

{ TfmMain }

procedure TfmMain.Button1Click(Sender: TObject);
begin
  Repeater1.Terminate;
  Repeater2.Terminate;
  Repeater3.Terminate;
  Repeater4.Terminate;
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  FList.Clear;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
//  FMemoryPool := TDynamicQueue.Create(true);
//
//  FList := TWaitFreeObjectList.Create;
//
//  Repeater1 := TThreadRepeater.Create(Self);
//  Repeater1.Execute(on_RepeatClear);
//
//  Repeater2 := TThreadRepeater.Create(Self);
//  Repeater2.Execute(on_RepeatAdd);
//
//  Repeater3 := TThreadRepeater.Create(Self);
//  Repeater3.Execute(on_RepeatRemove);
//
//  Repeater4 := TThreadRepeater.Create(Self);
//  Repeater4.Execute(on_RepeatIterate);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
//  Repeater1.Stop;
//  Repeater2.Stop;
//  Repeater3.Stop;
//  Repeater4.Stop;
end;

procedure TfmMain.on_RepeatAdd(Sender: TObject);
var
  Node : TTestNode;
begin
  if FMemoryPool.Count < 1024 then begin
    Node := TTestNode.Create(Random($FFFF))
  end else begin
    if not FMemoryPool.Pop(Pointer(Node)) then begin
      Node := TTestNode.Create(Random($FFFF));
    end;
  end;

  FList.Add(Node);

  Sleep(10);
end;

procedure TfmMain.on_RepeatClear(Sender: TObject);
begin
  FList.Clear;
  Sleep(5);
end;

procedure TfmMain.on_RepeatIterate(Sender: TObject);
begin
  FList.Iterate(
    procedure (WaitFreeObject:IWaitFreeObject) begin
      Tag := Tag + TTestNode(WaitFreeObject).Data;
    end
  );
  Sleep(50);
end;

procedure TfmMain.on_RepeatRemove(Sender: TObject);
var
  WaitFreeObject : IWaitFreeObject;
begin
  WaitFreeObject := FList.GetFirstObject;
  if WaitFreeObject <> nil then FList.Remove(WaitFreeObject);
  Sleep(500);
end;

end.
