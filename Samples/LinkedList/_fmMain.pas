unit _fmMain;

interface

uses
  LinkedList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btDeleteFirstNodeOfList: TButton;
    btDeleteFirstNodeOfLinkedList: TButton;
    btDeleteLastNodeOfList: TButton;
    btDeleteLastNodeOfLinkedList: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btDeleteFirstNodeOfListClick(Sender: TObject);
    procedure btDeleteFirstNodeOfLinkedListClick(Sender: TObject);
    procedure btDeleteLastNodeOfListClick(Sender: TObject);
    procedure btDeleteLastNodeOfLinkedListClick(Sender: TObject);
  private
    FList : TList;
    FLinkedList : TLinkedList;
  public
  end;

var
  fmMain: TfmMain;

implementation

const
  _RepeatCount = 100;
  _ListLength = 16 * 1024;

{$R *.dfm}

procedure TfmMain.btDeleteFirstNodeOfListClick(Sender: TObject);
var
  Tick : Cardinal;
  Loop1, Loop2 : integer;
begin
  Tick := GetTickCount;

  for Loop1 := 0 to _RepeatCount do begin
    for Loop2 := 1 to _ListLength do FList.Add(Pointer(Loop2));
    while FList.Count > 0 do FList.Delete(0);
  end;

  moMsg.Lines.Add(Format('Delete First Node Of List: %d', [GetTickCount-Tick]));
end;

procedure TfmMain.btDeleteFirstNodeOfLinkedListClick(Sender: TObject);
var
  Tick : Cardinal;
  Item : TLinkedListNode;
  Loop1, Loop2 : integer;
begin
  Tick := GetTickCount;

  for Loop1 := 0 to _RepeatCount do begin
    for Loop2 := 1 to _ListLength do begin
      Item := TLinkedListNode.Create;
      Item.Data := Pointer(Loop2);
      FLinkedList.Add(Item);
    end;

    while not FLinkedList.IsEmpty do FLinkedList.Remove(FLinkedList.GetFirstNode);
  end;

  moMsg.Lines.Add(Format('Delete First Node Of LinkedList: %d', [GetTickCount-Tick]));
end;

procedure TfmMain.btDeleteLastNodeOfListClick(Sender: TObject);
var
  Tick : Cardinal;
  Loop1, Loop2 : integer;
begin
  Tick := GetTickCount;

  for Loop1 := 0 to _RepeatCount do begin
    for Loop2 := 1 to _ListLength do begin
      FList.Add(Pointer(Loop2));
    end;

    while FList.Count > 0 do FList.Delete(FList.Count-1);
  end;

  moMsg.Lines.Add(Format('Delete Last Node Of List: %d', [GetTickCount-Tick]));
end;

procedure TfmMain.btDeleteLastNodeOfLinkedListClick(Sender: TObject);
var
  Tick : Cardinal;
  Item : TLinkedListNode;
  Loop1, Loop2 : integer;
begin
  Tick := GetTickCount;

  for Loop1 := 0 to _RepeatCount do begin
    for Loop2 := 1 to _ListLength do begin
      Item := TLinkedListNode.Create;
      Item.Data := Pointer(Loop2);
      FLinkedList.Add(Item);
    end;

    while not FLinkedList.IsEmpty do FLinkedList.Remove(FLinkedList.GetLastNode);
  end;

  moMsg.Lines.Add(Format('Delete Last Node Of LinkedList: %d', [GetTickCount-Tick]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
  FLinkedList := TLinkedList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FList);
  FreeAndNil(FLinkedList);
end;

end.
