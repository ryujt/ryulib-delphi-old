unit _fmMain;

interface

uses
  BTree,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btAdd: TButton;
    moMsg: TMemo;
    btSearch: TButton;
    edSearch: TEdit;
    btList: TButton;
    btBulkAdd: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btListClick(Sender: TObject);
    procedure btBulkAddClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    FBTree : TBTree;
  public
  end;

var
  fmMain: TfmMain;

implementation

type
  TItem = class (TBTreeItem)
  private
  protected
    function Compare(const ACompareTo:TBTreeItem):integer; override;
    procedure Copy(const ACopyTo:TBTreeItem); override;
  public
    UserData : integer;
  end;

{ TBTreeItem }

function TItem.Compare(const ACompareTo: TBTreeItem): integer;
begin
  if TItem(ACompareTo).UserData < UserData then Result := -1
  else if TItem(ACompareTo).UserData > UserData then Result := 1
  else Result := 0;
end;

procedure TItem.Copy(const ACopyTo: TBTreeItem);
begin
  TItem(ACopyTo).UserData := UserData;
end;

{$R *.dfm}

procedure TfmMain.btAddClick(Sender: TObject);
var
  Item, ResultItem : TItem;
  Data : integer;
begin
  Data := Random(10);
  moMsg.Lines.Add(IntToStr(Data));

  Item := TItem.Create;
  Item.UserData := Data;

  ResultItem := TItem(FBTree.Add(Item));

  if ResultItem = nil then begin
    Caption := 'Added';
  end else begin
    Caption := 'Duplicated: ' + IntToStr(ResultItem.UserData);
    Item.Free;
  end;
end;

procedure TfmMain.btClearClick(Sender: TObject);
var
  Tick : Cardinal;
begin
  Tick := GetTickCount;

  FBTree.Clear;
//  while not FBTree.IsEmpty do FBTree.Remove(FBTree.Root);

  moMsg.Lines.Add(Format('btClearClick: %d', [GetTickCount - Tick]));
end;

procedure TfmMain.btListClick(Sender: TObject);
begin
  FBTree.Iterate(
    procedure (ABTreeItem:TBTreeItem; var ANeedStop:boolean) begin
        fmMain.moMsg.Lines.Add(IntToStr(TItem(ABTreeItem).UserData));
    end
  );
end;

procedure TfmMain.btSearchClick(Sender: TObject);
var
  Item, SearchResult : TItem;
begin
  Item := TItem.Create;
  Item.UserData := StrToInt(edSearch.Text);

  if FBTree.Search(Item, TBTreeItem(SearchResult)) then moMsg.Lines.Add(Format('%d is found.', [SearchResult.UserData]))
  else moMsg.Lines.Add(Format('%d is not found.', [Item.UserData]));

  Item.Free;
end;

procedure TfmMain.btBulkAddClick(Sender: TObject);
var
  Item : TItem;
  Loop: Integer;
  Tick : Cardinal;
begin
  Tick := GetTickCount;

  for Loop := 1 to 1024 * 1024 do begin
    Item := TItem.Create;
    Item.UserData := Random($FFFFFFFF);
    FBTree.Add(Item);
  end;

  moMsg.Lines.Add(Format('btBulkAddClick: %d', [GetTickCount - Tick]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBTree := TBTree.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBTree);
end;

end.
