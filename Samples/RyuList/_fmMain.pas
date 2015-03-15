unit _fmMain;

interface

uses
  RyuList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    ListBox: TListBox;
    Panel1: TPanel;
    btAdd: TButton;
    btFind: TButton;
    edFind: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btFindClick(Sender: TObject);
  private
    FList : TRyuList;
    function get_RandomString:string;
  public
  end;

var
  fmMain: TfmMain;

implementation

function CompareLogic(Item1, Item2: Pointer): Integer;
var
  pItem1 : ^string absolute Item1;
  pItem2 : ^string absolute Item2;
begin
  if pItem1^ > pItem2^ then Result := 1
  else if pItem1^ < pItem2^ then Result := -1
  else Result := 0;
end;

{$R *.dfm}

{ TfmMain }

procedure TfmMain.btAddClick(Sender: TObject);
var
  Loop : Integer;
  pItem : ^string;
begin
  New(pItem);
  pItem^ := get_RandomString;
  FList.InsertByOrder(pItem, CompareLogic);

  ListBox.Clear;
  for Loop := 0 to FList.Count-1 do begin
    pItem := FList.Items[Loop];
    ListBox.Items.Add(pItem^);
  end;
end;

procedure TfmMain.btFindClick(Sender: TObject);
var
  pItem : ^string;
  iIndex : Integer;
begin
  New(pItem);
  try
    pItem^ := edFind.Text;
    iIndex := FList.BinarySearch(pItem, CompareLogic);
    Caption := IntToStr(iIndex);
  finally
    Dispose(pItem);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Randomize;

  FList := TRyuList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

function TfmMain.get_RandomString: string;
const
  Samples : string = '01234567890ABCDEF';
var
  Loop : Integer;
begin
  Result := '';
  for Loop := 1 to 4 do Result := Result + Samples[Random(Length(Samples))+1];
end;

end.
