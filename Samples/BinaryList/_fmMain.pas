unit _fmMain;

interface

uses
  BinaryList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    ListBox: TListBox;
    Panel1: TPanel;
    btAdd: TButton;
    btTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    FList : TBinaryList;
    function get_RandomString:string;
    procedure add_Item;
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
  else begin
    Result := 0;
  end;
end;

{$R *.dfm}

{ TfmMain }

procedure TfmMain.add_Item;
var
  Loop : Integer;
  pItem : ^string;
begin
  New(pItem);
  Caption := IntToHex(Integer(pItem), 4);
  pItem^ := get_RandomString;

//  OutputDebugString(PChar(IntToHex(Integer(pItem), 4)));

  if FList.InsertByOrder(pItem, CompareLogic, true) then begin
    if FList.Items[FList.LastIndex] <> pItem then raise Exception.Create('Error');
    ListBox.Items.Add(pItem^);
  end else begin
    Dispose(pItem);
  end;
end;

procedure TfmMain.btAddClick(Sender: TObject);
var
  pItem : ^string;
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do begin
    pItem := FList.Items[Loop];
    Dispose(pItem);
  end;
  FList.Clear;

  ListBox.Clear;

  ListBox.Enabled := false;
  for Loop := 1 to 10000 do add_Item;
  ListBox.Enabled := true;
end;

procedure TfmMain.btTestClick(Sender: TObject);
var
  Src : string;
  pItem : ^string;
  iIndex : Integer;
  Loop: Integer;
begin
  for Loop := 0 to ListBox.Count-1 do begin
    Src := ListBox.Items[Loop];
    iIndex := FList.Search(@Src, CompareLogic);

    pItem := FList.Items[iIndex];
    if Src <> pItem^ then raise Exception.Create('Error');

    Caption := Format('%d / %d', [Loop+1, FList.Count]);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  FList := TBinaryList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do FreeMem(FList.Items[Loop]);
  FList.Free;
end;

function TfmMain.get_RandomString: string;
const
  Samples : string = '01234567890ABCDEF';
var
  Loop : Integer;
begin
  Result := '';
  for Loop := 1 to 8 do Result := Result + Samples[Random(Length(Samples))+1];
end;

end.
