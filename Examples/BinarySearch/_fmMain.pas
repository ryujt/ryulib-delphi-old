unit _fmMain;

interface

uses
  Strg, BinarySearch,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    ListBox: TListBox;
    Panel1: TPanel;
    btTest: TButton;
    procedure btTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBinarySearch : TBinarySearch;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btTestClick(Sender: TObject);
var
  pInt : PInteger;
  Loop, iLast: Integer;
begin
  for Loop := 1 to 1000 do begin
    New(pInt);
    pInt^ := Random(10000);
    FBinarySearch.Insert(pInt);
  end;

  ListBox.Clear;

  iLast := -1;

  for Loop := 0 to FBinarySearch.Count-1 do begin
    pInt := FBinarySearch.Items[Loop];
    ListBox.Items.Add( IntToStr(pInt^) );

    if iLast >= pInt^ then
      raise Exception.Create('Error');

    iLast := pInt^;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBinarySearch := TBinarySearch.Create;
  FBinarySearch.SetCompareFunction(
    function (A,B:pointer):integer
    var
      pA : PInteger absolute A;
      pB : PInteger absolute B;
    begin
      if pA^ > pB^ then Result := 1
      else if pA^ < pB^ then Result := -1
      else Result := 0;
    end
  );
end;

end.
