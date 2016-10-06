unit _fmMain;

interface

uses
  DuplicatedList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FDuplicatedList : TDuplicatedList;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Loop1, Loop2 : integer;
begin
  FDuplicatedList := TDuplicatedList.Create;

  for Loop2 := 0 to 3 do begin
    FDuplicatedList.AddChild(TDuplicatedList.Create);

    for Loop1 := 0 to 5 do
      FDuplicatedList.Childs[Loop2].Add(TObject(Loop1));
  end;

  ShowMessage(Format('ChildCount: %d, Childs[0].Count: %d', [FDuplicatedList.ChildCount, FDuplicatedList.Childs[0].Count]));
end;

end.
