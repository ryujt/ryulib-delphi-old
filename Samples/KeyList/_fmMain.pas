unit _fmMain;

interface

uses
  KeyList, KeyList32,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FKeyList : TKeyList;
    FKeyList32 : TKeyList32;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  a, b : boolean;
  Loop, iRandom: Integer;
begin
  for Loop := 1 to 10000 do begin
    iRandom := Random(255);
    a := FKeyList.CheckDuplicate(iRandom);
    b := FKeyList32.CheckDuplicate(iRandom);
    if a <> b then raise Exception.Create('');
  end;
end;

procedure TfmMain.Button2Click(Sender: TObject);
var
  Tick : Cardinal;
  Loop, iRandom: Integer;
begin
//  RandSeed := Tag;
//  Tick := GetTickCount;
//  for Loop := 1 to 1024*1024 do begin
//    iRandom := Random(MaxInt);
//    FKeyList.CheckDuplicate(iRandom);
//  end;
//  Memo1.Lines.Add(Format('%d - %d', [GetTickCount-Tick, FKeyList.Count]));

  RandSeed := Tag;
  Tick := GetTickCount;
  for Loop := 1 to 1024*1024 do begin
    iRandom := Random(MaxInt);
    FKeyList32.CheckDuplicate(iRandom);
  end;
  Memo1.Lines.Add(Format('%d - %d', [GetTickCount-Tick, FKeyList32.Count]));

  Tag := Random(MaxInt);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FKeyList := TKeyList.Create;
  FKeyList32 := TKeyList32.Create;
end;

end.
