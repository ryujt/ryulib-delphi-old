unit _fmMain;

interface

uses
  MemoryPool, MemoryRing,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btTest: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    FMemoryRing : TMemoryRing;
    procedure get_MemoryPage;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btTestClick(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 1 to 100000 do get_MemoryPage;
  moMsg.Lines.Add(Format('FMemoryRing.Size: %d, Queue.Size', [FMemoryRing.Size]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  FMemoryRing := TMemoryRing.Create(32, false);
end;

procedure TfmMain.get_MemoryPage;
var
  MemoryPage : TMemoryPage;
begin
  MemoryPage := FMemoryRing.GetMem(Random(1024));
//  moMsg.Lines.Add(Format('FMemoryRing.Size: %d, Queue.Size', [FMemoryRing.Size]));
end;

end.
