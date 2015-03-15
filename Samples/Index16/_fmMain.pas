unit _fmMain;

interface

uses
  Index16, Crc16,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TRecord = class (TComponent, IIndex16Record)
  private
    FCRC16 : word;
    function GetCRC:word;
    function Compare(ACompareTo:IIndex16Record):integer;
  public
    Data : integer;
    constructor Create(AOwner: TComponent); override;
  end;

  TfmMain = class(TForm)
    btInsert: TButton;
    moMsg: TMemo;
    edKey: TEdit;
    btFind: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btInsertClick(Sender: TObject);
    procedure btFindClick(Sender: TObject);
  private
    FIndex16 : TIndex16;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btFindClick(Sender: TObject);
var
  Rec, Find : TRecord;
begin
  Rec := TRecord.Create(Self);
  try
    Rec.Data := StrToInt(edKey.Text);
    Find := FIndex16.Find(Rec) as TRecord;

    if Find <> nil then
      fmMain.moMsg.Lines.Add(Format('Find: %d', [Find.Data]))
    else
      fmMain.moMsg.Lines.Add(Format('Can''t find: %s', [edKey.Text]))
  finally
    Rec.Free;
  end;
end;

procedure TfmMain.btInsertClick(Sender: TObject);
var
  Rec : TRecord;
begin
  Rec := TRecord.Create(Self);
  FIndex16.Add(Rec);

  fmMain.moMsg.Lines.Add(Format('Data: %d', [Rec.Data]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FIndex16 := TIndex16.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIndex16);
end;

{ TRecord }

function TRecord.Compare(ACompareTo: IIndex16Record): integer;
var
  CompareTo : TRecord;
begin
  CompareTo := ACompareTo as TRecord;

  if Data > CompareTo.Data then Result := 1
  else if Data < CompareTo.Data then Result := -1
  else Result := 0;
end;

constructor TRecord.Create(AOwner: TComponent);
begin
  inherited;

  Data := Random($FFFFFFFF);
  FCRC16 := GetCRC16(@Data, SizeOf(Data));
end;

function TRecord.GetCRC: word;
begin
  Result := FCRC16;
end;

end.
