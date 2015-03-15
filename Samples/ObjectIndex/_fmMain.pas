unit _fmMain;

interface

uses
  ObjectIndex, Room,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btAdd1: TButton;
    btFind: TButton;
    btAdd2: TButton;
    btRemove: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btAdd1Click(Sender: TObject);
    procedure btFindClick(Sender: TObject);
    procedure btAdd2Click(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
  private
    FObjectIndex : TObjectIndex<TRoom>;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btAdd1Click(Sender: TObject);
var
  Room : TRoom;
begin
  Room := FObjectIndex.Add('1234', function:TObject begin Result := TRoom.Create; end);
  Room.Key := '1234';
end;

procedure TfmMain.btAdd2Click(Sender: TObject);
var
  Room : TRoom;
begin
  Room := FObjectIndex.Add('4321', function:TObject begin Result := TRoom.Create; end);
  Room.Key := '4321';
end;

procedure TfmMain.btFindClick(Sender: TObject);
var
  Room : TRoom;
begin
  Room := FObjectIndex.Find('1234');
  Caption := Room.Key;
end;

procedure TfmMain.btRemoveClick(Sender: TObject);
begin
  FObjectIndex.Remove('1234');
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FObjectIndex := TObjectIndex<TRoom>.Create;
end;

end.
