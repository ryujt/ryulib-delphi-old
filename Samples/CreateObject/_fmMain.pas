unit _fmMain;

interface

uses
  RyuLibBase,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FList : TList;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TItem = class
  private
    FData : pointer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TItem }

constructor TItem.Create;
begin
  inherited;

  GetMem(FData, 1024);
  FillChar(FData^, 1024, 0);
end;

destructor TItem.Destroy;
begin
  FreeMem(FData);

  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Item : TItem;
  Loop: Integer;
begin
  for Loop := 1 to 1024 do begin
    RyuLibBase.CreateObject(TItem, Item);
    Item.Create;
    FList.Add(Item);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Item : TItem;
begin
  while FList.Count > 0 do begin
    Item := FList[0];
    FList.Delete(0);
    Item.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
end;

end.
