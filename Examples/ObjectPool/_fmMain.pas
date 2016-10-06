unit _fmMain;

interface

uses
  ObjectPool,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btUse: TButton;
    btRemove: TButton;
    procedure btUseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
  private
    FObjectPool : TObjectPool;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btUseClick(Sender: TObject);
var
  Edit : TEdit;
begin
  Edit := TEdit(FObjectPool.Objects[123]);
  Edit.Parent := Self;
end;

procedure TfmMain.btRemoveClick(Sender: TObject);
begin
  FObjectPool.Remove(FObjectPool.Objects[123], 123);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FObjectPool := TObjectPool.Create(TEdit);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FObjectPool);
end;

end.
