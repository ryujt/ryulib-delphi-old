unit _fmMain;

interface

uses
  AsynchronousStringList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btAddDatas: TButton;
    btCheck: TButton;
    btRemove: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddDatasClick(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
  private
    FList : TAsynchronousStringList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btAddDatasClick(Sender: TObject);
begin
  FList.Add('Ryu');
  FList.Add('Lee');
  FList.Add('Do');
  FList.Add('In');
end;

procedure TfmMain.btCheckClick(Sender: TObject);
begin
  moMsg.Lines.Add(BoolToStr(FList.Check('Do'), true));
end;

procedure TfmMain.btRemoveClick(Sender: TObject);
begin
  FList.Remove('Do');
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FList := TAsynchronousStringList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FList);
end;

end.
