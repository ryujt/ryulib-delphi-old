unit _fmMain;

interface

uses
  BooleanList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    btTest01: TButton;
    btTest02: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btTest01Click(Sender: TObject);
  private
    FBooleanList : TBooleanList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btTest01Click(Sender: TObject);
begin
  FBooleanList.Items[8] := true;
  FBooleanList.Items[9] := true;

  ShowMessage(IntToStr(FBooleanList.CountOfTrue));

  FBooleanList.Count := 20;

  ShowMessage(IntToStr(FBooleanList.CountOfTrue));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBooleanList := TBooleanList.Create;
  FBooleanList.Count := 10;
end;

end.
