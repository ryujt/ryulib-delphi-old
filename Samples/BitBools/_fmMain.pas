unit _fmMain;

interface

uses
  BitBools,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBitBools : TBitBools;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  ShowMessage(BoolToStr(FBitBools.Items[123], true));

  FBitBools.Items[123] := true;
  ShowMessage(BoolToStr(FBitBools.Items[123], true));

  FBitBools.Items[123] := false;
  ShowMessage(BoolToStr(FBitBools.Items[123], true));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBitBools := TBitBools.Create;
  FBitBools.Count := 512;
end;

end.
