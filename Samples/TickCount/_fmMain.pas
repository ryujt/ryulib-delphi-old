unit _fmMain;

interface

uses
  TickCount,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTickCount : TTickCount;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  Caption := IntToStr(FTickCount.Get);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTickCount := TTickCount.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FTickCount.Free;
end;

end.
