unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, BitmapProgressBar,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    BitmapProgressBar: TBitmapProgressBar;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  BitmapProgressBar.Percent := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BitmapProgressBar.Percent := 50;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BitmapProgressBar.Percent := 100;
end;

end.
