unit _fmMain;

interface

uses
  ScreenCapture,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    EditX: TSpinEdit;
    EditY: TSpinEdit;
    EditH: TSpinEdit;
    EditW: TSpinEdit;
    Button2: TButton;
    CheckBoxWithCursor: TCheckBox;
    ComboBoxMoniter: TComboBox;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FScreenCapture: TScreenCapture;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FScreenCapture.WithCursor := CheckBoxWithCursor.Checked;
  FScreenCapture.MonitorNo := ComboBoxMoniter.ItemIndex;
  FScreenCapture.Capture;
  Image1.Picture.Bitmap.Assign(FScreenCapture.Bitmap);
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  FScreenCapture.WithCursor := CheckBoxWithCursor.Checked;
  FScreenCapture.X := EditX.Value;
  FScreenCapture.Y := EditY.Value;
  FScreenCapture.Width := EditW.Value;
  FScreenCapture.Height:= EditH.Value;
  FScreenCapture.Capture;
  Image1.Picture.Bitmap.Assign(FScreenCapture.Bitmap);
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  i: Integer;
  L, T, W, H: Integer;
  MoniterInfo: String;
begin
  FScreenCapture := TScreenCapture.Create(Self);

  for i := 0 to Screen.MonitorCount - 1 do
  begin
    L := Screen.Monitors[i].Left;
    T := Screen.Monitors[i].Top;
    W := Screen.Monitors[i].Width;
    H := Screen.Monitors[i].Height;
    MoniterInfo := Format('%d¹ø X: %d Y: %d W: %d H: %d', [i+1, L, T, L+W, T+H]);
    ComboBoxMoniter.Items.Add(MoniterInfo);
  end;
  ComboBoxMoniter.Enabled := ComboBoxMoniter.Items.Count > 0;
  ComboBoxMoniter.ItemIndex := 0;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FScreenCapture.Free;
end;

end.
