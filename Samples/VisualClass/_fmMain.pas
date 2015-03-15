unit _fmMain;

interface

uses
  VisualClass,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TBox = class (TVisualClass)
  private
    FColor : TColor;
  public
    constructor Create(VisualClasses:TVisualClasses); override;

    procedure Draw(Bitmap:TBitmap); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
  end;

  TPicture = class (TVisualClass)
  private
    FBmp : TBitmap;
  public
    constructor Create(VisualClasses:TVisualClasses); reintroduce; virtual;
    destructor Destroy; override;

    procedure Draw(Bitmap:TBitmap); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;

    property Bmp : TBitmap read FBmp;
  end;

  TfmMain = class(TForm)
    imgMain: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBox1, FBox2, FBox3 : TBox;
    FPicture1 : TPicture;
    FVisualClasses : TVisualClasses;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FVisualClasses := TVisualClasses.Create(Self);
  FVisualClasses.Align := alClient;
  FVisualClasses.Parent := Self;
  FVisualClasses.Width := 320;
  FVisualClasses.Height := 240;
  FVisualClasses.DoubleBuffered := true;
  FVisualClasses.Draw;

  FBox1 := TBox.Create(FVisualClasses);
  FBox1.Left := 10;
  FBox1.Top  := 10;
  FBox1.Width  := 50;
  FBox1.Height := 50;

  FBox2 := TBox.Create(FVisualClasses);
  FBox2.Left := 100;
  FBox2.Top  := 100;
  FBox2.Width  := 50;
  FBox2.Height := 50;

  FBox3 := TBox.Create(FVisualClasses);
  FBox3.Left := 200;
  FBox3.Top  := 150;
  FBox3.Width  := 50;
  FBox3.Height := 50;

  FPicture1 := TPicture.Create(FVisualClasses);
  FPicture1.Left := 250;
  FPicture1.Top  := 200;
  FPicture1.Width  := 100;
  FPicture1.Height := 100;
  FPicture1.Bmp.Assign(imgMain.Picture.Bitmap);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FVisualClasses.Free;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  FVisualClasses.Draw;
end;

{ TBox }

constructor TBox.Create(VisualClasses: TVisualClasses);
begin
  inherited;

  FColor := clWhite;
end;

procedure TBox.Draw(Bitmap: TBitmap);
begin
  if Selected then Bitmap.Canvas.Brush.Color := clRed
  else Bitmap.Canvas.Brush.Color := FColor;

  Bitmap.Canvas.FillRect(Rect(Left, Top, Left+Width, Top+Height));
end;

procedure TBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  BringToFront;
end;

{ TPicture }

constructor TPicture.Create(VisualClasses: TVisualClasses);
begin
  inherited;

  FBmp := TBitmap.Create;
end;

destructor TPicture.Destroy;
begin
  FreeAndNil(FBmp);

  inherited;
end;

procedure TPicture.Draw(Bitmap: TBitmap);
begin
  if FBmp <> nil then
    Bitmap.Canvas.StretchDraw(Rect(Left, Top, Left+Width, Top+Height), FBmp);
end;

procedure TPicture.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  BringToFront;
end;

end.
