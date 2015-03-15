unit GlassForm;

interface

uses
  BitmapRgn,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, ExtCtrls;

type
  TGlassForm = class abstract (TForm)
  private
    FTimer : TTimer;
    procedure on_Timer(Sender:TObject);
    procedure on_BackGroundActivate(Sender: TObject);
    procedure on_BackGroundClose(Sender: TObject; var Action: TCloseAction);
  private
    FTopMargine: integer;
    FBitmap: TBitmap;
    FTransparentColor: TColor;
    procedure SetTopMargine(const Value: integer);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetTransparentColor(const Value: TColor);
  protected
    FBackGround : TForm;
    FIsNeedCrystalize : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Crystalize;
  published
    property Bitmap : TBitmap read FBitmap write SetBitmap;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor;
    property TopMargine :integer read FTopMargine write SetTopMargine;
  end;

implementation

{ TGlassForm }

constructor TGlassForm.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := nil;
  FTopMargine := 15;
  FIsNeedCrystalize := false;

  FBackGround := TForm.CreateNew(Self);
  FBackGround.BorderStyle := bsNone;
  FBackGround.FormStyle := fsStayOnTop;
  FBackGround.Color := clWhite;
  FBackGround.AlphaBlendValue := 150;
  FBackGround.AlphaBlend := true;
  FBackGround.Left   := Left;
  FBackGround.Top    := Top + TopMargine;
  FBackGround.Width  := Width;
  FBackGround.Height := Height - TopMargine;
  FBackGround.OnActivate := on_BackGroundActivate;
  FBackGround.OnClose := on_BackGroundClose;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10;
  FTimer.OnTimer := nil;
end;

procedure TGlassForm.Crystalize;
var
  Rgn : HRGN;
begin
  if not Assigned(Bitmap) then Exit;

  Rgn:= CreateBitmapRgn32(Bitmap, FTransparentColor);
  try
    SetWindowRgn(Handle, Rgn, true);
    Canvas.Draw(0, 0, Bitmap);
  finally
    DeleteObject(Rgn);
  end;
end;

destructor TGlassForm.Destroy;
begin
  FreeAndNil(FBackGround);
  FreeAndNil(FTimer);

  inherited;
end;

procedure TGlassForm.on_BackGroundActivate(Sender: TObject);
begin
  BringToFront;
end;

procedure TGlassForm.on_BackGroundClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Close;
end;

procedure TGlassForm.on_Timer(Sender: TObject);
begin
  if not Visible then Exit;

  FTimer.Enabled := false;
  try
    if FIsNeedCrystalize then begin
      FIsNeedCrystalize := false;
      Crystalize;
    end;

    FBackGround.Left := Left;
    FBackGround.Top  := Top + TopMargine;
  finally
    FTimer.Enabled := true;
  end;
end;

procedure TGlassForm.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
  FBitmap.PixelFormat := pf32bit;

  if Assigned(Value) then FTimer.OnTimer := on_Timer
  else FTimer.OnTimer := nil;
end;

procedure TGlassForm.SetTopMargine(const Value: integer);
begin
  FTopMargine := Value;
end;

procedure TGlassForm.SetTransparentColor(const Value: TColor);
begin
  FTransparentColor := Value;
end;

end.
