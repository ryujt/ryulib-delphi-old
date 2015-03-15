unit GlassContols;

interface

uses
  GlassForm,
  Windows, Messages, SysUtils, Variants, Classes, Graphics;

type
  TGlassContols = class (TComponent)
  private
    FFont: TFont;
    FBrush: TBrush;
    FWidth: integer;
    FTop: integer;
    FHeight: integer;
    FLeft: integer;
    procedure SetHeight(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetWidth(const Value: integer);
  protected
    function GlassForm:TGlassForm;
    procedure do_EraseBackGround; virtual;
    procedure do_Repaint; virtual;
    function get_TextWidth(AText:string):integer;
    function get_TextHeight(AText:string):integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Repaint;
  published
    property Font : TFont read FFont;
    property Brush : TBrush read FBrush;
    property Left : integer read FLeft write SetLeft;
    property Top : integer read FTop write SetTop;
    property Width : integer read FWidth write SetWidth;
    property Height : integer read FHeight write SetHeight;
  end;

implementation

type
  TGlassFormCopy = class (TGlassForm)
  end;

{ TGlassContols }

constructor TGlassContols.Create(AOwner: TComponent);
begin
  inherited;

  FFont := TFont.Create;
  FBrush := TBrush.Create;
end;

destructor TGlassContols.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FBrush);

  inherited;
end;

procedure TGlassContols.do_EraseBackGround;
var
  OldFont : TFont;
  OldBrush : TBrush;
begin
  OldFont := TFont.Create;
  OldBrush := TBrush.Create;
  try
    OldFont.Assign(GlassForm.Bitmap.Canvas.Font);
    OldBrush.Assign(GlassForm.Bitmap.Canvas.Brush);
    try
      GlassForm.Bitmap.Canvas.Brush.Style := bsSolid;
      GlassForm.Bitmap.Canvas.Brush.Color := GlassForm.TransparentColor;
      GlassForm.Bitmap.Canvas.FillRect(Rect(Left, Top, Left+Width, Top+Height));
    finally
      GlassForm.Bitmap.Canvas.Font.Assign(OldFont);
      GlassForm.Bitmap.Canvas.Brush.Assign(OldBrush);
    end;
  finally
    OldFont.Free;
  end;
end;

procedure TGlassContols.do_Repaint;
begin
end;

function TGlassContols.get_TextHeight(AText: string): integer;
var
  OldFont : TFont;
  OldBrush : TBrush;
begin
  OldFont := TFont.Create;
  OldBrush := TBrush.Create;
  try
    OldFont.Assign(GlassForm.Bitmap.Canvas.Font);
    OldBrush.Assign(GlassForm.Bitmap.Canvas.Brush);
    try
      GlassForm.Bitmap.Canvas.Font.Assign(Font);
      GlassForm.Bitmap.Canvas.Brush.Assign(Brush);

      Result := GlassForm.Bitmap.Canvas.TextHeight(AText);
    finally
      GlassForm.Bitmap.Canvas.Font.Assign(OldFont);
      GlassForm.Bitmap.Canvas.Brush.Assign(OldBrush);
    end;
  finally
    OldFont.Free;
  end;
end;

function TGlassContols.get_TextWidth(AText: string): integer;
var
  OldFont : TFont;
  OldBrush : TBrush;
begin
  OldFont := TFont.Create;
  OldBrush := TBrush.Create;
  try
    OldFont.Assign(GlassForm.Bitmap.Canvas.Font);
    OldBrush.Assign(GlassForm.Bitmap.Canvas.Brush);
    try
      GlassForm.Bitmap.Canvas.Font.Assign(Font);
      GlassForm.Bitmap.Canvas.Brush.Assign(Brush);

      Result := GlassForm.Bitmap.Canvas.TextWidth(AText);
    finally
      GlassForm.Bitmap.Canvas.Font.Assign(OldFont);
      GlassForm.Bitmap.Canvas.Brush.Assign(OldBrush);
    end;
  finally
    OldFont.Free;
  end;
end;

function TGlassContols.GlassForm: TGlassForm;
begin
  Result := Pointer(Owner);
end;

procedure TGlassContols.Repaint;
var
  OldFont : TFont;
  OldBrush : TBrush;
begin
  OldFont := TFont.Create;
  OldBrush := TBrush.Create;
  try
    OldFont.Assign(GlassForm.Bitmap.Canvas.Font);
    OldBrush.Assign(GlassForm.Bitmap.Canvas.Brush);
    try
      GlassForm.Bitmap.Canvas.Font.Assign(Font);
      GlassForm.Bitmap.Canvas.Brush.Assign(Brush);

      do_Repaint;

      TGlassFormCopy(GlassForm).FIsNeedCrystalize := true;
    finally
      GlassForm.Bitmap.Canvas.Font.Assign(OldFont);
      GlassForm.Bitmap.Canvas.Brush.Assign(OldBrush);
    end;
  finally
    OldFont.Free;
  end;
end;

procedure TGlassContols.SetHeight(const Value: integer);
begin
  FHeight := Value;
  Repaint;
end;

procedure TGlassContols.SetLeft(const Value: integer);
begin
  FLeft := Value;
  Repaint;
end;

procedure TGlassContols.SetTop(const Value: integer);
begin
  FTop := Value;
  Repaint;
end;

procedure TGlassContols.SetWidth(const Value: integer);
begin
  FWidth := Value;
  Repaint;
end;

end.
