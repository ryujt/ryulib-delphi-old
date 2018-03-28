// Original Source: http://www.delphigroups.info/2/6a/79020.html

unit TileScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TTileScrollBox = class(TScrollBox)
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure OnBitmapChange(Sender: TObject);
    procedure WMPaint(var message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TTileScrollBox]);
end;

constructor TTileScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := OnBitmapChange;
end;

destructor TTileScrollBox.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TTileScrollBox.OnBitmapChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TTileScrollBox.SetBitmap(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmap.Width := 0;
    Invalidate;
    Exit;
  end;

  FBitmap.Assign(Value);
end;

procedure TTileScrollBox.Paint;
var
  X, Y, W, H: LongInt;
  Rect: TRect;
  DC: HDC;
begin
  Rect := GetClientRect;
  W := FBitmap.Width;
  H := FBitmap.Height;
  Y := Rect.Top;
  if (H > 0) and (W > 0) then
  begin
    DC := GetDC(Self.Handle);
    while Y < Height do
    begin
      X := Rect.Left;
      while X < Width do
      begin
        BitBlt(DC, X, Y, W, H, FBitmap.Canvas.Handle, 0, 0, SrcCopy);
        Inc(X, W);
      end;
      Inc(Y, H);
    end;
  end;
end;

procedure TTileScrollBox.WMPaint(var message: TWMPaint);
begin
  inherited;

  Paint;
end;

end.
