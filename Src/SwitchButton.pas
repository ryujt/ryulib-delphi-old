unit SwitchButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TSwitchButton = class(TCustomControl)
  private
    FIsMouseOver : boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  private
    function get_IndexOf2:integer;
    function get_IndexOf3:integer;
    function get_IndexOf4:integer;
    function get_IndexOf6:integer;
    procedure draw_Bitmap(AIndex:integer);
    procedure set_ControlSize;
  private
    FBitmap: TBitmap;
    FSwitchOn: boolean;
    FOnChanged: TNotifyEvent;
    FClickEnabled: boolean;
    FClickOnly: boolean;
    FBitmapCount: integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FBitmapReference: TBitmap;
    FCenter: boolean;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetSwitchOn(const Value: boolean);
    procedure SetBitmapCount(const Value: integer);
    procedure SetBitmapReference(const Value: TBitmap);
    procedure SetCenter(const Value: boolean);
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure SetEnabled(Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Bitmap을 이용하는 그래픽 컨트롤(RyuLib 내에서)에 모드 적용
    property BitmapReference : TBitmap read FBitmapReference write SetBitmapReference;
  published
    property Align;
    property Anchors;
    property Enabled;
    property ShowHint;
    property Visible;
    property Font;
    property OnClick;

    property Caption;
  published
    property Bitmap : TBitmap read FBitmap write SetBitmap;
    property BitmapCount :integer read FBitmapCount write SetBitmapCount;

    property Center : boolean read FCenter write SetCenter;

    property ClickEnabled : boolean read FClickEnabled write FClickEnabled;
    property ClickOnly : boolean read FClickOnly write FClickOnly;
    property SwitchOn : boolean read FSwitchOn write SetSwitchOn;
  published
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TSwitchButton]);
end;

{ TSwitchButton }

procedure TSwitchButton.Click;
begin
  if (FClickEnabled = false) or (Enabled = false) then Exit;

  inherited;

  if not FClickOnly then SwitchOn := not SwitchOn;
end;

procedure TSwitchButton.CMMouseEnter(var Message: TMessage);
begin
  FIsMouseOver := true;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);

  Invalidate;
end;

procedure TSwitchButton.CMMouseLeave(var Message: TMessage);
begin
  FIsMouseOver := false;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);

  Invalidate;
end;

constructor TSwitchButton.Create(AOwner: TComponent);
begin
  inherited;

  FIsMouseOver := false;
  FBitmapCount := 3;

  Caption := '';

  FCenter := true;
  FClickEnabled := true;
  FClickOnly := false;

  FBitmap := TBitmap.Create;

  FBitmapReference := FBitmap;
end;

destructor TSwitchButton.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited;
end;

procedure TSwitchButton.draw_Bitmap(AIndex: integer);
var
  iTop, iBottom : integer;
  SrcRect, DstRect : TRect;
begin
  if FCenter then begin
    SrcRect := Rect(Width * AIndex, 0, Width * (AIndex + 1), FBitmapReference.Height);

    iTop := (Height - FBitmapReference.Height) div 2;
    iBottom := iTop + FBitmapReference.Height;
    DstRect := Rect(0, iTop, Width, iBottom);
  end else begin
    SrcRect := Rect(Width * AIndex, 0, Width * (AIndex + 1), Height);
    DstRect := Rect(0, 0, Width, Height);
  end;

  Canvas.CopyRect(DstRect, FBitmapReference.Canvas, SrcRect);
end;

function TSwitchButton.get_IndexOf2: integer;
begin
  if FSwitchOn then Result := 0
  else Result := 1;
end;

function TSwitchButton.get_IndexOf3: integer;
begin
  if Enabled = false then
     Result := 2
  else begin
    if FSwitchOn then Result := 0
    else Result := 1;
  end;
end;

function TSwitchButton.get_IndexOf4: integer;
begin
  if Enabled then begin
    if FSwitchOn then Result := 0
    else Result := 1;
  end else begin
    if FSwitchOn then Result := 2
    else Result := 3;
  end;
end;

function TSwitchButton.get_IndexOf6: integer;
begin
  if Enabled then begin
    if FSwitchOn then begin
      if not FIsMouseOver then Result := 0
      else Result := 1;
    end else begin
      if not FIsMouseOver then Result := 3
      else Result := 4;
    end;
  end else begin
    if FSwitchOn then Result := 2
    else Result := 5;
  end;
end;

procedure TSwitchButton.Paint;
var
  iIndex,
  iCaptionWidth, iCaptionHeight : integer;
begin
  inherited;

  if FBitmapReference.Width * FBitmapReference.Height = 0 then Exit;

  if csDesigning in ComponentState then
    with inherited Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  case FBitmapCount of
    2: iIndex := get_IndexOf2;
    3: iIndex := get_IndexOf3;
    4: iIndex := get_IndexOf4;
    6: iIndex := get_IndexOf6;
    else iIndex := 0;
  end;

  draw_Bitmap(iIndex);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  iCaptionWidth  := Canvas.TextWidth (Caption);
  iCaptionHeight := Canvas.TextHeight(Caption);

  Canvas.TextOut(
    (Width  div 2) - (iCaptionWidth  div 2),
    (Height div 2) - (iCaptionHeight div 2),
    Caption
  );
end;

procedure TSwitchButton.SetBitmap(const Value: TBitmap);
begin
  FBitmapReference := FBitmap;

  if Value = nil then begin
    FBitmap.Width  := 0;
    FBitmap.Height := 0;

    Invalidate;

    Exit;
  end;

  FBitmap.Assign(Value);

  set_ControlSize;

  Invalidate;
end;

procedure TSwitchButton.SetBitmapCount(const Value: integer);
begin
  if not (Value in [2, 3, 4, 6]) then
    raise Exception.Create('TSwitchButton.SetBitmapCount: BitmapCount는 2, 3, 4, 6 중에 하나여야 합니다.');

  FBitmapCount := Value;

  set_ControlSize;

  Invalidate;
end;

procedure TSwitchButton.SetBitmapReference(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmapReference := FBitmap;

    set_ControlSize;

    Invalidate;

    Exit;
  end;

  FBitmapReference := Value;

  set_ControlSize;

  Invalidate;
end;

procedure TSwitchButton.SetCenter(const Value: boolean);
begin
  FCenter := Value;

  Invalidate;
end;

procedure TSwitchButton.SetEnabled(Value: boolean);
begin
  inherited;

  set_ControlSize;

  Invalidate;
end;

procedure TSwitchButton.SetSwitchOn(const Value: boolean);
begin
  FSwitchOn := Value;

  set_ControlSize;

  Invalidate;

  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TSwitchButton.set_ControlSize;
begin
  Height := FBitmapReference.Height;
  Width  := FBitmapReference.Width div FBitmapCount;
end;

end.
