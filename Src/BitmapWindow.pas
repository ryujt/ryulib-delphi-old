unit BitmapWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms;

const
  CURSOR_ACTIVE_MARGIN = 5;

type
  TBitmapWindow = class(TCustomPanel)
  private
    FOldParentPos : TPoint;
    FOldParentSize : TPoint;
    FOldMousePos : TPoint;
    FState : TObject;
    FStateNull : TObject;
    FStateNormal : TObject;
    FStateLeftTop : TObject;
    FStateTopLine : TObject;
    FStateRightTop : TObject;
    FStateLeftLine : TObject;
    FStateRightLine : TObject;
    FStateLeftBottom : TObject;
    FStateBottomLine : TObject;
    FStateRightBottom : TObject;
    procedure SetState(const Value: TObject);
    property State : TObject read FState write SetState;
  private
    FCellWidth, FCellHeight : integer;
    procedure do_DrawBackground(Screen:TBitmap);
    procedure do_DrawTopLine(Screen:TBitmap);
    procedure do_DrawBottomLine(Screen:TBitmap);
    procedure do_DrawLeftLine(Screen:TBitmap);
    procedure do_DrawRightLine(Screen:TBitmap);
    procedure do_DrawLeftTop(Screen:TBitmap);
    procedure do_DrawRightTop(Screen:TBitmap);
    procedure do_DrawLeftBottom(Screen:TBitmap);
    procedure do_DrawRightBottom(Screen:TBitmap);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  private
    FBitmap : TBitmap;
    FActiveMouse: Boolean;
    FTargetControl: TWinControl;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetActiveMouse(const Value: Boolean);
    function GetInternalBitmap: TBitmap;
    procedure SetTargetControl(const Value: TWinControl);
    function GetTargetControl: TWinControl;
  protected
    FInternalBitmap : TBitmap;
    procedure paint_InternalBitmap;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TargetControl : TWinControl read GetTargetControl write SetTargetControl;
    property InternalBitmap : TBitmap read GetInternalBitmap;
  published
    property Align;
    property Anchors;
    property ShowHint;
    property Canvas;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  published
    property Bitmap : TBitmap read FBitmap write SetBitmap;
    property ActiveMouse: Boolean read FActiveMouse write SetActiveMouse;
  end;

procedure Register;

implementation

uses
  BMath;

procedure Register;
begin
  RegisterComponents('RyuLib', [TBitmapWindow]);
end;

type
  TState = class (TComponent)
  private
    function GetBitmapWindow:TBitmapWindow;
    procedure set_MouseType(X,Y:integer);
  public
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
  end;

  TStateNull = class(TState)
  public
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateNormal = class (TState)
  private
  public
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateLeftTop = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateTopLine = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateRightTop = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateLeftLine = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateRightLine = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateLeftBottom = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateBottomLine = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;

  TStateRightBottom = class (TState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
  end;


{ TBitmapWindow }

constructor TBitmapWindow.Create(AOwner: TComponent);
begin
  inherited;

  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;

  FTargetControl := nil;

  FStateNull := TStateNull.Create(Self);
  FStateNormal := TStateNormal.Create(Self);
  FStateLeftTop := TStateLeftTop.Create(Self);
  FStateTopLine := TStateTopLine.Create(Self);
  FStateRightTop := TStateRightTop.Create(Self);
  FStateLeftLine := TStateLeftLine.Create(Self);
  FStateRightLine := TStateRightLine.Create(Self);
  FStateLeftBottom := TStateLeftBottom.Create(Self);
  FStateBottomLine := TStateBottomLine.Create(Self);
  FStateRightBottom := TStateRightBottom.Create(Self);
  FState := FStateNormal;

  FInternalBitmap := TBitmap.Create;
  FInternalBitmap.PixelFormat := pf32bit;

  FBitmap := TBitmap.Create;
end;

destructor TBitmapWindow.Destroy;
begin
  FreeAndNil(FInternalBitmap);
  FreeAndNil(FBitmap);

  FState := nil;
  FStateNull.Free;
  FStateNormal.Free;
  FStateLeftTop.Free;
  FStateTopLine.Free;
  FStateRightTop.Free;
  FStateLeftLine.Free;
  FStateRightLine.Free;
  FStateLeftBottom.Free;
  FStateBottomLine.Free;
  FStateRightBottom.Free;

  inherited;
end;

procedure TBitmapWindow.do_DrawBackground(Screen: TBitmap);
var
  Src, Dst : TRect;
  LoopX, LoopY : integer;
begin
  Src := Bounds(FCellWidth, FCellHeight, FCellWidth, FCellHeight);

  for LoopY := 1 to Screen.Height div FCellHeight do
  for LoopX := 1 to Screen.Width div FCellWidth do begin
    Dst := Bounds(FCellWidth*(LoopX-1), FCellHeight*(LoopY-1), FCellWidth, FCellHeight);
    Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
  end;
end;

procedure TBitmapWindow.do_DrawBottomLine(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(FCellWidth, FBitmap.Height - FCellHeight, FCellWidth, FCellHeight);
  Dst := Bounds(0, Screen.Height-FCellHeight, FCellWidth, FCellHeight);

  repeat
    Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    Dst := Bounds(Dst.Left + FCellWidth, Dst.Top, FCellWidth, FCellHeight);
  until Dst.Left >= Screen.Width;
end;

procedure TBitmapWindow.do_DrawLeftBottom(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(0, FBitmap.Height-FCellHeight, FCellWidth, FCellHeight);
  Dst := Bounds(0, Screen.Height-FCellHeight, FCellWidth, FCellHeight);

  Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
end;

procedure TBitmapWindow.do_DrawLeftLine(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(0, FCellHeight, FCellWidth, FCellHeight);
  Dst := Bounds(0, 0, FCellWidth, FCellHeight);

  repeat
    Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    Dst := Bounds(Dst.Left, Dst.Top+FCellHeight, FCellWidth, FCellHeight);
  until Dst.Top >= Screen.Height;
end;

procedure TBitmapWindow.do_DrawLeftTop(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(0, 0, FCellWidth, FCellHeight);
  Dst := Bounds(0, 0, FCellWidth, FCellHeight);

  Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
end;

procedure TBitmapWindow.do_DrawRightBottom(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(FBitmap.Width - FCellWidth, FBitmap.Height - FCellHeight, FCellWidth, FCellHeight);
  Dst := Bounds(Screen.Width-FCellWidth, Screen.Height-FCellHeight, FCellWidth, FCellHeight);

  Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
end;

procedure TBitmapWindow.do_DrawRightLine(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(FBitmap.Width-FCellWidth, FCellHeight, FCellWidth, FCellHeight);
  Dst := Bounds(Screen.Width-FCellWidth, 0, FCellWidth, FCellHeight);

  repeat
    Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    Dst := Bounds(Dst.Left, Dst.Top+FCellHeight, FCellWidth, FCellHeight);
  until Dst.Top >= Screen.Height;
end;

procedure TBitmapWindow.do_DrawRightTop(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(FBitmap.Width-FCellWidth, 0, FCellWidth, FCellHeight);
  Dst := Bounds(Screen.Width-FCellWidth, 0, FCellWidth, FCellHeight);

  Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
end;

procedure TBitmapWindow.do_DrawTopLine(Screen: TBitmap);
var
  Src, Dst : TRect;
begin
  Src := Bounds(FCellWidth, 0, FCellWidth, FCellHeight);
  Dst := Bounds(0, 0, FCellWidth, FCellHeight);

  repeat
    Screen.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    Dst := Bounds(Dst.Left + FCellWidth, Dst.Top, FCellWidth, FCellHeight);
  until Dst.Left >= Screen.Width;
end;

function TBitmapWindow.GetInternalBitmap: TBitmap;
begin
  paint_InternalBitmap;
  Result := FInternalBitmap;
end;

function TBitmapWindow.GetTargetControl: TWinControl;
begin
  if FTargetControl = nil then Result := Parent
  else Result := FTargetControl;
end;

procedure TBitmapWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  TState(State).MouseDown(Button, Shift, X, Y);
end;

procedure TBitmapWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  TState(State).MouseMove(Shift, X, Y);
end;

procedure TBitmapWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  TState(State).MouseUp(Button, Shift, X, Y);
end;

procedure TBitmapWindow.Paint;
begin
  inherited;

  paint_InternalBitmap;

  Canvas.Draw(0, 0, FInternalBitmap);

  if csDesigning in ComponentState then
  with inherited Canvas do begin
    Pen.Style := psDash;
    Brush.Style := bsClear;
    Rectangle(0, 0, Width, Height);
  end;
end;

procedure TBitmapWindow.paint_InternalBitmap;
begin
  FCellWidth := FBitmap.Width div 3;
  FCellHeight := FBitmap.Height div 3;
  if FCellWidth * FCellHeight = 0 then Exit;

  FInternalBitmap.Width := Width;
  FInternalBitmap.Height := Height;

  do_DrawBackground(FInternalBitmap);
  do_DrawTopLine(FInternalBitmap);
  do_DrawBottomLine(FInternalBitmap);
  do_DrawLeftLine(FInternalBitmap);
  do_DrawRightLine(FInternalBitmap);
  do_DrawLeftTop(FInternalBitmap);
  do_DrawRightTop(FInternalBitmap);
  do_DrawLeftBottom(FInternalBitmap);
  do_DrawRightBottom(FInternalBitmap);
end;

procedure TBitmapWindow.SetActiveMouse(const Value: Boolean);
begin
  FActiveMouse := Value;

  if FActiveMouse then begin
    FState := FStateNormal
  end else begin
    FState := FStateNull;
    Cursor := crDefault;
  end;
end;

procedure TBitmapWindow.SetBitmap(const Value: TBitmap);
begin
  if Value = nil then begin
    FBitmap.Width := 0;
    Invalidate;
    Exit;
  end;

  FBitmap.Assign(Value);
  Invalidate;
end;

procedure TBitmapWindow.SetState(const Value: TObject);
begin
  FState := Value;
end;

procedure TBitmapWindow.SetTargetControl(const Value: TWinControl);
begin
  FTargetControl := Value;
end;

{ TState }

procedure TState.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if GetBitmapWindow.TargetControl = nil then Exit;

  GetBitmapWindow.FOldParentPos := Point(GetBitmapWindow.TargetControl.Left, GetBitmapWindow.TargetControl.Top);
  GetBitmapWindow.FOldParentSize := Point(GetBitmapWindow.TargetControl.Width, GetBitmapWindow.TargetControl.Height);
  GetCursorPos(GetBitmapWindow.FOldMousePos);

  if GetBitmapWindow.FCellWidth * GetBitmapWindow.FCellHeight = 0 then begin
    GetBitmapWindow.State := GetBitmapWindow.FStateNormal;
    Exit;
  end;

  with GetBitmapWindow do
    if BoxIn(X, Y, 0, 0, CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN) then State := FStateLeftTop
    else if BoxIn(X, Y, CURSOR_ACTIVE_MARGIN, 0, Width-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN) then State := FStateTopLine
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, 0, Width, CURSOR_ACTIVE_MARGIN) then State := FStateRightTop
    else if BoxIn(X, Y, 0, CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN) then State := FStateLeftLine
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Width, Height-CURSOR_ACTIVE_MARGIN) then State := FStateRightLine
    else if BoxIn(X, Y, 0, Height-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Height) then State := FStateLeftBottom
    else if BoxIn(X, Y, CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN, Width-CURSOR_ACTIVE_MARGIN, Height) then State := FStateBottomLine
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN, Width, Height) then State := FStateRightBottom
    else State := FStateNormal;
end;

procedure TState.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  set_MouseType(X, Y)
end;

procedure TState.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  GetBitmapWindow.State := GetBitmapWindow.FStateNormal;
end;

function TState.GetBitmapWindow: TBitmapWindow;
begin
  Result := Pointer(Owner);
end;

procedure TState.set_MouseType(X,Y:integer);
begin
  if GetBitmapWindow.FCellWidth * GetBitmapWindow.FCellHeight = 0 then begin
    GetBitmapWindow.Cursor := crDefault;
    Exit;
  end;

  with GetBitmapWindow do
    if BoxIn(X, Y, 0, 0, CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN) then Cursor := crSizeNWSE
    else if BoxIn(X, Y, CURSOR_ACTIVE_MARGIN, 0, Width-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN) then Cursor := crSizeNS
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, 0, Width, CURSOR_ACTIVE_MARGIN) then Cursor := crSizeNESW
    else if BoxIn(X, Y, 0, CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN) then Cursor := crSizeWE
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Width, Height-CURSOR_ACTIVE_MARGIN) then Cursor := crSizeWE
    else if BoxIn(X, Y, 0, Height-CURSOR_ACTIVE_MARGIN, CURSOR_ACTIVE_MARGIN, Height) then Cursor := crSizeNESW
    else if BoxIn(X, Y, CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN, Width-CURSOR_ACTIVE_MARGIN, Height) then Cursor := crSizeNS
    else if BoxIn(X, Y, Width-CURSOR_ACTIVE_MARGIN, Height-CURSOR_ACTIVE_MARGIN, Width, Height) then Cursor := crSizeNWSE
    else Cursor := crDefault;
end;

{ TStateNormal }

procedure TStateNormal.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if GetBitmapWindow.Cursor = crDefault then begin
    ReleaseCapture;
    GetBitmapWindow.TargetControl.Perform(WM_SysCommand, $F012, 0);
  end;
end;

{ TStateLeftTop }

procedure TStateLeftTop.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX, iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;
  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Left := GetBitmapWindow.FOldParentPos.X + iX;
  GetBitmapWindow.TargetControl.Top  := GetBitmapWindow.FOldParentPos.Y + iY;
  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X - iX;
  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y - iY;
end;

{ TStateTopLine }

procedure TStateTopLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Top  := GetBitmapWindow.FOldParentPos.Y + iY;
  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y - iY;
end;

{ TStateRightTop }

procedure TStateRightTop.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX, iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;
  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Top  := GetBitmapWindow.FOldParentPos.Y + iY;
  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X + iX;
  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y - iY;
end;

{ TStateLeftLine }

procedure TStateLeftLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;

  GetBitmapWindow.TargetControl.Left := GetBitmapWindow.FOldParentPos.X + iX;
  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X - iX;
end;

{ TStateRightLine }

procedure TStateRightLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;

  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X + iX;
end;

{ TStateLeftBottom }

procedure TStateLeftBottom.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX, iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;
  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Left := GetBitmapWindow.FOldParentPos.X + iX;
  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X - iX;
  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y + iY;
end;

{ TStateBottomLine }

procedure TStateBottomLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y + iY;
end;

{ TStateRightBottom }

procedure TStateRightBottom.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iX, iY : integer;
  MousePos : TPoint;
begin
  inherited;

  if GetBitmapWindow.TargetControl = nil then Exit;
  if Shift <> [ssLeft] then Exit;

  GetCursorPos(MousePos);

  iX := MousePos.X - GetBitmapWindow.FOldMousePos.X;
  iY := MousePos.Y - GetBitmapWindow.FOldMousePos.Y;

  GetBitmapWindow.TargetControl.Width := GetBitmapWindow.FOldParentSize.X + iX;
  GetBitmapWindow.TargetControl.Height := GetBitmapWindow.FOldParentSize.Y + iY;
end;

{ TStateNull }

procedure TStateNull.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TStateNull.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TStateNull.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
end;

end.
