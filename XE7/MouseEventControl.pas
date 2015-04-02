unit MouseEventControl;

interface

uses
  Scheduler, HandleComponent,
  Windows, Messages, SysUtils, Classes, Controls, SyncObjs;

const
  WM_CustomMouseMove = WM_USER + 1;

type
  {*
    마우스 이벤트를 대신처리해주는 클래스, 스레드를 이용해서 MouseMove 이벤트를 발생 시킨다.
    이유는 Main Thread가 바쁠 때, MouseMove 이벤트가 제대로 처리 안되는 현상 때문이다.
  }
  TMouseEventControl = class (THandleComponent)
  private
    FCS : TCriticalSection;
    FOldShift : TShiftState;
    FOldX, FOldY : integer;
  private
    procedure do_WM_CustomMouseMove(var AMsg:TMessage); message WM_CustomMouseMove;
  private
    procedure on_TargetControl_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure on_TargetControl_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure on_TargetControl_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FScheduler : TScheduler;
    procedure on_FScheduler_Timer(Sender:TObject);
  private
    FTargetControl: TWinControl;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    procedure SetTargetControl(const Value: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property TargetControl : TWinControl read FTargetControl write SetTargetControl;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

implementation

type
  TWinControlWrapper = class (TWinControl)
  end;

  TMouseMoveInfo = class
    Shift : TShiftState;
    X, Y : integer;
  end;

{ TMouseEventControl }

constructor TMouseEventControl.Create(AOwner: TComponent);
begin
  inherited;

  FTargetControl := nil;

  FOldShift := [];
  FOldX := -1;
  FOldY := -1;

  FCS := TCriticalSection.Create;

  FScheduler := TScheduler.Create;
  FScheduler.Interval := 25;
  FScheduler.OnTimer := on_FScheduler_Timer;

  FScheduler.Start;
end;

destructor TMouseEventControl.Destroy;
begin
  FreeAndNil(FScheduler);
  FreeAndNil(FCS);

  inherited;
end;

procedure TMouseEventControl.do_WM_CustomMouseMove(var AMsg: TMessage);
var
  MouseMoveInfo : TMouseMoveInfo;
begin
  MouseMoveInfo := Pointer(AMsg.WParam);
  try
    if Assigned(OnMouseMove) then OnMouseMove( Self, MouseMoveInfo.Shift, MouseMoveInfo.X, MouseMoveInfo.Y );
  finally
    MouseMoveInfo.Free;
  end;
end;

procedure TMouseEventControl.on_FScheduler_Timer(Sender: TObject);
var
  CurPos, ClientPos : TPoint;
  MouseMoveInfo : TMouseMoveInfo;
begin
  GetCursorPos( CurPos );

  if FTargetControl = nil then Exit;

  if (CurPos.X = FOldX) and (CurPos.Y = FOldY) then Exit;

  FOldX := CurPos.X;
  FOldY := CurPos.Y;

  FCS.Acquire;
  try
    ClientPos := FTargetControl.ScreenToClient( CurPos );

    if (ClientPos.X < 0) or (ClientPos.Y < 0) then Exit;
    if (ClientPos.X >= FTargetControl.Width) or (ClientPos.Y >= FTargetControl.Height) then Exit;

    MouseMoveInfo := TMouseMoveInfo.Create;
    MouseMoveInfo.Shift := FOldShift;
    MouseMoveInfo.X := ClientPos.X;
    MouseMoveInfo.Y := ClientPos.Y;
  finally
    FCS.Release;
  end;

  PostMessage( Handle, WM_CustomMouseMove, Integer(MouseMoveInfo), 0 );
end;

procedure TMouseEventControl.on_TargetControl_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown( Self, Button, Shift, X, Y );
end;

procedure TMouseEventControl.on_TargetControl_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FCS.Acquire;
  try
    FOldShift := Shift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseEventControl.on_TargetControl_MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown( Self, Button, Shift, X, Y );
end;

procedure TMouseEventControl.SetTargetControl(const Value: TWinControl);
begin
  FCS.Acquire;
  try
    FTargetControl := Value;

    TWinControlWrapper(FTargetControl).OnMouseDown := on_TargetControl_MouseDown;
    TWinControlWrapper(FTargetControl).OnMouseMove := on_TargetControl_MouseMove;
    TWinControlWrapper(FTargetControl).OnMouseUp   := on_TargetControl_MouseUp;
  finally
    FCS.Release;
  end;
end;

end.
