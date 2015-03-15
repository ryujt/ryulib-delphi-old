unit BitmapOutput;

interface

uses
  DebugTools, RyuLibBase, RyuGraphics, glCanvas,
  SyncObjs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus;

const
  WM_ErrorInDirectOutput = WM_USER + 1;

type
  TDrawBitmapFunction = reference to function(Bitmap:TBitmap):boolean;

  TBitmapOutput = class (TScrollBox)
  private
    FOldWidth, FOldHeight, FOldBitmapWidth, FOldBitmapHeight : integer;
    FIsScreenChanged : boolean;
  private
    FglCanvas : TglCanvas;
    procedure do_ResizeBitmap(ABitmap:TBitmap);
    procedure on_glCanvas_DblClick(Sender:TObject);
    procedure on_glCanvas_MouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
  private
    FOnClear: TNotifyEvent;
    FStrechDraw: boolean;
    FUseVideoAccelerator: boolean;
    procedure SetHint(const Value: string);
    function GetOnDrawData: TDataEvent;
    procedure SetOnDrawData(const Value: TDataEvent);
    function GetPenColor: TColor;
    procedure SetPenColor(const Value: TColor);
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
    function GetHint: string;
    function GetVersionOfOpenGL: string;
    function GetOnError: TIntegerEvent;
    procedure SetOnError(const Value: TIntegerEvent);
    procedure SetStrechDraw(const Value: boolean);
    procedure SetUseVideoAccelerator(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure DrawBitmap(ADrawBitmapFunction:TDrawBitmapFunction);
  published
    property VersionOfOpenGL : string read GetVersionOfOpenGL;
    property Hint : string read GetHint write SetHint;
    property CanDraw : boolean read GetCanDraw write SetCanDraw;
    property PenColor : TColor read GetPenColor write SetPenColor;
    property StrechDraw : boolean read FStrechDraw write SetStrechDraw;
    property UseVideoAccelerator : boolean read FUseVideoAccelerator write SetUseVideoAccelerator;
  published
    property OnClear : TNotifyEvent read FOnClear write FOnClear;
    property OnDrawData : TDataEvent read GetOnDrawData write SetOnDrawData;
    property OnError : TIntegerEvent read GetOnError write SetOnError;
  end;

implementation

{ TBitmapOutput }

procedure TBitmapOutput.Clear;
begin
  FglCanvas.Clear;
end;

constructor TBitmapOutput.Create(AOwner: TComponent);
begin
  inherited;

  FOldWidth  := 0;
  FOldHeight := 0;
  FOldBitmapWidth  := 0;
  FOldBitmapHeight := 0;

  FIsScreenChanged := true;
  FStrechDraw := true;

  BevelWidth := 1;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelKind  := bkNone;

  FglCanvas := TglCanvas.Create(Self);
  FglCanvas.Align := alNone;
  FglCanvas.Parent := Self;
  FglCanvas.OnDblClick := on_glCanvas_DblClick;
  FglCanvas.OnMouseDown := on_glCanvas_MouseDown;
end;

destructor TBitmapOutput.Destroy;
begin
  FreeAndNil(FglCanvas);

  inherited;
end;

procedure TBitmapOutput.do_ResizeBitmap(ABitmap: TBitmap);
const
  Margin = 5;
var
  iClientWidth, iClientHeight : integer;
  ptSrc, ptDst, ptResult : TPoint;
begin
  FIsScreenChanged :=
    FIsScreenChanged or
    (Width  <> FOldWidth) or
    (Height <> FOldHeight) or
    (ABitmap.Width  <> FOldBitmapWidth) or
    (ABitmap.Height <> FOldBitmapHeight);

  if not FIsScreenChanged then Exit;

  FOldWidth  := Width;
  FOldHeight := Height;
  FOldBitmapWidth  := ABitmap.Width;
  FOldBitmapHeight := ABitmap.Height;

  FIsScreenChanged := false;

  iClientWidth  := Width - Margin;
  iClientHeight := Height - Margin;

  // 이미지를 표시 할 충분한 크기라면 원본 크기로 출력한다.
  if (iClientWidth >= ABitmap.Width) and (iClientHeight >= ABitmap.Height) then begin
    FglCanvas.Width  := ABitmap.Width;
    FglCanvas.Height := ABitmap.Height;

    FglCanvas.Left := (iClientWidth  - FglCanvas.Width)  div 2;
    FglCanvas.Top  := (iClientHeight - FglCanvas.Height) div 2;

  end else begin
    if FStrechDraw then begin
      ptSrc := Point( ABitmap.Width, ABitmap.Height );
      ptDst := Point( iClientWidth, iClientHeight);
      ptResult := RatioSize( ptSrc, ptDst );

      FglCanvas.Width  := ptResult.X;
      FglCanvas.Height := ptResult.Y;

      FglCanvas.Left := (iClientWidth  - FglCanvas.Width)  div 2;
      FglCanvas.Top  := (iClientHeight - FglCanvas.Height) div 2;
    end else begin
      Self.HorzScrollBar.Position := 0;
      Self.VertScrollBar.Position := 0;
      FglCanvas.Left := 0;
      FglCanvas.Top  := 0;
      FglCanvas.Width  := ABitmap.Width;
      FglCanvas.Height := ABitmap.Height;
    end;
  end;
end;

procedure TBitmapOutput.DrawBitmap(ADrawBitmapFunction: TDrawBitmapFunction);
begin
  FglCanvas.Draw(
    function (Bitmap:TBitmap):boolean begin
      do_ResizeBitmap( Bitmap );
      Result := ADrawBitmapFunction( Bitmap );
    end
  );
end;

function TBitmapOutput.GetCanDraw: boolean;
begin
  Result := FglCanvas.CanDraw;
end;

function TBitmapOutput.GetHint: string;
begin
  Result := FglCanvas.Hint;
end;

function TBitmapOutput.GetOnDrawData: TDataEvent;
begin
  Result := FglCanvas.OnDrawData;
end;

function TBitmapOutput.GetOnError: TIntegerEvent;
begin
  Result := FglCanvas.OnError;
end;

function TBitmapOutput.GetPenColor: TColor;
begin
  Result := FglCanvas.PenColor;
end;

function TBitmapOutput.GetVersionOfOpenGL: string;
begin
  Result := FglCanvas.Version;
end;

procedure TBitmapOutput.on_glCanvas_DblClick(Sender: TObject);
begin
  SetStrechDraw( not FStrechDraw );
end;

procedure TBitmapOutput.on_glCanvas_MouseDown(Sender:TObject;  Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssRight] then begin
    FglCanvas.Clear;
    if Assigned(FOnClear) then FOnClear(Self);
  end;
end;

procedure TBitmapOutput.SetCanDraw(const Value: boolean);
begin
  FglCanvas.CanDraw := Value;
end;

procedure TBitmapOutput.SetHint(const Value: string);
begin
  FglCanvas.Hint := Value;
end;

procedure TBitmapOutput.SetOnDrawData(const Value: TDataEvent);
begin
  FglCanvas.OnDrawData := Value;
end;

procedure TBitmapOutput.SetOnError(const Value: TIntegerEvent);
begin
  FglCanvas.OnError := Value;
end;

procedure TBitmapOutput.SetPenColor(const Value: TColor);
begin
  FglCanvas.PenColor := Value;
end;

procedure TBitmapOutput.SetStrechDraw(const Value: boolean);
begin
  FStrechDraw := Value;
  FIsScreenChanged := true;
end;

procedure TBitmapOutput.SetUseVideoAccelerator(const Value: boolean);
begin
  FUseVideoAccelerator := Value;
  FglCanvas.UseGDI := not Value;
end;

end.

