unit NormalBitmap;

interface

uses
  RyuLibBase, BitmapOutputCanvas, RyuGraphics, BitmapData, AcrylBoard,
  SyncObjs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus;

type
  TfmNormalBitmap = class(TBitmapOutputCanvas)
    ScrollBox: TScrollBox;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  protected
    function GetScreenHint: string; override;
    procedure SetScreenHint(const Value: string); override;
  private
    FCS : TCriticalSection;
    FBitmapData : TBitmapData;
  private
    FAcrylBoard : TAcrylBoard;
    procedure on_AcrylBoard_MouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure on_AcrylBoard_DblClick(Sender: TObject);
    procedure set_ImageToCenter;
  private
    FStretch: boolean;
    FOnRightClick: TNotifyEvent;
    procedure SetStretch(const Value: boolean);
    function GetOnDrawData: TDataEvent;
    procedure SetOnDrawData(const Value: TDataEvent);
    function GetPenColor: TColor;
    procedure SetPenColor(const Value: TColor);
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCanvasWidth:integer; override;
    function GetCanvasHeight:integer; override;

    procedure DrawBitmap(ABitmap:TBitmap); override;

    procedure ClearAcrylBoard;
  public
    property ScreenHint : string read GetScreenHint write SetScreenHint;
    property Stretch : boolean read FStretch write SetStretch;
    property CanDraw : boolean read GetCanDraw write SetCanDraw;
    property PenColor : TColor read GetPenColor write SetPenColor;
    property OnRightClick : TNotifyEvent read FOnRightClick write FOnRightClick;
    property OnDrawData : TDataEvent read GetOnDrawData write SetOnDrawData;
  end;

implementation

{$R *.dfm}

{ TfmNormalBitmap }

procedure TfmNormalBitmap.ClearAcrylBoard;
begin
  FAcrylBoard.Clear;
end;

constructor TfmNormalBitmap.Create(AOwner: TComponent);
begin
  inherited;

  FStretch := true;

  MakeOpaque(Self);

  FCS := TCriticalSection.Create;
  FBitmapData := TBitmapData.Create;

  FAcrylBoard := TAcrylBoard.Create(Self);
  FAcrylBoard.AutoSize := true;
  FAcrylBoard.Parent := Self;
  FAcrylBoard.Bitmap.PixelFormat := pf32bit;
  FAcrylBoard.OnMouseDown := on_AcrylBoard_MouseDown;
  FAcrylBoard.OnDblClick := on_AcrylBoard_DblClick;
end;

destructor TfmNormalBitmap.Destroy;
begin
  FCS.Acquire;
  try
    FreeAndNil(FBitmapData);
  finally
    FCS.Release;
  end;

  FreeAndNil(FCS);
  FreeAndNil(FAcrylBoard);

  inherited;
end;

procedure TfmNormalBitmap.DrawBitmap(ABitmap: TBitmap);
begin
  try
    FCS.Acquire;
    try
      FBitmapData.Assign( ABitmap );
    finally
      FCS.Release;
    end;
  except
    on E : Exception do raise Exception.Create('TfmNormalBitmap.DrawBitmap (AssignBitmap) - ' + E.Message);
  end;
end;

function TfmNormalBitmap.GetCanDraw: boolean;
begin
  Result := FAcrylBoard.CanDraw;
end;

function TfmNormalBitmap.GetCanvasHeight: integer;
begin
  if Stretch then Result := Height
  else Result := 0;
end;

function TfmNormalBitmap.GetCanvasWidth: integer;
begin
  if Stretch then Result := Width
  else Result := 0;
end;

function TfmNormalBitmap.GetOnDrawData: TDataEvent;
begin
  Result := FAcrylBoard.OnDrawData;
end;

function TfmNormalBitmap.GetPenColor: TColor;
begin
  Result := FAcrylBoard.PenColor;
end;

function TfmNormalBitmap.GetScreenHint: string;
begin
  Result := FAcrylBoard.Hint;
end;

procedure TfmNormalBitmap.on_AcrylBoard_DblClick(Sender: TObject);
begin
  Stretch := not Stretch;
end;

procedure TfmNormalBitmap.on_AcrylBoard_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Shift = [ssRight]) and Assigned(FOnRightClick) then FOnRightClick(Self);
end;

procedure TfmNormalBitmap.SetCanDraw(const Value: boolean);
begin
  FAcrylBoard.CanDraw := Value;
end;

procedure TfmNormalBitmap.SetOnDrawData(const Value: TDataEvent);
begin
  FAcrylBoard.OnDrawData := Value;
end;

procedure TfmNormalBitmap.SetPenColor(const Value: TColor);
begin
  FAcrylBoard.PenColor := Value;
end;

procedure TfmNormalBitmap.SetScreenHint(const Value: string);
begin
  FAcrylBoard.Hint := Value;
end;

procedure TfmNormalBitmap.SetStretch(const Value: boolean);
begin
  FStretch := Value;

  if Value then begin
    FAcrylBoard.Parent := Self;

    set_ImageToCenter;

    ScrollBox.Visible := false;
  end else begin
    ScrollBox.Visible := true;

    FAcrylBoard.Parent := ScrollBox;
    FAcrylBoard.Left := 0;
    FAcrylBoard.Top  := 0;
  end;

  if Assigned(OnStretchChanged) then OnStretchChanged(Self);
end;

procedure TfmNormalBitmap.set_ImageToCenter;
var
  iLeft, iTop : integer;
begin
  iLeft := (Width  div 2) - (FAcrylBoard.Bitmap.Width  div 2);
  iTop  := (Height div 2) - (FAcrylBoard.Bitmap.Height div 2);

  if iLeft <> FAcrylBoard.Left then FAcrylBoard.Left := iLeft;
  if iTop  <> FAcrylBoard.Top  then FAcrylBoard.Top  := iTop;
end;

procedure TfmNormalBitmap.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  try
    if not FBitmapData.Changed then Exit;

    FCS.Acquire;
    try
      if FBitmapData.GetBitmap(FAcrylBoard.Bitmap) then FAcrylBoard.Invalidate;
    finally
      FCS.Release;
    end;

    if Stretch then set_ImageToCenter;
  finally
    Timer.Enabled := true;
  end;
end;

end.
