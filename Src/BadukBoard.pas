unit BadukBoard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

const
  _BadukBoardWidth = 19;  

type
  TLineState = (
    lsLT, lsMT, lsRT,
    lsLM, lsMM, lsRM,
    lsLB, lsMB, lsRB
  );

  TStoneState = (ssNone, ssBlack, ssWhite);

  TBoardClickEvent = procedure (Sender:TObject; IndexX,IndexY:integer) of object;

  TBadukBlock = class
  private
    procedure do_DrawStone(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer; AStoneState:TStoneState);
    procedure do_LT(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_MT(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_RT(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_LM(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_MM(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_RM(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_LB(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_MB(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
    procedure do_RB(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
  public
    LineState : TLineState;
    StoneState : TStoneState;
    procedure Draw(ACanvas:TCanvas; AX,AY:integer; ABlockSize:integer);
  end;

  TBadukBoard = class (TGraphicControl)
  private
    FMouseDownX, FMouseDownY : integer;
    FStartX, FStartY : integer; // ¹ÙµÏÆÇÀÌ ±×·ÁÁú ÁÂ»ó´Ü ÁÂÇ¥
    FBlockSize : integer;
    FBlocks : array [0.._BadukBoardWidth-1, 0.._BadukBoardWidth-1] of TBadukBlock;
    procedure do_CreateBlocks;
    procedure do_PrepareCoordinates;
    procedure do_PaintBackground;
    procedure do_PaintBlocks;
  private
    FOnBoardClick: TBoardClickEvent;
    function GetCells(IndexX, IndexY: integer): TStoneState;
  protected
    procedure Paint; override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PutStone(AX,AY:integer; AStoneState:TStoneState);

    property Cells[IndexX,IndexY:integer] : TStoneState read GetCells;
  published
    property Align;
    property OnBoardClick : TBoardClickEvent read FOnBoardClick write FOnBoardClick; 
  end;

procedure Register;

procedure ToggleStoneState(var AStoneState:TStoneState);

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBadukBoard]);
end;

procedure ToggleStoneState(var AStoneState:TStoneState);
begin
  case AStoneState of
    ssNone: ;
    ssBlack: AStoneState := ssWhite;
    ssWhite: AStoneState := ssBlack;
  end;  
end;

{ TBadukBoard }

constructor TBadukBoard.Create(AOwner: TComponent);
begin
  inherited;

  FStartX := 0;
  FStartY := 0;

  do_CreateBlocks;
end;

destructor TBadukBoard.Destroy;
var
  LoopX, LoopY : Integer;
begin
  for LoopX := 0 to _BadukBoardWidth - 1 do
  for LoopY := 0 to _BadukBoardWidth - 1 do FBlocks[LoopX, LoopY].Free;

  inherited;
end;

procedure TBadukBoard.do_CreateBlocks;
var
  LoopX, LoopY : Integer;
begin
  for LoopX := 0 to _BadukBoardWidth - 1 do
  for LoopY := 0 to _BadukBoardWidth - 1 do begin
    FBlocks[LoopX, LoopY] := TBadukBlock.Create;
    FBlocks[LoopX, LoopY].StoneState := ssNone;
    FBlocks[LoopX, LoopY].LineState  := lsMM;
  end;

  for LoopX := 0 to _BadukBoardWidth - 1 do FBlocks[LoopX, 0].LineState := lsMT;
  for LoopX := 0 to _BadukBoardWidth - 1 do FBlocks[LoopX, _BadukBoardWidth-1].LineState := lsMB;

  for LoopY := 0 to _BadukBoardWidth - 1 do FBlocks[0, LoopY].LineState := lsLM;
  for LoopY := 0 to _BadukBoardWidth - 1 do FBlocks[_BadukBoardWidth-1, LoopY].LineState := lsRM;

  FBlocks[0, 0].LineState := lsLT;
  FBlocks[_BadukBoardWidth-1, 0].LineState := lsRT;
  FBlocks[0, _BadukBoardWidth-1].LineState := lsLB;
  FBlocks[_BadukBoardWidth-1, _BadukBoardWidth-1].LineState := lsRB;
end;

procedure TBadukBoard.do_PaintBackground;
begin
  Canvas.Brush.Color := $0056E0EF;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TBadukBoard.do_PaintBlocks;
var
  LoopX, LoopY : Integer;
begin
  for LoopX := 0 to _BadukBoardWidth - 1 do
  for LoopY := 0 to _BadukBoardWidth - 1 do
    FBlocks[LoopX, LoopY].Draw(
      Canvas,
      FStartX + LoopX*FBlockSize,
      FStartY + LoopY*FBlockSize,
      FBlockSize
    );
end;

procedure TBadukBoard.do_PrepareCoordinates;
var
  iBoardSize : integer;
begin
  iBoardSize := Width;
  FStartX := 0;
  FStartY := (Height - iBoardSize) div 2;

  if iBoardSize > Height then begin
    iBoardSize := Height;
    FStartY := 0;
    FStartX := (Width - iBoardSize) div 2;
  end;

  FBlockSize := iBoardSize div _BadukBoardWidth;
end;

function TBadukBoard.GetCells(IndexX, IndexY: integer): TStoneState;
begin
  Result := FBlocks[IndexX, IndexY].StoneState;
end;

procedure TBadukBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  X := X - FStartX;
  Y := Y - FStartY;

  X := X div FBlockSize;
  Y := Y div FBlockSize;
  
  FMouseDownX := X;
  FMouseDownY := Y;
end;

procedure TBadukBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Assigned(FOnBoardClick) then begin
    X := X - FStartX;
    Y := Y - FStartY;

    X := X div FBlockSize;
    Y := Y div FBlockSize;

    if (FMouseDownX = X) and (FMouseDownY = Y) then FOnBoardClick(Self, X, Y);
  end;
end;

procedure TBadukBoard.Paint;
begin
  inherited;

  if csDesigning in ComponentState then
  with inherited Canvas do begin
    Pen.Style := psDash;
    Brush.Style := bsClear;
    Rectangle(0, 0, Width, Height);
  end;

  do_PaintBackground;
  do_PrepareCoordinates;
  do_PaintBlocks;
end;

procedure TBadukBoard.PutStone(AX, AY: integer; AStoneState: TStoneState);
begin
  if (AX in [0.._BadukBoardWidth-1]) and (AY in [0.._BadukBoardWidth-1]) then begin
    FBlocks[AX, AY].StoneState := AStoneState;
    FBlocks[AX, AY].Draw(Canvas, FStartX + AX*FBlockSize, FStartY + AY*FBlockSize, FBlockSize);
  end;
end;

{ TBadukBlock }

procedure TBadukBlock.do_DrawStone(ACanvas: TCanvas; AX, AY, ABlockSize: integer; AStoneState: TStoneState);
begin
  case StoneState of
    ssNone: Exit;

    ssBlack: begin
      ACanvas.Pen.Color := clWhite;
      ACanvas.Brush.Color := clBlack;
    end;

    ssWhite: begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Brush.Color := clWhite;
    end;
  end;
  
  ACanvas.Ellipse(AX, AY, AX+ABlockSize, AY+ABlockSize);
end;

procedure TBadukBlock.do_LB(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY + iHalf);
end;

procedure TBadukBlock.do_LM(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);

  ACanvas.MoveTo(AX + iHalf, AY + iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY + iHalf);
end;

procedure TBadukBlock.do_LT(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY+ iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY+ iHalf);

  ACanvas.MoveTo(AX + iHalf, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);
end;

procedure TBadukBlock.do_MB(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX, AY+ iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY+ iHalf);

  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);
end;

procedure TBadukBlock.do_MM(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);

  ACanvas.MoveTo(AX, AY + iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY + iHalf);
end;

procedure TBadukBlock.do_MT(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX, AY + iHalf);
  ACanvas.LineTo(AX + ABlockSize, AY + iHalf);

  ACanvas.MoveTo(AX + iHalf, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);
end;

procedure TBadukBlock.do_RB(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);

  ACanvas.MoveTo(AX, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);
end;

procedure TBadukBlock.do_RM(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX + iHalf, AY);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);

  ACanvas.MoveTo(AX, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);
end;

procedure TBadukBlock.do_RT(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
var
  iHalf : integer;
begin
  iHalf := ABlockSize div 2;
  ACanvas.MoveTo(AX, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + iHalf);
  ACanvas.LineTo(AX + iHalf, AY + ABlockSize);
end;

procedure TBadukBlock.Draw(ACanvas: TCanvas; AX, AY, ABlockSize: integer);
begin
  ACanvas.Pen.Color := clBlack;
  case LineState of
    lsLT: do_LT(ACanvas, AX, AY, ABlockSize);
    lsMT: do_MT(ACanvas, AX, AY, ABlockSize);
    lsRT: do_RT(ACanvas, AX, AY, ABlockSize);
    lsLM: do_LM(ACanvas, AX, AY, ABlockSize);
    lsMM: do_MM(ACanvas, AX, AY, ABlockSize);
    lsRM: do_RM(ACanvas, AX, AY, ABlockSize);
    lsLB: do_LB(ACanvas, AX, AY, ABlockSize);
    lsMB: do_MB(ACanvas, AX, AY, ABlockSize);
    lsRB: do_RB(ACanvas, AX, AY, ABlockSize);
  end;

  do_DrawStone(ACanvas, AX, AY, ABlockSize, StoneState);
end;

end.
