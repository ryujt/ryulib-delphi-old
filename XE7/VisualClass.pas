unit VisualClass;

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
  TVisualClasses = class;

  TVisualClass = class abstract (TObject)
  private
    FExDragStartPos : TPoint;
    FExDragPos : TPoint;
    procedure do_DrawDragBox(X,Y:integer);
  private
    FVisualClasses : TVisualClasses;
    FWidth : integer;
    FTop : integer;
    FHeight : integer;
    FLeft : integer;
    FSelected : boolean;
    procedure SetHeight(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetSelected(const Value: boolean);
  public
    constructor Create(VisualClasses:TVisualClasses); reintroduce; virtual;
    destructor Destroy; override;

    procedure Draw(Bitmap:TBitmap); virtual; abstract;
    function IsMyArea(X,Y:integer):boolean; virtual;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); virtual;
    procedure MouseMove(Shift:TShiftState; X,Y:integer); virtual;
    procedure MouseUp(Shift:TShiftState; X,Y:integer); virtual;

    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;

    procedure Refresh;

    procedure StartDrag(X,Y:integer);
    procedure Draging(X,Y:integer);
    procedure EndDrag(X,Y:integer);

    procedure BringToFront;
    procedure SendToBack;

    property VisualClasses : TVisualClasses read FVisualClasses;
    property Left : integer read FLeft write SetLeft;
    property Top : integer read FTop write SetTop;
    property Width : integer read FWidth write SetWidth;
    property Height : integer read FHeight write SetHeight;
    property Selected : boolean read FSelected write SetSelected;
  end;

  TVisualClassState = class abstract
  private
  protected
    FVisualClasses : TVisualClasses;
  public
    constructor Create(VisualClasses:TVisualClasses); reintroduce;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); virtual;
    procedure MouseMove(Shift:TShiftState; X,Y:integer); virtual;
    procedure MouseUp(Shift:TShiftState; X,Y:integer); virtual;
  end;

  TVisualClassStateNormal = class (TVisualClassState)
  private
  public
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer); override;
  end;

  TVisualClassStateSelecting = class (TVisualClassState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:integer); override;
    procedure MouseUp(Shift:TShiftState; X,Y:integer); override;
  end;

  TVisualClassStateMoving = class (TVisualClassState)
  private
  public
    procedure MouseMove(Shift:TShiftState; X,Y:integer); override;
    procedure MouseUp(Shift:TShiftState; X,Y:integer); override;
  end;

  TVisualClasses = class (TCustomControl)
  private
    FExMousePos : TPoint;
    FExDragPos : TPoint;
    FExMouseEnteredItem, FMouseEnteredItem : TVisualClass;
    FState : TVisualClassState;
    FVisualClassStateNormal : TVisualClassStateNormal;
    FVisualClassStateSelecting : TVisualClassStateSelecting;
    FVisualClassStateMoving : TVisualClassStateMoving;
    procedure do_DrawDragBox(X,Y:integer);
  private
    FDrawing : boolean;
    FItems : TList;
    FSelectedItems : TList;
    FGarbageExist : boolean;

    procedure do_Clear;
    procedure do_Add(Item:TVisualClass);
    procedure do_Remove(Item:TVisualClass);

    procedure do_ClearSelected;
    procedure do_AddSelected(Item:TVisualClass);
    procedure do_RemoveSelected(Item:TVisualClass);

    procedure do_DeleteGarbage;

    procedure do_Draw;

    function find_ObjectAt(X,Y:integer):TVisualClass;

    procedure on_MouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure on_MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);
    procedure on_MouseUp(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
  private
    FBitmap : TBitmap;
    FControl : TControl;
    FWorkingItem : TVisualClass;
    function GetItems(Index: integer): TVisualClass;
    function GetCount: integer;
    function GetSelectedItems(Index: integer): TVisualClass;
    function GetSelCount: integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginDraw;
    procedure Draw;
    procedure EndDraw;

    function Select(X,Y:integer):TVisualClass;

    procedure StartDrag(X,Y:integer);
    procedure Draging(X,Y:integer);
    procedure EndDrag(X,Y:integer);

    property Items [Index:integer] : TVisualClass read GetItems;
    property SelectedItems[Index:integer] : TVisualClass read GetSelectedItems;
  published
    property Canvas;
    property Bitmap : TBitmap read FBitmap;
    property Control : TControl read FControl write FControl;
    property Count : integer read GetCount;
    property SelCount : integer read GetSelCount;
    property WorkingItem : TVisualClass read FWorkingItem write FWorkingItem;
  end;

implementation

{ TVisualClasses }

procedure TVisualClasses.do_Add(Item: TVisualClass);
begin
  FItems.Add(Item);
end;

procedure TVisualClasses.do_AddSelected(Item: TVisualClass);
begin
  if FSelectedItems.IndexOf(Item) = -1 then FSelectedItems.Add(Item);
end;

procedure TVisualClasses.do_Clear;
var
  Loop : Integer;
  Item : TVisualClass;
begin
  for Loop := 0 to Count - 1 do begin
    Item := Items[Loop];
    if Item = nil then Continue;

    Item.Free;
  end;

  FItems.Clear;

  FGarbageExist := false;
end;

procedure TVisualClasses.do_ClearSelected;
var
  Loop : Integer;
  Item : TVisualClass;
begin
  for Loop := 0 to Count - 1 do begin
    Item := Items[Loop];
    if Item = nil then Continue;

    Item.Selected := false;
  end;

  FSelectedItems.Clear;
end;

procedure TVisualClasses.BeginDraw;
begin
  FDrawing := true;
end;

constructor TVisualClasses.Create(AOwner: TComponent);
begin
  inherited;

  FGarbageExist := false;
  FWorkingItem := nil;
  FExMouseEnteredItem := nil;

  FDrawing := false;

  FItems := TList.Create;
  FSelectedItems := TList.Create;

  FBitmap := TBitmap.Create;

  OnMouseDown := on_MouseDown;
  OnMouseMove := on_MouseMove;
  OnMouseUp   := on_MouseUp;

  FVisualClassStateNormal := TVisualClassStateNormal.Create(Self);
  FVisualClassStateSelecting := TVisualClassStateSelecting.Create(Self);
  FVisualClassStateMoving := TVisualClassStateMoving.Create(Self);

  FState := FVisualClassStateNormal;
end;

destructor TVisualClasses.Destroy;
begin
  do_ClearSelected;
  do_Clear;

  FBitmap.Free;
  FSelectedItems.Free;
  FItems.Free;

  FVisualClassStateNormal.Free;
  FVisualClassStateSelecting.Free;
  FVisualClassStateMoving.Free;

  inherited;
end;

procedure TVisualClasses.do_DeleteGarbage;
var
  Loop : Integer;
begin
  for Loop := Count-1 downto 0 do
    if FItems[Loop] = nil then FItems.Delete(Loop);
  FGarbageExist := false;
end;

procedure TVisualClasses.do_Draw;
var
  Loop : integer;
begin
  FBitmap.Width := Width;
  FBitmap.Height := Height;
  FBitmap.TransparentColor := $123456;
  FBitmap.Transparent := true;

  FBitmap.Canvas.Brush.Color := $123456;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));

  if FGarbageExist then do_DeleteGarbage;

  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then begin
      FGarbageExist := true;
      Continue;
    end;

    Items[Loop].Draw(Bitmap);
  end;

  Repaint;
end;

procedure TVisualClasses.do_DrawDragBox(X, Y: integer);
begin
  Canvas.Pen.Mode := pmXor;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FExMousePos.X, FExMousePos.Y, X, Y);
end;

procedure TVisualClasses.Draging(X, Y: integer);
begin
  do_DrawDragBox(FExDragPos.X, FExDragPos.Y);
  do_DrawDragBox(X, Y);
  FExDragPos := Point(X, Y);
end;

procedure TVisualClasses.Draw;
begin
  if FDrawing = false then do_Draw;
end;

procedure TVisualClasses.EndDrag(X, Y: integer);
begin
  do_DrawDragBox(FExDragPos.X, FExDragPos.Y);
end;

procedure TVisualClasses.EndDraw;
begin
  do_Draw;
  FDrawing := false;
end;

function TVisualClasses.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TVisualClasses.GetItems(Index: integer): TVisualClass;
begin
  Result := Pointer(FItems[Index]);
end;

function TVisualClasses.GetSelCount: integer;
begin
  Result := FSelectedItems.Count;
end;

function TVisualClasses.GetSelectedItems(Index: integer): TVisualClass;
begin
  Result := Pointer(FSelectedItems[Index]);
end;

function TVisualClasses.find_ObjectAt(X,Y:integer): TVisualClass;
var
  Loop : integer;
begin
  Result := nil;
  for Loop := Count - 1 downto 0 do begin
    if Items[Loop] = nil then Continue;

    if Items[Loop].IsMyArea(X, Y) then begin
      Result := Items[Loop];
      Break;
    end;
  end;
end;

procedure TVisualClasses.on_MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
begin
  FExMousePos := Point(X, Y);
  WorkingItem := find_ObjectAt(X, Y);
  if WorkingItem <> nil then WorkingItem.MouseDown(Button, Shift, X, Y);

  FState.MouseDown(Button, Shift, X, Y);
end;

procedure TVisualClasses.on_MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: Integer);
var
  bCondition : boolean;
begin
  FMouseEnteredItem := find_ObjectAt(X, Y);

  bCondition :=
    (FExMouseEnteredItem <> nil) and
    (FExMouseEnteredItem <> FMouseEnteredItem);
  if bCondition then begin
    FExMouseEnteredItem.MouseLeave;
    FExMouseEnteredItem := nil;
  end;

  bCondition :=
    (FState = FVisualClassStateNormal) and
    (FMouseEnteredItem <> nil) and
    (FMouseEnteredItem <> FExMouseEnteredItem);
  if bCondition then begin
    FMouseEnteredItem.MouseEnter;
    FExMouseEnteredItem := FMouseEnteredItem;
  end;

  FState.MouseMove(Shift, X, Y);
end;

procedure TVisualClasses.on_MouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
begin
  FState.MouseUp(Shift, X, Y);

  if WorkingItem <> nil then WorkingItem.MouseUp(Shift, X, Y);
end;

procedure TVisualClasses.Paint;
begin
  if csDesigning in ComponentState then
     with inherited Canvas do begin
       Pen.Style :=   psDash;
       Brush.Style := bsClear;
       Rectangle(0, 0, Width, Height);
     end;
  if Width*Height = 0 then Exit;

  Canvas.Draw(0, 0, Bitmap);
end;

procedure TVisualClasses.do_Remove(Item: TVisualClass);
var
  iIndex : integer;
begin
  Item.Selected := false;

  iIndex := FItems.IndexOf(Item);
  if iIndex = -1 then Exit;

  FItems[iIndex] := nil;
end;

procedure TVisualClasses.do_RemoveSelected(Item: TVisualClass);
begin
  FSelectedItems.Remove(Item);
end;

function TVisualClasses.Select(X, Y: integer): TVisualClass;
var
  Loop : integer;
  Item : TVisualClass;
begin
  Result := nil;

  if FGarbageExist then do_DeleteGarbage;

  for Loop := Count - 1 downto 0 do begin
    Item := Items[Loop];
    if Item = nil then begin
      FGarbageExist := true;
      Continue;
    end;

    if Item.IsMyArea(X, Y) then begin
      Result := Item;
      Break;
    end;
  end;
end;

procedure TVisualClasses.StartDrag(X, Y: integer);
begin
  do_DrawDragBox(X, Y);
  FExDragPos := Point(X, Y);
end;

{ TVisualClass }

procedure TVisualClass.BringToFront;
var
  pTemp : pointer;
  iIndex : integer;
begin
  // 이런 경우는 발생하지 않는다.  하지만, 안전장치.
  if VisualClasses.Count = 0 then Exit;

  // 이런 경우는 발생하지 않는다.  하지만, 안전장치.
  iIndex := VisualClasses.FItems.IndexOf(Self);
  if iIndex = -1 then Exit;

  pTemp := VisualClasses.FItems[iIndex];
  VisualClasses.FItems[iIndex] := VisualClasses.FItems[VisualClasses.Count-1];
  VisualClasses.FItems[VisualClasses.Count-1] := pTemp;

  Refresh;
end;

constructor TVisualClass.Create(VisualClasses:TVisualClasses);
begin
  inherited Create;

  FVisualClasses := VisualClasses;     
  FSelected := false;

  VisualClasses.do_Add(Self);
end;

destructor TVisualClass.Destroy;
begin
  VisualClasses.do_Remove(Self);

  inherited;
end;

procedure TVisualClass.do_DrawDragBox(X, Y: integer);
begin
  VisualClasses.Canvas.Pen.Mode := pmXor;
  VisualClasses.Canvas.Pen.Width := 1;
  VisualClasses.Canvas.Pen.Style := psDot;
  VisualClasses.Canvas.Pen.Color := clWhite;
  VisualClasses.Canvas.Brush.Style := bsClear;
  VisualClasses.Canvas.Rectangle(X, Y, X+Width, Y+Height);
end;

procedure TVisualClass.Draging(X, Y: integer);
begin
  do_DrawDragBox(FExDragPos.X, FExDragPos.Y);

  X := Left + X - FExDragStartPos.X;
  Y := Top  + Y - FExDragStartPos.Y;
  do_DrawDragBox(X, Y);
  FExDragPos := Point(X, Y);
end;

procedure TVisualClass.EndDrag(X, Y: integer);
begin
  do_DrawDragBox(FExDragPos.X, FExDragPos.Y);
end;

function TVisualClass.IsMyArea(X, Y: integer): boolean;
begin
  Result:=
    ((X >= Left) and (X <= Left+Width)) and
    ((Y >= Top)  and (Y <= Top+Height));
end;

procedure TVisualClass.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
end;

procedure TVisualClass.MouseEnter;
begin
end;

procedure TVisualClass.MouseLeave;
begin
end;

procedure TVisualClass.MouseMove(Shift: TShiftState; X, Y: integer);
begin
end;

procedure TVisualClass.MouseUp(Shift: TShiftState; X, Y: integer);
begin
end;

procedure TVisualClass.Refresh;
begin
  if VisualClasses <> nil then VisualClasses.Draw;
end;

procedure TVisualClass.SendToBack;
var
  pTemp : pointer;
  iIndex : integer;
begin
  // 이런 경우는 발생하지 않는다.  하지만, 안전장치.
  if VisualClasses.Count = 0 then Exit;

  // 이런 경우는 발생하지 않는다.  하지만, 안전장치.
  iIndex := VisualClasses.FItems.IndexOf(Self);
  if iIndex = -1 then Exit;

  pTemp := VisualClasses.FItems[iIndex];
  VisualClasses.FItems[iIndex] := VisualClasses.FItems[0];
  VisualClasses.FItems[0] := pTemp;

  Refresh;
end;

procedure TVisualClass.SetHeight(const Value: integer);
begin
  if FHeight = Value then Exit;

  FHeight := Value;
  Refresh;
end;

procedure TVisualClass.SetLeft(const Value: integer);
begin
  if FLeft = Value then Exit;

  FLeft := Value;
  Refresh;
end;

procedure TVisualClass.SetSelected(const Value: boolean);
begin
  if FSelected = Value then Exit;  

  FSelected := Value;
  Refresh;

  if Value then VisualClasses.do_AddSelected(Self)
  else VisualClasses.do_RemoveSelected(Self);
end;

procedure TVisualClass.SetTop(const Value: integer);
begin
  if FTop = Value then Exit;

  FTop := Value;
  Refresh;
end;

procedure TVisualClass.SetWidth(const Value: integer);
begin
  if FWidth = Value then Exit;

  FWidth := Value;
  Refresh;
end;

procedure TVisualClass.StartDrag(X, Y: integer);
begin
  FExDragStartPos := Point(X, Y);

  do_DrawDragBox(Left, Top);
  FExDragPos := Point(Left, Top);
end;

{ TVisualClassState }

constructor TVisualClassState.Create(VisualClasses: TVisualClasses);
begin
  inherited Create;

  FVisualClasses := VisualClasses;
end;

procedure TVisualClassState.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  raise EAbort.Create(ClassName+'.MouseDown');
end;

procedure TVisualClassState.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  raise EAbort.Create(ClassName+'.MouseMove');
end;

procedure TVisualClassState.MouseUp(Shift: TShiftState; X, Y: integer);
begin
  raise EAbort.Create(ClassName+'.MouseUp');
end;

{ TVisualClassStateNormal }

procedure TVisualClassStateNormal.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Loop : integer;
begin
  if not (ssLeft in Shift) then Exit;

  if FVisualClasses.WorkingItem = nil then begin
    FVisualClasses.BeginDraw;
    try
      FVisualClasses.do_ClearSelected;
    finally
      FVisualClasses.EndDraw;
    end;
    
    FVisualClasses.StartDrag(X, Y);
    FVisualClasses.FState := FVisualClasses.FVisualClassStateSelecting;
  end else begin
    FVisualClasses.BeginDraw;
    try
      if (FVisualClasses.WorkingItem.Selected = false) and (not (ssCtrl in Shift)) then begin
        FVisualClasses.do_ClearSelected;
        FVisualClasses.WorkingItem.Selected := true;
      end else if (FVisualClasses.WorkingItem.Selected = true) and (ssCtrl in Shift) then FVisualClasses.WorkingItem.Selected := false
      else FVisualClasses.WorkingItem.Selected := true;
    finally
      FVisualClasses.EndDraw;
    end;

    // 드래그 하는 동안은 XOr로 박스만 그린다.
    for Loop := 0 to FVisualClasses.FSelectedItems.Count - 1 do begin
      FVisualClasses.WorkingItem := Pointer(FVisualClasses.FSelectedItems[Loop]);
      FVisualClasses.WorkingItem.StartDrag(X, Y);
    end;

    FVisualClasses.FState := FVisualClasses.FVisualClassStateMoving;
  end;
end;

procedure TVisualClassStateNormal.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if FVisualClasses.FMouseEnteredItem <> nil then FVisualClasses.Cursor := crHandPoint
  else FVisualClasses.Cursor := crDefault;
end;

{ TVisualClassStateSelecting }

procedure TVisualClassStateSelecting.MouseMove(Shift: TShiftState; X,
  Y: integer);
begin
  FVisualClasses.Draging(X, Y);
end;

procedure Swap(var a,b:integer);
var
  c : integer;
begin
  c := a;
  a := b;
  b := c;
end;

function BoxIn(X,Y:integer; Rect:TRect):boolean;
begin
  Result :=
    ((X >= Rect.Left) and (X <= Rect.Right)) and
    ((Y >= Rect.Top)  and (Y <= Rect.Bottom));
end;

procedure TVisualClassStateSelecting.MouseUp(Shift: TShiftState; X, Y: integer);
var
  Loop : integer;
  Box : TRect;
  Item : TVisualClass;
begin
  FVisualClasses.EndDrag(X, Y);

  if FVisualClasses.FExMousePos.X > X then Swap(FVisualClasses.FExMousePos.X, X);
  if FVisualClasses.FExMousePos.Y > Y then Swap(FVisualClasses.FExMousePos.Y, Y);

  Box := Rect(FVisualClasses.FExMousePos.X, FVisualClasses.FExMousePos.Y, X, Y);

  FVisualClasses.BeginDraw;
  try
    for Loop := 0 to FVisualClasses.FItems.Count - 1 do begin
      Item := Pointer(FVisualClasses.FItems[Loop]);
      Item.Selected :=
        BoxIn(Item.Left, Item.Top, Box) and
        BoxIn(Item.Left+Item.Width, Item.Top, Box) and
        BoxIn(Item.Left, Item.Top+Item.Height, Box) and
        BoxIn(Item.Left+Item.Width, Item.Top+Item.Height, Box);
    end;
  finally
    FVisualClasses.EndDraw;
  end;

  FVisualClasses.FState := FVisualClasses.FVisualClassStateNormal;
end;

{ TVisualClassStateSelectedMoving }

procedure TVisualClassStateMoving.MouseMove(Shift: TShiftState; X,
  Y: integer);
var
  Loop : integer;
  Item : TVisualClass;
begin
  for Loop := 0 to FVisualClasses.FSelectedItems.Count - 1 do begin
    Item := Pointer(FVisualClasses.FSelectedItems[Loop]);
    Item.Draging(X, Y);
  end;
end;

procedure TVisualClassStateMoving.MouseUp(Shift: TShiftState; X,
  Y: integer);
var
  Loop : integer;
  Item : TVisualClass;
begin
  FVisualClasses.BeginDraw;
  try
    for Loop := 0 to FVisualClasses.FSelectedItems.Count - 1 do begin
      Item := Pointer(FVisualClasses.FSelectedItems[Loop]);

      Item.EndDrag(X, Y);

      Item.Left := Item.Left + X - FVisualClasses.FExMousePos.X;
      Item.Top  := Item.Top  + Y - FVisualClasses.FExMousePos.Y;
    end;
  finally
    FVisualClasses.EndDraw;
  end;

  FVisualClasses.FState := FVisualClasses.FVisualClassStateNormal;
end;

end.
