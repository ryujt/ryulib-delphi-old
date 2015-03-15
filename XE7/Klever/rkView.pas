unit rkView;

//  rkView © 2010 by Roy Magne Klever. All rights reserved
//
//  This file is not distributable without permission by Roy Magne Klever
//  WEB: www.rmklever.com
//  Mail: roymagne@rmklever.com
//
//  version 1.2, april 2010

interface

uses
  Windows, Messages, WinTypes, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StrUtils, ExtCtrls, Math, Stdctrls, Buttons,
  rkIntegerList;

const
  CM_REFRESH = WM_USER + 2000; // Custom Message...

type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;
  TLine24 = array[0..0] of TRGB24;
  PLine24 = ^TLine24;

  TWMMouseWheel = record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TsvItemState = (svNormal, svSelected, svHot, svHotSelected);

  // View events...
  TOnSelectingEvent = procedure(Sender: TObject; Count: Integer) of
    object;
  TOnHeaderPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Header:
    TRect;
    Offset, Active: Integer; State: TsvItemState; Columns: array of Integer) of
      object;
  TOnCellPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    IdxA, Idx: Integer; State: TsvItemState) of object;
  TOnListPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    IdxA, Idx: Integer; State: TsvItemState; Columns: array of Integer) of
      object;
  TOnCellSelectEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    Idx: Integer; var Selected: Boolean) of object;
  TOnCellHitEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Idx, x, y: Integer; var Selected: Boolean) of object;
  TOnHeaderClickEvent = procedure(Sender: TObject; Column: Integer) of object;
  TOnHintShowEvent = procedure(Sender: Tobject; idx, column: Integer;
    var text: string; var Show: Boolean) of object;

  /// Main class
  TrkCustomView = class(TCustomControl)
  private
    FFocused: Boolean;
    FModified: Boolean;
    FMouseItem: Integer;
    FMouseAtEnd: Boolean;
    FDownTick: DWord;
    FDownX: Integer;
    FDownY: Integer;
    FHeaderHot: Integer;
    FHeaderState: TsvItemState;
    FDragColumn: Integer;
    FDragging: Boolean;
    FDragOffs: Integer;
    FDragStartX: Integer;
    FDragStartSize: Integer;
    InHeader: Boolean;
    FCtrlSize: Integer;
    CellColSize: Integer;
    SelScroll: boolean;
    SelStart, SelEnd: tPoint;
    viewDeltaScroll, ScrollOffset: Integer;
    viewDeltaScrollX, ScrollOffsetX: Integer;
    maxXScroll: Integer;
    SelTimer: TTimer;
    Ra, Ga, Ba: array[0..255] of Byte;
    FMainBmp: TBitmap;
    FColorSelection: TRGB24;
    SCUp: Boolean;
    SCY, SCPos, SCValue: Integer;
    sbVert: TScrollBar;
    sbHorz: TScrollBar;
    FScrollValOld: integer;
    FScrollValNew: integer;
    FScrollPos: Integer;
    Range: Integer;
    hsbVisible: Boolean;
    vsbVisible: Boolean;
    YAdjOff: Integer;
    pnlVert, pnlHorz, pnlDummy: TPanel;
    dblClicked: boolean;
    FSelected: Integer;
    FHotTracking: Boolean;
    FPaintGrid: Boolean;
    FMulti: Boolean;
    FListWidth: Integer;
    FColumns: array of Integer;
    FHintColumn: Integer;
    FLastIndex: Integer;
    FNrSelected: Integer;
    FItemArrange: Boolean;
    FPopupID: Integer;
    FBorderStyle: TBorderStyle;
    FFullRepaint: Boolean;
    FLocked: Boolean;
    FTransparent: Boolean;
    FUpdating: Boolean;
    FOnResize: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnDblClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FAlignment: TAlignment;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMRefresh(var Message: TMessage); message CM_REFRESH;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure OnVertScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure CalcSelectionSimple;
    procedure CalcSelection;
    function GetVertPos: Integer;
    procedure MoveSelectedTo(idx: Integer);
    procedure SetTransparent(const Value: Boolean);
  protected
    FBackColor: TColor;
    FCellAuto: Boolean;
    FCellHeight: Integer;
    FCellOffset: Integer;
    FCellSelect: Boolean;
    FCellSpace: Integer;
    FCellSpaceX: Integer;
    FCellWidth: Integer;
    FCellXOffset: Integer;
    FCenterView: Boolean;
    FHeaderHeight: Integer;
    FHeaderVisible: Boolean;
    FLinesColor: TColor;
    FReArrange: Boolean;
    FUseAsList: Boolean;
    FOnSelecting: TOnSelectingEvent;
    FOnCellSelect: TOnCellSelectEvent;
    FOnCellHit: TOnCellHitEvent;
    FOnHeaderPaint: TOnHeaderPaintEvent;
    FOnHeaderClick: TOnHeaderClickEvent;
    FOnCellPaint: TOnCellPaintEvent;
    FOnListPaint: TOnListPaintEvent;
    FOnHintShow: TOnHintShowEvent;
    FCanFocused: boolean;
    function GetColumns: string;
    procedure PaintView;
    procedure PaintWindow(DC: HDC); override;
    procedure SetColumns(Value: string);
    procedure SetUseAsList(Value: Boolean);
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure SetCanFocused(Value: Boolean);
    procedure SetSelColor(Value: TColor);
    function GetSelColor: TColor;
    function IsFocused: Boolean;
    procedure SimpleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DblClick; override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    function SimpleDoKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnSelScroll(Sender: Tobject);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    IsEditing: Boolean;
    BugStr: string;
    BugCount: Integer;
    Items: TIntList;
    Selection: TIntList;
    IdxItem: Integer;
    LastIdxItem: Integer;
    HotIdx: Integer;
    ViewIdx: Integer;
    Selecting: Boolean;
    FSelRect, FPrevSel, SelRect: TRect;
    SelX, SelY: Integer;
    HotX, HotY: Integer;
    ImgWidth, ImgHeight, ViewColumns, ViewRows: Integer;
    ScrollMode: Boolean;
    function VtScrollbar: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HzScrollbar: Boolean;
    procedure CalcViewSimple;
    procedure CalcHorzSB;
    procedure CalcView(Clear: Boolean);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateView;
    procedure Clear;
    function TextWidth(text: string): Integer;
    function GetColumnSize(Column: Integer): Integer;
    procedure SetInView(grp, idx: Integer);
    procedure SetAtTop(grp, idx: Integer);
    function GetItemRect(grp, idx: Integer): TRect;
    function GetItemAtXY(pt: TPoint): Integer;
    function IdxAtXY(pt: TPoint; HitTest: Boolean): integer;
    function ItemAtXY(pt: TPoint; HitTest: Boolean): integer;
    function ItemCloseToXY(X, Y: Integer): integer;
    procedure SelectAll(Inverted: Boolean);
    function Scroll(delta, value: integer): integer;
    procedure Resize; override;
    property CellAutoAdj: Boolean read FCellAuto write FCellAuto default False;
    property CellHeight: Integer read FCellHeight write FCellHeight default 150;
    property CellOffset: Integer read FCellOffset write FCellOffset default 10;
    property CellSelect: Boolean read FCellSelect write FCellSelect default False;
    property CellSpace: Integer read FCellSpace write FCellSpace default 10;
    property CellWidth: Integer read FCellWidth write FCellWidth default 150;
    property CenterView: Boolean read FCenterView write FCenterView default False;
    property Color: TColor read FBackColor write FBackColor default clWindow;
    property ColorLines: TColor read FLinesColor write FLinesColor default clSilver;
    property Columns: string read GetColumns write SetColumns;
    property ColorSel: TColor read GetSelColor write SetSelColor default clHighlight;
    property HeaderHeight: Integer read FHeaderHeight write FHeaderHeight default 22;
    property HeaderVisible: Boolean read FHeaderVisible write FHeaderVisible default False;
    property ItemDragging: Boolean read FReArrange write FReArrange default False;
    property Modified: Boolean read FModified;
    property NumberSel: Integer read FNrSelected default 0;
    property ParentColor;
    property Selected: Integer read FSelected;
    property UseAsList: Boolean read FUseAsList write SetUseAsList default False;
    property VertScrollPos: Integer read GetVertPos;
    property OnCellHit: TOnCellHitEvent read FOnCellHit write FOnCellHit;
    property OnCellPaint: TOnCellPaintEvent read FOnCellPaint write FOnCellPaint;
    property OnCellSelect: TOnCellSelectEvent read FOnCellSelect write FOnCellSelect;
    property OnHeaderClick: TOnHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderPaint: TOnHeaderPaintEvent read FOnHeaderPaint write FOnHeaderPaint;
    property OnListPaint: TOnListPaintEvent read FOnListPaint write FOnListPaint;
  published
    // Published
    property Align;
    property Anchors;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property Hint;
    property Visible;
    property TabOrder;
    property Ctl3D;
    property ParentCtl3D;
    property DragCursor;
    property DragMode;
    property MultipleSelection: Boolean read FMulti write FMulti default False;
    property HotTracking: Boolean read FHotTracking write FHotTracking default False;
    property PaintGrid: Boolean read FPaintGrid write FPaintGrid default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font;
    property ParentFont default False;
    property TabStop;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FonMouseMove write FOnMouseMove;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnStartDrag;
    property OnHintShow: TOnHintShowEvent read FOnHintShow write FOnHintShow;
    property OnSelecting: TOnSelectingEvent read FOnSelecting write FOnSelecting;
  end;

  // main class
  TrkView = class(TrkCustomView)
  published
    // Published
    property CellWidth: Integer read FCellWidth write FCellWidth default 150;
    property CellHeight: Integer read FCellHeight write FCellHeight default 150;
    property CellOffset: Integer read FCellOffset write FCellOffset default 10;
    property CellSpace: Integer read FCellSpace write FCellSpace default 10;
    property HeaderHeight: Integer read FHeaderHeight write FHeaderHeight default 22;
    property CellAutoAdj: Boolean read FCellAuto write FCellAuto default False;
    property UseAsList: Boolean read FUseAsList write SetUseAsList default False;
    property CenterView: Boolean read FCenterView write FCenterView default False;
    property CellSelect: Boolean read FCellSelect write FCellSelect default False;
    property HeaderVisible: Boolean read FHeaderVisible write FHeaderVisible default False;
    property Columns: string read GetColumns write SetColumns;
    property Color: TColor read FBackColor write FBackColor default clWindow;
    property ColorSel: TColor read GetSelColor write SetSelColor default clHighlight;
    property ColorLines: TColor read FLinesColor write FLinesColor default clSilver;
    property ParentColor;
    property ItemDragging: Boolean read FReArrange write FReArrange default False;
    property OnCellHit: TOnCellHitEvent read FOnCellHit write FOnCellHit;
    property OnCellSelect: TOnCellSelectEvent read FOnCellSelect write FOnCellSelect;
    property OnHeaderPaint: TOnHeaderPaintEvent read FOnHeaderPaint write FOnHeaderPaint;
    property OnHeaderClick: TOnHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnCellPaint: TOnCellPaintEvent read FOnCellPaint write FOnCellPaint;
    property OnListPaint: TOnListPaintEvent read FOnListPaint write FOnListPaint;
  end;

procedure Register;

implementation

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
{$IFDEF DFS_COMPILER_2}
    GetViewportOrgEx(DC, @Position);
{$ELSE}
    GetViewportOrgEx(DC, Position);
{$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

constructor TrkCustomView.Create(AOwner: TComponent);
var
  w, h: integer;
begin
  inherited;
  bugStr := '';
  bugCount := 0;
  w := GetSystemMetrics(SM_CXVSCROLL); // Width of a vertical scrollbar...
  h := GetSystemMetrics(SM_CXHSCROLL); // Width of a horizontal scrollbar...
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls, csReplicatable,
    csNeedsBorderPaint];
  Width := 185; // Init width
  Height := 82; //  ""  height
  // Cell data
  FCellWidth := 150;
  FCellHeight := 150;
  FCellOffset := 10;
  FCellSpace := 10;
  FCellSpaceX := 10;
  FCtrlSize := 0;
  FReArrange := False;
  FHeaderHeight := 22;
  FDragColumn := -1;
  FDragging := False;
  FHeaderState := svNormal;
  FHeaderHot := -1;
  FAlignment := taCenter;
  FBorderStyle := bsSingle;
  Color := clWindow;
  ColorSel := clHighlight;
  ColorLines := clSilver;
  FFullRepaint := True;
  FSelected := -1;
  HotIdx := -1;
  FPopupID := -1;
  FCanFocused := True;
  FHeaderVisible := False;
  Items := TIntList.Create;
  Selection := TIntList.Create;
  FMainBmp := TBitmap.Create;
  FMainBmp.PixelFormat := pf24Bit;
  SelTimer := TTimer.Create(self);
  SelTimer.Enabled := false;
  SelTimer.Interval := 15;
  SelTimer.OnTimer := OnSelScroll;
  pnlHorz := TPanel.Create(Self);
  pnlHorz.Parent := Self;
  pnlHorz.Align := alBottom;
  pnlHorz.BevelInner := TBevelCut(0);
  pnlHorz.BevelOuter := TBevelCut(0);
  pnlHorz.Height := h;
  pnlHorz.ParentBackground := false;
  pnlHorz.Visible := false;
  pnlVert := TPanel.Create(Self);
  pnlVert.Parent := Self;
  pnlVert.Align := alRight;
  pnlVert.BevelInner := TBevelCut(0);
  pnlVert.BevelOuter := TBevelCut(0);
  pnlVert.Width := w;
  pnlVert.ParentBackground := false;
  pnlVert.Visible := false;
  pnlDummy := TPanel.Create(pnlHorz);
  pnlDummy.Parent := pnlHorz;
  pnlDummy.Align := alRight;
  pnlDummy.BevelInner := TBevelCut(0);
  pnlDummy.BevelOuter := TBevelCut(0);
  pnlDummy.Width := w;
  pnlDummy.ParentBackground := false;
  pnlDummy.Visible := false;
  sbVert := TScrollBar.Create(pnlVert);
  sbVert.Parent := pnlVert;
  sbVert.Kind := sbVertical;
  sbVert.LargeChange := 1;
  sbVert.SmallChange := 1;
  sbVert.Align := alClient;
  sbVert.OnScroll := OnVertScroll;
  sbVert.TabStop := false;
  sbVert.Visible := true;
  sbHorz := TScrollBar.Create(pnlHorz);
  sbHorz.Parent := pnlHorz;
  sbHorz.Kind := sbHorizontal;
  sbHorz.LargeChange := 1;
  sbHorz.SmallChange := 1;
  sbHorz.Align := alClient;
  sbHorz.OnScroll := OnHorzScroll;
  sbHorz.TabStop := false;
  sbHorz.Visible := true;
  hsbVisible := false;
  vsbVisible := false;
  Selecting := false;
  SelStart.X := 0;
  SelStart.Y := 0;
  SelEnd.X := 0;
  SelEnd.Y := 0;
  SelScroll := false;
  ViewIdx := -1;
  CenterView := False;
  HotTracking := True;
  CellSelect := True;
  PaintGrid := False;
  FCellAuto := False;
  FMulti := False;
  IsEditing := False;
  FModified := False;
  Invalidate;
  ShowHint := True;
  ParentFont := False;
  FTransparent:= False;
end;

destructor TrkCustomView.Destroy;
begin
  Items.Free;
  Selection.Free;
  FMainBmp.Free;
  inherited;
end;

procedure TrkCustomView.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkCustomView.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      FMainBmp.Canvas.Font := Font;
      PaintView;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TrkCustomView.ReSize;
begin
  pnlDummy.Visible := pnlVert.Visible;
  CalcView(False);
  if Assigned(FOnReSize) then
    FOnReSize(Self);
end;

function TrkCustomView.GetColumns: string;
var
  i: Integer;
begin
  Result := '';
  if High(FColumns) > 0 then
  begin
    for i := Low(FColumns) to High(FColumns) - 1 do
      Result := Result + IntToStr(FColumns[i]) + ', ';
    Result := Result + IntToStr(FColumns[High(FColumns)]);
  end
  else
    Result := '';
end;

procedure TrkCustomView.SetColumns(Value: string);
var
  i: Integer;
  s: TStringList;
begin
  s := TStringList.Create;
  s.Text := StringReplace(Value, ',', #13, [rfReplaceAll]);
  SetLength(FColumns, s.Count);
  i := 0;
  FListWidth := 0;
  while i < s.Count do
  begin
    FColumns[i] := StrToIntDef(Trim(s[i]), 0);
    FListWidth := FListWidth + FColumns[i];
    inc(i);
  end;
  s.Free;
end;

procedure TrkCustomView.SetUseAsList(Value: Boolean);
begin
  FUseAsList := Value;
  if FUseAsList then
    FCellWidth := FListWidth;
end;

procedure TrkCustomView.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SetFocus;
  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TrkCustomView.CMExit(var Message: TCMExit);
begin
  if Assigned(OnExit) then
    OnExit(Self);
  inherited;
end;

procedure TrkCustomView.CMRefresh(var Message: TMessage);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if not (csDesigning in ComponentState) then
    PaintView;
  inherited;
end;

procedure TrkCustomView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrkCustomView.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  // Answer Delphi that this component wants to handle its own arrow key press:
  message.result := DLGC_WANTARROWS;
end;

procedure TrkCustomView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TrkCustomView.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then
    Message.Result := 1;
end;

procedure TrkCustomView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if fFullRepaint then
    Invalidate;
  inherited;
  if not (csLoading in ComponentState) then
    Resize;
end;

procedure TrkCustomView.WMMouseWheel(var Msg: TWMMouseWheel);
var
  newPos: integer;
begin
  if not vsbVisible then
    Exit;
  if Msg.WheelDelta <> 0 then
  begin
    if Msg.WheelDelta < 0 then
    begin
      newPos := sbVert.Position + sbVert.SmallChange;
      if newPos > Range then
        newPos := Range;
      sbVert.Position := newPos;
    end
    else
      sbVert.Position := sbVert.Position - sbVert.SmallChange;
    Msg.Result := 1;
  end;
  if IsEditing then
    SetFocus;
  invalidate;
end;

procedure TrkCustomView.OnVertScroll(Sender: TObject; ScrollCode: TScrollCode; var
  ScrollPos: Integer);
begin
  FScrollValOld := FScrollValNew;
  FScrollValNew := ScrollPos;
  FScrollPos := FScrollValOld - FScrollValNew;
  if FScrollValOld <> FScrollValNew then
  begin
    if IsEditing then
      SetFocus;
    IsEditing := False;
    Invalidate;
  end;
end;

procedure TrkCustomView.OnHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var
  ScrollPos: Integer);
begin
  Invalidate;
end;

procedure TrkCustomView.Clear;
begin
  if csDesigning in ComponentState then
    Exit;
  Items.Clear;
  Selection.Clear;
  sbVert.PageSize := -1;
  sbVert.Position := 0;
  CalcView(True);
  FNrSelected := 0;
  IdxItem := -1;
  FSelected := -1;
  HotIdx := -1;
  Invalidate;
  FModified := False;
end;

function TrkCustomView.Scroll(delta, value: integer): integer;
var
  newPos: integer;
begin
  if delta < 0 then
  begin
    newPos := sbVert.Position + Value;
    if newPos > Range then
      newPos := Range;
  end
  else
    newPos := sbVert.Position - Value;
  sbVert.Position := newPos;
  Result := 1;
end;

function TrkCustomView.GetItemRect(grp, idx: Integer): TRect;
var
  x, y: Integer;
begin
  if Items.Count = 0 then
    Exit;
  y := FCellOffset;
  if FCellAuto then
    x := FCellSpaceX + (Idx mod ViewColumns) * CellColSize
  else
    x := FCellXOffset + (Idx mod ViewColumns) * ImgWidth;
  y := y + (Trunc(idx / ViewColumns) * ImgHeight);
  Result := Rect(x, y, x + FCellWidth, y + FCellHeight - 1);
end;

function TrkCustomView.GetItemAtXY(pt: TPoint): Integer;
begin
  Result := ItemAtXY(pt, (not FCellSelect));
end;

function TrkCustomView.IdxAtXY(pt: TPoint; HitTest: Boolean): integer;
begin
  Result := ItemAtXY(pt, HitTest);
  if Result > -1 then
    Result := Items[Result];
end;

function TrkCustomView.ItemAtXY(pt: TPoint; HitTest: Boolean): integer;
var
  x, y, Idx, xpos, ypos, i, j, k, vOff, xVal: Integer;
  xOk, yOK, Hit: Boolean;
begin
  Result := -1;
  if Items.Count = 0 then
    Exit;
  x := pt.X;
  y := pt.Y;
  if HeaderVisible then
    y := y - FHeaderHeight;
  Idx := -1;
  FHintColumn := -1;
  if vsbVisible then
    vOff := sbVert.Position
  else
    vOff := 0;
  if FCellAuto then
  begin
    j := (CellColSize * ViewColumns) - FCellSpaceX;
    xVal := FCellSpaceX;
  end
  else
  begin
    j := FCellXOffset + (ViewColumns * ImgWidth);
    xVal := FCellXOffset;
  end;
  if (x >= xVal) and (x < j) then
  begin
    if FCellAuto then
    begin
      xpos := x div CellColSize; //  in what column is x?
      k := FCellSpaceX + (xpos * CellColSize);
      i := k + FCellWidth;
    end
    else
    begin
      xpos := ((x - FCellXOffset) div ImgWidth);
      k := FCellXOffset + (xpos * ImgWidth);
      i := k + ImgWidth - FCellSpaceX;
    end;
    xOK := (x >= k) and (x <= i);
    i := ViewRows * ImgHeight;
    ypos := (y + (i - (i - (vOff - FCellOffset)))) div ImgHeight;
    i := FCellOffset + (ypos * ImgHeight) + ImgHeight - FCellSpace;
    j := vOff + y;
    yOk := (j >= FCellOffset) and (j <= i);
    if (xOk and yOk) then
    begin // where in cell is mouse ;)
      SelX := x - k;
      SelY := i - j;
      Idx := xpos + (ypos * ViewColumns);
      Hit := true;
      if (HitTest) then
        if (Assigned(FOnCellHit) and (Idx < Items.Count)) then
          FOnCellHit(Self, FMainBmp.Canvas, Items[Idx], SelX, SelY, Hit);
      if (not Hit) then
        Idx := -1;
      FHintcolumn := -1; // do calc current column
      if FUseAsList then
      begin
        if hsbVisible then
          x := x + sbHorz.Position;
        j := FCellOffset;
        if (x > FCellOffset) and (x < (FCellWidth) + FCellOffset) then
          for i := Low(FColumns) to High(FColumns) do
          begin
            j := j + FColumns[i];
            if (x < j) and (FHintcolumn = -1) then
              FHintColumn := i;
          end;
      end;
    end;
  end;
  if Idx >= Items.Count then
    Idx := -1;
  Result := Idx;
end;

function TrkCustomView.ItemCloseToXY(X, Y: Integer): integer;
var
  xpos, ypos, Selected, i, j, k: integer;
  ColumnOk, RowOk, AtEnd: Boolean;
begin
  xpos := 0;
  ypos := 0;
  ColumnOk := false;
  RowOk := false;
  FMouseAtEnd := false;
  AtEnd := false;

  j := FCellXOffset + (ViewColumns * ImgWidth);

  if (x < FCellXOffset) then
    x := FCellXOffset;
  if (x >= j) then
    x := j;

  if (x >= FCellXOffset) and (x <= j) then
  begin
    xpos := ((x - FCellXOffSet) div ImgWidth);
    i := ((xpos * ImgWidth) + ImgWidth) + FCellXOffset;
    k := FCellXOffset + (xpos * ImgWidth);
    if (x >= k) and (x <= i) then
      ColumnOk := true
    else
      ColumnOk := false;
    if ColumnOk and (x >= (k + (FCellWidth div 2))) and (x <= i) then
      AtEnd := true;
    if (xpos >= ViewColumns) then
    begin
      xpos := ViewColumns - 1;
      AtEnd := True;
    end;

    i := ViewRows * ImgHeight;
    ypos := (y + (i - (i - (sbVert.Position - FCellOffset)))) div
      ImgHeight;
    i := FCellOffset + (ypos * ImgHeight) + ImgHeight - FCellSpace;
    j := sbVert.Position + y;
    if (j >= FCellOffset) and (j <= i) then
      RowOk := true
    else
      RowOk := false;
  end;

  Selected := xpos + (ypos * ViewColumns);

  FMouseItem := -1;
  if ((ColumnOk) and (RowOk)) then
  begin
    if (Selected >= Items.Count) then
    begin
      Selected := Items.Count - 1;
      FMouseAtEnd := True
    end
    else
      FMouseAtEnd := AtEnd;
    FMouseItem := Selected;
    Result := Selected
  end
  else
    Result := -1;
end;

procedure TrkCustomView.PaintView;
var
  x, y, w, h, cx, cy, nx, ny, fx, fy, d1, scr: integer;
  XPos, YPos, Index, Col, Row, idxLimit, ImageHeight: Integer;
  YCount, bx, by, bdx, bdy, addx, addy, imw, imh: Integer;
  R: TRect;
  slSize, slMain: integer;
  slPnt: PRGB24;
  bool: boolean;
  SItem: TsvItemState;
  i: Integer;
begin
  ScrollOffset := -FCellOffset;
  if sbVert.Position > Range then
    sbVert.Position := Range;
  w := ClientWidth - FCtrlSize;
  scr := w;
  h := ClientHeight;
  if UseAsList then
  begin
    if (FCellWidth + (2 * FCellOffset)) > w then
      w := FCellWidth + (2 * FCellOffset) + 1;
    if hsbVisible then
      h := ClientHeight - pnlHorz.Height;
  end;
  imw := w - 2;
  imh := h - 1;

  FMainBmp.Canvas.Brush.Color := Color;
  FMainBmp.Width := w;
  FMainBmp.Height := h;
  if FTransparent then
    DrawParentImage(Self, FMainBmp.Canvas)
  else
    FMainBmp.Canvas.FillRect(Rect(0, 0, imw + 2, imh + 1));

  if (not (csDesigning in ComponentState)) and (not FUpdating) then
  begin
    slMain := Integer(FMainBmp.ScanLine[0]);
    slSize := Integer(FMainBmp.ScanLine[1]) - slMain;
    if vsbVisible then
    begin
      YCount := ((sbVert.Position - FCellOffset) + FCellSpace) div ImgHeight;
      YPos := FCellOffset - sbVert.Position + YCount * ImgHeight;
      ScrollOffset := sbVert.Position - FCellOffset;
    end
    else
    begin
      YCount := 0;
      YPos := FCellOffset;
    end;
    Index := YCount * ViewColumns;
    if hsbVisible then
      ScrollOffsetX := sbHorz.Position
    else
      ScrollOffsetX := 0;
    ViewIdx := Index; // First index in view...
    bool := false;
    if ViewColumns <= 0 then
      ViewColumns := 1;
    if HeaderVisible then
      YPos := YPos + FHeaderHeight;
    if (Index < Items.Count) and (Items.Count > 0) then
      repeat
        if FCellAuto then
          XPos := FCellSpaceX + (Index mod ViewColumns) * CellColSize
        else
          XPos := FCellXOffset + (Index mod ViewColumns) * ImgWidth;
        R := Rect(XPos, YPos, XPos + FCellWidth, YPos + FCellHeight);
        if R.Bottom > 0 then
        begin
          if R.Top > Height then
            Break;
          SItem := svNormal;
          if (Selection.IndexOf(Items[Index]) <> -1) then
            SItem := svSelected;
          if Index = HotIdx then
            SItem := svHot;
          if IsEditing and (Index = FSelected) then
            SItem := svNormal;
          if (FUseAsList) and (Assigned(TMethod(FOnListPaint).Code)) then
            FOnListPaint(Self, FMainBmp.Canvas, R, Index, Items[Index], SItem,
              FColumns)
          else if Assigned(TMethod(FOnCellPaint).Code) then
            FOnCellPaint(Self, FMainBmp.Canvas, R, Index, Items[Index], SItem);

          if (not Selecting) and FItemArrange then
          begin
            if FMouseItem = Index then
            begin
              FMainBmp.Canvas.Pen.Color := clBlack;
              FMainBmp.Canvas.Pen.Width := 2;
              if not UseAsList then
              begin
                if FMouseAtEnd then
                  d1 := R.Right + (FCellSpace div 2)
                else
                  d1 := R.Left - (FCellSpace div 2) + 1;
                FMainBmp.Canvas.MoveTo(d1, R.Top);
                FMainBmp.Canvas.LineTo(d1, R.Bottom);
              end
              else
              begin
                if FMouseAtEnd then
                  d1 := R.Bottom + (FCellSpace div 2)
                else
                  d1 := R.Top - (FCellSpace div 2) + 1;
                FMainBmp.Canvas.MoveTo(R.Left, d1);
                FMainBmp.Canvas.LineTo(R.Right, d1);
              end;
              FMainBmp.Canvas.Pen.Width := 1;
            end;
          end;

        end;
        Inc(Index);
        if (Index mod ViewColumns) = 0 then
          Inc(YPos, ImgHeight);
        bool := (YPos >= Height) or (Index = Items.Count)
      until bool;
    if (UseAsList) and (PaintGrid) and (High(FColumns) > 0) then
    begin
      FMainBmp.Canvas.Pen.Color := FLinesColor;
      bx := FCellOffset;
      for i := 0 to High(FColumns) do
      begin
        bx := bx + FColumns[i];
        FMainBmp.Canvas.MoveTo(bx, 0);
        FMainBmp.Canvas.LineTo(bx, FMainBmp.Height);
      end;
    end;
    // Selection starts here
    if (Selecting) and (SelStart.X <> SelEnd.X) and (SelStart.Y <> SelEnd.Y) then
    begin
      bx := min(SelStart.X, SelEnd.X);
      bdx := max(SelStart.X, SelEnd.X);
      by := min(SelStart.Y, SelEnd.Y);
      bdy := max(SelStart.Y, SelEnd.Y);
      by := by - ScrollOffset;
      bdy := bdy - ScrollOffset;
      if hsbVisible then
      begin
        if bx < sbHorz.Position then
          bx := sbHorz.Position;
        if bdx > sbHorz.Position + scr - 1 then
          bdx := sbHorz.Position + scr - 1;
      end
      else
      begin
        if bx < 0 then
          bx := 0;
        if bdx > imw then
          bdx := imw;
      end;
      if HeaderVisible then
      begin
        by := by + FHeaderHeight;
        bdy := bdy + FHeaderHeight;
        if by < FHeaderHeight then
          by := FHeaderHeight;
      end
      else if by < 0 then
        by := 0;
      if bdy > imh then
        bdy := imh;
      // Smart Alphablend
      row := (slSize * by + slMain) + (bx * 3);
      for y := by to bdy do
      begin
        slPnt := pRGB24(row);
        for x := bx to bdx do
        begin
          slPnt.r := Ra[slPnt.r];
          slPnt.g := Ga[slPnt.g];
          slPnt.b := Ba[slPnt.b];
          inc(slPnt)
        end;
        inc(row, slSize);
      end;
      FMainBmp.Canvas.Pen.Width := 1;
      FMainBmp.Canvas.Pen.Color := ColorSel;
      FMainBmp.Canvas.Brush.Style := bsClear;
      FMainBmp.Canvas.Rectangle(Rect(bx, by, bdx + 1, bdy + 1));
    end;
    // Paint Header if needed
    if FHeaderVisible then
    begin
      R := Rect(0, 0, FMainBmp.Width - 1, FHeaderHeight);
      if Assigned(TMethod(FOnHeaderPaint).Code) then
        FOnHeaderPaint(Self, FMainBmp.Canvas, R, FCellXOffset, FHeaderHot,
          FHeaderState, FColumns);
      if FDragging then
      begin
        FMainBmp.Canvas.Pen.Color := FLinesColor;
        x := FCellOffset;
        for i := 0 to FDragColumn do
          x := x + FColumns[i];
        FMainBmp.Canvas.MoveTo(x, 0);
        FMainBmp.Canvas.LineTo(x, FMainBmp.Height);
      end;
    end;
  end;
  if UseAsList then
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height,
      FMainBmp.Canvas.Handle, sbHorz.Position, 0, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height,
      FMainBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TrkCustomView.HzScrollbar: Boolean;
begin
  Result := ((FCellWidth + (2 * FCellOffset)) > (ClientWidth - FCtrlSize)) and
    (UseAsList);
end;

function TrkCustomView.VtScrollbar: Boolean;
var
  ImgHeight, ImgWidth, w: Integer;
  VMax, ViewHeight, ViewColumns: Integer;
begin
  ImgWidth := FCellWidth + FCellSpace;
  ImgHeight := FCellHeight + FCellSpace;
  w := ClientWidth - (2 * FCellOffset);
  ViewColumns := w div ImgWidth;
  if (ViewColumns = 0) or (UseAsList) then
    ViewColumns := 1;
  ViewHeight := ClientHeight - (FCellOffset - FCellSpace);
  VMax := FCellOffset + (ImgHeight * Ceil((Items.Count / ViewColumns)));
  Result := VMax > ViewHeight;
end;

procedure TrkCustomView.CalcViewSimple;
var
  w, ViewHeight: Integer;
  VMax, VPos, Rows: Integer;
begin
  ImgWidth := FCellWidth + FCellSpace;
  ImgHeight := FCellHeight + FCellSpace;
  if (Items.Count < 1) then
  begin
    pnlVert.Visible := False;
    FCtrlSize := 0;
    Exit;
  end;
  if VtScrollbar then
    FCtrlSize := pnlVert.Width
  else
    FCtrlSize := 0;
  w := (ClientWidth - (2 * FCellOffset)) - FCtrlSize;
  ViewColumns := w div ImgWidth;
  if (ViewColumns = 0) or (UseAsList) then
    ViewColumns := 1;
  hsbVisible := HzScrollbar;
  ViewHeight := ClientHeight - (FCellOffset - FCellSpace);
  if HeaderVisible then
    ViewHeight := ViewHeight - HeaderHeight;
  if hsbVisible then
    ViewHeight := ViewHeight - pnlHorz.Height;
  VMax := FCellOffset + (ImgHeight * Ceil((Items.Count / ViewColumns)));
  vsbVisible := VMax > ViewHeight;
  pnlVert.Visible := vsbVisible;
  if vsbVisible then
  begin
    Range := VMax - ViewHeight;
    VPos := sbVert.Position;
    if VPos > Range then
      VPos := Range;
    if sbVert.PageSize > VMax then         // Need more testing ...
      sbVert.PageSize := 0
    else
      sbVert.PageSize := ViewHeight + 1;
    sbVert.SetParams(VPos, 0, VMax);
    sbVert.SmallChange := ImgHeight;
    sbVert.LargeChange := ViewHeight;
  end;
  pnlHorz.Visible := HzScrollbar;
  hsbVisible := pnlHorz.Visible;
  if vsbVisible then
    FCtrlSize := pnlVert.Width
  else
    FCtrlSize := 0;
  if FCellAuto then
    FCellSpaceX := (((ClientWidth - FCtrlSize) div ViewColumns) - FCellWidth) shr 1
  else
    FCellSpaceX := FCellSpace;
  FCellXOffset := (ClientWidth - ((ViewColumns * ImgWidth) - FCellSpaceX)) shr 1;
  if (hsbVisible) or (not CenterView) then
    FCellXOffset := FCellOffset;
  CalcHorzSB;
  pnlDummy.Visible := vsbVisible and hsbVisible;
  CellColSize := (ClientWidth - FCtrlSize) div ViewColumns;
  Rows := Round(((ClientHeight / ImgHeight)) + 0.50);
  ViewRows := Rows;
end;

procedure TrkCustomView.CalcHorzSB;
var
  w, delta, nhv, range, page: Integer;
  MinV, MaxV, PosV, WidthSB, LargeV: Integer;
begin
  if pnlHorz.Visible and UseASList then
  begin
    w := ClientWidth - FCtrlSize;
    delta := (FCellWidth + (2 * FCellOffset)) - w;
    WidthSB := sbHorz.ClientWidth;
    Page := sbHorz.PageSize;
    PosV := sbHorz.Position;
    MinV := 0;
    MaxV := w + delta;
    nhv := MaxV;
    if nhv < 0 then
      nhv := 0;
    if nhv > WidthSB then
      MaxV := nhv
    else
      MaxV := 0;
    range := 1 + MaxV - WidthSB;
    if range < 0 then
    begin
      range := 0;
      MaxV := 0;
    end;
    sbHorz.SmallChange := 1;
    if Page >= MaxV then
      Page := -1
    else
      Page := WidthSB + 1;
    if (PosV > range) and (range >= 0) then
      PosV := range;
    if Page > MaxV then
      Page := 0;
    sbHorz.PageSize := Page;
    if Page < 1 then
      LargeV := 1
    else
      LargeV := MaxV - Page;
    sbHorz.LargeChange := LargeV;
    sbHorz.SetParams(PosV, MinV, MaxV);
    maxXScroll := Delta;
  end;
end;

procedure TrkCustomView.CalcView(Clear: Boolean);
begin
  ImgWidth := FCellWidth + FCellSpaceX;
  ImgHeight := FCellHeight + FCellSpace;
  if Clear then
  begin
    sbVert.PageSize := 0;
    sbHorz.PageSize := 0;
  end;
  CalcViewSimple;
  CalcHorzSB;
end;

procedure TrkCustomView.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TrkCustomView.EndUpdate;
begin
  FUpdating := False;
  CalcView(False);
  Invalidate;
end;

procedure TrkCustomView.CalcSelectionSimple;
var
  i: integer;
  Sel, bool: Boolean;
  Cell, R: TRect;
  PixPosY: Integer;
begin
  if Items.Count = 0 then
    Exit;
  PixPosY := FCellOffset;
  Selection.Clear;
  bool := False;
  i := 0;
  repeat
    if UseAsList then
      Cell.Left := FCellOffset
    else if FCellAuto then
      Cell.Left := FCellSpaceX + (i mod ViewColumns) * CellColSize
    else
      Cell.Left := FCellXOffset + (i mod ViewColumns) * ImgWidth;
    Cell.Top := PixPosY - FCellOffset;
    Cell.Right := Cell.Left + FCellWidth;
    Cell.Bottom := Cell.Top + FCellHeight;
    Sel := IntersectRect(r, SelRect, Cell);
    if (Sel) and (not CellSelect) then
      if Assigned(TMethod(FOnCellSelect).Code) then
        FOnCellSelect(Self, FMainBmp.Canvas, Cell, Items[i], Sel);
    if Sel then
    begin
      if not bool then
      begin
        bool := True;
        IdxItem := i;
      end;
      Selection.Add(Items[i]);
    end;
    Inc(i);
    if (i < Items.Count) and (i mod ViewColumns = 0) then
      Inc(PixPosY, (FCellHeight + FCellSpace));
  until (i = Items.Count) or (PixPosY > SelRect.Bottom + FCellHeight);
end;

procedure TrkCustomView.CalcSelection;
begin
  SelRect.Left := Min(SelStart.X, SelEnd.X);
  SelRect.Right := Max(SelStart.X, SelEnd.X);
  SelRect.Top := Min(SelStart.Y, SelEnd.Y);
  SelRect.Bottom := Max(SelStart.Y, SelEnd.Y);
  CalcSelectionSimple;
  if Assigned(TMethod(FOnSelecting).Code) then
    FOnSelecting(Self, Selection.Count);
end;

procedure TrkCustomView.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkCustomView.WMSETFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FFocused := True;
    Invalidate;
  end;
end;

procedure TrkCustomView.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FFocused := False;
    Invalidate;
  end;
end;

procedure TrkCustomView.SetCanFocused;
begin
  FCanFocused := Value;
  if FCanFocused then
    TabStop := True
  else
    TabStop := False;
end;

procedure TrkCustomView.SetSelColor;
var
  C: LongInt;
  i: Integer;
begin
  C := ColorToRGB(Value);
  FColorSelection.B := Byte(C shr 16);
  FColorSelection.G := Byte(C shr 8);
  FColorSelection.R := Byte(C);
  for i := 0 to 255 do
  begin
    Ra[i] := (FColorSelection.R - i) shr 3 + i;
    Ga[i] := (FColorSelection.G - i) shr 3 + i;
    Ba[i] := (FColorSelection.B - i) shr 3 + i;
  end;
end;

procedure TrkCustomView.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

function TrkCustomView.GetSelColor;
begin
  Result := (FColorSelection.B shl 16) + (FColorSelection.G shl 8) +
    FColorSelection.R;
end;

function TrkCustomView.GetVertPos: Integer;
begin
  Result := sbVert.Position;
end;

function TrkCustomView.IsFocused;
begin
  Result := (FFocused) and (FCanFocused);
end;

procedure TrkCustomView.SimpleMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  sel, i, j, k: Integer;
begin
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    Sel := ItemAtXY(Point(X, Y), (not FCellSelect));
    if Sel <> -1 then
    begin
      if FMulti then
      begin
        if (ssShift in shift) and not (ssCtrl in shift) then
        begin
          Selection.Clear;
          i := Sel;
          j := FSelected;
          if j = -1 then
            j := 0;
          if j < i then
          begin
            k := j;
            j := i;
            i := k;
          end;
          for k := i to j do
            Selection.Add(Items[k]);
        end;
        if (ssCtrl in shift) and not (ssShift in shift) then
        begin
          i := Selection.IndexOf(Items[sel]);
          if i <> -1 then
            Selection.Delete(i)
          else
            Selection.Add(Items[sel]);
        end;
        if (not (ssCtrl in shift)) and (not (ssShift in shift)) then
        begin
          if Selection.IndexOf(Items[Sel]) = -1 then
          begin
            Selection.Clear;
            Selection.Add(Items[Sel]);
          end;
        end;
      end
      else
      begin
        Selection.Clear;
        Selection.Add(Items[Sel]);
      end;
      FSelected := Sel;
      SetInView(-1, FSelected);
      Invalidate;
    end
    else
    begin
      Selection.Clear;
      FSelected := -1;
      FPrevSel.Top := -1;
      FPrevSel.Left := -1;
      FPrevSel.Right := -1;
      FPrevSel.Bottom := -1;
      SelStart.X := X;
      SelStart.Y := Y + ScrollOffset;
      if FHeaderVisible then
        SelStart.Y := SelStart.Y - FHeaderHeight;
      SelEnd := SelStart;
      Selecting := FMulti;
      viewDeltaScroll := 0;
      viewDeltaScrollX := 0;
      SelTimer.Enabled := True;
      Invalidate;
    end;
    if Assigned(FOnSelecting) then
      FOnSelecting(Self, Selection.Count);
  end;
  FLastIndex := FSelected;
end;

procedure TrkCustomView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (CanFocus) then
    SetFocus;

  if FReArrange then
  begin
    FMouseItem := -1;
    FItemArrange := true;
    if (Selection.Count > 0) and (ssShift in shift) and (ssCtrl in shift) then
      Exit
    else
      FItemArrange := False;
  end;

  if Items.Count = 0 then
    Exit;
  FDownX := x;
  FDownY := y;
  FDownTick := GetTickCount;

  SelTimer.Enabled := False;
  SCPos := sbVert.Position;
  if ScrollMode then
    Exit;

  if (FHeaderVisible) and (y < FHeaderHeight) and (y >= 0) then
  begin
    InHeader := True;
    if (FDragColumn <> -1) then
    begin
      FDragging := True;
      FDragStartX := x;
      FDragStartSize := FColumns[FDragColumn];
    end;
    if (not FDragging) and (FHeaderHot <> -1) and (Button = mbLeft) then
    begin
      FHeaderState := svSelected;
      if Assigned(TMethod(FOnHeaderClick).Code) then
        OnHeaderClick(Self, FHeaderHot);
    end;
    Invalidate;
    Exit;
  end
  else
    InHeader := False;
  if dblClicked then
    Exit;
  if (UseAsList) and (hsbVisible) then
    x := x + sbHorz.Position;
  SimpleMouseDown(Button, Shift, X, Y);
  y := y + YAdjOff;
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TrkCustomView.UpdateView;
begin
  if Items.Count > 0 then
    SetInView(-1, ViewIdx);
end;

procedure TrkCustomView.MoveSelectedTo(idx: Integer);
var
  i, l: integer;
begin
  if (idx <> -1) then // we have movement :)
  begin
    if FMouseAtEnd then
      inc(idx);
    l := 0;
    for i := Items.Count - 1 downto 0 do
      if (Selection.IndexOf(Items[i]) <> -1) then
      begin
        if i < idx then
          l := l + 1;
        Items.Delete(i);
      end;
    idx := idx - l;
    for i := 0 to Selection.Count - 1 do
      Items.Insert(idx + i, Selection[i]);
    Invalidate;
    FModified := True;
  end;
end;

procedure TrkCustomView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
var
  i: Integer;
  dwTicks, dwElapsed: DWord;
begin
  FDragging := False;
  FDragColumn := -1;
  SelTimer.Enabled := false;
  Selecting := false;
  SelScroll := false;

  if dblClicked then
    dblClicked := false;

  if (FItemArrange) and (FReArrange) then
  begin
    i := ItemAtXY(Point(x, y), False);
    if i <> -1 then // we have movement :)
      MoveSelectedTo(i);
  end;

  if ScrollMode then
  begin
    dwTicks := GetTickCount;
    if dwTicks >= FDownTick then
      dwElapsed := dwTicks - FDownTick
    else
      dwElapsed := $FFFFFFFF - FDownTick + dwTicks;
    SCUp := SCY < 0;
    SCValue := Abs(SCY);
    SelTimer.Enabled := (dwElapsed < 175);
    Exit;
  end;

  Invalidate;
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TrkCustomView.CMHintShow(var Message: TCMHintShow);
var
  InfoTip: string;
  Item, i, j: Integer;
  R: TRect;
  bool: Boolean;
begin
  if Assigned(FOnHintShow) then
  begin
    Item := ItemAtXY(Message.HintInfo.CursorPos, False);
    if (Item <> -1) then
    begin
      R := GetItemRect(-1, Item);
      Item := Items[Item];
    end
    else
      Exit;
    if vsbVisible then
      R.Top := (R.Top - sbVert.Position) + 1
    else
      R.Top := R.Top + 1;
    R.Bottom := (R.Top + FCellHeight);
    if FHeaderVisible then
    begin
      R.Top := R.Top + FHeaderHeight;
      R.Bottom := R.Top + FCellHeight;
      i := 0;
      j := FcellOffset;
      while i < FHintColumn do
      begin
        j := j + FColumns[i];
        i := i + 1;
      end;
      R.Left := j;
      R.Right := j + FColumns[i];
    end;
    bool := False;
    OnHintShow(Self, Item, FHintColumn, InfoTip, bool);
    bugStr := 'Bummer: ' + IntToStr(FHintColumn);
    if (not bool) or (InfoTip = '') or (Selecting) or (FDragging) or (IsEditing)
      then
      Message.Result := -1
    else
    begin
      Message.HintInfo.HintStr := InfoTip;
      Message.HintInfo.CursorRect := R;
      Message.HintInfo.HideTimeout := 120000;
      Message.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure TrkCustomView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
  ViewHeight, i, n, j, k: Integer;
  bool, dummy: Boolean;
  s: string;
begin
  if dblClicked then
    dblClicked := false;

  if ScrollMode then
  begin
    if (ssLeft in Shift) then
    begin
      SCY := FDownY - Y;
      i := SCPos + SCY;
      if i < sbVert.Min then
        sbVert.Position := sbVert.Min
      else if i > sbVert.Max then
        sbVert.Position := sbVert.Max
      else
        sbVert.Position := i;
      Invalidate;
    end;
    Exit;
  end;

  bool := False;
  if (FHeaderState = svSelected) and (ssLeft in Shift) then
    Exit;
  if not (ssLeft in Shift) then
    Selecting := false;
  if Selecting then
  begin
    if FHeaderVisible then
    begin
      y := y - FHeaderHeight;
      if x < 0 then
        viewDeltaScrollX := x * abs(x)
      else if x > (ClientWidth - FCtrlSize) then
        viewDeltaScrollX := (x - (ClientWidth - FCtrlSize)) * (x - (ClientWidth
          - FCtrlSize))
      else
        viewDeltaScrollX := 0;
      if (viewDeltaScrollX <> 0) and (ssShift in Shift) then
        viewDeltaScrollX := viewDeltaScrollX * 2;
    end;
    if (FHeaderVisible) and (hsbVisible) then
      ViewHeight := ClientHeight - (pnlHorz.Height + FHeaderHeight)
    else if (FHeaderVisible) then
      ViewHeight := ClientHeight - FHeaderHeight
    else
      ViewHeight := ClientHeight;
    if y < 0 then
      viewDeltaScroll := y * abs(y)
    else if y > ViewHeight then
      viewDeltaScroll := (y - ViewHeight) * (y - ViewHeight)
    else
      viewDeltaScroll := 0;
    if (viewDeltaScroll <> 0) and (ssShift in Shift) then
      viewDeltaScroll := viewDeltaScroll * 2;
    SelEnd.X := X + ScrollOffsetX;
    SelEnd.Y := Y + ScrollOffset;
    CalcSelection;
    bool := True;
  end
  else if (FHeaderVisible) and (FDragging) then
  begin
    i := FDragStartSize + (x - FDragStartX);
    if i > 2 then
      FColumns[FDragColumn] := i
    else
      FColumns[FDragColumn] := 2;
    for i := 0 to High(FColumns) do
      s := s + IntToStr(FColumns[i]) + ',';
    j := Length(s);
    SetLength(s, j - 1);
    Columns := s;
    FCellWidth := FListWidth;
    CalcViewSimple;
    bool := True;
  end
  else
  begin
    if HotTracking then
    begin
      if hsbVisible then
        x := x + sbHorz.Position;
      if (FHeaderVisible) and (y < FHeaderHeight) and (y >= 0) then
      begin
        if High(FColumns) > 0 then
        begin
          j := 0;
          k := 0;
          x := x - FCellXOffset;
          dummy := False;
          FDragColumn := -1;
          n := FHeaderHot;
          FHeaderHot := -1;
          FHeaderState := svNormal;
          while j <= High(FColumns) do
          begin
            k := k + FColumns[j];
            if (abs(x - k) < 5) and (not Dummy) then
            begin
              FDragColumn := j;
              FDragOffs := (x - k);
              Self.Cursor := crHSplit;
              dummy := True;
            end;
            if (x > 0) and (x < FCellWidth) then
            begin
              if (x <= k) then
              begin
                FHeaderHot := j;
                FHeaderState := svHot;
                j := High(FColumns);
              end;
            end;
            j := j + 1;
          end;
          bool := (n <> FHeaderHot);
          if (not dummy) then
          begin
            Self.Cursor := crDefault;
            FDragColumn := -1;
          end;
        end;
      end
      else
      begin
        Self.Cursor := crDefault;
        if (FHeaderHot <> -1) then
        begin
          FHeaderHot := -1;
          FHeaderState := svNormal;
          bool := True;
        end;

        FItemArrange := (ssShift in shift) and (ssCtrl in Shift) and (ssLeft in
          shift) and FReArrange;
        if FItemArrange then
        begin
          ItemCloseToXY(x, y);
          bool := True;
        end
        else
        begin
          Idx := ItemAtXY(Point(x, y), (not FCellSelect));
          if Idx <> HotIdx then
          begin
            HotIdx := Idx;
            bool := True;
          end;
        end;
      end;
    end;
    if Assigned(OnMouseMove) then
      OnMouseMove(Self, Shift, x, y);
  end;
  if bool then
    Invalidate;
end;

procedure TrkCustomView.CMMouseLeave(var Message: TMessage);
begin
  FHeaderHot := -1;
  FHeaderState := svNormal;
  HotIdx := -1;
  Invalidate;
end;

procedure TrkCustomView.DblClick;
begin
  FDownTick := GetTickCount;
  if IsEditing then
    Exit;
  dblClicked := True;
  if Assigned(OnDblClick) and (Not InHeader) then
    OnDblClick(Self);
end;

procedure TrkCustomView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TrkCustomView.OnSelScroll(Sender: TObject);
var
  i: integer;
  bool: Boolean;
begin
  if ScrollMode then
  begin
    if SCUp then
      i := -SCValue
    else
      i := SCValue;
    i := sbVert.Position + i;
    if i < sbVert.Min then
      sbVert.Position := sbVert.Min
    else if i > sbVert.Max then
      sbVert.Position := sbVert.Max
    else
      sbVert.Position := i;
    Invalidate;
    if SCValue <> 0 then
      SCValue := Trunc((SCValue - Ln(SCValue)) / 1.1);
    SelTimer.Enabled := SCValue > 0;
    Exit;
  end;

  bool := false;
  if (viewDeltaScroll <> 0) then
  begin
    if viewDeltaScroll < 0 then
      SelEnd.Y := ScrollOffset
    else
      SelEnd.Y := ScrollOffset + ClientHeight;
    SelEnd.Y := SelEnd.Y + viewDeltaScroll;

    if vsbVisible then
    begin
      i := sbVert.Position + viewDeltaScroll;
      if i <> sbVert.Position then
      begin
        sbVert.Position := i;
        bool := true;
      end;
    end;
  end;

  if (hsbVisible) and (viewDeltaScrollX <> 0) then
  begin
    if viewDeltaScrollX < 0 then
      SelEnd.X := ScrollOffsetX
    else
      SelEnd.X := ScrollOffsetX + ClientWidth;

    i := sbHorz.Position + viewDeltaScrollX;
    if i <> sbHorz.Position then
    begin
      if i >= maxXScroll then
        i := maxXScroll;
      sbHorz.Position := i;
      bool := true;
    end;
  end;

  if bool then
    PostMessage(Handle, CM_REFRESH, 0, 0);

  CalcSelection;
  SelTimer.Enabled := Selecting;
end;

procedure TrkCustomView.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
end;

function TrkCustomView.SimpleDoKey(var Key: Word; Shift: TShiftState): Boolean;
var
  idx, old, i, j, k, maxInt: Integer;
  DoInvalid, Done: Boolean;
begin
  j := sbVert.Position;
  maxInt := Range;
  idx := -1;
  old := -1;
  Done := True;
  DoInvalid := False;
  if (FSelected = -1) and (Selection.Count = 0) then
  begin
    if (Shift = []) then
    begin
      FSelected := 0;
      FLastIndex := 0;
      DoInvalid := true;
      Done:= False;
    end;
  end else
  begin
    idx := FSelected;
    old := idx;
    case Key of
      vk_Home:
        begin
          if (ssCtrl in Shift) then
          begin
            sbVert.Position := 0;
            DoInvalid := True;
          end;
        end;
      vk_End:
        begin
          sbVert.Position := sbVert.Max;
          DoInvalid := True;
        end;
      vk_Next:
        begin
          i := j + ClientHeight;
          if i > sbVert.Max then
            sbVert.Position := maxInt
          else
            sbVert.Position := i;
          DoInvalid := True;
        end;
      vk_Prior:
        begin
          i := j - ClientHeight;
          if i < 0 then
            sbVert.Position := 0
          else
            sbVert.Position := i;
          DoInvalid := True;
        end;
      vk_Down: if idx + ViewColumns < Items.Count then
          idx := idx + ViewColumns;
      vk_Up: if idx - ViewColumns >= 0 then
          idx := idx - ViewColumns;
      vk_Left: if idx - 1 >= 0 then
          idx := idx - 1;
      vk_Right: if idx + 1 < Items.Count then
          idx := idx + 1;
      vk_Return: DblClick;
    else
      Done := False;
    end;
  end;
  if (idx <> old) and (idx <> -1) then
  begin
    Selection.Clear;
    FSelected := idx;
    SetInView(-1, FSelected);
    if (ssShift in Shift) then
    begin
      if FLastIndex <> -1 then
      begin
        j := Min(FSelected, FLastIndex);
        k := Max(FSelected, FLastIndex);
        for i := j to k do
          Selection.Add(Items[i]);
      end;
    end
    else
      Selection.Add(Items[idx]);
    DoInvalid := True;
  end;
  if (not (ssShift in Shift)) then
    FLastIndex := FSelected;
  if DoInvalid then
    Invalidate;
  if Assigned(FOnSelecting) then
    FOnSelecting(Self, Selection.Count);
  Result := Done;
end;

function TrkCustomView.TextWidth(text: string): Integer;
begin
  Result := FMainBmp.Canvas.TextWidth(text);
end;

function TrkCustomView.GetColumnSize(Column: Integer): Integer;
begin
  if (Column >= Low(FColumns)) and (Column <= High(FColumns)) then
    Result := FColumns[Column]
  else
    Result := -1;
end;

procedure TrkCustomView.SetInView(grp, idx: Integer);
var
  y, i, h, t: Integer;
  R: TRect;
begin
  YAdjOff := 0;
  i := idx;
  if i < 0 then
    Exit;
  h := ClientHeight;
  if hsbVisible then
    h := (h - pnlHorz.Height) - 1;
  R := GetItemRect(-1, idx);
  if FHeaderVisible then
  begin
    R.Top := R.Top + (FHeaderHeight - FCellSpace);
    R.Bottom := (R.Bottom + (FHeaderHeight - FCellSpace));
    t := FHeaderHeight
  end
  else
    t := 0;
  if R.Top < (sbVert.Position + t) then
  begin
    YAdjOff := sbVert.Position;
    sbVert.Position := (R.Top - FCellSpace) - t;
    YAdjOff := YAdjOff - sbVert.Position;
  end;
  y := (sbVert.Position + h);
  if R.Bottom > y then
  begin
    YAdjOff := sbVert.Position;
    sbVert.Position := (R.Bottom - h) + 2;
    YAdjOff := YAdjOff - sbVert.Position;
  end;
  Invalidate;
end;

procedure TrkCustomView.SelectAll(Inverted: Boolean);
var
  i, j, n: Integer;
begin
  for i:= 0 to Items.Count - 1 do
  begin
    j:= Items[i];
    n:= Selection.IndexOf(j);
    if (Inverted) then
    begin
      if (n <> -1) then
        Selection.Delete(n)
      else
        Selection.Add(j);
    end
    else
      if (n = -1) then
        Selection.Add(j);
  end;
  Invalidate;
  FSelected:= -1;
  if Assigned(TMethod(FOnSelecting).Code) then
    FOnSelecting(Self, Selection.Count);
end;

procedure TrkCustomView.SetAtTop(grp, idx: Integer);
var
  i, t: Integer;
  R: TRect;
begin
  CalcView(False);
  YAdjOff := 0;
  i := idx;
  if i < 0 then
    Exit;
  R := GetItemRect(-1, idx);
  if FHeaderVisible then
  begin
    R.Top := R.Top + (FHeaderHeight - FCellSpace);
    R.Bottom := (R.Bottom + (FHeaderHeight - FCellSpace));
  end;
  if FHeaderVisible then
    t := FHeaderHeight
  else
    t := 0;
  YAdjOff := sbVert.Position;
  sbVert.Position := (R.Top - FCellSpace) - t;
  YAdjOff := YAdjOff - sbVert.Position;
  Invalidate;
end;

procedure TrkCustomView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Done: Boolean;
begin
  if (Items.Count = 0) then
  begin
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
    Exit;
  end;
  Done := SimpleDoKey(Key, Shift);
  if (not Done) and Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TrkCustomView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TrkCustomView.WMKeyDown(var Message: TWMKeyDown);
begin
  with Message do
    KeyDown(CharCode, KeyDataToShiftState(KeyData));
end;

procedure TrkCustomView.WMKeyUp(var Message: TWMKeyDown);
begin
  with Message do
    KeyUp(CharCode, KeyDataToShiftState(KeyData));
end;

procedure Register;
begin
  RegisterComponents('rmKlever', [TrkView]);
end;

end.

