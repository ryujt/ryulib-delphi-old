unit rkSmartView;

// SmartView by Roy Magne Klever
// © 2011 by Roy Magne Klever. All rights reserved
//
// This file is not distributable without permission by Roy Magne Klever
// WEB: www.rmklever.com
// Mail: roymagne@rmklever.com
//
// version 1.0

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StrUtils, ExtCtrls, Math, Stdctrls, Buttons,
  rkIntegerList;

const
  CM_REFRESH = WM_USER + 2000; // Custom Message...
  CM_INLINEEDIT = WM_USER + 2003; // Edit message...

type
  TLine8 = array [0 .. 0] of Byte;
  PLine8 = ^Byte;
  PRGB24 = ^TRGB24;

  TRGB24 = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TLine24 = array [0 .. 0] of TRGB24;
  PLine24 = ^TLine24;

  TWMMouseWheel = record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0:
        (XPos: SmallInt;
          YPos: SmallInt);
      1:
        (Pos: TSmallPoint;
          Result: Longint);
  end;

  TsvItemState = (svNormal, svSelected, svHot, svHotSelected, svDrag);

  // SmartGroup items
  PSmartGroup = ^TSmartGroup;

  TSmartGroup = record
    Caption: string;
    Expanded: Boolean;
    Selected: Boolean;
    Items: TIntList;
    Tag: Integer;
  end;

  // View events...
  TOnSelectingEvent = procedure(Sender: TObject; Count: Integer) of object;
  TOnHeaderPaintEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Header: TRect; Offset, Active: Integer; State: TsvItemState;
    Columns: array of Integer) of object;
  TOnCellPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    IdxA, Idx: Integer; State: TsvItemState) of object;
  TOnListPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    IdxA, Idx: Integer; State: TsvItemState;
    Columns: array of Integer) of object;
  TOnCellEditEvent = procedure(Sender: TObject; Idx: Integer;
    var x, y, width: Integer; var Text: string) of object;
  TOnDividerPaintEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Cell: TRect; Group: PSmartGroup; State: TsvItemState) of object;
  TOnDividerHitEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Grp, x: Integer; var Selected: Boolean) of object;
  TOnCellSelectEvent = procedure(Sender: TObject; Canvas: TCanvas; Cell: TRect;
    Idx: Integer; var Selected: Boolean) of object;
  TOnCellHitEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Idx, x, y: Integer; var Selected: Boolean) of object;
  TOnHeaderClickEvent = procedure(Sender: TObject; Column: Integer) of object;
  TOnEditAcceptEvent = procedure(Sender: TObject; Idx: Integer;
    var Text: string; var Accept: Boolean) of object;
  TOnHintShowEvent = procedure(Sender: TObject; Idx, Column: Integer;
    var Text: string; var Show: Boolean) of object;

  TrkCustomSmartView = Class;
  TrkHeaderItem = Class;

  TOnHeaderClick = procedure(Sender: TObject; Idx: Integer;
    Item: TrkHeaderItem) of object;

  TrkHeaderItem = Class(TCollectionItem)
  private
    FAccsending: Boolean;
    FActive: Boolean;
    FCaption: String;
    FSize: Integer;
    FTag: Integer;
  public
    Constructor Create(Collection: TCollection); Override;
    Destructor Destroy; Override;
  protected
    FOnHeaderClick: TOnHeaderClick;
  published
    property Accsending: Boolean read FAccsending write FAccsending;
    property Active: Boolean read FActive write FActive default False;
    property Caption: string read FCaption write FCaption;
    property Size: Integer read FSize write FSize;
    property Tag: Integer read FTag write FTag;
    property OnHeaderClick
      : TOnHeaderClick read FOnHeaderClick write FOnHeaderClick;
  end;

  TrkHeaderItems = Class(TCollection)
  private
    FOwner: TrkCustomSmartView;
  protected
    function GetItem(Index: Integer): TrkHeaderItem;
    function GetOwner: TPersistent; override;
  public
    property Items[Index: Integer]: TrkHeaderItem read GetItem; default;
    function Add: TrkHeaderItem;
    Constructor Create(AOwner: TrkCustomSmartView);
  End;

  // Main class
  TrkCustomSmartView = class(TCustomControl)
  private
    FFocused: Boolean;
    FModified: Boolean;
    FMouseItem: Integer;
    FMouseAtEnd: Boolean;
    FDownTick: DWord;
    FDownX: Integer;
    FDownY: Integer;
    // Header management
    FHeaderHot: Integer;
    FHeaderState: TsvItemState;
    FDragColumn: Integer;
    FDragging: Boolean;
    FDragOffs: Integer;
    FDragStartX: Integer;
    FDragStartSize: Integer;
    InHeader: Boolean;
    // Selection
    SelScroll: Boolean;
    SelStart, SelEnd: tPoint;
    viewDeltaScroll, ScrollOffset: Integer;
    viewDeltaScrollX, ScrollOffsetX: Integer;
    maxXScroll: Integer;
    SelTimer: TTimer;
    Ra, Ga, Ba: array [0 .. 255] of Byte;
    // Painting
    FMainBmp: TBitmap;
    FColorSelection: TRGB24;
    FShowGroupMarker: Boolean;
    // Scrollmode
    SCUp: Boolean;
    SCY, SCPos, SCValue: Integer;
    // Scrollbar
    FScrollValOld: Integer;
    FScrollValNew: Integer;
    FScrollPos: Integer;
    Range: Integer;
    hsbVisible: Boolean;
    vsbVisible: Boolean;
    YAdjOff: Integer;
    // Parts of component  Easier to maintain scrollbars :)
    pnlVert, pnlHorz, pnlDummy: TPanel;
    dblClicked: Boolean;
    FHotGrp: Integer;
    FHotTracking: Boolean;
    FListWidth: Integer;
    FColumns: array of Integer;
    FHintColumn: Integer;
    FLastIndex: Integer;
    FNrSelected: Integer;
    FPopupID: Integer;
    FBorderStyle: TBorderStyle;
    FFullRepaint: Boolean;
    FLocked: Boolean;
    EditInline: TEdit;
    EditLastValue: string;
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
    FHeader: TrkHeaderItems;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMRefresh(var Message: TMessage); message CM_REFRESH;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMIsToolControl(var Message: TMessage); message CM_ISTOOLCONTROL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure OnVertScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure OnHorzScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure CalcSelectionGroups;
    procedure CalcSelectionSimple;
    procedure CalcSelection;
    function VtScrollbar: Boolean;
    function GetSelected: tPoint;
    procedure SetSelected(const Value: TPoint);
    function CalcColumns: Integer;
    procedure SetHeader(const Value: TrkHeaderItems);
  protected
    CellColSize: Integer;
    FAllowEdit: Boolean;
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
    FCtrlSize: Integer;
    FDividerHeight: Integer;
    FEditFilename: Boolean;
    FHeaderHeight: Integer;
    FHeaderVisible: Boolean;
    FLinesColor: TColor;
    FMulti: Boolean;
    FPaintGrid: Boolean;
    FUseAsList: Boolean;
    FUseGroups: Boolean;
    FOnSelecting: TOnSelectingEvent;
    FOnCellSelect: TOnCellSelectEvent;
    FOnCellHit: TOnCellHitEvent;
    FOnHeaderPaint: TOnHeaderPaintEvent;
    FOnHeaderClick: TOnHeaderClickEvent;
    FOnCellPaint: TOnCellPaintEvent;
    FOnListPaint: TOnListPaintEvent;
    FOnCellEdit: TOnCellEditEvent;
    FOnDividerPaint: TOnDividerPaintEvent;
    FOnDividerHit: TOnDividerHitEvent;
    FOnEditAccept: TOnEditAcceptEvent;
    FOnHintShow: TOnHintShowEvent;
    FCanFocused: Boolean;
    procedure PaintSimple;
    procedure PaintGroups;
    procedure PaintWindow(DC: HDC); override;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    function GetColumns: string;
    procedure SetColumns(Value: string);
    procedure SetUseAsList(Value: Boolean);
    procedure SetCanFocused(Value: Boolean);
    procedure SetSelColor(Value: TColor);
    function GetSelColor: TColor;
    function IsFocused: Boolean;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditExit(Sender: TObject);
    procedure SimpleMouseDown(Button: TMouseButton; Shift: TShiftState;
      x, y: Integer);
    function GrpDoShiftSelect: Boolean;
    procedure GrpMouseDown(Button: TMouseButton; Shift: TShiftState;
      x, y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DblClick; override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    function SimpleDoKey(var Key: Word; Shift: TShiftState): Boolean;
    function GrpDoKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnSelScroll(Sender: TObject);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    property CellWidth: Integer read FCellWidth write FCellWidth default 150;
    property CellHeight: Integer read FCellHeight write FCellHeight default 150;
    property CellOffset: Integer read FCellOffset write FCellOffset default 10;
    property CellSpace: Integer read FCellSpace write FCellSpace default 10;
    property HeaderHeight
      : Integer read FHeaderHeight write FHeaderHeight default 22;
    property DividerHeight
      : Integer read FDividerHeight write FDividerHeight default
      22;
    property CellAutoAdj: Boolean read FCellAuto write FCellAuto default False;
    property Grouped: Boolean read FUseGroups write FUseGroups default False;
    property MultipleSelection: Boolean read FMulti write FMulti default False;
    property UseAsList
      : Boolean read FUseAsList write SetUseAsList default False;
    property CenterView
      : Boolean read FCenterView write FCenterView default False;
    property CellSelect
      : Boolean read FCellSelect write FCellSelect default False;
    property PaintGrid: Boolean read FPaintGrid write FPaintGrid default False;
    property EditFilename
      : Boolean read FEditFilename write FEditFilename default
      False;
    property HeaderVisible
      : Boolean read FHeaderVisible write FHeaderVisible default
      False;
    property Columns: string read GetColumns write SetColumns;
    property Color: TColor read FBackColor write FBackColor default clWindow;
    property ColorSel: TColor read GetSelColor write SetSelColor default
      clHighlight;
    property ColorLines
      : TColor read FLinesColor write FLinesColor default clSilver;
    property ShowGroupMarker
      : Boolean read FShowGroupMarker write FShowGroupMarker default False;
  public
    IsEditing: Boolean;
    BugStr: string;
    BugCount: Integer;
    Items: TIntList;
    InView: TIntList;
    Selection: TIntList;
    SmartGroups: TList;
    IdxGrp: Integer;
    IdxItem: Integer;
    LastIdxGrp: Integer;
    LastIdxItem: Integer;
    HotIdx: Integer;
    HotGrp: Integer;
    ViewGrp, ViewIdx: Integer;
    sbVert: TScrollBar;
    sbHorz: TScrollBar;

    HeaderCanDrag: Boolean;
    HeaderDrag: Boolean;
    HeaderDragIdx: Integer;
    HeaderPoint: tPoint;

    // Selection
    Selecting: Boolean;
    FSelRect, FPrevSel, SelRect: TRect;
    SelX, SelY: Integer;
    HotX, HotY: Integer;
    // View data
    ImgWidth, ImgHeight, ViewColumns, ViewRows: Integer;
    ScrollMode: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearSmartGroups;
    function AddSmartGroup(s: string): PSmartGroup;
    procedure DeleteSmartGroup(n: Integer);
    procedure EditCaption;
    procedure DoPaint;
    procedure SetExpanded(Value: Boolean);
    procedure CalcGroupPos(Value: Integer; var State: Byte; var Group: Integer;
      var Idx: Integer; var Offset: Integer; var gsize: Integer);
    function HzScrollbar: Boolean;
    procedure CalcViewSimple;
    procedure CalcViewGroups;
    procedure CalcHorzSB;
    procedure CalcView(Clear: Boolean);
    procedure UpdateView;
    procedure Clear;
    function TextWidth(Text: string): Integer;
    function TextHeight(Text: string): Integer;
    function GetColumnSize(Column: Integer): Integer;
    procedure SetInView(Grp, Idx: Integer);
    procedure SetAtTop(Grp, Idx: Integer);
    function GetItemRect(Grp, Idx: Integer): TRect;
    function GetItemAtXY(pt: tPoint): Integer;
    function GrpIdxAtXY(pt: tPoint; HitTest: Boolean): tPoint;
    function GrpItemAtXY(pt: tPoint; HitTest: Boolean): tPoint;
    function IdxAtXY(pt: tPoint; HitTest: Boolean): Integer;
    function ItemAtXY(pt: tPoint; HitTest: Boolean): Integer;
    function ItemCloseToXY(x, y: Integer): Integer;
    procedure SelectAll(Inverted: Boolean);
    function Scroll(delta, Value: Integer): Integer;
    procedure Resize; override;
    property Header: TrkHeaderItems read FHeader write SetHeader;
    property HotGrpIndex: Integer read FHotGrp;
    property Selected: tPoint read GetSelected write SetSelected;
    property NumberSel: Integer read FNrSelected default 0;
    property Modified: Boolean read FModified;
  published
    // Published
    property Align;
    property AllowEdit: Boolean read FAllowEdit write FAllowEdit default False;
    property Anchors;
    property BorderStyle
      : TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property HotTracking
      : Boolean read FHotTracking write FHotTracking default False;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseWheel
      : TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
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
    property OnSelecting
      : TOnSelectingEvent read FOnSelecting write FOnSelecting;
    property OnCellHit: TOnCellHitEvent read FOnCellHit write FOnCellHit;
    property OnCellSelect
      : TOnCellSelectEvent read FOnCellSelect write FOnCellSelect;
    property OnCellEdit: TOnCellEditEvent read FOnCellEdit write FOnCellEdit;
    property OnEditAccept
      : TOnEditAcceptEvent read FOnEditAccept write FOnEditAccept;
    property OnHeaderPaint
      : TOnHeaderPaintEvent read FOnHeaderPaint write FOnHeaderPaint;
    property OnHeaderClick
      : TOnHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnCellPaint
      : TOnCellPaintEvent read FOnCellPaint write FOnCellPaint;
    property OnListPaint
      : TOnListPaintEvent read FOnListPaint write FOnListPaint;
    property OnDividerHit
      : TOnDividerHitEvent read FOnDividerHit write FOnDividerHit;
    property OnDividerPaint
      : TOnDividerPaintEvent read FOnDividerPaint write FOnDividerPaint;
  end;

  // main class
  TrkSmartView = class(TrkCustomSmartView)
  published
    // Published
    property CellWidth: Integer read FCellWidth write FCellWidth default 150;
    property CellHeight: Integer read FCellHeight write FCellHeight default 150;
    property CellOffset: Integer read FCellOffset write FCellOffset default 10;
    property CellSpace: Integer read FCellSpace write FCellSpace default 10;
    property HeaderHeight
      : Integer read FHeaderHeight write FHeaderHeight default 22;
    property DividerHeight
      : Integer read FDividerHeight write FDividerHeight default
      22;
    property CellAutoAdj: Boolean read FCellAuto write FCellAuto default False;
    property Grouped: Boolean read FUseGroups write FUseGroups default False;
    property MultipleSelection: Boolean read FMulti write FMulti default False;
    property UseAsList
      : Boolean read FUseAsList write SetUseAsList default False;
    property CenterView
      : Boolean read FCenterView write FCenterView default False;
    property CellSelect
      : Boolean read FCellSelect write FCellSelect default False;
    property PaintGrid: Boolean read FPaintGrid write FPaintGrid default False;
    property EditFilename
      : Boolean read FEditFilename write FEditFilename default
      False;
    property HeaderVisible
      : Boolean read FHeaderVisible write FHeaderVisible default
      False;
    property Columns: string read GetColumns write SetColumns;
    property Color: TColor read FBackColor write FBackColor default clWindow;
    property ColorSel: TColor read GetSelColor write SetSelColor default
      clHighlight;
    property ColorLines
      : TColor read FLinesColor write FLinesColor default clSilver;
  end;

procedure Register;

implementation

constructor TrkCustomSmartView.Create(AOwner: TComponent);
var
  w, h: Integer;
begin
  inherited;
  BugStr := '';
  BugCount := 0;
  w := GetSystemMetrics(SM_CXVSCROLL); // Width of a vertical scrollbar...
  h := GetSystemMetrics(SM_CXHSCROLL); // Width of a horizontal scrollbar...
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls, csReplicatable,
    csNeedsBorderPaint];
  width := 185; // Init width
  Height := 82; // ""  height
  // Cell data
  FCellWidth := 150;
  FCellHeight := 150;
  FCellOffset := 10;
  FCellSpace := 10;
  FCellSpaceX := 10;
  FCtrlSize := 0;
  FHeaderHeight := 22;
  FDividerHeight := 22;
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
  HotIdx := -1;
  HotGrp := -1;
  FPopupID := -1;
  FCanFocused := True;
  FAllowEdit := False;
  FEditFilename := False;
  FHeaderVisible := False;
  Items := TIntList.Create;
  InView := TIntList.Create;
  Selection := TIntList.Create;
  SmartGroups := TList.Create;
  FMainBmp := TBitmap.Create;
  FMainBmp.PixelFormat := pf24Bit;
  FShowGroupMarker := False;
  SelTimer := TTimer.Create(self);
  SelTimer.Enabled := False;
  SelTimer.Interval := 15;
  SelTimer.OnTimer := OnSelScroll;
  pnlHorz := TPanel.Create(self);
  pnlHorz.Parent := self;
  pnlHorz.Align := alBottom;
  pnlHorz.BevelInner := TBevelCut(0);
  pnlHorz.BevelOuter := TBevelCut(0);
  pnlHorz.Height := h;
  pnlHorz.ParentBackground := False;
  pnlHorz.Visible := False;
  pnlVert := TPanel.Create(self);
  pnlVert.Parent := self;
  pnlVert.Align := alRight;
  pnlVert.BevelInner := TBevelCut(0);
  pnlVert.BevelOuter := TBevelCut(0);
  pnlVert.width := w;
  pnlVert.ParentBackground := False;
  pnlVert.Visible := False;
  pnlDummy := TPanel.Create(pnlHorz);
  pnlDummy.Parent := pnlHorz;
  pnlDummy.Align := alRight;
  pnlDummy.BevelInner := TBevelCut(0);
  pnlDummy.BevelOuter := TBevelCut(0);
  pnlDummy.width := w;
  pnlDummy.ParentBackground := False;
  pnlDummy.Visible := False;
  sbVert := TScrollBar.Create(pnlVert);
  sbVert.Parent := pnlVert;
  sbVert.Kind := sbVertical;
  sbVert.LargeChange := 1;
  sbVert.SmallChange := 1;
  sbVert.Align := alClient;
  sbVert.OnScroll := OnVertScroll;
  sbVert.TabStop := False;
  sbVert.Visible := True;
  sbHorz := TScrollBar.Create(pnlHorz);
  sbHorz.Parent := pnlHorz;
  sbHorz.Kind := sbHorizontal;
  sbHorz.LargeChange := 1;
  sbHorz.SmallChange := 1;
  sbHorz.Align := alClient;
  sbHorz.OnScroll := OnHorzScroll;
  sbHorz.TabStop := False;
  sbHorz.Visible := True;
  hsbVisible := False;
  vsbVisible := False;
  Selecting := False;
  SelStart.x := 0;
  SelStart.y := 0;
  SelEnd.x := 0;
  SelEnd.y := 0;
  SelScroll := False;
  ViewIdx := -1;
  CenterView := False;
  HotTracking := True;
  CellSelect := True;
  PaintGrid := False;
  FCellAuto := False;
  FUseGroups := False;
  FMulti := False;
  IsEditing := False;
  FModified := False;
  // inline Editing
  if not(csDesigning in ComponentState) then
  begin
    EditInline := TEdit.Create(self);
    EditInline.Ctl3D := False;
    EditInline.Visible := False;
    EditInline.OnKeyDown := EditKeyDown;
    EditInline.OnKeyPress := EditKeyPress;
    EditInline.OnExit := EditExit;
    EditInline.AutoSize := False;
    EditInline.Parent := self;
    EditInline.ParentFont := True;
    EditInline.Height := 19;
    EditLastValue := '';
  end;
  Invalidate;
  ShowHint := True;
end;

destructor TrkCustomSmartView.Destroy;
begin
  ClearSmartGroups;
  Items.Free;
  InView.Free;
  Selection.Free;
  SmartGroups.Free;
  FMainBmp.Free;
  inherited;
end;

procedure TrkCustomSmartView.DoPaint;
begin
  if FUseGroups then
    PaintGroups
  else
    PaintSimple;
end;

procedure TrkCustomSmartView.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkCustomSmartView.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      FMainBmp.Canvas.Font := Font;
      if FUseGroups then
        PaintGroups
      else
        PaintSimple;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TrkCustomSmartView.Resize;
begin
  pnlDummy.Visible := pnlVert.Visible;
  CalcView(False);
  if Assigned(FOnResize) then
    FOnResize(self);
end;

function TrkCustomSmartView.GetColumns: string;
var
  i: Integer;
begin
  Result := '';
  if High(FColumns) > 0 then
  begin
    for i := Low(FColumns) to High(FColumns) - 1 do
      Result := Result + IntToStr(FColumns[i]) + ', ';
    Result := Result + IntToStr(FColumns[ High(FColumns)]);
  end
  else
    Result := '';
end;

procedure TrkCustomSmartView.SetColumns(Value: string);
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

procedure TrkCustomSmartView.SetUseAsList(Value: Boolean);
begin
  FUseAsList := Value;
  if FUseAsList then
    FCellWidth := FListWidth;
end;

procedure TrkCustomSmartView.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    SetFocus;
  if Assigned(OnEnter) then
    OnEnter(self);
end;

procedure TrkCustomSmartView.CMExit(var Message: TCMExit);
begin
  if Assigned(OnExit) then
    OnExit(self);
  inherited;
end;

procedure TrkCustomSmartView.CMRefresh(var Message: TMessage);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if not(csDesigning in ComponentState) then
    if FUseGroups then
      PaintGroups
    else
      PaintSimple;
  inherited;
end;

procedure TrkCustomSmartView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrkCustomSmartView.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  // Answer Delphi that this component wants to handle its own arrow key press:
  message.Result := DLGC_WANTARROWS;
end;

procedure TrkCustomSmartView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TrkCustomSmartView.CMIsToolControl(var Message: TMessage);
begin
  if not FLocked then
    Message.Result := 1;
end;

procedure TrkCustomSmartView.WMWindowPosChanged
  (var Message: TWMWindowPosChanged);
begin
  if FFullRepaint then
    Invalidate;
  inherited;
  if not(csLoading in ComponentState) then
    Resize;
end;

procedure TrkCustomSmartView.WMMouseWheel(var Msg: TWMMouseWheel);
var
  newPos: Integer;
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
  Invalidate;
end;

procedure TrkCustomSmartView.OnVertScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TrkCustomSmartView.OnHorzScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Invalidate;
end;

procedure TrkCustomSmartView.Clear;
begin
  if csDesigning in ComponentState then
    Exit;
  Items.Clear;
  ClearSmartGroups;
  Selection.Clear;
  sbVert.PageSize := -1;
  sbVert.Position := 0;
  CalcView(True);
  FNrSelected := 0;
  IdxGrp := -1;
  IdxItem := -1;
  HotIdx := -1;
  HotGrp := -1;
  ViewGrp := -1;
  ViewIdx := -1;
  Invalidate;
  FModified := False;
end;

function TrkCustomSmartView.Scroll(delta, Value: Integer): Integer;
var
  newPos: Integer;
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

function TrkCustomSmartView.GetItemRect(Grp, Idx: Integer): TRect;
var
  x, y, i, xVal: Integer;
  Group: PSmartGroup;
  rows: Integer;
begin
  if Items.Count = 0 then
    Exit;
  if FCellAuto then
    xVal := FCellSpace
  else
    xVal := FCellXOffset;
  y := FCellOffset;
  if (FUseGroups) then
  begin
    y := y + FDividerHeight + FCellSpace;
    i := 0;
    while (i < Grp) do
    begin
      Group := SmartGroups[i];
      y := y + FDividerHeight;
      if Group.Expanded then
      begin
        y := y + FCellSpace;
        rows := Group.Items.Count div ViewColumns;
        if (Group.Items.Count mod ViewColumns) > 0 then
          inc(rows, 1);
        y := y + (rows * ImgHeight);
      end;
      i := i + 1;
    end;
    if (Idx = -1) then
    begin
      x := xVal;
      y := y - (FDividerHeight + FCellSpace);
      Result := Rect(x, y, FMainBmp.width - x, y + DividerHeight);
      Exit;
    end;
  end
  else if (FUseGroups) and (Idx = -1) then
  begin
    x := xVal;
    y := y - (FDividerHeight + FCellSpace);
    Result := Rect(x, y, FMainBmp.width - x, y + DividerHeight);
  end;
  if FCellAuto then
    x := FCellSpaceX + (Idx mod ViewColumns) * CellColSize
  else
    x := FCellXOffset + (Idx mod ViewColumns) * ImgWidth;
  y := y + (Trunc(Idx / ViewColumns) * ImgHeight);
  Result := Rect(x, y, x + FCellWidth, y + FCellHeight - 1);
end;

function TrkCustomSmartView.GetItemAtXY(pt: tPoint): Integer;
begin
  if Grouped then
    Result := GrpItemAtXY(pt, (not FCellSelect)).y
  else
    Result := ItemAtXY(pt, (not FCellSelect));
end;

function TrkCustomSmartView.GrpIdxAtXY(pt: tPoint; HitTest: Boolean): tPoint;
var
  p: PSmartGroup;
begin
  Result := GrpItemAtXY(pt, HitTest);
  if Result.x > -1 then
  begin
    p := SmartGroups[Result.x];
    if Result.y > -1 then
      Result.y := p.Items[Result.y];
  end;
end;

function TrkCustomSmartView.GrpItemAtXY(pt: tPoint; HitTest: Boolean): tPoint;
var
  XPos, vOff, i, j, k, x, y, xVal: Integer;
  Hit: Boolean;
  gState: Byte;
  gGrp, gIdx, gOff, gsize: Integer;
  Group: PSmartGroup;
begin
  x := pt.x;
  y := pt.y;
  if HeaderVisible then
    y := y - FHeaderHeight;
  if vsbVisible then
    vOff := y + sbVert.Position
  else
    vOff := y;
  SelX := -1;
  SelY := -1;
  CalcGroupPos(vOff, gState, gGrp, gIdx, gOff, gsize);
  if (gState = 3) and (gGrp <> -1) then
  begin
    Group := SmartGroups[gGrp]; // Set group
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
        XPos := x div CellColSize; // in what column is x?
        k := FCellSpaceX + (XPos * CellColSize);
        i := k + FCellWidth;
      end
      else
      begin
        XPos := ((x - FCellXOffset) div ImgWidth);
        k := FCellXOffset + (XPos * ImgWidth);
        i := k + ImgWidth - FCellSpaceX;
      end;
      if ((x >= k) and (x <= i)) and ((gOff + ImgHeight) > FCellSpace) then
      // where in cell is mouse ;)
      begin
        SelX := x - k;
        SelY := FCellHeight + gOff;
        HotX := SelX; // rmkHot
        HotY := SelY;
        k := gIdx + XPos;
        Hit := True;
        if (HitTest) then
          if (Assigned(FOnCellHit)) and (k < Group.Items.Count) then
            FOnCellHit(self, FMainBmp.Canvas, Group.Items[k], SelX, SelY, Hit);
        if (Hit) and (k < Group.Items.Count) then
          Result := Point(gGrp, k)
        else
          Result := Point(-1, -1);
        FHintColumn := -1; // Also calc current column
        if FUseAsList then
        begin
          if hsbVisible then
            x := x + sbHorz.Position;
          j := FCellOffset;
          if (x > FCellOffset) and (x < (FCellWidth) + FCellOffset) then
            for i := Low(FColumns) to High(FColumns) do
            begin
              j := j + FColumns[i];
              if (x < j) and (FHintColumn = -1) then
                FHintColumn := i;
            end;
        end;
      end
      else
        Result := Point(-1, -1);
    end
    else
      Result := Point(-1, -1);
  end
  else if (gState = 1) then
  begin
    if hsbVisible then
      x := x + sbHorz.Position;
    if (x > FCellOffset) and (x < FMainBmp.width - FCellOffset) then
    begin
      HotX := x - FCellOffset;
      HotY := gOff;
      Result := Point(gGrp, -1);
    end
    else
      Result := Point(-1, -1);
  end
  else
    Result := Point(-1, -1);
end;

function TrkCustomSmartView.IdxAtXY(pt: tPoint; HitTest: Boolean): Integer;
begin
  Result := ItemAtXY(pt, HitTest);
  if Result > -1 then
    Result := Items[Result];
end;

function TrkCustomSmartView.ItemAtXY(pt: tPoint; HitTest: Boolean): Integer;
var
  x, y, Idx, XPos, YPos, i, j, k, vOff, xVal: Integer;
  xOk, yOK, Hit: Boolean;
begin
  Result := -1;
  if Items.Count = 0 then
    Exit;
  x := pt.x;
  y := pt.y;
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
      XPos := x div CellColSize; // in what column is x?
      k := FCellSpaceX + (XPos * CellColSize);
      i := k + FCellWidth;
    end
    else
    begin
      XPos := ((x - FCellXOffset) div ImgWidth);
      k := FCellXOffset + (XPos * ImgWidth);
      i := k + ImgWidth - FCellSpaceX;
    end;
    xOk := (x >= k) and (x <= i);
    i := ViewRows * ImgHeight;
    YPos := (y + (i - (i - (vOff - FCellOffset)))) div ImgHeight;
    i := FCellOffset + (YPos * ImgHeight) + ImgHeight - FCellSpace;
    j := vOff + y;
    yOK := (j >= FCellOffset) and (j <= i);
    if (xOk and yOK) then
    begin // where in cell is mouse ;)
      SelX := x - k;
      SelY := i - j;
      Idx := XPos + (YPos * ViewColumns);
      Hit := True;
      if (HitTest) then
        if (Assigned(FOnCellHit) and (Idx < Items.Count)) then
          FOnCellHit(self, FMainBmp.Canvas, Items[Idx], SelX, SelY, Hit);
      if (not Hit) then
        Idx := -1;
      FHintColumn := -1; // do calc current column
      if FUseAsList then
      begin
        if hsbVisible then
          x := x + sbHorz.Position;
        j := FCellOffset;
        if (x > FCellOffset) and (x < (FCellWidth) + FCellOffset) then
          for i := Low(FColumns) to High(FColumns) do
          begin
            j := j + FColumns[i];
            if (x < j) and (FHintColumn = -1) then
              FHintColumn := i;
          end;
      end;
    end;
  end;
  if Idx >= Items.Count then
    Idx := -1;
  Result := Idx;
end;

function TrkCustomSmartView.ItemCloseToXY(x, y: Integer): Integer;
var
  XPos, YPos, Selected, i, j, k: Integer;
  ColumnOk, RowOk, AtEnd: Boolean;
begin
  XPos := 0;
  YPos := 0;
  ColumnOk := False;
  RowOk := False;
  FMouseAtEnd := False;
  AtEnd := False;
  j := FCellXOffset + (ViewColumns * ImgWidth);
  if (x < FCellXOffset) then
    x := FCellXOffset;
  if (x >= j) then
    x := j;
  if (x >= FCellXOffset) and (x <= j) then
  begin
    XPos := ((x - FCellXOffset) div ImgWidth);
    i := ((XPos * ImgWidth) + ImgWidth) + FCellXOffset;
    k := FCellXOffset + (XPos * ImgWidth);
    if (x >= k) and (x <= i) then
      ColumnOk := True
    else
      ColumnOk := False;
    if ColumnOk and (x >= (k + (FCellWidth div 2))) and (x <= i) then
      AtEnd := True;
    if (XPos >= ViewColumns) then
    begin
      XPos := ViewColumns - 1;
      AtEnd := True;
    end;
    i := ViewRows * ImgHeight;
    YPos := (y + (i - (i - (sbVert.Position - FCellOffset)))) div ImgHeight;
    i := FCellOffset + (YPos * ImgHeight) + ImgHeight - FCellSpace;
    j := sbVert.Position + y;
    if (j >= FCellOffset) and (j <= i) then
      RowOk := True
    else
      RowOk := False;
  end;
  Selected := XPos + (YPos * ViewColumns);
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

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: tPoint;
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
    SetViewportOrgEx(DC, Position.x - Left, Position.y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TrkCustomSmartView.PaintSimple;
var
  x, y, w, h, cx, cy, nx, ny, fx, fy, d1, scr: Integer;
  XPos, YPos, Index, Col, Row, idxLimit, ImageHeight: Integer;
  YCount, bx, by, bdx, bdy, addx, addy, imw, imh: Integer;
  R: TRect;
  slSize, slMain: Integer;
  slPnt: PRGB24;
  bool: Boolean;
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
  FMainBmp.width := w;
  FMainBmp.Height := h;
  FMainBmp.Canvas.FillRect(Rect(0, 0, imw + 2, imh + 1));

  // if Transparent then
  // DrawParentImage(self, FMainBmp.Canvas);

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
  bool := False;
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
        if IsEditing and (Index = IdxItem) then
          SItem := svNormal;
        if (FUseAsList) and (Assigned(TMethod(FOnListPaint).Code)) then
          FOnListPaint(self, FMainBmp.Canvas, R, Index, Items[Index], SItem,
            FColumns)
        else if Assigned(TMethod(FOnCellPaint).Code) then
          FOnCellPaint(self, FMainBmp.Canvas, R, Index, Items[Index], SItem);
      end;
      inc(Index);
      if (Index mod ViewColumns) = 0 then
        inc(YPos, ImgHeight);
      bool := (YPos >= Height) or (Index = Items.Count)
    until bool;
    if (UseAsList) and (PaintGrid) and ( High(FColumns) > 0) then
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
  if (Selecting) and (SelStart.x <> SelEnd.x) and (SelStart.y <> SelEnd.y) then
  begin
    bx := min(SelStart.x, SelEnd.x);
    bdx := max(SelStart.x, SelEnd.x);
    by := min(SelStart.y, SelEnd.y);
    bdy := max(SelStart.y, SelEnd.y);
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
    Row := (slSize * by + slMain) + (bx * 3);
    for y := by to bdy do
    begin
      slPnt := PRGB24(Row);
      for x := bx to bdx do
      begin
        slPnt.R := Ra[slPnt.R];
        slPnt.G := Ga[slPnt.G];
        slPnt.B := Ba[slPnt.B];
        inc(slPnt)
      end;
      inc(Row, slSize);
    end;
    FMainBmp.Canvas.Pen.width := 1;
    FMainBmp.Canvas.Pen.Color := ColorSel;
    FMainBmp.Canvas.Brush.Style := bsClear;
    FMainBmp.Canvas.Rectangle(Rect(bx, by, bdx + 1, bdy + 1));
  end;
  // Paint Header if needed
  if FHeaderVisible then
  begin
    R := Rect(0, 0, FMainBmp.width - 1, FHeaderHeight);
    if (Assigned(TMethod(FOnHeaderPaint).Code)) then
      FOnHeaderPaint(self, FMainBmp.Canvas, R, FCellXOffset, FHeaderHot,
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
  if UseAsList then
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height, FMainBmp.Canvas.Handle,
      sbHorz.Position, 0, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height, FMainBmp.Canvas.Handle,
      0, 0, SRCCOPY);
end;

procedure TrkCustomSmartView.CalcGroupPos(Value: Integer; var State: Byte;
  var Group: Integer; var Idx: Integer; var Offset: Integer;
  var gsize: Integer);
var
  i, j: Integer;
  y1, y2, gs, gp: Integer;
  bool: Boolean;
  Item: PSmartGroup;
  rows: Integer;
begin
  Group := -1;
  Offset := 0;
  Idx := 0;
  State := 255;
  if SmartGroups.Count <= 0 then
    Exit;
  gsize := 0;
  gp := FCellOffset;
  if Value < FCellOffset then
  begin
    Group := 0;
    State := 0;
    Idx := 0;
    Offset := FCellOffset - Value;
  end
  else
  begin // must be in a group
    Value := Value - FCellOffset;
    i := 0;
    y1 := 0;
    rows := 1;
    bool := False;
    repeat
      Item := SmartGroups[i];
      if (Item.Items.Count > 0) and (Item.Expanded) then
      begin
        gs := FDividerHeight + FCellSpace;
        rows := Item.Items.Count div ViewColumns;
        if (Item.Items.Count mod ViewColumns) > 0 then
          inc(rows, 1);
        gs := gs + (rows * ImgHeight);
      end
      else
        gs := FDividerHeight;
      if gsize = 0 then
        gsize := rows;
      if ((Value - y1) <= gs) then
      begin
        y2 := y1 + FDividerHeight;
        if (Value >= y1) and (Value <= y2) then
        begin
          Group := i;
          State := 1;
          Idx := 0;
          Offset := (Value - y1);
          bool := True;
        end
        else
        begin
          if (Item.Items.Count > 0) then
          begin
            y1 := y2;
            y2 := y2 + FCellSpace;
            if (Value >= y1) and (Value <= y2) then
            begin
              Group := i;
              State := 2;
              Idx := 0;
              Offset := FCellSpace - (Value - y1);
              bool := True;
            end
            else
            begin
              y1 := y2;
              j := Item.Items.Count div ViewColumns;
              if (Item.Items.Count mod ViewColumns) > 0 then
                inc(j, 1);
              y2 := y2 + (j * ImgHeight);
              if (Value >= y1) and (Value < y2) then
              begin
                Group := i;
                State := 3;
                Offset := 0;
                Value := Value - y1;
                j := Value div ImgHeight;
                Idx := j * ViewColumns;
                j := j * ImgHeight;
                j := j - Value;
                Offset := j;
                bool := True;
              end;
            end;
          end
        end;
      end;
      gp := gp + gs;
      y1 := gp - FCellOffset;
      inc(i);
      bool := bool or (i >= SmartGroups.Count);
    until bool;
  end;
end;

function TrkCustomSmartView.HzScrollbar: Boolean;
begin
  Result := ((FCellWidth + (2 * FCellOffset)) > (ClientWidth - FCtrlSize)) and
    (UseAsList);
end;

function TrkCustomSmartView.VtScrollbar: Boolean;
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

function TrkCustomSmartView.CalcColumns: Integer;
var
  cw, n: Integer;
begin
  cw := ClientWidth - FCtrlSize;
  if FCellAuto then
  begin
    Result := cw div ImgWidth;
    FCellSpaceX := 1 + ((cw div Result) - FCellWidth) shr 1;
  end
  else
  begin
    FCellSpaceX := FCellSpace;
    n := ClientWidth - FCtrlSize;
    Result := (n - ((2 * FCellOffset) - FCellSpaceX)) div ImgWidth;
  end;
  if (Result = 0) or (UseAsList) then
    Result := 1;
  if (hsbVisible) or (not CenterView) then
    FCellXOffset := FCellOffset
  else
    FCellXOffset := (cw - ((Result * ImgWidth) - FCellSpaceX)) shr 1;
end;

procedure TrkCustomSmartView.CalcViewSimple;
var
  ViewHeight: Integer;
  VMax, VPos: Integer;
begin
  ImgWidth := FCellWidth + FCellSpace;
  ImgHeight := FCellHeight + FCellSpace;
  FCtrlSize := 0;
  if (Items.Count < 1) then
  begin
    pnlVert.Visible := False;
    Exit;
  end;
  if VtScrollbar then
    FCtrlSize := pnlVert.width;
  ViewColumns := CalcColumns;
  hsbVisible := HzScrollbar;
  ViewHeight := ClientHeight;
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
    sbVert.PageSize := 0;
    sbVert.SetParams(VPos, 0, VMax);
    sbVert.SmallChange := ImgHeight;
    sbVert.LargeChange := ViewHeight;
    sbVert.PageSize := ViewHeight;
  end;
  pnlHorz.Visible := HzScrollbar;
  hsbVisible := pnlHorz.Visible;
  ViewColumns := CalcColumns;
  CalcHorzSB;
  pnlDummy.Visible := vsbVisible and hsbVisible;
  CellColSize := (ClientWidth - FCtrlSize) div ViewColumns;
end;

procedure TrkCustomSmartView.CalcViewGroups;
var
  Group: PSmartGroup;
  i, TotSize, GrpSize, GrpRows, ViewHeight: Integer;
  hzSB: Boolean;
  rows, VMax, VPos: Integer;
begin
  ImgWidth := FCellWidth + FCellSpace;
  ImgHeight := FCellHeight + FCellSpace;
  FCtrlSize := 0;
  hzSB := HzScrollbar;
  if UseAsList then
    ViewColumns := 1
  else
    ViewColumns := CalcColumns;
  TotSize := FCellOffset;
  if SmartGroups.Count > 0 then
  begin
    for i := 0 to SmartGroups.Count - 1 do
    begin
      Group := SmartGroups[i];
      if (Group.Items.Count > 0) and (Group.Expanded) then
      begin
        GrpSize := FDividerHeight + FCellSpace;
        GrpRows := Ceil(Group.Items.Count / ViewColumns);
        GrpSize := GrpSize + (GrpRows * ImgHeight);
      end
      else
        GrpSize := FDividerHeight;
      TotSize := TotSize + GrpSize;
    end;
  end;
  TotSize := TotSize + FCellOffset;
  ViewHeight := ClientHeight;
  if FHeaderVisible then
    ViewHeight := ViewHeight - FHeaderHeight;
  if hzSB then
    ViewHeight := ViewHeight - pnlHorz.Height;
  rows := Ceil(ViewHeight / ImgHeight);
  ViewRows := rows;
  vsbVisible := TotSize > ViewHeight;
  if vsbVisible then
    FCtrlSize := pnlVert.width;
  hsbVisible := hzSB or HzScrollbar;
  pnlVert.Visible := vsbVisible;
  pnlHorz.Visible := hsbVisible;
  pnlDummy.Visible := hsbVisible and vsbVisible;
  ViewColumns := CalcColumns;

  TotSize := FCellOffset;
  if SmartGroups.Count > 0 then
  begin
    for i := 0 to SmartGroups.Count - 1 do
    begin
      Group := SmartGroups[i];
      if (Group.Items.Count > 0) and (Group.Expanded) then
      begin
        GrpSize := FDividerHeight + FCellSpace;
        GrpRows := Ceil(Group.Items.Count / ViewColumns);
        GrpSize := GrpSize + (GrpRows * ImgHeight);
      end
      else
        GrpSize := FDividerHeight;
      TotSize := TotSize + GrpSize;
    end;
  end;
  TotSize := TotSize + FCellOffset;

  FCellXOffset := (ClientWidth - FCtrlSize - ((ViewColumns * ImgWidth)
        - FCellSpaceX)) shr 1;
  if (hsbVisible) or (not CenterView) then
    FCellXOffset := FCellOffset;
  CalcHorzSB; // Update Horizontal scrollbar
  CellColSize := (ClientWidth - FCtrlSize) div ViewColumns;
  if (not vsbVisible) then
    Exit;
  ViewHeight := ClientHeight;
  if FHeaderVisible then
    ViewHeight := ViewHeight - FHeaderHeight;
  if hsbVisible then
    ViewHeight := ViewHeight - pnlHorz.Height;
  VMax := TotSize;
  Range := VMax - ViewHeight;
  VPos := sbVert.Position;
  if VPos > Range then
    VPos := Range;
  sbVert.PageSize := 0;
  sbVert.SetParams(VPos, 0, VMax);
  sbVert.SmallChange := ImgHeight;
  sbVert.LargeChange := ViewHeight;
  sbVert.PageSize := ViewHeight;
  ImgWidth := FCellWidth + FCellSpaceX;
  ImgHeight := FCellHeight + FCellSpace;
  CellColSize := (ClientWidth - FCtrlSize) div ViewColumns;
end;

procedure TrkCustomSmartView.CalcHorzSB;
var
  w, delta, nhv, Range, page: Integer;
  MinV, MaxV, PosV, WidthSB, LargeV: Integer;
begin
  if hsbVisible then
  begin
    w := ClientWidth - FCtrlSize;
    delta := (FCellWidth + (2 * FCellOffset)) - w;
    try
      WidthSB := sbHorz.ClientWidth;
      page := sbHorz.PageSize;
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
      Range := 1 + MaxV - WidthSB;
      if Range < 0 then
      begin
        Range := 0;
        MaxV := 0;
      end;
      if page >= MaxV then
        page := -1
      else
        page := WidthSB + 1;
      if (PosV > Range) and (Range >= 0) then
        PosV := Range;
      if page < 1 then
        LargeV := 1
      else
        LargeV := MaxV - page;
      sbHorz.PageSize := 0;
      sbHorz.SetParams(PosV, MinV, MaxV);
      sbHorz.SmallChange := 1;
      sbHorz.LargeChange := LargeV;
      sbHorz.PageSize := page;
      maxXScroll := delta;
    except
      ShowMessage('Not good...');
    end;
  end;
end;

procedure TrkCustomSmartView.CalcView(Clear: Boolean);
begin
  ImgWidth := FCellWidth + FCellSpaceX;
  ImgHeight := FCellHeight + FCellSpace;
  if Clear then
  begin
    sbVert.PageSize := 0;
    sbHorz.PageSize := 0;
  end;
  if FUseGroups then
    CalcViewGroups
  else
    CalcViewSimple;
  CalcHorzSB;
end;

procedure TrkCustomSmartView.PaintGroups;
var
  x, y, w, h, cy, nx, ny, fx, fy, d1, scr: Integer;
  XPos, YPos, Index, Col, Row: Integer;
  XCount, YCount, bx, by, bdx, bdy, addx, addy, imw, imh: Integer;
  R: TRect;
  slSize, slMain: Integer;
  slPnt: PRGB24;
  Item: PSmartGroup;
  gId, gOff, gsize: Integer;
  gState: Byte;
  i, j: Cardinal;
  bool: Boolean;
  SItem: TsvItemState;
  SDivider: TsvItemState;
  //
  r1: TRect;
  HotH: Integer;
  //
begin
  //FShowGroupMarker:= True;
  if not vsbVisible then
    sbVert.Position := 0;
  ScrollOffset := 0;
  ScrollOffsetX := 0;
  if sbVert.Position > Range then
    sbVert.Position := Range;
  w := ClientWidth - FCtrlSize;
  scr := w;
  if UseAsList then
    if (FCellWidth + (2 * FCellOffset)) > w then
      w := FCellWidth + (2 * FCellOffset) + 1;
  h := ClientHeight;
  imw := w - 2;
  imh := h - 1;
  FMainBmp.Canvas.Brush.Color := Color;
  FMainBmp.width := w;
  FMainBmp.Height := h;
  FMainBmp.Canvas.FillRect(Rect(0, 0, imw + 2, imh + (ImgHeight - 1)));
  slMain := Integer(FMainBmp.ScanLine[0]);
  slSize := Integer(FMainBmp.ScanLine[1]) - slMain;
  ScrollOffset := sbVert.Position - FCellOffset;
  if hsbVisible then
    ScrollOffsetX := sbHorz.Position;
  if vsbVisible then
    i := sbVert.Position
  else
    i := 0;
  CalcGroupPos(i, gState, gId, Index, gOff, gsize);
  ViewGrp := gId;
  ViewIdx := Index; // First index in view...
  InView.Clear;
  if FHeaderVisible then
    YPos := FHeaderHeight
  else
    YPos := 0;
  if (gId > -1) then
  begin
    Item := SmartGroups[gId];
    bool := False;
    // RMKX
    if (FShowGroupMarker) and (FHotGrp = gId) then
    begin
      r1.Top := YPos;
      r1.Left := FCellOffset;
      r1.Right := FMainBmp.width - FCellOffset;
      if Item.Expanded then
      begin
        HotH := Ceil(Item.Items.Count / ViewColumns);
        HotH := (HotH * ImgHeight) + FDividerHeight + FCellSpace + FCellOffset;
      end
      else
        HotH := FDividerHeight;
      HotH := HotH - sbVert.Position;
      r1.Bottom := min(r1.Top + HotH, ClientHeight);
      if (r1.Bottom - HotH) < YPos then
        r1.Top := r1.Top - 1;
      FMainBmp.Canvas.Pen.Color := ColorSel;
      if sbVert.Position > FCellOffset then
        FMainBmp.Canvas.Rectangle(r1);
      // FMainBmp.Canvas.Brush.Color:= $00F4F4FF;
      // FMainBmp.Canvas.FillRect(r1);
    end;
    repeat
      case gState of
        0, 1:
          begin
            if gState = 0 then
              YPos := YPos + gOff
            else
              YPos := YPos - gOff;
            R.Top := YPos;
            R.Bottom := YPos + FDividerHeight;
            R.Left := FCellOffset;
            R.Right := FMainBmp.width - FCellOffset;
            if (FShowGroupMarker) and (FHotGrp = gId) then
            begin
              r1 := R;
              if Item.Expanded then
              begin
                HotH := Ceil(Item.Items.Count / ViewColumns);
                HotH := (HotH * ImgHeight) + FDividerHeight + FCellSpace;
              end
              else
                HotH := FDividerHeight;
              r1.Bottom := min(r1.Top + HotH, ClientHeight);
              FMainBmp.Canvas.Pen.Color := ColorSel;
              FMainBmp.Canvas.Rectangle(r1);
            end;
            if (Assigned(TMethod(FOnDividerPaint).Code)) and (Item <> nil) then
            begin
              SDivider := svNormal;
              if ((IdxGrp = gId) and (IdxItem = -1)
                { or (Item.Selected) } ) then
                SDivider := svSelected;
              if ((HotGrp = gId) and (HotIdx = -1)) then
                SDivider := svHot;
              FOnDividerPaint(self, FMainBmp.Canvas, R, Item, SDivider);
            end;
            Index := 0;
            YPos := YPos + FDividerHeight;
            if (Item.Items.Count > 0) and (Item.Expanded) then
              YPos := YPos + FCellSpace;
          end;
        2:
          YPos := YPos + gOff;
        3:
          begin
            YPos := YPos + gOff;
            if FCellAuto then
              XPos := FCellSpaceX + (Index mod ViewColumns) * CellColSize
            else
              XPos := FCellXOffset + (Index mod ViewColumns) * ImgWidth;
            R := Rect(XPos, YPos, XPos + FCellWidth, YPos + FCellHeight);
            if R.Top > Height then
              Break;
            d1 := Item.Items[Index];
            if Index <= Item.Items.Count - 1 then
            begin
              InView.Add(Index);
              SItem := svNormal;
              if (Selection.IndexOf(d1) <> -1) then
                SItem := svSelected;
              if (Index = HotIdx) and (gId = HotGrp) then
                SItem := svHot;
              if (Index = IdxItem) and (gId = IdxGrp) and (IsEditing) then
                SItem := svNormal;
              if (FUseAsList) and (Assigned(TMethod(FOnListPaint).Code)) then
                FOnListPaint(self, FMainBmp.Canvas, R, Index, d1, SItem,
                  FColumns)
              else if (Assigned(TMethod(FOnCellPaint).Code)) and (Item <> nil)
                then
                FOnCellPaint(self, FMainBmp.Canvas, R, Index, d1, SItem);
            end;
          end;
      end;
      gOff := 0;
      if gState <> 3 then
        if (not Item.Expanded) then
        begin
          gState := 1;
          gId := gId + 1;
          if gId >= SmartGroups.Count then
            YPos := Height
          else
            Item := SmartGroups[gId];
          Index := 0;
        end
        else
          gState := 3
        else
        begin
          inc(Index);
          if (Index mod ViewColumns) = 0 then
            inc(YPos, ImgHeight);
        end;
      if (Index = Item.Items.Count) and (gState = 3) then
      begin
        if (Index mod ViewColumns) <> 0 then
          inc(YPos, ImgHeight);
        gState := 1;
        gId := gId + 1;
        if gId < SmartGroups.Count then
          Item := SmartGroups[gId];
        Index := 0;
      end;
      bool := (YPos >= Height) or (gId >= SmartGroups.Count);
    until bool;
  end;
  if (UseAsList) and (PaintGrid) and ( High(FColumns) > 0) then
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
  if (Selecting) and (SelStart.x <> SelEnd.x) and (SelStart.y <> SelEnd.y) then
  begin
    bx := min(SelStart.x, SelEnd.x);
    bdx := max(SelStart.x, SelEnd.x);
    by := min(SelStart.y, SelEnd.y);
    bdy := max(SelStart.y, SelEnd.y);
    by := (by - ScrollOffset);
    bdy := (bdy - ScrollOffset);
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
    if UseAsList then
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
    Row := (slSize * by + slMain) + (bx * 3);
    for y := by to bdy do
    begin
      slPnt := PRGB24(Row);
      for x := bx to bdx do
      begin
        slPnt.R := Ra[slPnt.R];
        slPnt.G := Ga[slPnt.G];
        slPnt.B := Ba[slPnt.B];
        inc(slPnt)
      end;
      inc(Row, slSize);
    end;
    FMainBmp.Canvas.Pen.width := 1;
    FMainBmp.Canvas.Pen.Color := ColorSel;
    FMainBmp.Canvas.Brush.Style := bsClear;
    FMainBmp.Canvas.Rectangle(Rect(bx, by, bdx + 1, bdy + 1));
  end;
  // Paint Header if needed
  if FHeaderVisible then
  begin
    R := Rect(0, 0, FMainBmp.width - 1, FHeaderHeight);
    if (Assigned(TMethod(FOnHeaderPaint).Code)) then
      FOnHeaderPaint(self, FMainBmp.Canvas, R, FCellXOffset, FHeaderHot,
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

  if UseAsList then
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height, FMainBmp.Canvas.Handle,
      sbHorz.Position, 0, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, scr, FMainBmp.Height, FMainBmp.Canvas.Handle,
      0, 0, SRCCOPY);
  FScrollPos := 0;
end;

procedure TrkCustomSmartView.CalcSelectionGroups;
var
  i: Integer;
  Sel, bool: Boolean;
  Cell, R: TRect;
  Grp: PSmartGroup;
  GrpIdx: Integer;
  PixPosY: Integer;
begin
  if SmartGroups.Count <= 0 then
    Exit;
  PixPosY := FCellOffset;
  GrpIdx := 0;
  Selection.Clear;
  bool := False;
  repeat
    Grp := SmartGroups[GrpIdx];
    if (Grp.Items.Count > 0) and (Grp.Expanded) then
    begin
      PixPosY := PixPosY + FDividerHeight + FCellSpace;
      i := 0;
      while i < Grp.Items.Count do
      begin
        if FCellAuto then
          Cell.Left := FCellSpaceX + (i mod ViewColumns) * CellColSize
        else
          Cell.Left := FCellXOffset + (i mod ViewColumns) * ImgWidth;
        Cell.Top := PixPosY - FCellOffset;
        Cell.Right := Cell.Left + FCellWidth;
        Cell.Bottom := Cell.Top + FCellHeight;
        Sel := IntersectRect(R, SelRect, Cell);
        if Sel and (not CellSelect) then
          if Assigned(TMethod(FOnCellSelect).Code) then
            FOnCellSelect(self, FMainBmp.Canvas, Cell, Grp.Items[i], Sel);
        if Sel then
        begin
          if not bool then
          begin
            bool := True;
            IdxGrp := GrpIdx;
            IdxItem := i;
          end;
          Selection.Add(Grp.Items[i]);
        end;
        inc(i);
        if (i < Grp.Items.Count) and (i mod ViewColumns = 0) then
          inc(PixPosY, (FCellHeight + FCellSpace));
      end;
      inc(PixPosY, (FCellHeight + FCellSpace));
    end
    else
      inc(PixPosY, FDividerHeight);
    inc(GrpIdx);
  until (GrpIdx = SmartGroups.Count) or (PixPosY > SelRect.Bottom);
end;

procedure TrkCustomSmartView.CalcSelectionSimple;
var
  i: Integer;
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
    if FCellAuto then
      Cell.Left := FCellSpaceX + (i mod ViewColumns) * CellColSize
    else
      Cell.Left := FCellXOffset + (i mod ViewColumns) * ImgWidth;
    Cell.Top := PixPosY - FCellOffset;
    Cell.Right := Cell.Left + FCellWidth;
    Cell.Bottom := Cell.Top + FCellHeight;
    Sel := IntersectRect(R, SelRect, Cell);
    if (Sel) and (not CellSelect) then
      if Assigned(TMethod(FOnCellSelect).Code) then
        FOnCellSelect(self, FMainBmp.Canvas, Cell, Items[i], Sel);
    if Sel then
    begin
      if not bool then
      begin
        bool := True;
        IdxItem := i;
      end;
      Selection.Add(Items[i]);
    end;
    inc(i);
    if (i < Items.Count) and (i mod ViewColumns = 0) then
      inc(PixPosY, (FCellHeight + FCellSpace));
  until (i = Items.Count) or (PixPosY > SelRect.Bottom + FCellHeight);
end;

procedure TrkCustomSmartView.CalcSelection;
begin
  SelRect.Left := min(SelStart.x, SelEnd.x);
  SelRect.Right := max(SelStart.x, SelEnd.x);
  SelRect.Top := min(SelStart.y, SelEnd.y);
  SelRect.Bottom := max(SelStart.y, SelEnd.y);
  if FUseGroups then
    CalcSelectionGroups
  else
    CalcSelectionSimple;
  if Assigned(TMethod(FOnSelecting).Code) then
    FOnSelecting(self, Selection.Count);
end;

procedure TrkCustomSmartView.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkCustomSmartView.WMSETFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FFocused := True;
    Invalidate;
  end;
end;

procedure TrkCustomSmartView.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FFocused := False;
    Invalidate;
  end;
end;

procedure TrkCustomSmartView.SetCanFocused;
begin
  FCanFocused := Value;
  if FCanFocused then
    TabStop := True
  else
    TabStop := False;
end;

procedure TrkCustomSmartView.SetSelColor;
var
  C: Longint;
  i: Integer;
begin
  C := ColorToRGB(Value);
  FColorSelection.B := (C shr 16) and $FF;
  FColorSelection.G := (C shr 8) and $FF;
  FColorSelection.R := C and $FF;
  for i := 0 to 255 do
  begin
    Ra[i] := Byte((FColorSelection.R - i) shr 3 + i);
    Ga[i] := Byte((FColorSelection.G - i) shr 3 + i);
    Ba[i] := Byte((FColorSelection.B - i) shr 3 + i);
  end;
end;

procedure TrkCustomSmartView.SetSelected(const Value: tPoint);
begin
  IdxGrp := Value.x;
  IdxItem := Value.y;
end;

function TrkCustomSmartView.GetSelColor;
begin
  Result := (FColorSelection.B shl 16) + (FColorSelection.G shl 8)
    + FColorSelection.R;
end;

function TrkCustomSmartView.GetSelected: tPoint;
begin
  Result.x := IdxGrp;
  Result.y := IdxItem;
end;

function TrkCustomSmartView.IsFocused;
begin
  Result := (FFocused) and (FCanFocused);
end;

procedure TrkCustomSmartView.EditCaption;
var
  x, y, w, i, h: Integer;
  Group: PSmartGroup;
  txt: string;
  R: TRect;
begin
  if Grouped then
    i := min(IdxGrp, IdxItem)
  else
    i := IdxItem;
  if (i < 0) or (not AllowEdit) then
    Exit;
  h := ClientHeight;
  if FHeaderVisible then
    h := h - FHeaderHeight;
  if hsbVisible then
    h := h - pnlHorz.Height;
  if Grouped then
    R := GetItemRect(IdxGrp, IdxItem)
  else
    R := GetItemRect(-1, IdxItem);
  if R.Top < sbVert.Position then
    sbVert.Position := R.Top - FCellSpace;
  y := (sbVert.Position + h) - ImgHeight;
  if R.Top > y then
    sbVert.Position := R.Bottom - (h - FCellSpace);
  if hsbVisible then
    sbHorz.Position := 0;
  IsEditing := True;
  Paint;
  EditInline.ParentFont := True;
  if Assigned(FOnCellEdit) then
    if Grouped then
    begin
      Group := SmartGroups[IdxGrp];
      FOnCellEdit(self, Group.Items[IdxItem], x, y, w, txt);
    end
    else
      FOnCellEdit(self, Items[IdxItem], x, y, w, txt);
  EditInline.Left := R.Left + x;
  if vsbVisible then
    EditInline.Top := (R.Top + y) - sbVert.Position
  else
    EditInline.Top := R.Top + y;
  if FHeaderVisible then
  begin
    EditInline.Top := EditInline.Top + FHeaderHeight;
    EditInline.width := max(FColumns[0], 120 + w)
  end
  else
    EditInline.width := (ImgWidth - FCellSpaceX) + w;
  EditInline.AutoSelect := not FEditFilename;
  EditInline.Text := txt;
  EditInline.Visible := True;
  EditLastValue := UpperCase(txt);
  if FEditFilename then
  begin
    EditInline.SelStart := 0;
    i := Length(txt) - Length(ExtractFileExt(txt));
    EditInline.SelLength := i;
  end;
  EditInline.SetFocus;
end;

procedure TrkCustomSmartView.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Idx: Integer;
  done: Boolean;
  txt: string;
begin
  if Grouped then
    Idx := PSmartGroup(SmartGroups[IdxGrp]).Items[IdxItem]
  else
    Idx := Items[IdxItem];
  done := False;
  if Key = VK_Escape then
    done := True
  else if Key = VK_Return then
  begin
    done := True;
    txt := EditInline.Text;
    if Assigned(FOnEditAccept) then
      FOnEditAccept(self, Idx, txt, done);
    if (not done) then
      EditInline.Text := txt;
  end
  else
    done := False;
  EditInline.Visible := not done;
  IsEditing := EditInline.Visible;
  if done then
    SetFocus;
end;

procedure TrkCustomSmartView.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TrkCustomSmartView.EditExit(Sender: TObject);
begin
  EditLastValue := '';
  EditInline.Visible := False;
  IsEditing := False;
  SetFocus;
  Invalidate;
end;

procedure TrkCustomSmartView.SimpleMouseDown(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  Sel, i, j, k: Integer;
begin
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    Sel := ItemAtXY(Point(x, y), (not FCellSelect));
    if Sel <> -1 then
    begin
      if FMulti then
      begin
        if (ssShift in Shift) and not(ssCtrl in Shift) then
        begin
          Selection.Clear;
          i := Sel;
          j := IdxItem;
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
        if (ssCtrl in Shift) and not(ssShift in Shift) then
        begin
          i := Selection.IndexOf(Items[Sel]);
          if i <> -1 then
            Selection.Delete(i)
          else
            Selection.Add(Items[Sel]);
        end;
        if (not(ssCtrl in Shift)) and (not(ssShift in Shift)) then
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
      IdxItem := Sel;
      SetInView(-1, IdxItem);
      Invalidate;
    end
    else
    begin
      Selection.Clear;
      IdxItem := -1;
      FPrevSel.Top := -1;
      FPrevSel.Left := -1;
      FPrevSel.Right := -1;
      FPrevSel.Bottom := -1;
      SelStart.x := x;
      SelStart.y := y + ScrollOffset;
      if FHeaderVisible then
        SelStart.y := SelStart.y - FHeaderHeight;
      SelEnd := SelStart;
      Selecting := FMulti;
      viewDeltaScroll := 0;
      viewDeltaScrollX := 0;
      SelTimer.Enabled := True;
      Invalidate;
    end;
    if Assigned(FOnSelecting) then
      FOnSelecting(self, Selection.Count);
  end;
  FLastIndex := IdxItem;
end;

function TrkCustomSmartView.GrpDoShiftSelect: Boolean;
// Do selection with shift key held down ;)
var
  g1, g2, i1, i2, i: Integer;
  B: Boolean;
  G: PSmartGroup;
begin
  B := False;
  g1 := min(IdxGrp, LastIdxGrp);
  g2 := max(IdxGrp, LastIdxGrp);
  i1 := min(IdxItem, LastIdxItem);
  i2 := max(IdxItem, LastIdxItem);
  if (g1 <> -1) then
  begin
    B := True;
    Selection.Clear;
    if g1 = g2 then
    begin
      if i1 = -1 then
        i1 := 0;
      if i2 = -1 then
        i2 := 0;
      G := SmartGroups[g1];
      for i := i1 to i2 do
        Selection.Add(G.Items[i]);
    end
    else
    begin
      if g1 = IdxGrp then
      begin
        i1 := IdxItem;
        i2 := LastIdxItem;
      end
      else
      begin
        i1 := LastIdxItem;
        i2 := IdxItem;
      end;
      G := SmartGroups[g1];
      if (G.Expanded) then
      begin
        if i1 = -1 then
          i1 := 0;
        for i := i1 to G.Items.Count - 1 do
          Selection.Add(G.Items[i]);
      end;
      G := SmartGroups[g2];
      if (G.Expanded) then
        if i2 > -1 then
          for i := 0 to i2 do
            Selection.Add(G.Items[i]);
      inc(g1);
      while (g1 < g2) do
      begin
        G := SmartGroups[g1];
        if G.Expanded then
          for i := 0 to G.Items.Count - 1 do
            Selection.Add(G.Items[i]);
        inc(g1);
      end;
    end;
  end;
  GrpDoShiftSelect := B;
end;

procedure TrkCustomSmartView.GrpMouseDown(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  i: Integer;
  Group: PSmartGroup;
  pt: tPoint;
  GrpHit: Boolean;
begin
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    pt := GrpItemAtXY(Point(x, y), not CellSelect);
    IdxGrp := pt.x;
    IdxItem := pt.y;
    if (IdxGrp <> -1) and (IdxItem <> -1) then
    begin
      Group := SmartGroups[IdxGrp];
      if FMulti then
      begin
        if (ssShift in Shift) and not(ssCtrl in Shift) then
          GrpDoShiftSelect;
        if (ssCtrl in Shift) and not(ssShift in Shift) then
        begin
          i := Selection.IndexOf(Group.Items[IdxItem]);
          if i <> -1 then
            Selection.Delete(i)
          else
            Selection.Add(Group.Items[IdxItem]);
        end;
        if (not(ssCtrl in Shift)) and (not(ssShift in Shift)) then
        begin
          if Selection.IndexOf(Group.Items[IdxItem]) = -1 then
          begin
            Selection.Clear;
            Selection.Add(Group.Items[IdxItem]);
          end;
        end;
      end
      else
      begin
        Selection.Clear;
        Selection.Add(Group.Items[IdxItem]);
      end;
      SetInView(IdxGrp, IdxItem);
    end
    else if (IdxGrp <> -1) and (IdxItem = -1) then
    begin
      if hsbVisible then
        x := x + sbHorz.Position;
      if (x > FCellOffset) and (x < FMainBmp.width - FCellOffset) then
      begin
        Selection.Clear;
        Group := PSmartGroup(SmartGroups[IdxGrp]);
        // GrpHit:= Group.Expanded;
        // if Assigned(FOnDividerHit) then
        // FOnDividerHit(Self, FMainBmp.Canvas, IdxGrp, x, GrpHit);
        // Group.Expanded:= GrpHit;
        if (Button = mbLeft) then
          Group.Expanded := not Group.Expanded;
        CalcViewGroups;
        SetInView(IdxGrp, 0);
        sbVert.Position := sbVert.Position + 1;
        sbVert.Position := sbVert.Position - 1;
        Invalidate;
      end;
      SetInView(IdxGrp, IdxItem);
    end
    else
    begin
      Selection.Clear;
      IdxItem := -1;
      FPrevSel.Top := -1;
      FPrevSel.Left := -1;
      FPrevSel.Right := -1;
      FPrevSel.Bottom := -1;
      SelStart.x := x;
      SelStart.y := y + ScrollOffset;
      if FHeaderVisible then
        SelStart.y := SelStart.y - FHeaderHeight;
      SelEnd := SelStart;
      Selecting := FMulti;
      viewDeltaScroll := 0;
      viewDeltaScrollX := 0;
      SelTimer.Enabled := FMulti;
      Invalidate;
    end;
    if Assigned(FOnSelecting) then
      FOnSelecting(self, Selection.Count);
  end;
  LastIdxGrp := IdxGrp;
  LastIdxItem := IdxItem;
end;

procedure TrkCustomSmartView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
begin
  if (CanFocus) then
    SetFocus;
  if (Items.Count = 0) and (SmartGroups.Count = 0) then
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
        OnHeaderClick(self, FHeaderHot);
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
  if FUseGroups then
    GrpMouseDown(Button, Shift, x, y)
  else
    SimpleMouseDown(Button, Shift, x, y);
  y := y + YAdjOff;
  if Assigned(OnMouseDown) then
    OnMouseDown(self, Button, Shift, x, y);
end;

procedure TrkCustomSmartView.UpdateView;
begin
  if Items.Count > 0 then
    SetInView(ViewGrp, ViewIdx);
end;

procedure TrkCustomSmartView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: Integer);
var
  dwTicks, dwElapsed: DWord;
  Edit: Boolean;
begin
  FDragging := False;
  FDragColumn := -1;
  SelTimer.Enabled := False;
  Selecting := False;
  SelScroll := False;
  if dblClicked then
    dblClicked := False;
  Edit := False;
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
  if (not InHeader) and (not dblClicked) and (Button = mbLeft) then
  begin
    dwTicks := GetTickCount;
    if dwTicks >= FDownTick then
      dwElapsed := dwTicks - FDownTick
    else
      dwElapsed := $FFFFFFFF - FDownTick + dwTicks;
    if (dwElapsed > 300) and (Abs(FDownX - x) < Mouse.DragThreshold) and
      (Abs(FDownY - y) < Mouse.DragThreshold) and (FAllowEdit) then
      Edit := True;
  end;
  if Edit then
    EditCaption
  else
  begin
    Invalidate;
    if Assigned(OnMouseUp) then
      OnMouseUp(self, Button, Shift, x, y);
  end;
end;

procedure TrkCustomSmartView.CMHintShow(var Message: TCMHintShow);
var
  InfoTip: string;
  Item, i, j: Integer;
  R: TRect;
  pt: tPoint;
  bool: Boolean;
begin
  if Assigned(FOnHintShow) then
  begin
    if FUseGroups then
    begin
      pt := GrpItemAtXY(Message.HintInfo.CursorPos, False);
      if (pt.x <> -1) and (pt.y <> -1) then
      begin
        R := GetItemRect(pt.x, pt.y);
        Item := PSmartGroup(SmartGroups[pt.x]).Items[pt.y];
      end
      else
        Exit;
    end
    else
    begin
      Item := ItemAtXY(Message.HintInfo.CursorPos, False);
      if (Item <> -1) then
      begin
        R := GetItemRect(-1, Item);
        Item := Items[Item];
      end
      else
        Exit;
    end;
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
      j := FCellOffset;
      while i < FHintColumn do
      begin
        j := j + FColumns[i];
        i := i + 1;
      end;
      R.Left := j;
      R.Right := j + FColumns[i];
    end;
    bool := False;
    OnHintShow(self, Item, FHintColumn, InfoTip, bool);
    if (not bool) or (InfoTip = '') or (Selecting) or (FDragging) or
      (IsEditing) then
      Message.Result := -1
    else
    begin
      InflateRect(R, 1, 1);
      Message.HintInfo.HintStr := InfoTip;
      Message.HintInfo.CursorRect := R;
      Message.HintInfo.HideTimeout := 120000;
      Message.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure TrkCustomSmartView.MouseMove(Shift: TShiftState; x, y: Integer);
var
  Idx, Grp: Integer;
  pt: tPoint;
  ViewHeight, i, n, j, k, d0, d1, d2, d3: Integer;
  b1: Byte;
  bool, dummy: Boolean;
  s: string;
begin
  if dblClicked then
  begin
    dblClicked := False;
    Exit;
  end;
  if (Items.Count = 0) and (SmartGroups.Count = 0) then
    Exit;
  if ScrollMode then
  begin
    if (ssLeft in Shift) then
    begin
      SCY := FDownY - y;
      i := SCPos + SCY;
      if i < sbVert.min then
        sbVert.Position := sbVert.min
      else if i > sbVert.max then
        sbVert.Position := sbVert.max
      else
        sbVert.Position := i;
      Invalidate;
    end;
    Exit;
  end;
  if not Selecting then
  begin
    CalcGroupPos(sbVert.Position + y, b1, d0, d1, d2, d3);
    FHotGrp := d0;
  end;
  bool := False;
  if (FHeaderState = svSelected) and (ssLeft in Shift) then
    Exit;
  if not(ssLeft in Shift) then
    Selecting := False;
  if Selecting then
  begin
    if FHeaderVisible then
    begin
      y := y - FHeaderHeight;
      if x < 0 then
        viewDeltaScrollX := x * Abs(x)
      else if x > (ClientWidth - FCtrlSize) then
        viewDeltaScrollX := (x - (ClientWidth - FCtrlSize)) *
          (x - (ClientWidth - FCtrlSize))
      else
        viewDeltaScrollX := 0;
      if (viewDeltaScrollX <> 0) and (ssShift in Shift) then
        viewDeltaScrollX := viewDeltaScrollX * 2;
    end;
    if (FHeaderVisible) and (hsbVisible) then
      ViewHeight := ClientHeight - (pnlHorz.Height + FHeaderHeight)
    else
    if (FHeaderVisible) then
      ViewHeight := ClientHeight - FHeaderHeight
    else
      ViewHeight := ClientHeight;
    if y < 0 then
      viewDeltaScroll := y * Abs(y)
    else
    if y > ViewHeight then
      viewDeltaScroll := (y - ViewHeight) * (y - ViewHeight)
    else
      viewDeltaScroll := 0;
    if (viewDeltaScroll <> 0) and (ssShift in Shift) then
      viewDeltaScroll := viewDeltaScroll * 2;
    SelEnd.x := x + ScrollOffsetX;
    SelEnd.y := y + ScrollOffset;
    CalcSelection;
    bool := True;
  end
  else
  if (FHeaderVisible) and (FDragging) then
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
    if FUseGroups then
      CalcViewGroups
    else
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
            if (Abs(x - k) < 5) and (not dummy) then
            begin
              FDragColumn := j;
              FDragOffs := (x - k);
              self.Cursor := crHSplit;
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
            self.Cursor := crDefault;
            FDragColumn := -1;
          end;
        end;
      end
      else
      begin
        self.Cursor := crDefault;
        if (FHeaderHot <> -1) then
        begin
          FHeaderHot := -1;
          FHeaderState := svNormal;
          bool := True;
        end;
        if (Grouped) then
        begin
          pt := GrpItemAtXY(Point(x, y), (not FCellSelect));
          Grp := pt.x;
          Idx := pt.y;
          if (Idx <> HotIdx) or (Grp <> HotGrp) then
          begin
            HotIdx := Idx;
            HotGrp := Grp;
            bool := True;
          end;
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
      OnMouseMove(self, Shift, x, y);
  end;
  if bool then
    Invalidate;
end;

procedure TrkCustomSmartView.CMMouseLeave(var Message: TMessage);
begin
  FHeaderHot := -1;
  FHeaderState := svNormal;
  HotIdx := -1;
  HotGrp := -1;
  Invalidate;
end;

procedure TrkCustomSmartView.DblClick;
begin
  FDownTick := GetTickCount;
  if IsEditing then
    Exit;
  dblClicked := True;
  if not InHeader then
    if Assigned(OnDblClick) then
      OnDblClick(self);
end;

procedure TrkCustomSmartView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWord = (0, WS_BORDER);
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

procedure TrkCustomSmartView.OnSelScroll(Sender: TObject);
var
  i: Integer;
  bool: Boolean;
begin
  if ScrollMode then
  begin
    if SCUp then
      i := -SCValue
    else
      i := SCValue;
    i := sbVert.Position + i;
    if i < sbVert.min then
      sbVert.Position := sbVert.min
    else if i > sbVert.max then
      sbVert.Position := sbVert.max
    else
      sbVert.Position := i;
    Invalidate;
    if SCValue <> 0 then
      SCValue := Trunc((SCValue - Ln(SCValue)) / 1.1);
    SelTimer.Enabled := SCValue > 0;
    Exit;
  end;
  bool := False;
  if (viewDeltaScroll <> 0) then
  begin
    if viewDeltaScroll < 0 then
      SelEnd.y := ScrollOffset
    else
      SelEnd.y := ScrollOffset + ClientHeight;
    SelEnd.y := SelEnd.y + viewDeltaScroll;
    if vsbVisible then
    begin
      i := sbVert.Position + viewDeltaScroll;
      if i <> sbVert.Position then
      begin
        sbVert.Position := i;
        bool := True;
      end;
    end;
  end;
  if (hsbVisible) and (viewDeltaScrollX <> 0) then
  begin
    if viewDeltaScrollX < 0 then
      SelEnd.x := ScrollOffsetX
    else
      SelEnd.x := ScrollOffsetX + ClientWidth;
    i := sbHorz.Position + viewDeltaScrollX;
    if i <> sbHorz.Position then
    begin
      if i >= maxXScroll then
        i := maxXScroll;
      sbHorz.Position := i;
      bool := True;
    end;
  end;
  if bool then
    PostMessage(Handle, CM_REFRESH, 0, 0);
  CalcSelection;
  SelTimer.Enabled := Selecting;
end;

procedure TrkCustomSmartView.ClearSmartGroups;
var
  i: Integer;
  p: PSmartGroup;
begin
  if not Assigned(SmartGroups) then
    Exit;
  for i := SmartGroups.Count - 1 downto 0 do
  begin
    p := SmartGroups.Items[i];
    p.Caption := '';
    p.Items.Free;
    Dispose(p);
  end;
  SmartGroups.Clear;
  IdxGrp := -1;
  IdxItem := -1;
end;

procedure TrkCustomSmartView.DeleteSmartGroup(n: Integer);
var
  p: PSmartGroup;
begin
  if (n >= 0) and (n <= SmartGroups.Count) then
  begin
    p := SmartGroups.Items[n];
    p.Caption := '';
    p.Items.Free;
    Dispose(p);
    SmartGroups.Delete(n);
  end;
end;

procedure TrkCustomSmartView.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
end;

function TrkCustomSmartView.AddSmartGroup(s: string): PSmartGroup;
var
  Item: PSmartGroup;
begin
  New(Item);
  try
    Item.Caption := s;
    Item.Items := TIntList.Create;
    try
      SmartGroups.Add(Item);
    except
      Dispose(Item);
    end;
  except
    raise ;
  end;
  Result := Item;
end;

procedure TrkCustomSmartView.SetExpanded(Value: Boolean);
var
  i: Integer;
begin
  if Grouped then
  begin
    for i := 0 to SmartGroups.Count - 1 do
      PSmartGroup(SmartGroups[i]).Expanded := Value;
    CalcView(True);
    Invalidate;
  end;
end;

procedure TrkCustomSmartView.SetHeader(const Value: TrkHeaderItems);
begin
  FHeader.Assign(Value);
end;

function TrkCustomSmartView.SimpleDoKey(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Idx, old, i, j, k, maxInt: Integer;
  DoInvalid, done: Boolean;
begin
  j := sbVert.Position;
  maxInt := Range;
  Idx := -1;
  old := -1;
  done := True;
  DoInvalid := False;
  if (IdxItem = -1) and (Selection.Count = 0) then
  begin
    if (Shift = []) then
    begin
      IdxItem := 0;
      FLastIndex := 0;
      DoInvalid := True;
      done := False;
    end;
  end
  else
  begin
    Idx := IdxItem;
    old := Idx;
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
          sbVert.Position := sbVert.max;
          DoInvalid := True;
        end;
      vk_Next:
        begin
          i := j + ClientHeight;
          if i > sbVert.max then
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
      vk_Down:
        if Idx + ViewColumns < Items.Count then
          Idx := Idx + ViewColumns;
      vk_Up:
        if Idx - ViewColumns >= 0 then
          Idx := Idx - ViewColumns;
      vk_Left:
        if Idx - 1 >= 0 then
          Idx := Idx - 1;
      vk_Right:
        if Idx + 1 < Items.Count then
          Idx := Idx + 1;
      VK_Return:
        DblClick;
      vk_F2:
        EditCaption;
    else
      done := False;
    end;
  end;
  if (Idx <> old) and (Idx <> -1) then
  begin
    Selection.Clear;
    IdxItem := Idx;
    SetInView(-1, IdxItem);
    if (ssShift in Shift) then
    begin
      if FLastIndex <> -1 then
      begin
        j := min(IdxItem, FLastIndex);
        k := max(IdxItem, FLastIndex);
        for i := j to k do
          Selection.Add(Items[i]);
      end;
    end
    else
      Selection.Add(Items[Idx]);
    DoInvalid := True;
  end;
  if (not(ssShift in Shift)) then
    FLastIndex := IdxItem;
  if DoInvalid then
    Invalidate;
  if Assigned(FOnSelecting) then
    FOnSelecting(self, Selection.Count);
  Result := done;
end;

function TrkCustomSmartView.TextHeight(Text: string): Integer;
begin
  FMainBmp.Canvas.Font.Assign(Font);
  Result := FMainBmp.Canvas.TextHeight(Text);
end;

function TrkCustomSmartView.TextWidth(Text: string): Integer;
begin
  FMainBmp.Canvas.Font.Assign(Font);
  Result := FMainBmp.Canvas.TextWidth(Text);
end;

function TrkCustomSmartView.GetColumnSize(Column: Integer): Integer;
begin
  if (Column >= Low(FColumns)) and (Column <= High(FColumns)) then
    Result := FColumns[Column]
  else
    Result := -1;
end;

procedure TrkCustomSmartView.SetInView(Grp, Idx: Integer);
var
  y, i, h, t, v: Integer;
  R: TRect;
begin
  if (Grp >= SmartGroups.Count) and (Grouped) then
    Exit;
  YAdjOff := 0;
  if Grouped then
    i := max(Grp, Idx)
  else
    i := Idx;
  if i < 0 then
    Exit;
  h := ClientHeight;
  if hsbVisible then
    h := (h - pnlHorz.Height) - 1;
  if Grouped then
    R := GetItemRect(Grp, Idx)
  else
    R := GetItemRect(-1, Idx);
  if FHeaderVisible then
  begin
    R.Top := R.Top + (FHeaderHeight - FCellSpace);
    R.Bottom := (R.Bottom + (FHeaderHeight - FCellSpace));
    t := FHeaderHeight
  end
  else
    t := 0;
  v := sbVert.Position;
  if (R.Top > v) and (R.Bottom < v + h) then
  begin
    sbVert.Position := sbVert.Position + 1;
    sbVert.Position := sbVert.Position - 1;
    Exit;
  end;
  if R.Top <= (sbVert.Position + t) then
  begin
    YAdjOff := sbVert.Position;
    sbVert.Position := (R.Top - FCellSpace) - t;
    YAdjOff := YAdjOff - sbVert.Position;
  end;
  y := (sbVert.Position + h);
  if R.Bottom >= y then
  begin
    YAdjOff := sbVert.Position;
    sbVert.Position := (R.Bottom - h) + 2;
    YAdjOff := YAdjOff - sbVert.Position;
  end;
  Invalidate;
end;

procedure TrkCustomSmartView.SelectAll(Inverted: Boolean);
var
  i, j, G, n: Integer;
  Grp: PSmartGroup;
begin
  if Grouped then
  begin
    for G := 0 to SmartGroups.Count - 1 do
    begin
      Grp := SmartGroups[G];
      if Grp.Expanded then
        for i := 0 to Grp.Items.Count - 1 do
        begin
          j := Grp.Items[i];
          n := Selection.IndexOf(j);
          if (Inverted) then
          begin
            if (n <> -1) then
              Selection.Delete(n)
            else
              Selection.Add(j);
          end
          else if (n = -1) then
            Selection.Add(j);
        end;
    end;
  end
  else
    for i := 0 to Items.Count - 1 do
    begin
      j := Items[i];
      n := Selection.IndexOf(j);
      if (Inverted) then
      begin
        if (n <> -1) then
          Selection.Delete(n)
        else
          Selection.Add(j);
      end
      else if (n = -1) then
        Selection.Add(j);
    end;
  Invalidate;
  IdxItem := -1;
  if Assigned(TMethod(FOnSelecting).Code) then
    FOnSelecting(self, Selection.Count);
end;

procedure TrkCustomSmartView.SetAtTop(Grp, Idx: Integer);
var
  i, t: Integer;
  R: TRect;
begin
  YAdjOff := 0;
  CalcView(False);
  if Grouped then
    i := max(Grp, Idx)
  else
    i := Idx;
  if i < 0 then
    Exit;
  if Grouped then
    R := GetItemRect(Grp, Idx)
  else
    R := GetItemRect(-1, Idx);
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

function TrkCustomSmartView.GrpDoKey(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Grp, Idx, i, j: Integer;
  DoInvalid, DoU, done, First: Boolean;
  G: PSmartGroup;

  procedure ShiftSel(Grp, Idx: Integer);
  begin
    IdxGrp := Grp;
    IdxItem := Idx;
    GrpDoShiftSelect;
  end;

begin
  Grp := -1;
  Idx := -1;
  j := sbVert.Position;
  DoInvalid := False;
  DoU := False;
  done := True;
  First := False;
  if (IdxGrp = -1) then
  begin
    Selection.Clear;
    IdxGrp := 0;
    IdxItem := -1;
    DoInvalid := True;
    DoU := False;
  end
  else
  begin
    Grp := IdxGrp;
    Idx := IdxItem;
    G := SmartGroups[Grp];
    case Key of
      vk_Home:
        if (ssCtrl in Shift) then
          sbVert.Position := 0;
      vk_End:
        sbVert.Position := sbVert.max;
      vk_Next:
        begin
          i := j + ClientHeight;
          if i > sbVert.max then
            sbVert.Position := Range
          else
            sbVert.Position := i;
        end;
      vk_Prior:
        begin
          i := j - ClientHeight;
          if i < 0 then
            sbVert.Position := 0
          else
            sbVert.Position := i;
        end;
      vk_Down:
        begin
          if (ssCtrl in Shift) then
          begin
            i := j + ImgHeight;
            if i > sbVert.max then
              sbVert.Position := Range
            else
              sbVert.Position := i;
          end
          else
          begin
            if Idx = -1 then
            begin
              if G.Expanded then
                Idx := 0
              else if Grp < SmartGroups.Count - 1 then
                inc(Grp);
            end
            else
            begin
              if (Idx + ViewColumns < G.Items.Count) and (G.Expanded) then
                Idx := Idx + ViewColumns
              else
              begin
                if Grp < SmartGroups.Count - 1 then
                begin
                  inc(Grp);
                  Idx := -1;
                end;
              end;
            end;
          end;
          if (ssShift in Shift) then
            ShiftSel(Grp, Idx);
          DoInvalid := True;
          DoU := True;
        end;
      vk_Up:
        begin
          if (ssCtrl in Shift) then
          begin
            i := j - ImgHeight;
            if i < 0 then
              sbVert.Position := 0
            else
              sbVert.Position := i;
          end
          else
          begin
            if (Idx - ViewColumns >= 0) and (G.Expanded) then
            begin
              if Idx <> -1 then
                Idx := Idx - ViewColumns
            end
            else
            begin
              if Idx > -1 then
                Idx := -1
              else
              begin
                if Grp > 0 then
                begin
                  Dec(Grp);
                  if PSmartGroup(SmartGroups[Grp]).Expanded then
                    Idx := PSmartGroup(SmartGroups[Grp]).Items.Count - 1
                  else
                    Idx := -1;
                end;
              end;
            end;
          end;
          if (ssShift in Shift) then
            ShiftSel(Grp, Idx);
          DoInvalid := True;
          DoU := True;
        end;
      vk_Left:
        begin
          if (Idx = -1) and (not(ssCtrl in Shift)) then
          begin
            if (G.Expanded) then
              G.Expanded := False;
            CalcView(True);
            Idx := -1;
          end
          else if (ssCtrl in Shift) then
          begin
            for i := 0 to SmartGroups.Count - 1 do
              PSmartGroup(SmartGroups[i]).Expanded := False;
            CalcView(True);
            Idx := -1;
          end
          else if Idx > 0 then
            Idx := Idx - 1
          else
            Idx := -1;
          if (ssShift in Shift) then
            ShiftSel(Grp, Idx);
          DoInvalid := True;
          DoU := True;
        end;
      vk_Right:
        begin
          if (Idx = -1) and (not(ssCtrl in Shift)) then
          begin
            if (not G.Expanded) then
              G.Expanded := True;
            CalcView(True);
            Idx := 0;
          end
          else if (ssCtrl in Shift) then
          begin
            for i := 0 to SmartGroups.Count - 1 do
              PSmartGroup(SmartGroups[i]).Expanded := True;
            CalcView(True);
            Idx := -1;
            First := True;
            sbVert.Position := Range;
          end
          else
          begin
            if (Idx < G.Items.Count - 1) and (G.Expanded) then
              Idx := Idx + 1
            else
            begin
              if Grp < SmartGroups.Count - 1 then
              begin
                inc(Grp);
                Idx := -1;
              end;
            end;
          end;
          if (ssShift in Shift) then
            ShiftSel(Grp, Idx);
          DoInvalid := True;
          DoU := True;
        end;
      VK_Return:
        begin
          DblClick;
        end;
      vk_Space:
        begin
          if (Assigned(OnClick)) then
            OnClick(self);
        end;
      vk_F1:
        Application.HelpContext(HelpContext);
      vk_F2:
        EditCaption;
    else
      begin
        DoU := False;
        done := False;
      end;
    end;
  end;
  if (DoU) and (not(ssShift in Shift)) then
  begin
    SetInView(Grp, Idx);
    if First then
      Idx := 0;
    G := SmartGroups[Grp];
    Selection.Clear;
    if Idx > -1 then
    begin
      Selection.Add(G.Items[Idx]);
      IdxItem := G.Items[Idx];
    end;
    IdxGrp := Grp;
    IdxItem := Idx;
  end
  else if (ssShift in Shift) and (DoU) then
  begin
    IdxGrp := Grp;
    IdxItem := Idx;
    SetInView(Grp, Idx);
  end;
  if DoInvalid then
    Invalidate;
  if (not(ssShift in Shift)) then
  begin
    FLastIndex := IdxItem;
    LastIdxGrp := IdxGrp;
    LastIdxItem := IdxItem;
  end;
  if Assigned(FOnSelecting) then
    FOnSelecting(self, Selection.Count);
  Result := done;
end;

procedure TrkCustomSmartView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Items.Count = 0) and (SmartGroups.Count = 0) then
    Exit;
  if FUseGroups then
    GrpDoKey(Key, Shift)
  else
    SimpleDoKey(Key, Shift);
  inherited;
end;

procedure TrkCustomSmartView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TrkCustomSmartView.WMKeyDown(var Message: TWMKeyDown);
begin
  with Message do
    KeyDown(CharCode, KeyDataToShiftState(KeyData));
end;

procedure TrkCustomSmartView.WMKeyUp(var Message: TWMKeyDown);
begin
  with Message do
    KeyUp(CharCode, KeyDataToShiftState(KeyData));
end;

procedure Register;
begin
  RegisterComponents('SmartView', [TrkSmartView]);
end;

{ TrkHeaderItem }

constructor TrkHeaderItem.Create(Collection: TCollection);
begin
  inherited;
  FAccsending := True;
  FActive := False;
  FCaption := '';
  FSize := 50;
  FTag := 0;
end;

destructor TrkHeaderItem.Destroy;
begin
  inherited;
end;

{ TrkHeaderItems }

function TrkHeaderItems.Add: TrkHeaderItem;
begin
  Result := TrkHeaderItem( inherited Add);
end;

constructor TrkHeaderItems.Create(AOwner: TrkCustomSmartView);
begin
  inherited Create(TrkHeaderItem);
  FOwner := AOwner;
end;

function TrkHeaderItems.GetItem(Index: Integer): TrkHeaderItem;
begin
  Result := TrkHeaderItem( inherited GetItem(index));
end;

function TrkHeaderItems.GetOwner: TPersistent;
begin
  Result := TPersistent(FOwner);
end;

end.
