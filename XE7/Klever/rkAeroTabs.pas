unit rkAeroTabs;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

// AeroTabs by Roy Magne Klever
// © 2010 by Roy Magne Klever. All rights reserved
//
// This file is not distributable without permission by Roy Magne Klever
// WEB: www.rmklever.com
// Mail: roymagne@rmklever.com
//
// version 1.0

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms, Messages,
  ImgList, Dialogs, Menus, StdCtrls, GDIPObj, GDIPAPI,
  Types, UxTheme, Themes;

const
  MinWidth: Integer = 12;
  MaxWidth: Integer = 192;
  PolyTabCount = 10;

type
  TabState = (stNormal, stHot, stSelected);
  TOnTabCloseEvent = procedure(Sender: TObject; Index: Integer;
    var Close: Boolean) of object;
  TOnGetImageIndexEvent = procedure(Sender: TObject; Tab: Integer;
    var Index: Integer) of object;
  TabPoly = array [0 .. 9] of TPoint;

  TrkAeroTabs = class(TCustomControl)
  private
    { Private declarations }
    FActiveTab: Integer;
    FBmp: TBitmap;
    FButtonDown: Boolean;
    FCanDrag: Boolean;
    FCloseDown: Boolean;
    FColor: TColor;
    FColorBackground: TColor;
    FDefPopup: TPopupMenu;
    FDragging: Boolean;
    FDragPoint: TPoint;
    FDragTab: Integer;
    FFont: TFont;
    FHotButton: Boolean;
    FHotClose: Boolean;
    FHotIdx: Integer;
    FImages: TCustomImageList;
    FShowButton: Boolean;
    FTabClose: Integer;
    FTabOffset: Integer;
    FTabOffsetTop: Integer;
    FTabX: Integer;
    FTabs: TStringList;
    FTabsHeight: Integer;
    FTabOverLap: Integer;
    FTabWidth: Integer;
    FShowImages: Boolean;
    FShowClose: Boolean;
    FTransparent: Boolean;
    FOnAddClick: TNotifyEvent;
    FOnCloseTab: TOnTabCloseEvent;
    FOnGetImageIdx: TOnGetImageIndexEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnTabChange: TNotifyEvent;
    procedure CalcTabWidth;
    procedure PaintTabs;
    procedure SetTabs(const Value: TStringList);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetActiveTab(const Value: Integer);
    procedure SetTabOffset(const Value: Integer);
    procedure SetTabOffsetTop(const Value: Integer);
    procedure SetShowButton(const Value: Boolean);
    function OverButton(x, y: Integer): Boolean;
    function OverClose(x, y: Integer): Integer;
    function GetTabPoly(Idx: Integer): TabPoly;
    procedure SetShowClose(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure SetBackground(const Value: TColor);
    procedure SetColors;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure DoPopup(Sender: TObject);
    procedure DoPopupClick(Sender: TObject);
  protected
    { Protected declarations }
    FirstRun: Boolean;
    InCreate: Boolean;
    Brushes: array [0 .. 6] of TGPSolidBrush;
    Pens: array [0 .. 6] of TGPPen;
    procedure Click; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTab(ATab: string);
    procedure AddObject(ATab: string; AObject: TObject);
    procedure DeleteTab(AIndex: Integer);
    function GetTabAtXY(x, y: Integer): Integer;
    procedure InsertTab(AIndex: Integer; ATab: string);
    procedure InsertObj(AIndex: Integer; ATab: string; AObject: TObject);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property ActiveTab: Integer read FActiveTab write SetActiveTab default 0;
    property AllowTabDrag: Boolean read FCanDrag Write FCanDrag default False;
    property Color: TColor read FColor write SetColor default clWindow;
    property ColorBackground: TColor read FColorBackground write SetBackground
      default clBtnFace;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property Images: TCustomImageList read FImages write FImages;
    property PopupMenu;
    property ShowButton: Boolean read FShowButton write SetShowButton default
      True;
    property ShowClose: Boolean read FShowClose write SetShowClose default True;
    property ShowHint;
    property ShowImages: Boolean read FShowImages write SetShowImages default
      False;
    property TabOffset: Integer read FTabOffset write SetTabOffset default 0;
    property TabOffsetTop: Integer read FTabOffsetTop write SetTabOffsetTop
      default 0;
    property Tabs: TStringList read FTabs write SetTabs;
    property Visible;
    property OnAddClick: TNotifyEvent read FOnAddClick write FOnAddClick;
    property OnClick;
    property OnCloseTab: TOnTabCloseEvent read FOnCloseTab write FOnCloseTab;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex: TOnGetImageIndexEvent read FOnGetImageIdx write
      FOnGetImageIdx;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnTabChange: TNotifyEvent read FOnTabChange write FOnTabChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('rmklever', [TrkAeroTabs]);
end;

{ TrkAeroTabs }

function Blend(Color1, Color2: TColor; A: Byte): TColor;
var
  c1, c2: LongInt;
  r, g, b, v1, v2: Byte;
begin
  A := Round(2.55 * A);
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  v1 := Byte(c1);
  v2 := Byte(c2);
  r := Byte(A * (v1 - v2) shr 8 + v2);
  v1 := Byte(c1 shr 8);
  v2 := Byte(c2 shr 8);
  g := Byte(A * (v1 - v2) shr 8 + v2);
  v1 := Byte(c1 shr 16);
  v2 := Byte(c2 shr 16);
  b := Byte(A * (v1 - v2) shr 8 + v2);
  Result := (b shl 16) + (g shl 8) + r;
end;

function GetTabsPoly(x1, y1, x2, y2: Integer): TabPoly; inline;
begin
  Result[0].x := x1;
  Result[0].y := y2;
  Result[1].x := x1 + 3;
  Result[1].y := y2 - 4;
  Result[2].x := x1 + 11;
  Result[2].y := y1 + 3;
  Result[3].x := x1 + 13;
  Result[3].y := y1 + 1;
  Result[4].x := x1 + 15;
  Result[4].y := y1;
  Result[5].x := x2 - 15;
  Result[5].y := y1;
  Result[6].x := x2 - 13;
  Result[6].y := y1 + 1;
  Result[7].x := x2 - 11;
  Result[7].y := y1 + 3;
  Result[8].x := x2 - 3;
  Result[8].y := y2 - 4;
  Result[9].x := x2;
  Result[9].y := y2;
end;

procedure TrkAeroTabs.AddObject(ATab: string; AObject: TObject);
begin
  Tabs.AddObject(ATab, AObject);
  FActiveTab := Tabs.Count - 1;
  Invalidate;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self);
end;

procedure TrkAeroTabs.AddTab(ATab: string);
begin
  Tabs.Add(ATab);
  FActiveTab := Tabs.Count - 1;
  Invalidate;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self);
end;

procedure TrkAeroTabs.CalcTabWidth;
var
  tw, w: Integer;
begin
  tw := ClientWidth - (FTabOffset + FTabOverLap);
  if FShowButton then
    tw := tw - 24;
  if Tabs.Count > 0 then
    w := (tw div FTabs.Count)
  else
    w := 0;
  if w < MinWidth then
    w := MinWidth;
  if ((w - FTabOverLap) * FTabs.Count) > tw then
    w := MaxWidth;
  if w > MaxWidth then
    w := MaxWidth;
  FTabWidth := w + FTabOverLap;
end;

procedure TrkAeroTabs.Click;
begin
  inherited;
end;

procedure TrkAeroTabs.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TrkAeroTabs.CMMouseLeave(var Msg: TMessage);
begin
  FHotIdx := -1;
  if not FCloseDown then
    FTabX := -1;
  Invalidate;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TrkAeroTabs.SetColors;
var
  i: Integer;
  c: TColor;
begin
  // Tabs border
  if not FirstRun then
  begin
    for i := Low(Brushes) to High(Brushes) do
      Brushes[i].Free;
    for i := Low(Pens) to High(Pens) do
      Pens[i].Free;
  end;
  // Tabs
  c := ColorToRGB(FColor);
  Brushes[0] := TGPSolidBrush.Create(MakeColor(196, 216, 216, 219));
  Brushes[1] := TGPSolidBrush.Create(MakeColor(224, 240, 240, 244));
  Brushes[2] := TGPSolidBrush.Create(MakeColor(255, Byte(c), Byte(c shr 8),
      Byte(c shr 16)));
  c := Blend(c, clBlack, 50);
  Pens[0] := TGPPen.Create(MakeColor(255, 152, 152, 152));
  Pens[1] := TGPPen.Create(MakeColor(255, 176, 176, 176));
  Pens[2] := TGPPen.Create(MakeColor(255, Byte(c), Byte(c shr 8), Byte(c shr 16)));
  Pens[6] := TGPPen.Create(MakeColor(212, 196, 196, 196));
  Pens[6].SetWidth(1.6);
  // Tab close button
  Pens[3] := TGPPen.Create(MakeColor(255, 156, 156, 156));
  Pens[4] := TGPPen.Create(MakeColor(255, 255, 255, 255));
  Pens[5] := TGPPen.Create(MakeColor(255, 74, 103, 140));
  Pens[3].SetWidth(1.6);
  Pens[4].SetWidth(1.6);
  // Close tab
  Brushes[3] := TGPSolidBrush.Create(MakeColor(255, 192, 52, 52));
  Brushes[4] := TGPSolidBrush.Create(MakeColor(255, 52, 52, 52));
  // Helpers
  Brushes[5] := TGPSolidBrush.Create(MakeColor(128, 255, 255, 255));
  Brushes[6] := TGPSolidBrush.Create(MakeColor(255, 255, 255, 255));
  FirstRun := False;
end;

procedure TrkAeroTabs.DoPopupClick(Sender: TObject);
var
  i: Integer;
begin
  i := (Sender as TMenuItem).Tag;
  if i = 99 then
  begin
    if Assigned(FOnAddClick) then
      FOnAddClick(Self);
  end
  else
    ActiveTab := i;
  Invalidate;
end;

procedure TrkAeroTabs.DoPopup(Sender: TObject);
var
  i: Integer;
  AMenuItem: TMenuItem;

  procedure AddMenuItem(Name: string; Idx: Integer);
  begin
    AMenuItem := TMenuItem.Create(FDefPopup);
    AMenuItem.Caption := Name;
    AMenuItem.OnClick := DoPopupClick;
    AMenuItem.Tag := Idx;
    AMenuItem.RadioItem := (Idx <> -1) and (Idx <> 99);
    AMenuItem.Checked := Idx = FActiveTab;
    FDefPopup.Items.Add(AMenuItem);
  end;

begin
  for i := FDefPopup.Items.Count - 1 downto 0 do
    FDefPopup.Items[i].Free;
  AddMenuItem('New tab', 99);
  AddMenuItem('-', -1);
  for i := 0 to Tabs.Count - 1 do
    AddMenuItem(Tabs[i], i);
end;

constructor TrkAeroTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InCreate := True;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  Width := 150;
  Height := 30;
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf32Bit;
  FCanDrag := False;
  FColor := clWindow;
  FColorBackground := clBtnFace;
  FDefPopup := TPopupMenu.Create(nil);
  FDefPopup.OnPopup := DoPopup;
  FirstRun := True;
  SetColors;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Color := clBlack;
  FFont.Size := 8;
  FShowButton := True;
  FShowImages := False;
  FShowClose := True;
  FTabOffset := 0;
  FTabOffsetTop := 0;
  FTabs := TStringList.Create;
  FTabOverLap := 17;
  FTransparent:= False;
  ActiveTab := 0;
  FHotButton := False;
  FHotIdx := -1;
  FCloseDown := False;
  FTabX := -1;
  InCreate:= False;
end;

procedure TrkAeroTabs.DeleteTab(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Tabs.Count) then
  begin
    Tabs.Delete(AIndex);
    if (FActiveTab > Tabs.Count - 1) then
      FActiveTab := Tabs.Count - 1;
    if Assigned(FOnTabChange) then
      FOnTabChange(Self);
    Invalidate;
  end;
end;

destructor TrkAeroTabs.Destroy;
var
  i: Integer;
begin
  for i := Low(Brushes) to High(Brushes) do
    Brushes[i].Free;
  for i := Low(Pens) to High(Pens) do
    Pens[i].Free;
  FFont.Free;
  FTabs.Free;
  FBmp.Free;
  FDefPopup.Free;
  inherited Destroy;
end;

function PtInPoly(const Points: array of TPoint; x, y: Integer): Boolean;
var
  Count, K, J: Integer;
begin
  Result := False;
  Count := Length(Points);
  J := Count - 1;
  for K := 0 to Count - 1 do
  begin
    if ((Points[K].y <= y) and (y < Points[J].y)) or
      ((Points[J].y <= y) and (y < Points[K].y)) then
    begin
      if (x < (Points[J].x - Points[K].x) * (y - Points[K].y) /
          (Points[J].y - Points[K].y) + Points[K].x) then
        Result := not Result;
    end;
    J := K;
  end;
end;

function PtInEllipse(const Pt: TPoint; const EBR: TRect): Boolean;
begin
  Result := (Sqr((Pt.x * 2 - EBR.Left - EBR.Right) / (EBR.Right - EBR.Left))
      + Sqr((Pt.y * 2 - EBR.Top - EBR.Bottom) / (EBR.Bottom - EBR.Top))) <= 1;
end;

function TrkAeroTabs.GetTabPoly(Idx: Integer): TabPoly;
var
  x1, y1, x2, y2: Integer;
begin
  x1 := FTabOffset;
  if Idx > 0 then
    x1 := x1 + (Idx * (FTabWidth - FTabOverLap));
  x2 := x1 + FTabWidth;
  y1 := FTabOffsetTop;
  y2 := ClientHeight - 1;
  Result:= GetTabsPoly(x1, y1, x2, y2);
end;

procedure TrkAeroTabs.InsertObj(AIndex: Integer; ATab: string;
  AObject: TObject);
begin
  if (AIndex >= 0) and (AIndex < Tabs.Count) then
  begin
    Tabs.InsertObject(AIndex, ATab, AObject);
    FActiveTab := AIndex;
    Invalidate;
    if Assigned(FOnTabChange) then
      FOnTabChange(Self);
  end;
end;

procedure TrkAeroTabs.InsertTab(AIndex: Integer; ATab: string);
begin
  if (AIndex >= 0) and (AIndex < Tabs.Count) then
  begin
    Tabs.Insert(AIndex, ATab);
    FActiveTab := AIndex;
    Invalidate;
    if Assigned(FOnTabChange) then
      FOnTabChange(Self);
  end;
end;

function TrkAeroTabs.GetTabAtXY(x, y: Integer): Integer;
var
  i, Idx: Integer;
  InFrontOf, InBackOf: Boolean;
  Tab: TabPoly;
begin
  Result := -1;
  Idx := x - FTabOffset;
  if (Idx > 0) and (Idx < ((FTabWidth - FTabOverLap) * Tabs.Count)
      + FTabOverLap) then
  begin
    i := Idx div (FTabWidth - FTabOverLap);
    if (i > 0) then
    begin
      Tab:= GetTabPoly(i - 1);
      InFrontOf := PtInPoly(Tab, x, y);
      if InFrontOf and (ActiveTab = i) then
      begin
        Tab:= GetTabPoly(i);
        InFrontOf := not PtInPoly(Tab, x, y);
      end;
    end
    else
      InFrontOf := False;
    if i < Tabs.Count - 1 then
    begin
      Tab:= GetTabPoly(i + 1);
      InBackOf := PtInPoly(Tab, x, y);
    end
    else
      InBackOf := False;
    if InFrontOf then
      i := i - 1;
    if InBackOf then
      i := i + 1;
    Tab:= GetTabPoly(i);
    if not PtInPoly(Tab, x, y) then
      i := -1;
    if i < Tabs.Count then
      Result := i
    else
      Result := -1;
  end
end;

function TrkAeroTabs.OverButton(x, y: Integer): Boolean;
var
  x1, y1, x2, y2: Integer;
  Button: array [0 .. 7] of TPoint;
begin
  x1 := (FTabOffset + ((FTabWidth - FTabOverLap) * Tabs.Count) -
      (FTabWidth - FTabOverLap)) + FTabWidth;
  y1 := FTabOffsetTop + 6;
  x2 := x1 + 20;
  y2 := y1 + 15;
  Button[0].x := x1 + 3;
  Button[0].y := y2;
  Button[1].x := x1;
  Button[1].y := y2 - 2;
  Button[2].x := x1 - 4;
  Button[2].y := y1 + 2;
  Button[3].x := x1 - 3;
  Button[3].y := y1;
  Button[4].x := x2 - 7;
  Button[4].y := y1;
  Button[5].x := x2 - 4;
  Button[5].y := y1 + 2;
  Button[6].x := x2;
  Button[6].y := y2 - 2;
  Button[7].x := x2;
  Button[7].y := y2;
  Result := PtInPoly(Button, x, y);
end;

function TrkAeroTabs.OverClose(x, y: Integer): Integer;
var
  i, n, m: Integer;
  r: TRect;
  Tab: TabPoly;
begin
  Result := -1;
  i := GetTabAtXY(x, y);
  FTabClose := i;
  if i <> -1 then
  begin
    Tab:= GetTabPoly(i);
    n := Tab[High(TabPoly)].X - 26;
    m := FTabOffsetTop + ((FTabsHeight) shr 1) + 2;
    r.Left := n - 3;
    r.Top := m - 8;
    r.Right := r.Left + 12;
    r.Bottom := r.Top + 12;
    if PtInRect(r, Point(x, y)) then
      Result := i;
  end;
end;

procedure TrkAeroTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: Integer);
var
  i: Integer;
  bool: Boolean;
  Pt: TPoint;
begin
  inherited;
  FTabX := -1;
  FCanDrag := False;
  FCloseDown := False;
  FDragging := False;
  FDragPoint.x := -1;
  FDragPoint.y := -1;
  if Button = mbLeft then
  begin
    FTabX := OverClose(x, y);
    if (FTabX = -1) or (not FShowClose) then
    begin
      i := GetTabAtXY(x, y);
      if (i <> -1) then
      begin
        ActiveTab := i;
        if Assigned(FOnTabChange) then
          FOnTabChange(Self);
        Invalidate;
        FDragTab := i;
        FCanDrag := True;
        FDragPoint.x := x;
        FDragPoint.y := y;
      end;
    end
    else
    begin
      FCloseDown := True;
      Invalidate;
    end;

    if FShowButton then
    begin
      bool := OverButton(x, y);
      if bool <> FButtonDown then
        Invalidate;
      FButtonDown := bool;
    end;
  end
  else if Button = mbRight then
    if not Assigned(PopupMenu) then
    begin
      GetCursorPos(Pt);
      FDefPopup.Popup(Pt.x, Pt.y);
    end;
end;

procedure TrkAeroTabs.CMHintShow(var Message: TCMHintShow);
var
  i, J: Integer;
  TabR: TRect;
  Pt: TPoint;
  Tab: TabPoly;
begin
  Pt := Message.HintInfo.CursorPos;
  i := GetTabAtXY(Pt.x, Pt.y);
  if (i <> -1) then
  begin
    J := FTabWidth;
    if (FShowImages) and (FTabWidth > 80) then
      J := J - (17 + FTabOverLap);
    if FShowClose then
      J := J - 31;
    if FBmp.Canvas.TextWidth(Tabs[i]) > J then
    begin
      Tab:= GetTabPoly(i);
      TabR.TopLeft := Point(Tab[0].x, Tab[4].y);
      TabR.BottomRight := Point(Tab[High(TabPoly)].x, Tab[High(TabPoly)].y);
      Message.HintInfo.HintStr := Tabs[i];
      Message.HintInfo.CursorRect := TabR;
      Message.HintInfo.HideTimeout := 12000;
      Message.Result := 0;
    end;
  end
  else
    inherited;
end;

function Distance(x1, y1, x2, y2: Integer): Integer;
var
  i, A, r0, r1: Integer;
begin
  r0 := x2 - x1;
  r1 := y2 - y1;
  A := r0 * r0 + r1 * r1;
  r1 := (1 + A) div 2;
  for i := 1 to 12 do
  begin
    r0 := r1;
    if r0 > 0 then
      r1 := (r0 + (A div r0)) div 2;
  end;
  Distance := r1;
end;

procedure TrkAeroTabs.MouseMove(Shift: TShiftState; x, y: Integer);
var
  i: Integer;
  bool: Boolean;
begin
  inherited;

  if not FDragging then
  begin
    Hint := '';
    i := GetTabAtXY(x, y);
    if (i <> FHotIdx) then
    begin
      FHotIdx := i;
      Invalidate;
    end;

    i := OverClose(x, y);
    if not FCloseDown then
      FTabX := i;
    FHotClose := (FTabX = i);

    if FShowButton then
    begin
      bool := OverButton(x, y);
      if bool <> FHotButton then
      begin
        FHotButton := bool;
        Invalidate;
      end;
    end;

    if (ssLeft in Shift) and (not FDragging) then
      if (FCanDrag) then
        if (FDragPoint.x <> -1) and (FDragPoint.y <> -1) then
        begin
          if (Distance(x, y, FDragPoint.x, FDragPoint.y) > 20) then
          begin
            FCanDrag := False;
            FDragging := True;
            FDragPoint.x := -1;
            FDragPoint.y := -1;
          end;
        end;
  end;

  if FDragging then
  begin
    i := GetTabAtXY(x, y);
    if (i <> FDragTab) and (i <> -1) then
    begin
      Tabs.Move(FDragTab, i);
      ActiveTab := i;
      FDragTab := i;
      FHotClose := False;
    end;
  end;

  Invalidate;
end;

procedure TrkAeroTabs.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: Integer);
var
  CloseTab: Boolean;
begin
  inherited;
  FDragging := False;
  if (FShowButton) and (OverButton(x, y) and (FButtonDown)) then
  begin
    if Assigned(FOnAddClick) then
      FOnAddClick(Self);
    Invalidate;
  end;

  if FShowClose then
  begin
    if (FShowClose) and (OverClose(x, y) = FTabX) and (FCloseDown) then
    begin
      if Assigned(FOnCloseTab) then
      begin
        FOnCloseTab(Self, FTabX, CloseTab);
        if CloseTab then
          DeleteTab(FTabX);
      end;
      Invalidate;
    end;
  end;
  FButtonDown := False;
  FHotButton := False;
  FCloseDown := False;
  FHotClose := False;
  FTabX := -1;
end;

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
    SetViewportOrgEx(DC, Position.x - Left, Position.y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TrkAeroTabs.PaintTabs;
type
  PRGB = ^TRGB;
  TRGB = record
    b, g, r, a: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 65565] of TRGB;
var
  i, n, m, x1, x2, y1, y2, bx, by: Integer;
  txt: string;
  TabsR, TabR: TRect;
  Tab: TabPoly;
  TabAdd: array [0 .. 12] of TPoint;
  DownColor: array [0 .. 1] of GDIPAPI.ARGB;
  TabsCanvas: TGPGraphics;
  brushButtonDown: TGPPathGradientBrush;
  bImages, bClose: Boolean;
  slSize, slPtr: Integer;
  w, h: Integer;
  Row: PRGBArray;
  slSize2, slPtr2: Integer;
  Row2: PRGBArray;
  A: Byte;
  Mask: TBitmap;
  MaskCanvas: TGPGraphics;
begin
  if InCreate then
    Exit;
  FBmp.Width := ClientWidth;
  FBmp.Height := ClientHeight;
  FBmp.Canvas.Font := FFont;
  FBmp.Canvas.Brush.Color := FColorBackground;
  if FTransparent then
    DrawParentImage(Self, FBmp.Canvas)
  else
  begin
    // Clear canvas
    slPtr := Integer(FBmp.ScanLine[0]);
    slSize := Integer(FBmp.ScanLine[1]) - slPtr;
    for h := 0 to FBmp.Height - 1 do
    begin
      Row := PRGBArray(slPtr);
      for w := 0 to FBmp.Width - 1 do
      begin
        Row[w].r := Byte(FColorBackground);
        Row[w].g := Byte(FColorBackground shr 8);
        Row[w].b := Byte(FColorBackground shr 16);
        Row[w].a := 0;
      end;
      slPtr := slPtr + slSize;
    end;
    // canvas cleared
  end;

  if Tabs.Count <> 0 then
  begin
    CalcTabWidth;

    // Make mask image
    Mask := TBitmap.Create;
    Mask.PixelFormat := pf32Bit;
    Mask.Width := FTabWidth + 1;
    Mask.Height := ClientHeight;
    MaskCanvas := TGPGraphics.Create(Mask.Canvas.Handle);
    MaskCanvas.SetSmoothingMode(SmoothingModeHighQuality);
    Mask.Canvas.Brush.Color := clBlack;
    Mask.Canvas.FillRect(Mask.Canvas.ClipRect);
    x1 := 0;
    x2 := x1 + FTabWidth;
    y1 := FTabOffsetTop;
    y2 := ClientHeight - 1;
    FTabsHeight := y2 - y1;
    Tab := GetTabsPoly(x1, y1, x2, y2);
    MaskCanvas.FillPolygon(Brushes[6], PGPPoint(@Tab), PolyTabCount);
    MaskCanvas.DrawPolygon(Pens[1], PGPPoint(@Tab), PolyTabCount);
    // mask done

    TabsR := ClientRect;
    TabsCanvas := TGPGraphics.Create(FBmp.Canvas.Handle);
    TabsCanvas.SetSmoothingMode(SmoothingModeHighQuality);
    x1 := FTabOffset + ((FTabWidth - FTabOverLap) * Tabs.Count) -
      (FTabWidth - FTabOverLap);
    x2 := x1 + FTabWidth;
    y1 := FTabOffsetTop;
    y2 := ClientHeight - 1;
    bx := (x1 + FTabWidth);
    by := y1 + 2;
    bImages := (FShowImages) and Assigned(FOnGetImageIdx) and Assigned(FImages)
      and (FTabWidth > 80);
    bClose := FShowClose;
    for i := FTabs.Count - 1 downto 0 do
    begin
      if i <> FActiveTab then
      begin
        txt := FTabs[i];
        Tab := GetTabsPoly(x1, y1, x2, y2);
        if i = FHotIdx then
        begin
          TabsCanvas.FillPolygon(Brushes[1], PGPPoint(@Tab), PolyTabCount);
          TabsCanvas.DrawPolygon(Pens[1], PGPPoint(@Tab), PolyTabCount);
        end
        else
        begin
          TabsCanvas.FillPolygon(Brushes[0], PGPPoint(@Tab), PolyTabCount);
          TabsCanvas.DrawPolygon(Pens[0], PGPPoint(@Tab), PolyTabCount);
        end;

        // use mask image to remove overlaping tab area
        slPtr := Integer(FBmp.ScanLine[y1]);
        slSize := Integer(FBmp.ScanLine[y1 + 1]) - slPtr;
        slPtr2 := Integer(Mask.ScanLine[y1]);
        slSize2 := Integer(Mask.ScanLine[y1 + 1]) - slPtr2;
        if i > 0 then
        begin
          for h := y1 to y2 - 1 do
          begin
            Row := PRGBArray(slPtr);
            Row2 := PRGBArray(slPtr2);
            for w := 0 to FTabOverLap - 1 do
              if Row2[w + (FTabWidth - FTabOverLap) + 1].g <> 0 then
              begin
                Row[w + x1].r := Byte(FColorBackground);
                Row[w + x1].g := Byte(FColorBackground shr 8);
                Row[w + x1].b := Byte(FColorBackground shr 16);
                Row[w + x1].A := 0;
              end;
            slPtr := slPtr + slSize;
            slPtr2 := slPtr2 + slSize2;
          end;
        end;
        // overlaping tab area cleared

        TabR.TopLeft := Point(x1 + FTabOverLap, y1);
        TabR.BottomRight := Point(x2 - FTabOverLap, y2);
        if bImages then
        begin
          FOnGetImageIdx(Self, i, n);
          m := ((ClientHeight - FTabOffsetTop) div 2) - 8;
          FImages.Draw(FBmp.Canvas, TabR.Left - 3, FTabOffsetTop + m, n);
          TabR.Left := TabR.Left + 16;
        end;

        // Paint close button?
        if bClose then
        begin
          n := x2 - 26;
          m := FTabOffsetTop + (FTabsHeight shr 1) + 2;
          if (FTabX = i) and FHotClose then
          begin
            if FCloseDown then
              TabsCanvas.FillEllipse(Brushes[4], n - 3, m - 9, 12, 12)
            else
              TabsCanvas.FillEllipse(Brushes[3], n - 3, m - 9, 12, 12);
            TabsCanvas.DrawLine(Pens[4], n, m, n + 6, m - 6);
            TabsCanvas.DrawLine(Pens[4], n + 6, m, n, m - 6);
          end
          else
          begin
            TabsCanvas.DrawLine(Pens[3], n, m, n + 6, m - 6);
            TabsCanvas.DrawLine(Pens[3], n + 6, m, n, m - 6);
          end;
          TabR.Right := TabR.Right - 12;
        end;

        FBmp.Canvas.Brush.Style := bsClear;
        FBmp.Canvas.Font.Color := $00404040;
        DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), TabR,
          DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);

        slPtr := Integer(FBmp.ScanLine[TabR.Top + 1]);
        slSize := Integer(FBmp.ScanLine[TabR.Top + 2]) - slPtr;
        Row := PRGBArray(slPtr);
        A := Row[TabR.Left].A;
        for h := TabR.Top + 1 to TabR.Bottom - 1 do
        begin
          Row := PRGBArray(slPtr);
          for w := TabR.Left to TabR.Right - 1 do
            Row[w].A := A;
          slPtr := slPtr + slSize;
        end;

      end;
      x1 := x1 - (FTabWidth - FTabOverLap);
      x2 := x1 + FTabWidth;
    end;

    // Paint Active Tab
    if x1 < 0 then
      x1 := (FTabWidth - FTabOverLap) * FActiveTab;
    x1 := x1 + FTabOffset;
    x2 := x1 + FTabWidth;
    txt := FTabs[FActiveTab];

    //Tab := GetTabsPoly(x1, y1 - 1, x2, y2 - 1);
    //Tab := GetTabsPoly(x1, y1, x2, y2);
    //TabsCanvas.DrawPolygon(Pens[6], PGPPoint(@Tab), PolyTabCount);

    Tab := GetTabsPoly(x1, y1, x2, y2);
    TabsCanvas.FillPolygon(Brushes[2], PGPPoint(@Tab), PolyTabCount);
    TabsCanvas.DrawPolygon(Pens[2], PGPPoint(@Tab), PolyTabCount);
    FBmp.Canvas.Pen.Color := clGray;
    FBmp.Canvas.MoveTo(0, y2);
    FBmp.Canvas.LineTo(ClientWidth, y2);
    FBmp.Canvas.Pen.Color := FColor;
    FBmp.Canvas.MoveTo(x1 + 1, y2);
    FBmp.Canvas.LineTo(x2, y2);
    TabR.TopLeft := Point(x1 + FTabOverLap, y1);
    TabR.BottomRight := Point((x2) - FTabOverLap, y2);
    if bImages then
    begin
      FOnGetImageIdx(Self, FActiveTab, n);
      m := ((ClientHeight - FTabOffsetTop) div 2) - 8;
      FImages.Draw(FBmp.Canvas, TabR.Left - 3, FTabOffsetTop + m, n);
      TabR.Left := TabR.Left + 16;
    end;
    if bClose then
    begin
      n := x2 - 26;
      m := FTabOffsetTop + (FTabsHeight shr 1) + 2;
      if (FTabX = ActiveTab) and FHotClose then
      begin
        if FCloseDown then
          TabsCanvas.FillEllipse(Brushes[4], n - 3, m - 9, 12, 12)
        else
          TabsCanvas.FillEllipse(Brushes[3], n - 3, m - 9, 12, 12);
        TabsCanvas.DrawLine(Pens[4], n, m, n + 6, m - 6);
        TabsCanvas.DrawLine(Pens[4], n + 6, m, n, m - 6);
      end
      else
      begin
        TabsCanvas.DrawLine(Pens[3], n, m, n + 6, m - 6);
        TabsCanvas.DrawLine(Pens[3], n + 6, m, n, m - 6);
      end;
      TabR.Right := TabR.Right - 12;
    end;
    FBmp.Canvas.Brush.Style := bsClear;
    FBmp.Canvas.Font.Color := clBlack;
    DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), TabR,
      DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or
        DT_VCENTER);

    slPtr := Integer(FBmp.ScanLine[TabR.Top]);
    slSize := Integer(FBmp.ScanLine[TabR.Top + 1]) - slPtr;
    for h := TabR.Top to TabR.Bottom - 2 do
    begin
      Row := PRGBArray(slPtr);
      for w := TabR.Left to TabR.Right - 1 do
        Row[w].A := Row[TabR.Right].A;
      slPtr := slPtr + slSize;
    end;

    // Paint Add button
    if FShowButton then
    begin
      x1 := bx;
      x2 := x1 + 20;
      y1 := by + 2;
      y2 := y1 + 15;
      Tab[0].x := x1 + 3;
      Tab[0].y := y2;
      Tab[1].x := x1;
      Tab[1].y := y2 - 2;
      Tab[2].x := x1 - 4;
      Tab[2].y := y1 + 2;
      Tab[3].x := x1 - 3;
      Tab[3].y := y1;
      Tab[4].x := x2 - 7;
      Tab[4].y := y1;
      Tab[5].x := x2 - 4;
      Tab[5].y := y1 + 2;
      Tab[6].x := x2;
      Tab[6].y := y2 - 2;
      Tab[7].x := x2;
      Tab[7].y := y2;
      if FButtonDown then
      begin
        brushButtonDown := TGPPathGradientBrush.Create(PGPPoint(@Tab), 8);
        brushButtonDown.SetCenterColor(MakeColor(255, 255, 255, 255));
        n := 1;
        DownColor[0] := MakeColor(255, 220, 220, 220);
        brushButtonDown.SetSurroundColors(PARGB(@DownColor), n);
        TabsCanvas.FillPolygon(brushButtonDown, PGPPoint(@Tab), 8);
        brushButtonDown.Free;
        TabsCanvas.DrawPolygon(Pens[1], PGPPoint(@Tab), 8);
      end
      else
      begin
        if FHotButton then
        begin
          TabsCanvas.FillPolygon(Brushes[1], PGPPoint(@Tab), 8);
          TabsCanvas.DrawPolygon(Pens[1], PGPPoint(@Tab), 8);
        end
        else
        begin
          TabsCanvas.FillPolygon(Brushes[0], PGPPoint(@Tab), 8);
          TabsCanvas.DrawPolygon(Pens[0], PGPPoint(@Tab), 8);
        end
      end;
      x1 := bx + 4;
      y1 := by + 8;
      TabAdd[0].x := x1;
      TabAdd[0].y := y1;
      TabAdd[1].x := x1 + 3;
      TabAdd[1].y := y1;
      TabAdd[2].x := x1 + 3;
      TabAdd[2].y := y1 - 3;
      TabAdd[3].x := x1 + 6;
      TabAdd[3].y := y1 - 3;
      TabAdd[4].x := x1 + 6;
      TabAdd[4].y := y1;
      TabAdd[5].x := x1 + 9;
      TabAdd[5].y := y1;
      TabAdd[6].x := x1 + 9;
      TabAdd[6].y := y1 + 3;
      TabAdd[7].x := x1 + 6;
      TabAdd[7].y := y1 + 3;
      TabAdd[8].x := x1 + 6;
      TabAdd[8].y := y1 + 6;
      TabAdd[9].x := x1 + 3;
      TabAdd[9].y := y1 + 6;
      TabAdd[10].x := x1 + 3;
      TabAdd[10].y := y1 + 3;
      TabAdd[11].x := x1;
      TabAdd[11].y := y1 + 3;
      TabAdd[12].x := x1;
      TabAdd[12].y := y1;
      TabsCanvas.FillPolygon(Brushes[2], PGPPoint(@TabAdd), 13);
      TabsCanvas.DrawPolygon(Pens[5], PGPPoint(@TabAdd), 13);
    end;
    TabsCanvas.Free;
    MaskCanvas.Free;
    Mask.Free;
  end;

  Row := PRGBArray(Integer(FBmp.ScanLine[ClientHeight - 1]));
  for w := 0 to FBmp.Width - 1 do
    Row[w].a := 255;

  BitBlt(Canvas.Handle, 0, 0, FBmp.Width, FBmp.Height, FBmp.Canvas.Handle, 0,
    0, SRCCOPY);
end;

procedure TrkAeroTabs.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      PaintTabs;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TrkAeroTabs.Resize;
begin
  Height := 25 + FTabOffsetTop;
  Invalidate;
end;

procedure TrkAeroTabs.SetActiveTab(const Value: Integer);
begin
  if (Value >= 0) and (Value < FTabs.Count) then
  begin
    FActiveTab := Value;
    if Assigned(FOnTabChange) then
      FOnTabChange(Self);
  end
  else
    FActiveTab := 0;
  Invalidate;
end;

procedure TrkAeroTabs.SetBackground(const Value: TColor);
begin
  FColorBackground := Value;
  SetColors;
  Invalidate;
end;

procedure TrkAeroTabs.SetColor(const Value: TColor);
begin
  FColor := Value;
  SetColors;
  Invalidate;
end;

procedure TrkAeroTabs.SetFont(const Value: TFont);
begin
  if (FFont <> Value) then
    FFont.Assign(Value);
  Invalidate;
end;

procedure TrkAeroTabs.SetShowButton(const Value: Boolean);
begin
  FShowButton := Value;
  Invalidate;
end;

procedure TrkAeroTabs.SetShowClose(const Value: Boolean);
begin
  FShowClose := Value;
  Invalidate;
end;

procedure TrkAeroTabs.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
  Invalidate;
end;

procedure TrkAeroTabs.SetTabOffset(const Value: Integer);
begin
  FTabOffset := Value;
  Invalidate;
end;

procedure TrkAeroTabs.SetTabOffsetTop(const Value: Integer);
begin
  FTabOffsetTop := Value;
  Height := 25 + Value;
  Invalidate;
end;

procedure TrkAeroTabs.SetTabs(const Value: TStringList);
begin
  FTabs.Assign(Value);
  CalcTabWidth;
  if FActiveTab > Tabs.Count - 1 then
    ActiveTab := Tabs.Count - 1;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self);
  Invalidate;
end;

procedure TrkAeroTabs.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkAeroTabs.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkAeroTabs.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not(csLoading in ComponentState) then
    Invalidate;
end;

end.
