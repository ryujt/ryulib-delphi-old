unit rkSmartTabs;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
// SmartTabs by Roy Magne Klever
// ?2011 by Roy Magne Klever. All rights reserved
//
// This file is not distributable without permission by Roy Magne Klever
// WEB: www.rmklever.com
// Mail: roymagne@rmklever.com
//
// version 1.8 July 2011
// Licensed under MPL 1.1

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms, Messages,
  ImgList, Dialogs, Menus, StdCtrls, GDIPObj, GDIPAPI;

const
  MinWidth: Integer = 12;
  MaxWidth: Integer = 192;
  PinnedWidth: Integer = 52;
  PolyTabCount = 10;
  TextFadeOut = 16;

type
  TabState = (stNormal, stHot, stSelected);
  TOnCustomDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AText: string;
    AState: TabState; AIndex: Integer) of object;
  TOnTabCloseEvent = procedure(Sender: TObject; Index: Integer; var Close: Boolean) of object;
  TOnGetImageIndexEvent = procedure(Sender: TObject; Tab: Integer;
    var Index: Integer) of object;
  TabPoly = array [0 .. 9] of TPoint;

  TrkSmartTabs = class(TCustomControl)
  private
    { Private declarations }
    FActiveTab: Integer;
    FBmp: TBitmap;
    FButtonDown: Boolean;
    FCanDrag: Boolean;
    FCloseDown: Boolean;
    FCloseHide: Boolean;
    FColorBackground: TColor;
    FColorBrdActive: TColor;
    FColorBrdHot: TColor;
    FColorBrdInActive: TColor;
    FColorTabActive: TColor;
    FColorTabHot: TColor;
    FColorTabInActive: TColor;
    FColorTxtActive: TColor;
    FColorTxtHot: TColor;
    FColorTxtInActive: TColor;
    FColorOutActive: TColor;
    FColorOutHot: TColor;
    FColorOutInActive: TColor;
    FDefPopup: TPopupMenu;
    FDragging: Boolean;
    FDragOff: Integer;
    FDragPoint: TPoint;
    FDragX: Integer;
    FDragTab: Integer;
    FGdiPTExt: Boolean;
    FHotButton: Boolean;
    FHotClose: Boolean;
    FHotIdx: Integer;
    FImages: TCustomImageList;
    FLevelTabActive: Byte;
    FLevelTabHot: Byte;
    FLevelTabInActive: Byte;
    FMinCloseWidth: Integer;
    FOutline: Boolean;
    FPinnedStr: String;
    FShowButton: Boolean;
    FPinnedTabs: Integer;
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
    FOnCustomDraw: TOnCustomDrawEvent;
    FOnGetImageIdx: TOnGetImageIndexEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnTabChange: TNotifyEvent;
    FSeeThruTabs: Boolean;
    procedure CalcTabWidth;
    procedure PaintTabs;
    procedure SetTabs(const Value: TStringList);
    procedure SetActiveTab(const Value: Integer);
    procedure SetTabOffset(const Value: Integer);
    procedure SetShowButton(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function OverButton(x, y: Integer): Boolean;
    function OverClose(x, y: Integer): Integer;
    function GetTabPoly(Idx: Integer): TabPoly;
    procedure SetShowClose(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure SetBackground(const Value: TColor);
    procedure SetColors;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure DoPopup(Sender: TObject; APoint: TPoint);
    procedure DoPopupClick(Sender: TObject);
    procedure SetCloseHide(const Value: Boolean);
    procedure SetPinnedStr(const Value: String);
    procedure SetColorActive(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorInActive(const Value: TColor);
    procedure SetLevelTabActive(const Value: Byte);
    procedure SetLevelTabHot(const Value: Byte);
    procedure SetLevelTabInActive(const Value: Byte);
    procedure SetColTxtActive(const Value: TColor);
    procedure SetColTxtHot(const Value: TColor);
    procedure SetColTxtInActive(const Value: TColor);
    procedure SetColBrdActive(const Value: TColor);
    procedure SetColBrdHot(const Value: TColor);
    procedure SetColBrdInActive(const Value: TColor);
    procedure SetSeeThruTabs(const Value: Boolean);
    procedure SetOutline(const Value: Boolean);
    procedure SetGdiPText(const Value: Boolean);
    procedure SetOutActive(const Value: TColor);
    procedure SetOutHot(const Value: TColor);
    procedure SetOutInActive(const Value: TColor);
  protected
    { Protected declarations }
    ClsBtnX: Integer;
    FirstRun: Boolean;
    InCreate: Boolean;
    Brushes: array [0 .. 7] of TGPSolidBrush;
    Pens: array [0 .. 5] of TGPPen;
    procedure Click; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
  public
    { Public declarations }
    PopUpTab: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTab(ATab: string);
    procedure AddObject(ATab: string; AObject: TObject);
    procedure DeleteTab(AIndex: Integer);
    function GetTabAtXY(x, y: Integer): Integer;
    procedure InsertTab(AIndex: Integer; ATab: string);
    procedure InsertObj(AIndex: Integer; ATab: string; AObject: TObject);
    procedure PinTab(AIndex: Integer);
    procedure UnPinTab(AIndex: Integer);
    function GetTabName(AIndex: Integer): String;
    procedure SetTabName(AIndex: Integer; AName: String);
    function TabPinned(AIndex: Integer): Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property ActiveTab: Integer read FActiveTab write SetActiveTab;
    property AllowTabDrag: Boolean read FCanDrag Write FCanDrag default False;
    property AutoHideClose: Boolean read FCloseHide write SetCloseHide default False;
    property ColorTabActive: TColor read FColorTabActive write SetColorActive;
    property ColorTabHot: TColor read FColorTabHot write SetColorHot;
    property ColorTabInActive: TColor read FColorTabInActive write SetColorInActive;
    property ColorTxtActive: TColor read FColorTxtActive write SetColTxtActive;
    property ColorTxtHot: TColor read FColorTxtHot write SetColTxtHot;
    property ColorTxtInActive: TColor read FColorTxtInActive write SetColTxtInActive;
    property ColorBrdActive: TColor read FColorBrdActive write SetColBrdActive;
    property ColorBrdHot: TColor read FColorBrdHot write SetColBrdHot;
    property ColorBrdInActive: TColor read FColorBrdInActive write SetColBrdInActive;
    property ColorBackground: TColor read FColorBackground write SetBackground default clBtnFace;
    property ColorOutActive: TColor read FColorOutActive write SetOutActive default clGray;
    property ColorOutHot: TColor read FColorOutHot write SetOutHot default clGray;
    property ColorOutInActive: TColor read FColorOutInActive write SetOutInActive default clGray;
    property Enabled;
    property Font;
    property GdiPlusText: Boolean read FGdiPTExt write SetGdiPText default False;
    property Images: TCustomImageList read FImages write FImages;
    property LevelTabActive: Byte read FLevelTabActive write SetLevelTabActive;
    property LevelTabHot: Byte read FLevelTabHot write SetLevelTabHot;
    property LevelTabInActive: Byte read FLevelTabInActive write SetLevelTabInActive;
    property Outline: Boolean read FOutline write SetOutline default False;
    property PinnedStr: String read FPinnedStr write SetPinnedStr;
    property PopupMenu;
    property SeeThruTabs: Boolean read FSeeThruTabs write SetSeeThruTabs;
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property ShowClose: Boolean read FShowClose write SetShowClose default True;
    property ShowHint;
    property ShowImages: Boolean read FShowImages write SetShowImages default False;
    property TabOffset: Integer read FTabOffset write SetTabOffset default 0;
    property Tabs: TStringList read FTabs write SetTabs;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnAddClick: TNotifyEvent read FOnAddClick write FOnAddClick;
    property OnClick;
    property OnCloseTab: TOnTabCloseEvent read FOnCloseTab write FOnCloseTab;
    property OnCustomDraw: TOnCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex: TOnGetImageIndexEvent read FOnGetImageIdx write FOnGetImageIdx;
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
  RegisterComponents('rmklever', [TrkSmartTabs]);
end;

{ TrkSmartTabs }

function Blend(Color1, Color2: TColor; Value: Byte): TColor;
var
  i, j: LongInt;
begin
  Value := Round(2.56 * Value);
  i := ColorToRGB(Color1);
  j := ColorToRGB(Color2);
  Result := Byte((Value * (Byte(i) - Byte(j))) shr 8 + Byte(j));
  Result := Result + (Byte((Value * (Byte(i shr 8) - Byte(j shr 8))) shr 8 +
    Byte(j shr 8)) shl 8);
  Result := Result + (Byte((Value * (Byte(i shr 16) - Byte(j shr 16))) shr 8 +
    Byte(j shr 16)) shl 16);
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

procedure TrkSmartTabs.AddObject(ATab: string; AObject: TObject);
begin
  if AObject <> nil then
    Tabs.AddObject(ATab, AObject)
  else
    Tabs.Add(ATab);
  FActiveTab := Tabs.Count - 1;
  Invalidate;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self);
end;

procedure TrkSmartTabs.AddTab(ATab: string);
begin
  AddObject(ATab, nil);
end;

procedure TrkSmartTabs.CalcTabWidth;
var
  i, pw, tc, tw, w: Integer;
begin
  tw := ClientWidth - (FTabOffset + FTabOverLap);
  if FShowButton then
    tw := tw - 24;
  FPinnedTabs := 0;
  for i := 0 to Tabs.Count - 1 do
    if TabPinned(i) then
      inc(FPinnedTabs);

  pw := FPinnedTabs * (PinnedWidth - FTabOverLap);
  tc := Tabs.Count - FPinnedTabs;
  tw := tw - pw;
  if tc > 0 then
    w := tw div tc
  else
    w := 0;
  if w < MinWidth then
    w := MinWidth;
  if ((w - FTabOverLap) * tc) > tw then
    w := MaxWidth;
  if w > MaxWidth then
    w := MaxWidth;
  FTabWidth := w + FTabOverLap;
end;

procedure TrkSmartTabs.Click;
begin
  inherited;
end;

procedure TrkSmartTabs.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TrkSmartTabs.CMMouseLeave(var Msg: TMessage);
begin
  FHotIdx := -1;
  if not FCloseDown then
    FTabX := -1;
  Invalidate;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TrkSmartTabs.SetColors;
var
  i: Integer;
  c1, c2, c3: TColor;
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
  c1 := ColorToRGB(FColorTabActive);
  c2 := ColorToRGB(FColorTabInActive);
  c3 := ColorToRGB(FColorTabHot);
  Brushes[0] := TGPSolidBrush.Create(MakeColor(FLevelTabInActive, Byte(c2), Byte(c2 shr 8),
      Byte(c2 shr 16)));
  Brushes[1] := TGPSolidBrush.Create(MakeColor(FLevelTabHot, Byte(c3), Byte(c3 shr 8),
      Byte(c3 shr 16)));
  Brushes[2] := TGPSolidBrush.Create(MakeColor(FLevelTabActive, Byte(c1), Byte(c1 shr 8),
      Byte(c1 shr 16)));

  c1 := FColorBrdActive;
  c3 := FColorBrdHot;
  c2 := FColorBrdInActive;
  Pens[0] := TGPPen.Create(MakeColor(255, Byte(c2), Byte(c2 shr 8), Byte(c2 shr 16)));
  Pens[1] := TGPPen.Create(MakeColor(255, Byte(c3), Byte(c3 shr 8), Byte(c3 shr 16)));
  Pens[2] := TGPPen.Create(MakeColor(255, Byte(c1), Byte(c1 shr 8), Byte(c1 shr 16)));

  // Tab close button
  Pens[3] := TGPPen.Create(MakeColor(255, 175, 175, 175));
  Pens[4] := TGPPen.Create(MakeColor(255, 255, 255, 255));
  Pens[5] := TGPPen.Create(MakeColor(255, 74, 103, 140));
  Pens[3].SetWidth(1.6);
  Pens[4].SetWidth(1.6);
  // Close tab
  Brushes[3] := TGPSolidBrush.Create(MakeColor(255, 192, 52, 52));
  Brushes[4] := TGPSolidBrush.Create(MakeColor(255, 52, 52, 52));
  // Mask brush
  Brushes[5] := TGPSolidBrush.Create(MakeColor(255, 255, 255, 255));

  // Add button
  Brushes[6] := TGPSolidBrush.Create(MakeColor(255, 255, 255, 255));
  Brushes[7] := TGPSolidBrush.Create(MakeColor(64, 255, 255, 255));

  FirstRun := False;
end;

procedure TrkSmartTabs.SetColTxtActive(const Value: TColor);
begin
  FColorTxtActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColTxtHot(const Value: TColor);
begin
  FColorTxtHot := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColTxtInActive(const Value: TColor);
begin
  FColorTxtInActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetGdiPText(const Value: Boolean);
begin
  FGdiPTExt := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetLevelTabActive(const Value: Byte);
begin
  FLevelTabActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetLevelTabHot(const Value: Byte);
begin
  FLevelTabHot := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetLevelTabInActive(const Value: Byte);
begin
  FLevelTabInActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetOutActive(const Value: TColor);
begin
  FColorOutActive := Value;
  Invalidate
end;

procedure TrkSmartTabs.SetOutHot(const Value: TColor);
begin
  FColorOutHot := Value;
  Invalidate
end;

procedure TrkSmartTabs.SetOutInActive(const Value: TColor);
begin
  FColorOutInActive := Value;
  Invalidate
end;

procedure TrkSmartTabs.SetOutline(const Value: Boolean);
begin
  FOutline := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetPinnedStr(const Value: String);
begin
  FPinnedStr := Value;
  Invalidate;
end;

procedure TrkSmartTabs.DoPopupClick(Sender: TObject);
var
  i: Integer;
begin
  i := (Sender as TMenuItem).Tag;
  if (i = 95) and (PopUpTab <> -1) then
    PinTab(PopUpTab)
  else if (i = 96) and (PopUpTab <> -1) then
    UnPinTab(PopUpTab)
  else if i = 99 then
  begin
    if Assigned(FOnAddClick) then
      FOnAddClick(Self);
  end
  else
    ActiveTab := i;
  Invalidate;
end;

procedure TrkSmartTabs.DoPopup(Sender: TObject; APoint: TPoint);
var
  i: Integer;
  AMenuItem: TMenuItem;

  procedure AddMenuItem(Name: string; Idx: Integer);
  begin
    AMenuItem := TMenuItem.Create(FDefPopup);
    AMenuItem.Caption := Name;
    AMenuItem.OnClick := DoPopupClick;
    AMenuItem.Tag := Idx;
    AMenuItem.RadioItem := (Idx <> -1) and (Idx < 95);
    AMenuItem.Checked := Idx = FActiveTab;
    FDefPopup.Items.Add(AMenuItem);
  end;

begin
  // TODO: 속성으로 처리 (선택 가능하도록)
  Exit;

  if FDefPopup <> nil then
    FreeAndNil(FDefPopup);
  FDefPopup := TPopupMenu.Create(nil);
  AddMenuItem('New tab', 99);
  AddMenuItem('-', -1);
  if PopUpTab <> -1 then
  begin
    if not TabPinned(PopUpTab) then
      AddMenuItem('Pin this tab', 95)
    else if TabPinned(PopUpTab) then
      AddMenuItem('Unpin this tab', 96);
    AddMenuItem('-', -1);
  end;
  for i := 0 to Tabs.Count - 1 do
    AddMenuItem(GetTabName(i), i);
  FDefPopup.Popup(APoint.x, APoint.y);
end;

constructor TrkSmartTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InCreate := True;
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csAcceptsControls];
  Width := 150;
  Height := 30;
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf32Bit;
  FCanDrag := False;
  FColorBackground := clBtnFace;
  FColorOutActive := clGray;
  FColorOutHot := clGray;
  FColorOutInActive := clGray;
  FColorTabActive := clWhite;
  FColorTabHot := clWhite;
  FColorTabInActive := clWhite;
  FGdiPTExt:= False;
  FLevelTabActive := 255;
  FLevelTabHot := 192;
  FLevelTabInActive := 224;
  FOutline:= False;
  FirstRun := True;
  SetColors;
  FMinCloseWidth := 100;
  FPinnedStr := '!';
  FShowButton := True;
  FShowImages := False;
  FShowClose := True;
  FTabOffset := 0;
  FTabOffsetTop := 4;
  FTabs := TStringList.Create;
  FTabOverLap := 20;
  ActiveTab := 0;
  FHotButton := False;
  FHotIdx := -1;
  FCloseDown := False;
  FTabX := -1;
  InCreate := False;
end;

procedure TrkSmartTabs.DeleteTab(AIndex: Integer);
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

destructor TrkSmartTabs.Destroy;
var
  i: Integer;
begin
  for i := Low(Brushes) to High(Brushes) do
    Brushes[i].Free;
  for i := Low(Pens) to High(Pens) do
    Pens[i].Free;
  FTabs.Free;
  FBmp.Free;
  if FDefPopup <> nil then
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
    if ((Points[K].y <= y) and (y < Points[J].y)) or ((Points[J].y <= y) and (y < Points[K].y))
      then
    begin
      if (x < (Points[J].x - Points[K].x) * (y - Points[K].y) / (Points[J].y - Points[K].y)
          + Points[K].x) then
        Result := not Result;
    end;
    J := K;
  end;
end;

function PtInEllipse(const Pt: TPoint; const EBR: TRect): Boolean;
begin
  Result := (Sqr((Pt.x * 2 - EBR.Left - EBR.Right) / (EBR.Right - EBR.Left)) + Sqr
      ((Pt.y * 2 - EBR.Top - EBR.Bottom) / (EBR.Bottom - EBR.Top))) <= 1;
end;

function TrkSmartTabs.GetTabPoly(Idx: Integer): TabPoly;
var
  x1, y1, x2, y2, w: Integer;
  Pinned: Boolean;
begin
  Pinned := TabPinned(Idx);
  if Pinned then
  begin
    x1 := FTabOffset;
    w := PinnedWidth
  end
  else
  begin
    x1 := FTabOffset + (FPinnedTabs * (PinnedWidth - FTabOverLap));
    w := FTabWidth;
    Idx := Idx - FPinnedTabs;
  end;
  if Idx > 0 then
    x1 := x1 + (Idx * (w - FTabOverLap));
  x2 := x1 + w;
  y1 := FTabOffsetTop;
  y2 := ClientHeight - 1;
  Result := GetTabsPoly(x1, y1, x2, y2);
end;

procedure TrkSmartTabs.InsertObj(AIndex: Integer; ATab: string; AObject: TObject);
begin
  if (AIndex >= 0) and (AIndex < Tabs.Count) then
  begin
    if AObject <> nil then
      Tabs.InsertObject(AIndex, ATab, AObject)
    else
      Tabs.Insert(AIndex, ATab);

    CalcTabWidth;
    if TabPinned(AIndex) then
    begin
      if AIndex > FPinnedTabs then
      begin
        Tabs.Move(AIndex, FPinnedTabs);
        AIndex:= FPinnedTabs;
      end;
    end
    else
    begin
      if AIndex < FPinnedTabs then
      begin
        Tabs.Move(AIndex, FPinnedTabs);
        AIndex:= FPinnedTabs;
      end;
    end;

    FActiveTab := AIndex;

    Invalidate;
    if Assigned(FOnTabChange) then
      FOnTabChange(Self);
  end;
end;

procedure TrkSmartTabs.InsertTab(AIndex: Integer; ATab: string);
begin
  InsertObj(AIndex, ATab, nil);
end;

function TrkSmartTabs.GetTabAtXY(x, y: Integer): Integer;
var
  i, newX, ts, ms: Integer;
  InFrontOf, InBackOf, Pinned: Boolean;
  Tab: TabPoly;
begin
  Result := -1;
  newX := x - FTabOffset;
  ts := FPinnedTabs * (PinnedWidth - FTabOverLap);
  ms := ts + ((FTabWidth - FTabOverLap) * (FTabs.Count - FPinnedTabs)) + FTabOverLap;
  if newX > ms then
    Exit;
  // In pinned area?
  Pinned := (newX > 0) and (newX < (ts + FTabOverLap));
  if Pinned then
    i := newX div (PinnedWidth - FTabOverLap)
  else
  begin
    i := (newX - ts) div (FTabWidth - FTabOverLap);
    i := i + FPinnedTabs;
  end;
  if (i > 0) then
  begin
    Tab := GetTabPoly(i - 1);
    InFrontOf := PtInPoly(Tab, x, y);
    if InFrontOf and (ActiveTab = i) then
    begin
      Tab := GetTabPoly(i);
      InFrontOf := not PtInPoly(Tab, x, y);
    end;
  end
  else
    InFrontOf := False;
  if i < Tabs.Count - 1 then
  begin
    Tab := GetTabPoly(i + 1);
    InBackOf := PtInPoly(Tab, x, y);
  end
  else
    InBackOf := False;
  if InFrontOf then
    i := i - 1;
  if InBackOf then
    i := i + 1;
  Tab := GetTabPoly(i);
  if not PtInPoly(Tab, x, y) then
    i := -1;
  if i < Tabs.Count then
    Result := i
  else
    Result := -1;
end;

function TrkSmartTabs.GetTabName(AIndex: Integer): String;
begin
  Result:= '';
  if not ((AIndex >= 0) and (AIndex < Tabs.Count)) then
    Exit;
  Result:= Tabs[Aindex];
  if TabPinned(AIndex) then
    Delete(Result, 1, Length(FPinnedStr));
end;

function TrkSmartTabs.OverButton(x, y: Integer): Boolean;
var
  x1, y1, x2, y2: Integer;
  Button: array [0 .. 7] of TPoint;
begin
  x1 := ClsBtnX;
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

function TrkSmartTabs.OverClose(x, y: Integer): Integer;
var
  i, n, m: Integer;
  r: TRect;
  Tab: TabPoly;
begin
  Result := -1;
  i := GetTabAtXY(x, y);
  if TabPinned(i) then
    Exit;
  if FShowClose then
  begin
    if FCloseHide then
      if (i <> ActiveTab) and (FTabWidth < FMinCloseWidth) then
        Exit;
  end;
  FTabClose := i;
  if i <> -1 then
  begin
    Tab := GetTabPoly(i);
    n := Tab[High(TabPoly)].x - 24; //rmkClose old 26
    m := FTabOffsetTop + ((FTabsHeight) shr 1) + 4; //rmkClose
    r.Left := n - 3;
    r.Top := m - 8;
    r.Right := r.Left + 12;
    r.Bottom := r.Top + 12;
    if PtInRect(r, Point(x, y)) then
      Result := i;
  end;
end;

procedure TrkSmartTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  i: Integer;
  bool: Boolean;
  Pt: TPoint;
begin
  inherited;
  FTabX := -1;
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
      PopUpTab := GetTabAtXY(x, y);
      DoPopup(Self, Pt);
    end;
end;

procedure TrkSmartTabs.CMHintShow(var Message: TCMHintShow);
var
  i, J: Integer;
  TabR: TRect;
  Pt: TPoint;
  Tab: TabPoly;
begin
  if FDragging then
  begin
    inherited;
    Exit;
  end;
  Pt := Message.HintInfo.CursorPos;
  i := GetTabAtXY(Pt.x, Pt.y);
  if (i <> -1)  then
  begin
    J := FTabWidth;
    if (FShowImages) and (FTabWidth > 80) then
      J := J - (17 + FTabOverLap);
    if FShowClose then
      J := J - 31;
    if FBmp.Canvas.TextWidth(Tabs[i]) > J then
    begin
      Tab := GetTabPoly(i);
      TabR.TopLeft := Point(Tab[0].x, Tab[4].y);
      TabR.BottomRight := Point(Tab[ High(TabPoly)].x, Tab[ High(TabPoly)].y);
      Message.HintInfo.HintStr := GetTabName(i);
      Message.HintInfo.CursorRect := TabR;
      Message.HintInfo.HideTimeout := 12000;
      Message.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure TrkSmartTabs.MouseMove(Shift: TShiftState; x, y: Integer);
var
  i, J: Integer;
  b, bool: Boolean;
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
    begin
      J := FTabX;
      FTabX := i;
      if FTabX <> J then
        Invalidate;
    end;
    b := FHotClose;
    FHotClose := (FTabX = i);
    if b <> FHotClose then
      Invalidate;

    if FShowButton then
    begin
      b:= FHotButton;
      bool := OverButton(x, y);
      if bool <> FHotButton then
        FHotButton := bool;
      if b <> FHotButton then
        Invalidate;
    end;
    if (ssLeft in Shift) and (not FDragging) then
    begin
      if (FCanDrag) then
      begin
        if (FDragPoint.x <> -1) and (FDragPoint.y <> -1) then
        begin
          i := Trunc(SQRT(Sqr(x - FDragPoint.x) + Sqr(y - FDragPoint.y)));
          if (i < 10) then
          begin
            FDragging := True;
            FDragPoint.x := -1;
            FDragPoint.y := -1;
            if FActiveTab < FPinnedTabs then
              FDragOff := x - (FTabOffset + ((PinnedWidth - FTabOverLap) * FActiveTab))
            else
            begin
              i := FTabOffset + (FPinnedTabs * (PinnedWidth - FTabOverLap));
              FDragOff := x - (i + ((FActiveTab - FPinnedTabs) * (FTabWidth - FTabOverLap)));
            end;
          end;
        end;
      end;
    end;
  end;
  if FDragging then
  begin
    y := ClientHeight shr 1;
    i := GetTabAtXY(x, y);
    if (i <> FDragTab) and (i <> -1) then
    begin
      if ((TabPinned(FDragTab)) and TabPinned(i)) or
        ((not TabPinned(FDragTab)) and not TabPinned(i)) then
      begin
        Tabs.Move(FDragTab, i);
        FActiveTab := i;
        FDragTab := i;
      end;
      FHotClose := False;
    end;
    x := x - FDragOff;
    if x < FTabOffset then
      x := FTabOffset;
    if TabPinned(FDragTab) then
      i := PinnedWidth
    else
      i := FTabWidth;
    if x > ClientWidth - i then
      x := ClientWidth - i;
    FDragX := x;
    Invalidate;
  end;
end;

procedure TrkSmartTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  CloseTab: Boolean;
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    Invalidate;
  end;

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

procedure TrkSmartTabs.PaintTabs;
type
  PRGBA = ^TRGBA;

  TRGBA = record
    b, g, r, A: Byte;
  end;

  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array [0 .. 65565] of TRGBA;
var
  x, c1, i, n, m, x1, x2, x3, y1, y2, bx, by, ts, ax, tw: Integer;
  txt: string;
  TabsR, TabR, R2: TRect;
  Tab: TabPoly;
  TabAdd: array [0 .. 12] of TPoint;
  DownColor: array [0 .. 1] of GDIPAPI.ARGB;
  TabsCanvas: TGPGraphics;
  TabsFont: TGPFont;
  TabsTxtBrush: TGPSolidBrush;
  TxtRect: TGPRectF;
  TxtFormat: TGPStringFormat;
  brushButtonDown: TGPPathGradientBrush;

  baGB: TGPLinearGradientBrush;
  baGP: TGpPen;
  p1, p2: TGPPoint;

  bImages, bClose, Pinned: Boolean;
  slSize, slPtr: Integer;
  w, h: Integer;
  Row: PRGBAArray;
  slSize2, slPtr2: Integer;
  Row2: PRGBAArray;
  slSize3, slPtr3: Integer;
  Row3: PRGBAArray;
  A: Byte;
  Mask, Back: TBitmap;
  MaskCanvas: TGPGraphics;
  State: TabState;
  c2: LongInt;
  R, G, B, v1, v2: Byte;
begin
  if InCreate then
    Exit;
  FBmp.Width := ClientWidth;
  FBmp.Height := ClientHeight;
  FBmp.Canvas.Font.Assign(Font);
  FBmp.Canvas.Brush.Color := FColorBackground;
  Back := TBitmap.Create;
  Back.PixelFormat := pf32Bit;
  Back.Width := ClientWidth;
  Back.Height := ClientHeight;

  if FTransparent then
    DrawParentImage(Self, FBmp.Canvas)
  else
  begin
    // Clear canvas
    slPtr := Integer(FBmp.ScanLine[0]);
    slSize := Integer(FBmp.ScanLine[1]) - slPtr;
    for h := 0 to FBmp.Height - 1 do
    begin
      Row := PRGBAArray(slPtr);
      for w := 0 to FBmp.Width - 1 do
      begin
        Row[w].r := Byte(FColorBackground);
        Row[w].g := Byte(FColorBackground shr 8);
        Row[w].b := Byte(FColorBackground shr 16);
        Row[w].A := 0;
      end;
      slPtr := slPtr + slSize;
    end;
    // canvas cleared
  end;

  Back.Canvas.CopyRect(ClientRect, FBmp.Canvas, ClientRect);
  Back.Canvas.Font.Assign(Font);

  if Tabs.Count > 0 then
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
    MaskCanvas.FillPolygon(Brushes[5], PGPPoint(@Tab), PolyTabCount);
    MaskCanvas.DrawPolygon(Pens[1], PGPPoint(@Tab), PolyTabCount);
    // mask done

    TabsR := ClientRect;
    TabsCanvas := TGPGraphics.Create(FBmp.Canvas.Handle);
    TabsCanvas.SetSmoothingMode(SmoothingModeHighQuality);

    // Set antialiasing to use greyscale antialliasing
    //TabsCanvas.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

    Pinned := TabPinned(FTabs.Count - 1);
    ts := (PinnedWidth - FTabOverLap) * FPinnedTabs;
    ts := ts + ((FTabWidth - FTabOverLap) * (FTabs.Count - FPinnedTabs));

    if Pinned then
    begin
      ts := ts - (PinnedWidth - FTabOverLap);
      x1 := FTabOffset + ts;
      x2 := x1 + PinnedWidth;
      bx := (x1 + PinnedWidth);
    end
    else
    begin
      ts := ts - (FTabWidth - FTabOverLap);
      x1 := FTabOffset + ts;
      x2 := x1 + FTabWidth;
      bx := (x1 + FTabWidth);
    end;
    ClsBtnX := bx;
    y1 := FTabOffsetTop;
    y2 := ClientHeight - 1;
    by := y1 + 4;

    if FCloseHide then
      bClose := FShowClose and (FTabWidth > FMinCloseWidth)
    else
      bClose := FShowClose;

    for i := FTabs.Count - 1 downto 0 do
    begin
      if i <> FActiveTab then
      begin
        txt := FTabs[i];
        Pinned := TabPinned(i);

        if Pinned then
          bImages := (FShowImages) and Assigned(FOnGetImageIdx) and Assigned(FImages)
        else
          bImages := (FShowImages) and Assigned(FOnGetImageIdx) and Assigned(FImages) and
            (FTabWidth > 80);

        bClose := bClose and not Pinned;
        Tab := GetTabsPoly(x1, y1, x2, y2);

        State:= stNormal;
        if i = FHotIdx then
          State:= stHot;

        if i = FHotIdx then
          h:= 1
        else
          h:= 0;
        TabsCanvas.FillPolygon(Brushes[h], PGPPoint(@Tab), PolyTabCount);
        TabsCanvas.DrawPolygon(Pens[h], PGPPoint(@Tab), PolyTabCount);

        // use mask image to restore overlaping tab area
        if (i > 0) and (i <> FActiveTab + 1) and (not FSeeThruTabs) then
        begin
          slPtr := Integer(FBmp.ScanLine[y1]);
          slSize := Integer(FBmp.ScanLine[y1 + 1]) - slPtr;
          slPtr2 := Integer(Mask.ScanLine[y1]);
          slSize2 := Integer(Mask.ScanLine[y1 + 1]) - slPtr2;
          slPtr3 := Integer(Back.ScanLine[y1]);
          slSize3 := Integer(Back.ScanLine[y1 + 1]) - slPtr3;
          for h := y1 to y2 - 1 do
          begin
            Row := PRGBAArray(slPtr);
            Row2 := PRGBAArray(slPtr2);
            Row3 := PRGBAArray(slPtr3);
            for w := 0 to FTabOverLap - 1 do
              if Row2[w + (Mask.Width - FTabOverLap) + 1].a <> 0 then
              begin
                Row[w + x1].r := Row3[w + x1].r;
                Row[w + x1].g := Row3[w + x1].g;
                Row[w + x1].b := Row3[w + x1].b;
                Row[w + x1].A := Row3[w + x1].a;
              end;
            slPtr := slPtr + slSize;
            slPtr2 := slPtr2 + slSize2;
            slPtr3 := slPtr3 + slSize3;
          end;
        end;
        // overlaping tab area restored

        TabR.TopLeft := Point(x1 + FTabOverLap, y1);
        TabR.BottomRight := Point(x2 - FTabOverLap, y2);
        if bImages then
        begin
          FOnGetImageIdx(Self, i, n);
          m := ((ClientHeight - FTabOffsetTop) div 2) - 8;
          if Pinned then
            w := ((TabR.Right - TabR.Left) - FImages.Width) div 2
          else
            w := -3;
          FImages.Draw(FBmp.Canvas, TabR.Left + w, FTabOffsetTop + m, n);
          TabR.Left := TabR.Left + 16;
        end;

        if bClose then
        begin
          n := x2 - 24; //rmkClose old 26
          m := FTabOffsetTop + (FTabsHeight shr 1) + 4; // rmkClose
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
          TabR.Right := TabR.Right - 10;
        end;

        if not Pinned then
        begin
          if FGdiPTExt then
          begin
            TabsFont := TGPFont.Create(Font.Name, Font.Size);
            if i = FHotIdx then
              w := ColorToRGB(FColorTxtHot)
            else
              w := ColorToRGB(FColorTxtInActive);
            TabsTxtBrush := TGPSolidBrush.Create(MakeColor(255, Byte(w), Byte(w shr 8),
              Byte(w shr 16)));
            TxtRect.x := TabR.Left;
            TxtRect.y := TabR.Top;
            TxtRect.Width := TabR.Right - TabR.Left;
            TxtRect.Height := TabR.Bottom - 4;
            TxtFormat := TGPStringFormat.Create();
            //TxtFormat.SetTrimming(StringTrimmingEllipsisCharacter);
            TxtFormat.SetLineAlignment(StringAlignmentCenter);
            TxtFormat.SetFormatFlags(StringFormatFlagsNoWrap);
            TabsCanvas.DrawString(PChar(txt), Length(txt), TabsFont, TxtRect, TxtFormat,
              TabsTxtBrush);
            TabsFont.Free;
            TxtFormat.Free;
            TabsTxtBrush.Free;
          end
          else
          begin
            FBmp.Canvas.Brush.Style := bsClear;

            if FOutline then
            begin
              if i = FHotIdx then
                FBmp.Canvas.Font.Color:= FColorOutHot
              else
                FBmp.Canvas.Font.Color:= FColorOutInActive;
              R2:= TabR;
              R2.Left:= R2.Left - 1;
              R2.Right:= R2.Right - 1;
              DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
                DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
              R2:= TabR;
              R2.Left:= R2.Left + 1;
              R2.Right:= R2.Right + 1;
              DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
                DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
              R2:= TabR;
              R2.Top:= R2.Top + 1;
              R2.Bottom:= R2.Bottom + 1;
              DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
                DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
              R2:= TabR;
              R2.Top:= R2.Top - 1;
              R2.Bottom:= R2.Bottom - 1;
              DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
                DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
            end;
            if i = FHotIdx then
              FBmp.Canvas.Font.Color:= FColorTxtHot
            else
              FBmp.Canvas.Font.Color:= FColorTxtInActive;
            DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), TabR,
              DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
          end;

          // Fade out text at right side ...
          slPtr := Integer(FBmp.ScanLine[y1 + 1]);
          slSize := Integer(FBmp.ScanLine[y1 + 2]) - slPtr;
          slPtr2 := Integer(Back.ScanLine[y1 + 1]);
          slSize2 := Integer(Back.ScanLine[y1 + 2]) - slPtr2;
          if i = FHotIdx then
          begin
            a := FLevelTabHot;
            c1 := ColorToRGB(FColorTabHot)
          end
          else
          begin
            a := FLevelTabInActive;
            c1 := ColorToRGB(FColorTabInActive);
          end;
          r:= Byte(c1);
          g:= Byte(c1 shr 8);
          b:= Byte(c1 shr 16);
          x:= TabR.Right - TextFadeOut;
          for h := y1 + 1 to y2 - 1 do
          begin
            Row := PRGBAArray(slPtr);
            Row2 := PRGBAArray(slPtr2);
            c1:= 255;
            for w := x to x + TextFadeOut do
            begin
              v2 := Byte(Row2[w].r);
              v2:= Byte(a * (r - v2) shr 8 + v2);
              Row[w].R := Byte(c1 * (Row[w].R - v2) shr 8 + v2);
              v2 := Byte(Row2[w].g);
              v2:= Byte(a * (g - v2) shr 8 + v2);
              Row[w].G := Byte(c1 * (Row[w].G - v2) shr 8 + v2);
              v2 := Byte(Row2[w].b);
              v2:= Byte(a * (b - v2) shr 8 + v2);
              Row[w].B := Byte(c1 * (Row[w].B - v2) shr 8 + v2);
              c1:= c1 - 12;
            end;
            slPtr := slPtr + slSize;
            slPtr2 := slPtr2 + slSize2;
          end;
          // Fade done...
        end;
        if Assigned(FOnCustomDraw) then
          FOnCustomDraw(Self, FBmp.Canvas, Rect(x1, y1, x2, y2), Txt, State, i);
      end
      else
        ax := x1;
      Pinned := TabPinned(i - 1);
      if Pinned then
        w := PinnedWidth
      else
        w := FTabWidth;
      x1 := x1 - (w - FTabOverLap);
      x2 := x1 + w;
    end;
    // Paint Active Tab
    txt := FTabs[FActiveTab];
    Pinned := TabPinned(FActiveTab);
    if Pinned then
      bImages := (FShowImages) and Assigned(FOnGetImageIdx) and Assigned(FImages)
    else
      bImages := (FShowImages) and Assigned(FOnGetImageIdx) and Assigned(FImages) and
        (FTabWidth > 80);
    if Pinned then
      tw := PinnedWidth
    else
      tw := FTabWidth;
    bClose := FShowClose and not Pinned;
    x1 := ax;
    if FDragging then
      x1 := FDragX;
    x2 := x1 + tw;
    // use mask image to restore overlaping tab area
    if (not FDragging) and (not FSeeThruTabs) then
    begin
      slPtr := Integer(FBmp.ScanLine[y1]);
      slSize := Integer(FBmp.ScanLine[y1 + 1]) - slPtr;
      slPtr2 := Integer(Mask.ScanLine[y1]);
      slSize2 := Integer(Mask.ScanLine[y1 + 1]) - slPtr2;
      slPtr3 := Integer(Back.ScanLine[y1]);
      slSize3 := Integer(Back.ScanLine[y1 + 1]) - slPtr3;
      x3 := x2 - FTabOverLap;
      for h := y1 to y2 - 1 do
      begin
        Row := PRGBAArray(slPtr);
        Row2 := PRGBAArray(slPtr2);
        Row3 := PRGBAArray(slPtr3);
        for w := 0 to FTabOverLap - 1 do
          if Row2[w + (Mask.Width - FTabOverLap) + 1].a <> 0 then
          begin
            Row[w + x3].r := Row3[w + x3].r;
            Row[w + x3].g := Row3[w + x3].g;
            Row[w + x3].b := Row3[w + x3].b;
            Row[w + x3].A := Row3[w + x3].a;
            Row[x1 + FTabOverLap - w].r := Row3[x1 + FTabOverLap - w].r;
            Row[x1 + FTabOverLap - w].g := Row3[x1 + FTabOverLap - w].g;
            Row[x1 + FTabOverLap - w].b := Row3[x1 + FTabOverLap - w].b;
            Row[x1 + FTabOverLap - w].A := Row3[x1 + FTabOverLap - w].a;
          end;
        slPtr := slPtr + slSize;
        slPtr2 := slPtr2 + slSize2;
        slPtr3 := slPtr3 + slSize3;
      end;
    end;
    // overlaping tab area restored
    txt := FTabs[FActiveTab];
    Tab := GetTabsPoly(x1, y1, x2, y2);
    TabsCanvas.FillPolygon(Brushes[2], PGPPoint(@Tab), PolyTabCount);
    TabsCanvas.DrawPolygon(Pens[2], PGPPoint(@Tab), PolyTabCount);

    FBmp.Canvas.Pen.Color := clGray;
    FBmp.Canvas.MoveTo(0, y2);
    FBmp.Canvas.LineTo(ClientWidth, y2);
    FBmp.Canvas.Pen.Color := Color;
    FBmp.Canvas.MoveTo(x1 + 1, y2);
    FBmp.Canvas.LineTo(x2, y2);

    Row := PRGBAArray(Integer(FBmp.ScanLine[ClientHeight - 1]));
    for w := 0 to FBmp.Width - 1 do
      Row[w].A := 255;
    Row2 := PRGBAArray(Integer(Back.ScanLine[ClientHeight - 1]));
    A := FLevelTabActive;
    c1 := ColorToRGB(FColorTabActive);
    r:= Byte(c1);
    g:= Byte(c1 shr 8);
    b:= Byte(c1 shr 16);
    for w := x1 + 1 to x2 - 1 do
    begin
      v2 := Byte(Row2[w].r);
      Row[w].R := Byte(A * (r - v2) shr 8 + v2);
      v2 := Byte(Row2[w].g);
      Row[w].G := Byte(A * (g - v2) shr 8 + v2);
      v2 := Byte(Row2[w].b);
      Row[w].B := Byte(A * (b - v2) shr 8 + v2);
      Row[w].A := A;
    end;

    TabR.TopLeft := Point(x1 + FTabOverLap, y1);
    TabR.BottomRight := Point((x2) - FTabOverLap, y2);
    if bImages then
    begin
      FOnGetImageIdx(Self, FActiveTab, n);
      m := ((ClientHeight - FTabOffsetTop) div 2) - 8;
      if Pinned then
        w := ((TabR.Right - TabR.Left) - FImages.Width) div 2
      else
        w := -3;
      FImages.Draw(FBmp.Canvas, TabR.Left + w, FTabOffsetTop + m, n);
      TabR.Left := TabR.Left + 16;
    end;
    if bClose then
    begin
      n := x2 - 24; //rmkClose
      m := FTabOffsetTop + (FTabsHeight shr 1) + 4; //rmkClose  old 2
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
      TabR.Right := TabR.Right - 10;
    end;
    if not Pinned then
    begin
      if FGdiPTExt then
      begin
        TabsFont := TGPFont.Create(Font.Name, Font.Size);
        w := ColorToRGB(FColorTxtActive);
        TabsTxtBrush := TGPSolidBrush.Create(MakeColor(255, Byte(w), Byte(w shr 8),
          Byte(w shr 16)));
        TxtRect.x := TabR.Left;
        TxtRect.y := TabR.Top;
        TxtRect.Width := TabR.Right - TabR.Left;
        TxtRect.Height := TabR.Bottom - 4;
        TxtFormat := TGPStringFormat.Create();
        //TxtFormat.SetTrimming(StringTrimmingEllipsisCharacter);
        TxtFormat.SetLineAlignment(StringAlignmentCenter);
        TxtFormat.SetFormatFlags(StringFormatFlagsNoWrap);
        TabsCanvas.DrawString(PChar(txt), Length(txt), TabsFont, TxtRect, TxtFormat,
          TabsTxtBrush);
        TabsFont.Free;
        TxtFormat.Free;
        TabsTxtBrush.Free;
      end
      else
      begin
        FBmp.Canvas.Brush.Style := bsClear;
        if FOutline then
        begin
          FBmp.Canvas.Font.Color:= FColorOutActive;
          R2:= TabR;
          R2.Left:= R2.Left - 1;
          R2.Right:= R2.Right - 1;
          DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
            DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
          R2:= TabR;
          R2.Left:= R2.Left + 1;
          R2.Right:= R2.Right + 1;
          DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
            DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
          R2:= TabR;
          R2.Top:= R2.Top + 1;
          R2.Bottom:= R2.Bottom + 1;
          DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
            DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
          R2:= TabR;
          R2.Top:= R2.Top - 1;
          R2.Bottom:= R2.Bottom - 1;
          DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), R2,
            DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
        end;
        FBmp.Canvas.Font.Color:= FColorTxtActive;
           DrawText(FBmp.Canvas.Handle, PChar(txt), Length(txt), TabR,
        DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
      end;
      // Fade out text at right side ...
      slPtr := Integer(FBmp.ScanLine[y1 + 1]);
      slSize := Integer(FBmp.ScanLine[y1 + 2]) - slPtr;
      slPtr2 := Integer(Back.ScanLine[y1 + 1]);
      slSize2 := Integer(Back.ScanLine[y1 + 2]) - slPtr2;
      a := FLevelTabActive;
      c1 := ColorToRGB(FColorTabActive);
      r:= Byte(c1);
      g:= Byte(c1 shr 8);
      b:= Byte(c1 shr 16);
      x:= TabR.Right - TextFadeOut;
      for h := y1 + 1 to y2 - 1 do
      begin
        Row := PRGBAArray(slPtr);
        Row2 := PRGBAArray(slPtr2);
        c1:= 255;
        for w := x to x + TextFadeOut do
        begin
          v2 := Byte(Row2[w].r);
          v2:= Byte(a * (r - v2) shr 8 + v2);
          Row[w].R := Byte(c1 * (Row[w].R - v2) shr 8 + v2);
          v2 := Byte(Row2[w].g);
          v2:= Byte(a * (g - v2) shr 8 + v2);
          Row[w].G := Byte(c1 * (Row[w].G - v2) shr 8 + v2);
          v2 := Byte(Row2[w].b);
          v2:= Byte(a * (b - v2) shr 8 + v2);
          Row[w].B := Byte(c1 * (Row[w].B - v2) shr 8 + v2);
          c1:= c1 - 12;
        end;
        slPtr := slPtr + slSize;
        slPtr2 := slPtr2 + slSize2;
      end;
      // Fade done...
    end;
    if Assigned(FOnCustomDraw) then
      FOnCustomDraw(Self, FBmp.Canvas, Rect(x1, y1, x2, y2), Txt, stSelected, FActiveTab);
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
        brushButtonDown.SetCenterColor(MakeColor(64, 255, 255, 255));
        n := 1;
        DownColor[0] := MakeColor(48, 220, 220, 220);
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
          TabsCanvas.FillPolygon(Brushes[7], PGPPoint(@Tab), 8);
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
      TabsCanvas.FillPolygon(Brushes[6], PGPPoint(@TabAdd), 13);
      p1.X:= 0;
      p1.Y:= 0;
      p2.X:= 0;
      p2.Y:= 12;
      baGB:= TGPLinearGradientBrush.Create(p1, p2, MakeColor(48, 128, 128, 128), MakeColor(64, 0, 0, 0));
      baGP := TGPPen.Create(baGB);
      baGP.SetWidth(0.5);
      TabsCanvas.DrawPolygon(baGP, PGPPoint(@TabAdd), 13);
      baGB.Free;
      baGP.Free;
    end;
    TabsCanvas.Free;
    MaskCanvas.Free;
    Mask.Free;
  end;

  BitBlt(Canvas.Handle, 0, 0, FBmp.Width, FBmp.Height, FBmp.Canvas.Handle, 0, 0, SRCCOPY);
  Back.Free;
end;

procedure TrkSmartTabs.PaintWindow(DC: HDC);
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

procedure TrkSmartTabs.PinTab(AIndex: Integer);
begin
  if not TabPinned(AIndex) then
  begin
    Tabs.Move(AIndex, FPinnedTabs);
    Tabs[FPinnedTabs] := FPinnedStr + Tabs[FPinnedTabs];
    if AIndex = FActiveTab then
      ActiveTab := FPinnedTabs
    else if AIndex > FActiveTab then
      if (FActiveTab >= FPinnedTabs) and (FActiveTab < FTabs.Count - 1) then
        inc(FActiveTab);
    Invalidate;
  end;
end;

procedure TrkSmartTabs.Resize;
begin
  Height := 26 + FTabOffsetTop;
  Invalidate;
end;

procedure TrkSmartTabs.SetActiveTab(const Value: Integer);
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

procedure TrkSmartTabs.SetBackground(const Value: TColor);
begin
  FColorBackground := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetCloseHide(const Value: Boolean);
begin
  FCloseHide := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetColBrdActive(const Value: TColor);
begin
  FColorBrdActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColBrdHot(const Value: TColor);
begin
  FColorBrdHot := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColBrdInActive(const Value: TColor);
begin
  FColorBrdInActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColorActive(const Value: TColor);
begin
  FColorTabActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColorHot(const Value: TColor);
begin
  FColorTabHot := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetColorInActive(const Value: TColor);
begin
  FColorTabInActive := Value;
  SetColors;
  Invalidate;
end;

procedure TrkSmartTabs.SetSeeThruTabs(const Value: Boolean);
begin
  FSeeThruTabs := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetShowButton(const Value: Boolean);
begin
  FShowButton := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetShowClose(const Value: Boolean);
begin
  FShowClose := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetTabName(AIndex: Integer; AName: String);
begin
  if not ((AIndex >= 0) and (AIndex < Tabs.Count)) then
    Exit;
  if TabPinned(AIndex) then
    AName:= FPinnedStr + AName;
  Tabs[AIndex]:= AName;
end;

procedure TrkSmartTabs.SetTabOffset(const Value: Integer);
begin
  FTabOffset := Value;
  Invalidate;
end;

procedure TrkSmartTabs.SetTabs(const Value: TStringList);
begin
  FTabs.Assign(Value);
  CalcTabWidth;
  if FActiveTab > Tabs.Count - 1 then
    ActiveTab := Tabs.Count - 1;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self);
end;

procedure TrkSmartTabs.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

function TrkSmartTabs.TabPinned(AIndex: Integer): Boolean;
begin
  Result := False;
  if (AIndex > -1) and (AIndex < FTabs.Count) then
    Result := (Pos(PinnedStr, FTabs[AIndex]) = 1);
end;

procedure TrkSmartTabs.UnPinTab(AIndex: Integer);
var
  s: String;
begin
  if TabPinned(AIndex) then
  begin
    Dec(FPinnedTabs);
    Tabs.Move(AIndex, FPinnedTabs);
    s := Tabs[FPinnedTabs];
    Delete(s, 1, Length(FPinnedStr));
    Tabs[FPinnedTabs] := s;
    if AIndex = FActiveTab then
      ActiveTab := FPinnedTabs
    else if AIndex < FActiveTab then
      if (FActiveTab <= FPinnedTabs) and (FActiveTab > 0) then
        Dec(FActiveTab);
    Invalidate;
  end;
end;

procedure TrkSmartTabs.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkSmartTabs.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkSmartTabs.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not(csLoading in ComponentState) then
    Invalidate;
end;

end.
