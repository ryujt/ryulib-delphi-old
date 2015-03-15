unit rkPathViewer;

//  PathViewer by Roy Magne Klever
//  © 2010 by Roy Magne Klever. All rights reserved
//
//  This file is not distributable without permission by Roy Magne Klever
//  WEB: www.rmklever.com
//  Mail: roymagne@rmklever.com
//
//  version 1.2, December 2010
//
//  Licensed under MPL 1.1 licence

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, Windows, SysUtils, Controls, ExtCtrls, Graphics, Forms, Messages,
  ImgList, Dialogs, Menus, StdCtrls, Math, StrUtils, Themes;

const
  CM_REFRESH = WM_USER + 1295; // Custom Message...

type
  TpvItemState = (isNormal, isButtonPressed, isArrowPressed, isButtonHot, isArrowHot);
  TpvArrow = (pvaNone, pvaNormal, pvaBack);
  TOnDrawArrowEvent = procedure(Sender: TObject; PathOff: Integer; var ArrowStyle: TpvArrow) of object;
  TOnOwnerDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
    AText: string; AState: TpvItemState; AButton: Boolean) of object;
  TOnErrorEvent = procedure(Sender: TObject; AException: Byte) of object;
  TOnPathClickEvent = procedure(Sender: TObject; PathIdx: Integer) of object;
  TOnPopPathEvent = procedure(Sender: TObject; var APath: string; Off, Idx: Integer;var APopupMenu: TPopupMenu) of object;
  TOnPathEditEvent = procedure(Sender: TObject; AEdit: TEdit; var APath: string) of object;
  TOnPathEditedEvent = procedure(Sender: TObject; var APath: string) of object;

  TrkPathViewer = class(TCustomControl)
  private
    { Private declarations }
    FAllowEdit: Boolean;
    FAllowKeyNav: Boolean;
    FArrowStyle: TpvArrow;
    FAuto: Boolean;
    FBmp: TBitmap;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FBtnIdx: Integer;
    FButton: Boolean;
    FButtonDown: Boolean;
    FBtnWidth: Integer;
    FClickSetPath: Boolean;
    FCol1: TColor;
    FCol2: TColor;
    FCol3: TColor;
    FCol4: TColor;
    FCol5: TColor;
    FCol6: TColor;
    FCol7: TColor;
    FCol8: TColor;
    FCol9: TColor;
    FCol10: TColor;
    FCol11: TColor;
    FCustomPopup: TPopupMenu;
    FDefault: Boolean;
    FDropDown: Boolean;
    FDropBtnWidth: Integer;
    FDropBtnState: TpvItemState;
    FFramed: Boolean;
    FFromRight: Boolean;
    FHideFirst: Boolean;
    FHotPt: TPoint;
    FHotPos: Integer;
    FImages: TCustomImageList;
    FImagesBtn: TCustomImageList;
    FImageIdx: Integer;
    FImageIdxBtn: Integer;
    FInButton: Boolean;
    FKeys: Boolean;
    FLastPoint: TPoint;
    FMenu: TPopupMenu;
    FSeperator: Char;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnError: TOnErrorEvent;
    FOwnerDraw: Boolean;
    FPath: string;
    FPathEdit: TEdit;
    FPathIdx: Integer;
    FPaths: TStringList;
    FPopupPath: string;
    FPopUp: Boolean;
    FPopUpX: Integer;
    FShowArrow: Boolean;
    FState: TpvItemState;
    FBtnState: TpvItemState;
    FTransparent: Boolean;
    FOnBtnClick: TNotifyEvent;
    FOnDropBtnClick: TNotifyEvent;
    FOnDrawArrow: TOnDrawArrowEvent;
    FOnOwnerDraw: TOnOwnerDrawEvent;
    FOnPathChanged: TNotifyEvent;
    FOnPathClick: TOnPathClickEvent;
    FOnPathEdit: TOnPathEditEvent;
    FOnEditedPath: TOnPathEditedEvent;
    FOnPopup: TOnPopPathEvent;
    FOnPopupClick: TNotifyEvent;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure PaintPath;
    function CalcPath: Integer;
    procedure CMRefresh(var Message: TMessage); message CM_REFRESH;
    procedure CalcState;
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFramed(const Value: Boolean);
    procedure SetDefault(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure DoKey(const Key: Word; Shift: TShiftState);
    procedure PaintButton(ACanvas: TCanvas; ARect: TRect; AText: string; AState:
      TpvItemState; AButton: Boolean);
    procedure SetButton(const Value: Boolean);
    procedure SetColorBtn(const Index: Integer; const Value: TColor);
    procedure SetDropDown(const Value: Boolean);
    procedure SetFromRight(const Value: Boolean);
    procedure SetHideFirst(const Value: Boolean);
    procedure SetPath(const Value: string);
    procedure SetSeperator(const Value: Char);
  protected
    { Protected declarations }
    FPopupPos: TPoint;
    FPathOff: Integer;
    InCreate: Boolean;
    InEdit: Boolean;
    InShutdown: Boolean;
    procedure FMenuPopup(Sender: TObject);
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEdit;
    procedure ExitEditMode;
    procedure UpdateView;
    property PopupPath: string read FPopupPath;
    property PathOff: Integer read FPathOff;
    property PathParts: TStringList read FPaths;
  published
    { Published declarations }
    property Align;
    property AllowEdit: Boolean read FAllowEdit write FAllowEdit default True;
    property AllowKeyNav: Boolean read FAllowKeyNav write FAllowKeyNav default False;
    property Anchors;
    property DropDown: Boolean read FDropDown write SetDropDown default False;
    property Auto: Boolean read FAuto write FAuto default True;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BorderWidth;
    property BtnGreyGrad1: TColor index 0 read FCol1 write SetColorBtn default clBtnText;
    property BtnGreyGrad2: TColor index 1 read FCol2 write SetColorBtn default clBtnText;
    property BtnNormGrad1: TColor index 2 read FCol3 write SetColorBtn default clBtnText;
    property BtnNormGrad2: TColor index 3 read FCol4 write SetColorBtn default clBtnText;
    property BtnHotGrad1: TColor index 4 read FCol5 write SetColorBtn default clBtnText;
    property BtnHotGrad2: TColor index 5 read FCol6 write SetColorBtn default clBtnText;
    property BtnPenGray: TColor index 6 read FCol7 write SetColorBtn default clBtnText;
    property BtnPenNorm: TColor index 7 read FCol8 write SetColorBtn default clBtnText;
    property BtnPenShade1: TColor index 8 read FCol9 write SetColorBtn default clBtnText;
    property BtnPenShade2: TColor index 9 read FCol10 write SetColorBtn default clBtnText;
    property BtnPenArrow: TColor index 10 read FCol11 write SetColorBtn default clBtnText;
    property Button: Boolean read FButton write SetButton default False;
    property ClickSetPath: Boolean read FClickSetPath write FClickSetPath default True;
    property Color;
    property Ctl3D;
    property CustomPopupMenu: TPopupMenu read FCustomPopup write FCustomPopup;
    property ComputerAsDefault: Boolean read FDefault write SetDefault;
    property Enabled;
    property Font;
    property Framed: Boolean read FFramed write SetFramed default True;
    property HasSubItems: Boolean read FShowArrow write FShowArrow default False;
    property Height;
    property HideFirstLevel: Boolean read FHideFirst write SetHideFirst default False;
    property Images: TCustomImageList read FImages write FImages;
    property ImageIdx: Integer read FImageIdx write FImageIdx default -1;
    property ImagesBtn: TCustomImageList read FImagesBtn write FImagesBtn;
    property ImageIdxBtn: Integer read FImageIdxBtn write FImageIdxBtn default -1;
    property Left;
    property RightToLeft: Boolean read FFromRight write SetFromRight default False;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property ParentColor;
    property ParentBackground;
    property ParentCtl3d;
    property ParentFont;
    property Path: string read FPath write SetPath;
    property PopupMenu;
    property PathSepertor: Char read FSeperator write SetSeperator;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Top;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property Width;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
    property OnClick;
    property OnDblClick;
    property OnDrawArrow: TOnDrawArrowEvent read FOnDrawArrow write FOnDrawArrow;
    property OnDropBtnClick: TNotifyEvent read FOnDropBtnClick write FOnDropBtnClick;
    property OnEditedPath: TOnPathEditedEvent read FOnEditedPath write FOnEditedPath;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnOwnerDraw: TOnOwnerDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
    property OnPathChanged: TNotifyEvent read FOnPathChanged write FOnPathChanged;
    property OnPathClick: TOnPathClickEvent read FOnPathClick write FOnPathClick;
    property OnPathEdit: TOnPathEditEvent read FOnPathEdit write FOnPathEdit;
    property OnPopup: TOnPopPathEvent read FOnPopup write FOnPopup;
    property OnPopupClick: TNotifyEvent read FOnPopupClick write FOnPopupClick;
    property OnResize;
  end;

procedure PaintArrow(Canvas: TCanvas; x, y: Integer; Down, Right: Boolean);
procedure PaintBackArrow(Canvas: TCanvas; x, y: Integer;Down, Right: Boolean);

procedure Register;

implementation

procedure TrkPathViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FAllowKeyNav then
    DoKey(Key, Shift);
end;

procedure TrkPathViewer.UpdateView;
begin
  PaintPath;
end;

procedure TrkPathViewer.ExitEditMode;
begin
  if not InEdit then
    Exit;
  FPathEdit.Free;
  Invalidate;
  if HasParent then
    SetFocus;
  InEdit:= False;
end;

procedure TrkPathViewer.DoEdit;
var
  txt: string;
  w: Integer;
begin
  w:= 8;
  if FButton then
    w:= w + FBtnWidth;
  if FDropDown then
    w:= w + FDropBtnWidth;
  FPathEdit := TEdit.Create(nil);
  try
    InEdit:= True;
    FPathEdit.Parent := Self;
    FPathEdit.ParentFont := False;
    FPathEdit.BorderStyle := bsNone;
    FPathEdit.AutoSize := False;
    FPathEdit.Ctl3D := True;
    FPathEdit.Visible := False;
    FPathEdit.OnKeyDown := EditKeyDown;
    FPathEdit.OnKeyPress := EditKeyPress;
    FPathEdit.OnExit := EditExit;
    FPathEdit.Color := Color;
    if FButton and FFromRight then
      FPathEdit.Left := 4 + FBtnWidth
    else
      FPathEdit.Left := 4;
    FPathEdit.Width := ClientWidth - w;
    FPathEdit.Font.Assign(Font);
    FPathEdit.Height := FPathEdit.Font.Size + 6;
    FPathEdit.Top := ((ClientHeight - FPathEdit.Height) div 2);
    FPathEdit.AutoSelect := True;
    txt:= Path;
    if Assigned(FOnPathEdit) then
      FOnPathEdit(Self, FPathEdit, txt);
    FPathEdit.Text := txt;
    FPathEdit.Visible := True;
    PaintPath;
    FPathEdit.SetFocus;
  except
    FPathEdit.Free;
    Raise;
  end;
end;

procedure TrkPathViewer.DoKey(const Key: Word; Shift: TShiftState);
var
  mx, my, i: Integer;
begin
  if (InEdit) or (FPaths.Count = 0) then
    Exit;
  FButtonDown := False;
  FKeys:= True;
  FLastPoint:= Point(-1, -1);
  if FFromRight then
    mx:= ClientWidth - 22
  else
    mx:= 22;
  my:= ClientHeight shr 1;
  if FHotPos = -2 then
  begin
    FHotPt := Point(mx, my);
    CalcState;
    Invalidate;
    Exit;
  end;
  case Key of
    VK_RIGHT:
      begin
        if FFromRight then
        begin
          FHotPos:= FHotPos - 1;
          if FHotPos < FPathOff - 1 then
            FHotPos:= FPaths.Count - 1;
        end
        else
        begin
          FHotPos:= FHotPos + 1;
          if FHotPos >= FPaths.Count then
            FHotPos:= -1;
        end;
        if (FHotPos = FPathOff - 1) and FFromRight then
        begin
          mx:= ClientWidth - 22;
          FHotPos:= -1;
        end
        else
        begin
          for i:= FPathOff to FHotPos do
            if FFromRight then
              mx:= mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
            else
              mx:= mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
          if FHotPos <> -1 then
            if FFromRight then
              mx:= mx + 14
            else
              mx:= mx - 14;
        end;
        FHotPt := Point(mx, my);
        CalcState;
        Invalidate;
      end;
    VK_LEFT:
      begin
        if FFromRight then
        begin
          FHotPos:= FHotPos + 1;
          if FHotPos >= FPaths.Count then
            FHotPos:= -1;
        end
        else
        begin
          FHotPos:= FHotPos - 1;
          if FHotPos < FPathOff - 1 then
            FHotPos:= FPaths.Count - 1;
        end;

        if FHotPos = FPathOff - 1 then
        begin
          if FFromRight then
            mx:= ClientWidth - 22
          else
            mx:= 22;
          FHotPos:= -1;
        end else
          for i:= FPathOff to FHotPos do
            if FFromRight then
              mx:= mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
            else
              mx:= mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
        if FHotPos <> -1 then
          if FFromRight then
            mx:= mx + 14
          else
            mx:= mx - 14;
        FHotPt := Point(mx, my);
        CalcState;
        Invalidate;
      end;
    VK_DOWN, VK_UP:
      begin
        FBtnIdx:= FHotPos;
        if FHotPos = FPathOff - 1 then
        begin
          if FFromRight then
            mx:= ClientWidth - 22
          else
            mx:= 22;
          FHotPos:= -1;
        end else
          for i:= FPathOff to FHotPos do
            if FFromRight then
              mx:= mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
            else
              mx:= mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
        FHotPt := Point(mx, my);
        CalcState;
        FPopUp:= True;
        FButtonDown := True;
        PaintPath;
        if (FPopUp) and (FPopupPath <> '') then
        begin
          if (FAuto) then
            FMenu.Popup(FPopupPos.X, FPopupPos.Y)
          else if Assigned(FCustomPopup) then
            FCustomPopup.Popup(FPopupPos.X, FPopupPos.Y);
          PostMessage(Handle, CM_REFRESH, 0, 0);
        end;
      end;
    VK_RETURN:
      begin
        for i:= FPathOff to FHotPos do
          if FFromRight then
            mx:= mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
          else
            mx:= mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
        if FFromRight then
          mx:= mx + 14
        else
          mx:= mx - 14;
        FHotPt := Point(mx, my);
        FBtnIdx:= FHotPos;
        FButtonDown := True;
        CalcState;
        if (FState = isButtonPressed) and (FClickSetPath) and (FInButton) then
          if Assigned(FOnPathClick) then
          begin
            FOnPathClick(Self, FPathIdx);
            FButtonDown := False;
            CalcState;
          end;
      end
    else
  end;
end;

procedure TrkPathViewer.EditKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
var
  done: Boolean;
  s: String;
begin
  if key = VK_Escape then
    done := True
  else
  if key = VK_Return then
  begin
    done := True;
    s:= FPathEdit.Text;
    if Assigned(FOnEditedPath) then
      FOnEditedPath(Self, s);
  end else
    done := False;
  if done then
    ExitEditMode;
end;

procedure TrkPathViewer.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TrkPathViewer.EditExit(Sender: TObject);
begin
  ExitEditMode;
end;

procedure TrkPathViewer.FMenuPopup(Sender: TObject);
var
  i: Integer;
begin
  FPopUpX:= FHotPos;
  Screen.Cursor := crHourglass;
  try
    for i := FMenu.Items.Count - 1 downto 0 do
      FMenu.Items[i].Free;
    FMenu.Images:= FImages;
    if FFromRight then
      FMenu.Alignment:= paRight
    else
      FMenu.Alignment:= paLeft;
    if Assigned(FOnPopup) then
      FOnPopup(Self, FPopupPath, FPathOff, FPathIdx, FMenu);
    if Assigned(FOnPopupClick) then
      for i := FMenu.Items.Count - 1 downto 0 do
        FMenu.Items[i].OnClick:= FOnPopupClick;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TrkPathViewer.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      FBmp.Canvas.Font.Assign(Font);
      PaintPath;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;
  TGradientColors = array[0..255] of TRGBTriple;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3,
    y1, i, GSize: Integer;
  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
  GradientBmp: TBitmap;
begin
  rc1 := GetRValue(ColorToRGB(StartColor));
  gc1 := GetGValue(ColorToRGB(StartColor));
  bc1 := GetBValue(ColorToRGB(StartColor));
  rc2 := GetRValue(ColorToRGB(EndColor));
  gc2 := GetGValue(ColorToRGB(EndColor));
  bc2 := GetBValue(ColorToRGB(EndColor));
  rc3 := rc1 + (((rc2 - rc1) * 15) div 9);
  gc3 := gc1 + (((gc2 - gc1) * 15) div 9);
  bc3 := bc1 + (((bc2 - bc1) * 15) div 9);
  if rc3 < 0 then
    rc3 := 0
  else if rc3 > 255 then
    rc3 := 255;
  if gc3 < 0 then
    gc3 := 0
  else if gc3 > 255 then
    gc3 := 255;
  if bc3 < 0 then
    bc3 := 0
  else if bc3 > 255 then
    bc3 := 255;
  GradientBMP := TBitmap.Create;
  GradientBmp.PixelFormat := pf24bit;
  GradientBmp.Width := 1;
  GradientBmp.Height := (ARect.Bottom - ARect.Top) - 1;
  GSize := GradientBmp.Height;
  y1 := GSize div 2;
  for i := 0 to GSize - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    if (i < y1 - 1) then
    begin
      GradCol.rgbtRed := Byte(rc1 + (((rc2 - rc1) * (i)) div y1));
      GradCol.rgbtGreen := Byte(gc1 + (((gc2 - gc1) * (i)) div y1));
      GradCol.rgbtBlue := Byte(bc1 + (((bc2 - bc1) * (i)) div y1));
    end else
    begin
      GradCol.rgbtRed := Byte(rc3 + (((rc2 - rc3) * (i)) div GSize));
      GradCol.rgbtGreen := Byte(gc3 + (((gc2 - gc3) * (i)) div GSize));
      GradCol.rgbtBlue := Byte(bc3 + (((bc2 - bc3) * (i)) div GSize));
    end;
    Row[0] := GradCol;
  end;
  Canvas.StretchDraw(ARect, GradientBmp);
  GradientBmp.Free;
end;

procedure TrkPathViewer.CalcState;
var
  i, n: Integer;
  txt: string;
  R: TRect;
  ShowArrow, done, bool: Boolean;
  popX, popY: Integer;
  FSysArrow: Boolean;
begin
  if FPaths.Count = 0 then
    Exit;
  FPathOff:= CalcPath;
  if FPathOff > 0 then
    FArrowStyle:= pvaBack
  else
    FArrowStyle:= pvaNone;
  FSysArrow:= FArrowStyle <> pvaNone;
  if Assigned(FOnDrawArrow) then
    FOnDrawArrow(Self, FPathOff, FArrowStyle);
  if FFromRight then
  begin
    R.Right:= FBmp.Width - 22;
    R.Left:= R.Right;
  end
  else
  begin
    R.Left := 22;
    R.Right:= R.Left;
  end;
  n := FPathOff - 1;
  bool := FSysArrow;
  ShowArrow := True;
  FState := isNormal;
  FInButton:= (FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight) and (FButtonDown);
  FInButton:= FInButton or FKeys;
  txt := '';
  done := False;
  while (not done) do
  begin
    done := (n = FPaths.Count - 1);
    if bool then
      if FFromRight then
        R.Left:= R.Right - 14
      else
        R.Right := R.Left + 14;
    if not bool then
    begin
      inc(n);
      if (n > -1) and (n < FPaths.Count) then
        txt := FPaths[n];
      if FFromRight then
        R.Left := R.Right - FBmp.Canvas.TextWidth(txt) - 9
      else
        R.Right := R.Left + FBmp.Canvas.TextWidth(txt) + 9;
    end;
    if done then
      ShowArrow := (FShowArrow) or (FPathOff = FPaths.Count);
    if ShowArrow then
    begin
      if (FHotPt.X >= R.Left) and (FHotPt.X <= R.Right) then
      begin
        if FButtonDown then
        begin
          if bool then
            FState := isArrowPressed
          else
            FState := isButtonPressed;
        end else
        begin
          if bool then
            FState := isArrowHot
          else
            FState := isButtonHot;
        end;
        if FFromRight then
          popx := R.Right + 21
        else
          popx := R.Right - 35;
        popy := ClientHeight - 1;
        FPopupPos := ClientToScreen(Point(popX, popY));
        if (bool) and (n = FPathOff - 1) then
        begin
          FPopupPath := '*';
          FPathIdx := -1;
        end
        else
        begin
          FPopupPath:= '';
          for i:= 0 to n do
          if i = 0 then
            FPopupPath:= FPaths[i] + '\'
          else
            FPopupPath:= FPopupPath + FPaths[i] + '\';
          FPathIdx:= n;
        end;
        FHotPos := n;
        if (FButtonDown) and ((FHotPos <> FBtnIdx) or (not FInButton))then
          if (FState = isButtonPressed) or (FState = isArrowPressed) then
            FState:= isNormal;
      end;
    end;
    if FFromRight then
      R.Right:= R.Left
    else
      R.Left := R.Right;
    bool := not bool;
  end;

  FBtnState := isNormal;
  if (FState = isNormal) and (FButton) then // Check if in button...
  begin
    R:= ClientRect;
    if FFromRight then
      FInButton:= (FHotPt.X >= R.Left) and (FHotPt.X <= R.Left + FBtnWidth)
    else
      FInButton:= (FHotPt.X >= R.Right - FBtnWidth) and (FHotPt.X <= R.Right);
    FInButton:= FInButton and ((FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight));
    if FInButton then
      if FButtondown then
        FBtnState := isButtonPressed
      else
        FBtnState := isButtonHot;
  end;

  FDropBtnState := isNormal;
  if (FState = isNormal) and (FBtnState = isNormal) and (FDropDown)then // Check if in button...
  begin
    R:= ClientRect;
    if FButton then
      i:= FBtnWidth
    else
      i:= 0;
    if FFromRight then
      FInButton:= (FHotPt.X >= R.Left + i) and (FHotPt.X <= R.Left + i + FDropBtnWidth)
    else
      FInButton:= (FHotPt.X >= R.Right - i - FDropBtnWidth) and (FHotPt.X <= R.Right - i);
    FInButton:= FInButton and ((FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight));
    if FInButton then
      if FButtondown then
        FDropBtnState := isButtonPressed
      else
        FDropBtnState := isButtonHot;
  end;
end;

procedure PaintArrow(Canvas: TCanvas; x, y: Integer; Down, Right: Boolean);
begin
  with Canvas do
  begin
    if Down then
    begin
      MoveTo(x + 3, y + 3);
      LineTo(x + 7, y + 3);
      LineTo(x + 5, Y + 5);
      LineTo(x + 3, y + 3);
      LineTo(x + 6, y + 5);
      MoveTo(x + 2, y + 2);
      LineTo(x + 9, y + 2);
    end else
    if Right then
    begin
      MoveTo(x + 4, y);
      LineTo(x + 4, y + 4);
      LineTo(x + 2, y + 2);
      LineTo(x + 4, y);
      LineTo(x + 3, y + 3);
      LineTo(x + 5, y - 1);
      LineTo(x + 5, y + 6);
    end
    else
    begin
      MoveTo(x + 4, y);
      LineTo(x + 4, y + 4);
      LineTo(x + 6, y + 2);
      LineTo(x + 4, y);
      LineTo(x + 5, y + 3);
      LineTo(x + 3, y - 1);
      LineTo(x + 3, y + 6);
    end;
  end;
end;

procedure PaintBackArrow(Canvas: TCanvas; x, y: Integer; Down, Right: Boolean);
begin
  with Canvas do
  begin
    if Right then
    begin
      MoveTo(x + 6, y);
      LineTo(x + 8, y + 2);
      LineTo(x + 5, y + 5);
      MoveTo(x + 5, y);
      LineTo(x + 7, y + 2);
      LineTo(x + 4, y + 5);
      MoveTo(x + 2, y);
      LineTo(x + 4, y + 2);
      LineTo(x + 1, y + 5);
      MoveTo(x + 1, y);
      LineTo(x + 3, y + 2);
      LineTo(x + 0, y + 5);
    end
    else
    begin
      MoveTo(x + 2, y);
      LineTo(x + 0, y + 2);
      LineTo(x + 3, y + 5);
      MoveTo(x + 3, y);
      LineTo(x + 1, y + 2);
      LineTo(x + 4, y + 5);
      MoveTo(x + 6, y);
      LineTo(x + 4, y + 2);
      LineTo(x + 7, y + 5);
      MoveTo(x + 7, y);
      LineTo(x + 5, y + 2);
      LineTo(x + 8, y + 5);
    end;
  end;
end;

procedure TrkPathViewer.PaintButton(ACanvas: TCanvas; ARect: TRect; AText: string;
  AState: TpvItemState; AButton: Boolean);
var
  c1, c2: TColor;
  Grey: Boolean;
  r: TRect;
  y: Integer;
  uFormat: UINT;
begin
  r := ARect;
  if not AButton then
    r.Left := r.Left - 1;
  if AState <> isNormal then
  begin
    ACanvas.Font.Color := clBlack;
    Grey := AButton and (AState = isArrowHot);
    if Grey then
    begin
      c1 := FCol1;
      c2 := FCol2;
    end else
    begin
      if (AState = isArrowHot) then
      begin
        c1 := FCol3;
        c2 := FCol4;
      end else
      begin
        c1 := FCol5;
        c2 := FCol6;
      end;
    end;
    if Grey then
      ACanvas.Pen.Color := FCol7
    else
      ACanvas.Pen.Color := FCol8;
    ACanvas.MoveTo(r.Left, r.Top);
    ACanvas.LineTo(r.Left, r.Bottom);
    ACanvas.MoveTo(r.Right - 1, r.Top);
    ACanvas.LineTo(r.Right - 1, r.Bottom);
    r.Left := r.Left + 2;
    r.Right := r.Right - 2;
    r.Top := R.Top + 1;
    r.Bottom := R.Bottom - 1;
    if (AState = isArrowHot) or (AState = isButtonHot) then
      FillGradient(ACanvas, r, c1, c2)
    else
    begin
      InflateRect(r, 1, 1);
      FillGradient(ACanvas, r, c1, c2);
      InflateRect(r, -1, -1);
      r.Top := r.Top - 1;
      r.Bottom := r.Bottom + 1;
      ACanvas.Pen.Color := FCol9;
      ACanvas.MoveTo(r.Left - 1, r.Bottom - 1);
      ACanvas.LineTo(r.Left - 1, r.Top);
      ACanvas.LineTo(r.Right + 1, r.Top);
      ACanvas.MoveTo(r.Right, r.Top + 1);
      ACanvas.Pen.Color := FCol10;
      ACanvas.LineTo(r.Left, r.Top + 1);
      ACanvas.LineTo(r.Left, r.Bottom - 1);
      ACanvas.LineTo(r.Right + 1, r.Bottom - 1);
    end;
  end;
  if AButton then
  begin
    ACanvas.Brush.Style := bsClear;
    if (AState = isArrowPressed) or (AState = isButtonPressed) then
    begin
      r.Left := r.Left + 1;
      r.Top := r.Top + 1;
      r.Bottom := r.Bottom + 1;
    end;
    if FAllowKeyNav
      then uFormat := DT_CENTER or DT_VCENTER or DT_SINGLELINE
      else uFormat := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), r, uFormat);
  end else
  begin
    y := (((ARect.Bottom - ARect.Top) - 7) div 2) + 2;
    if AState = isNormal then
      ACanvas.Pen.Color := ACanvas.Font.Color
    else
      ACanvas.Pen.Color := FCol11;
    if AText = 'back' then
        PaintBackArrow(ACanvas, ARect.Right - 12, y, (AState = isButtonPressed) or
          (AState = isArrowPressed), FFromRight)
      else
        PaintArrow(ACanvas, ARect.Right - 12, y, (AState = isButtonPressed) or
          (AState = isArrowPressed), FFromRight);
    end;
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
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TrkPathViewer.PaintPath;
var
  i, n, x, w: Integer;
  txt: string;
  R: TRect;
  ShowArrow, Done, Bool: Boolean;
begin
  if InCreate then
    Exit;

  CalcState;
  FBmp.Width := ClientWidth;
  FBmp.Height := ClientHeight;
  FBmp.Canvas.Brush.Color := Color;
  FBmp.Canvas.Brush.Style := bsSolid;
  FBmp.Canvas.Pen.Color := Color;
  R := ClientRect;
  if Transparent then
    DrawParentImage(self, FBmp.Canvas)
  else
  begin
    if FFramed then
      FBmp.Canvas.Pen.Color := FBorderColor;
    FBmp.Canvas.Rectangle(R);
  end;
  if FFramed then
    InflateRect(R, -1, -1);
  if not InEdit then
  begin
    i := ((ClientHeight - 16) shr 1);
    if (FImageIdx <> -1) and (Assigned(FImages)) then
      if (FImageIdx > -1) and (FImageIdx < FImages.Count) then
        if FFromRight then
          FImages.Draw(FBmp.Canvas, FBmp.Width - 20, i, FImageIdx)
        else
          FImages.Draw(FBmp.Canvas, 4, i, FImageIdx);
    if FFromRight then
    begin
      R.Right := FBmp.Width - 22;
      R.Left:= R.Right;
    end
    else
    begin
      R.Left := 22;
      R.Right:= R.Left;
    end;
    if FArrowStyle = pvaBack then
      txt:= 'back'
    else
      txt := '';
    n:= FPathOff - 1;
    bool:= FArrowStyle <> pvaNone;
    done := False;
    ShowArrow := True;
    while not done do
    begin
      done := (n = FPaths.Count - 1);
      if bool then
        if FFromRight then
          R.Left:= R.Right - 14
        else
          R.Right := R.Left + 14;
      if not bool then
      begin
        inc(n);
        if (n > -1) and (n < FPaths.Count) then
          txt := FPaths[n];
        if FFromRight then
          R.Left:= R.Right -  FBmp.Canvas.TextWidth(txt) - 9
        else
          R.Right := R.Left + FBmp.Canvas.TextWidth(txt) + 9;
      end;
      if done then
        ShowArrow := (FShowArrow) or (FPathOff = FPaths.Count);
      if ShowArrow then
        if (FState <> isNormal) and (n = FHotPos) then
        begin
          if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
            FOnOwnerDraw(Self, FBmp.Canvas, R, txt, FState, not bool)
          else
          begin
            FBmp.Canvas.Font.Assign(Font);
            PaintButton(FBmp.Canvas, R, txt, FState, not bool);
          end;
        end else
        begin
          if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
            FOnOwnerDraw(Self, FBmp.Canvas, R, txt, isNormal, not bool)
          else
          begin
            FBmp.Canvas.Font.Assign(Font);
            PaintButton(FBmp.Canvas, R, txt, isNormal, not bool);
          end;
        end;
      if FFromRight then
        R.Right:= R.Left
      else
        R.Left := R.Right;
      bool := not bool;
    end;
  end;

  w:= 0;
  if (FButton) then
  begin
    w:= FBtnWidth;
    R:= ClientRect;
    if FFramed then
      InflateRect(R, -1, -1);
    if FFromRight then
    begin
      R.Right:= R.Left + w;
      x:= R.Right;
    end
    else
    begin
      R.Left:= R.Right - w;
      x:= R.Left;
    end;

    i:= 0;
    n:= 0;
    if Assigned(FImagesBtn) then
    begin
      i:= R.Left + ((w - FImagesBtn.Width) shr 1);
      n:= R.Top + ((ClientHeight - FImagesBtn.Height) shr 1);
    end;

    if FBtnState = isNormal then
    begin
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', isNormal, True)
      else
      begin
        FBmp.Canvas.Pen.Color := FBorderColor;
        FBmp.Canvas.MoveTo(x, r.Top);
        FBmp.Canvas.LineTo(x, r.Bottom);
        if Assigned(FImagesBtn) and (FImageIdxBtn <> -1) then
          FImagesBtn.Draw(FBmp.Canvas, i + 1, n - 1, FImageIdxBtn);
      end;
    end
    else
    begin
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', FBtnState, True)
      else
      begin
        PaintButton(FBmp.Canvas, R, '', FBtnState, True);
        if Assigned(FImagesBtn) and (FImageIdxBtn <> -1) then
          if FBtnState = isButtonHot then
            FImagesBtn.Draw(FBmp.Canvas, i + 1, n - 1, FImageIdxBtn)
          else
            FImagesBtn.Draw(FBmp.Canvas, i + 2, n, FImageIdxBtn)
      end;
    end;
  end;

  if FDropDown then
  begin
    w:= w + FDropBtnWidth;
    R:= ClientRect;
    if FFramed then
      InflateRect(R, -1, -1);

    if FFromRight then
      R.Right:= R.Left + w
    else
      R.Left:= R.Right - w;

    i:= R.Left + ((FDropBtnWidth - 7) shr 1);
    n:= R.Top + ((ClientHeight - 7) shr 1);
    if FDropBtnState = isNormal then
    begin
      FBmp.Canvas.Pen.Color := Font.Color;
      PaintArrow(FBmp.Canvas, i - 1,  n - 1, True, False);
    end
    else
    begin
      R.Right:= R.Left + FDropBtnWidth + 1;
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', FDropBtnState, True)
      else
      begin
        PaintButton(FBmp.Canvas, R, '', FDropBtnState, True);
        FBmp.Canvas.Pen.Color := Font.Color;
        if FDropBtnState = isButtonHot then
          PaintArrow(FBmp.Canvas, i - 1,  n - 1, True, False)
        else
          PaintArrow(FBmp.Canvas, i,  n, True, False);
      end;
    end;
  end;

  BitBlt(Canvas.Handle, 0, 0, FBmp.Width, FBmp.Height, FBmp.Canvas.Handle, 0, 0,
    SRCCOPY);
end;

procedure TrkPathViewer.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkPathViewer.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  // Answer Delphi that this component wants to handle its own arrow key press:
  message.result := DLGC_WANTARROWS;
end;

procedure TrkPathViewer.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkPathViewer.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TrkPathViewer.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if Assigned(OnEnter) then
    OnEnter(Self);
  FPopUpX := -2;
  if FAllowKeyNav then
    DoKey(VK_HOME, []);
end;

procedure TrkPathViewer.CMExit(var Message: TCMExit);
begin
  if Assigned(OnExit) then
    OnExit(Self);
  FKeys:= False;
  FHotPos := -2;
  FHotPt := Point(-1, -1);
  FState := isNormal;
  FPathIdx := -1;
  FPopupPath := '';
  FButtonDown := False;
  FPopUp := False;
  Invalidate;
  inherited;
end;

procedure TrkPathViewer.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not (csLoading in ComponentState) then
    Resize;
end;

procedure TrkPathViewer.CMRefresh(var Message: TMessage);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if not FKeys then
  begin
    FHotPos := -2;
    FHotPt := Point(-1, -1);
    FState := isNormal;
  end;
  FKeys:= False;
  FPathIdx := -1;
  FPopupPath := '';
  FButtonDown := False;
  FPopUp := False;
  PaintPath;
  inherited;
end;

procedure TrkPathViewer.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrkPathViewer.SetButton(const Value: Boolean);
begin
  FButton := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetFramed(const Value: Boolean);
begin
  FFramed := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetFromRight(const Value: Boolean);
begin
  FFromRight := Value;
  PaintPath;
end;

procedure TrkPathViewer.SetHideFirst(const Value: Boolean);
begin
  FHideFirst := Value;
  PaintPath;
end;

procedure TrkPathViewer.SetPath(const Value: string);
begin
  FPath := Value;
  FPaths.Clear;
  if FSeperator = '\' then
  begin
    FPath:= StringReplace(FPath, '\\', '|', [rfReplaceAll]);
    FPaths.Text := Trim(StringReplace(FPath, FSeperator, #13, [rfReplaceAll]));
    FPaths.Text:= StringReplace(FPaths.Text, '|', '\\', [rfReplaceAll]);
  end else
    FPaths.Text := Trim(StringReplace(FPath, FSeperator, #13, [rfReplaceAll]));
  if Assigned(FOnPathChanged) then
    FOnPathChanged(Self);
  Invalidate;
end;

procedure TrkPathViewer.SetOwnerDraw(const Value: Boolean);
begin
  FOwnerDraw := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetSeperator(const Value: Char);
begin
  FSeperator := Value;
  SetPath(FPath);
end;

procedure TrkPathViewer.SetColorBtn(const Index: Integer; const Value: TColor);
begin
  case Index of
    0 : FCol1:= Value;
    1 : FCol2:= Value;
    2 : FCol3:= Value;
    3 : FCol4:= Value;
    4 : FCol5:= Value;
    5 : FCol6:= Value;
    6 : FCol7:= Value;
    7 : FCol8:= Value;
    8 : FCol9:= Value;
    9 : FCol10:= Value;
    10 : FCol11:= Value;
    else
  end;
  Invalidate;
end;

procedure TrkPathViewer.SetDefault(const Value: Boolean);
begin
  FDefault := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetDropDown(const Value: Boolean);
begin
  FDropDown := Value;
  Invalidate;
end;

procedure TrkPathViewer.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

function TrkPathViewer.CalcPath: Integer;
// Calculate how many parts of the path that will fit in view
var
  i, w, btns: Integer;
begin
  Result := 0;
  btns:= 0;

  if FPaths.Count = 0 then
    Exit;
  FBmp.Canvas.Font.Assign(Font);
  w:= 22 + 14;

  if FButton then
    btns:= FBtnWidth;

  if FDropDown then
    btns:= btns + FDropBtnWidth;

  for i:= 0 to FPaths.Count - 1 do
    w:= w + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
  if not FShowArrow then
    w:= w - 14;
  if w < ClientWidth - (8 + btns) then
  begin
    if (FPaths.Count > 1) and (Result = 0) then
      if FHideFirst then Result:= 1;
    Exit;
  end;
  w := w - (ClientWidth - (8 + btns));
  i := 0;
  while (w > 0) and (i < FPaths.Count - 1) do
  begin
    w := w - (FBmp.Canvas.TextWidth(FPaths[i]) + 23);
    i := i + 1;
  end;
  if w > 0 then
    Result := FPaths.Count
  else
    Result := i;
  if (FPaths.Count > 1) and (Result = 0) then
    if FHideFirst then Result:= 1;
end;

constructor TrkPathViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InCreate:= True;
  Width := 150;
  Height := 25;
  ControlStyle := ControlStyle + [csClickEvents, csReplicatable, csNeedsBorderPaint, csCaptureMouse];
  Color := clWindow;
  FAuto := True;
  FAllowEdit:= True;
  FAllowKeyNav:= False;
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf24Bit;
  FBorderColor := clSilver;
  FBorderStyle := bsNone;
  FBtnIdx:= -1;
  FBtnWidth:= 25;
  FButton:= False;
  FButtonDown := False;
  FClickSetPath:= True;
  FCol1 := $F2F2F2;
  FCol2 := $E3E3E3;
  FCol3 := $FDF6EA;
  FCol4 := $F4D9A6;
  FCol5 := $FDF6EA;
  FCol6 := $FADFAC;
  FCol7 := $8F8F8F;
  FCol8 := $B17F3C;
  FCol9 := $927764;
  FCol10 := $EACF9C;
  FCol11 := clBlack;
  FDropDown:= False;
  FDropBtnWidth:= 18;
  FDropBtnState:= isNormal;
  FFramed := True;
  FFromRight:= False;
  FHideFirst:= False;
  FHotPos:= -2;  // -1 = HomeMenu
  FImages:= nil;
  FImageIdx := -1;
  FImagesBtn:= nil;
  FImageIdxBtn := -1;
  FMenu := TPopUpMenu.Create(nil);
  FMenu.OnPopup := FMenuPopup;
  FOwnerDraw:= False;
  FPath := '';
  FPathIdx := -1;
  FPaths := TStringList.Create;
  FPathOff := -1;
  FPopupPath := '';
  FSeperator:= '\';
  FState := isNormal;
  FShowArrow:= False;
  FTransparent:= False;
  ParentColor:= False;
  ParentCtl3d:= True;
  ParentFont:= True;
  InCreate := False;
  InEdit:= False;
end;

destructor TrkPathViewer.Destroy;
begin
  InShutdown:= True;
  ExitEditMode;
  FMenu.Free;
  FPaths.Free;
  FBmp.Free;
  inherited Destroy;
end;

procedure TrkPathViewer.Resize;
begin
  if InShutDown or InCreate then
    Exit;
  FPathOff := CalcPath;
  Invalidate;
end;

procedure TrkPathViewer.CreateParams(var Params: TCreateParams);
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

procedure TrkPathViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FHotPt := Point(X, Y);
    CalcState;

    if InEdit then
    begin
      if (FBtnState <> isNormal) then
        ExitEditMode;
      FButtonDown := True;
      CalcState;
      PaintPath;
      Exit;
    end;
    FBtnIdx:= FHotPos;
    if (FPopUpX = FHotPos) and (FState <> isNormal) and (FBtnState <> isNormal) then
    begin
      FPopUpX:= -2;
      Exit;
    end;
    if (FState = isArrowHot) then
      FPopUp := True
    else
      FPopUp := False;
    FButtonDown := True;
    if not InEdit then
      PaintPath;
    if (FPopUp) and (FPopupPath <> '') then
    begin
      if (FAuto) then
        FMenu.Popup(FPopupPos.X, FPopupPos.Y)
      else
      if Assigned(FCustomPopup) then
        FCustomPopup.Popup(FPopupPos.X, FPopupPos.Y);
      PostMessage(Handle, CM_REFRESH, 0, 0);
    end;
    if (FState = isNormal) and (FBtnState = isNormal) and (FDropBtnState = isNormal) and (FAllowEdit = True) and (not InEdit) then
      DoEdit;
  end;
  if (FState = isNormal) then
    FPopupPath := FPath;
  inherited;
end;

procedure TrkPathViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FPopUp then
    Exit;
  FHotPt := Point(X, Y);
  CalcState;
  if (FState = isButtonPressed) and (FClickSetPath) and (FInButton) then
  begin
    if Assigned(FOnPathClick) then
      FOnPathClick(Self, FPathIdx);
  end
  else
  if (FBtnState = isButtonPressed) and (FInButton) then
  begin
    if Assigned(FOnBtnClick) then
      FOnBtnClick(Self);
  end
  else
  if (FDropBtnState = isButtonPressed) and (FInButton) then
    if Assigned(FOnDropBtnClick) then
      FOnDropBtnClick(Self);

  FButtonDown := False;
  FPopUp := False;
  Invalidate;
  inherited;
end;

procedure TrkPathViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FKeys then
  begin
    if (FLastPoint.X = -1) and (FLastPoint.Y = -1) then
      FLastPoint:= Point(x, y)
    else
    if (Abs(FLastPoint.X - x) > 5) or (Abs(FLastPoint.Y - Y) > 5) then
      FKeys:= False;
  end else
  begin
    FHotPt := Point(X, Y);
    FHotPos:= -2;
    Invalidate;
  end;
  inherited;
end;

procedure TrkPathViewer.CMMouseEnter(var Message: TMessage);
begin
  FPopUpX := -2;
  Invalidate;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  inherited
end;

procedure TrkPathViewer.CMMouseLeave(var Message: TMessage);
begin
  if FPopUp then
    Exit;
  FKeys:= False;
  FButtonDown := False;
  FPopUp := False;
  FHotPt := Point(-1, -1);
  FHotPos := -2;
  FState:= isNormal;
  Invalidate;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('rmklever', [TrkPathViewer]);
end;

end.

