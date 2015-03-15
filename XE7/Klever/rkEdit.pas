unit rkEdit;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Graphics, Messages, Types,
  ImgList;

type
  TrkEdit = class(TCustomControl)
  private
    { Private declarations }
    FArrow: Boolean;
    FBmp: TBitmap;
    FBorderColor: TColor;
    FBtnCaption: String;
    FBtnPressed: Boolean;
    FBtnsLeft: Integer;
    FBtnsRight: Integer;
    FBtnsWidth: Integer;
    FEdit: TEdit;
    FFramed: Boolean;
    FInBtn: Boolean;
    FImages: TCustomImageList;
    FImgInactive: Integer;
    FImgActive: Integer;
    FImgActiveHot: Integer;
    FImgActiveDown: Integer;
    FHintActive: Boolean;
    FHintColor: TColor;
    FHintFont: TFont;
    FHintText: String;
    FTransparent: Boolean;
    FOnButtonClick: TNotifyEvent;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure PaintCtrl;
    procedure SetBtnCaption(const Value: String);
    procedure SetBtnWidth(const Value: Integer);
    procedure SetHintActive(const Value: Boolean);
    procedure SetHintColor(const Value: TColor);
    procedure SetHintFont(const Value: TFont);
    procedure SetHintText(const Value: String);
    procedure SetImgActive(const Value: Integer);
    procedure SetImgActiveDown(const Value: Integer);
    procedure SetImgActiveHot(const Value: Integer);
    procedure SetImgInactive(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFramed(const Value: Boolean);
    procedure CreateWnd;
    function InButton(x, y: Integer): Boolean;
  protected
    { Protected declarations }
    InCreate: Boolean;
    InEdit: Boolean;
    InShutdown: Boolean;
    BtnRect: TRect;
    CurImg: Integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BtnCaption: String read fBtnCaption write SetBtnCaption;
    property BtnWidth: Integer read FBtnsWidth write SetBtnWidth;
    property Color;
    property Enabled;
    property Framed: Boolean read FFramed write SetFramed default True;
    property ImgInactive: Integer read FImgInactive write SetImgInactive default -1;
    property ImgActive: Integer read FImgActive write SetImgActive default -1;
    property ImgActiveDown: Integer read FImgActiveDown write SetImgActiveDown default -1;
    property ImgActiveHot: Integer read FImgActiveHot write SetImgActiveHot default -1;
    property Images: TCustomImageList read FImages write FImages;
    property HintActive: Boolean read FHintActive write SetHintActive default True;
    property HintColor: TColor read FHintColor write SetHintColor default clGrayText;
    property HintText: String read FHintText write SetHintText;
    property HintFont: TFont read FHintFont write SetHintFont;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnButtonClick: TNotifyEvent read fOnButtonClick write fOnButtonClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('rmklever', [TrkEdit]);
end;

{ TrkEdit }

procedure TrkEdit.CMExit(var Message: TCMExit);
begin
  Self.Repaint;
end;

constructor TrkEdit.Create(AOwner: TComponent);
begin
  inherited;
  InCreate:= True;

  Width := 120;
  Height := 23;
  ControlStyle := ControlStyle + [csClickEvents, csReplicatable, csNeedsBorderPaint, csCaptureMouse];
  Color := clWindow;

  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf24Bit;

  CurImg:= -1;

  FBtnCaption:= FBtnCaption;
  FBtnPressed:= False;
  FBtnsWidth:=19;
  FInBtn:= False;

  FFramed:= True;
  FHintActive:= True;

  FImgInactive:= -1;
  FImgActive:= -1;
  FImgActiveHot:= -1;
  FImgActiveDown:= -1;

  FHintActive:= False;
  FHintColor:= clGray;
  FHintFont:= TFont.Create;
  FHintFont.Assign(Self.Font);
  FHintFont.Color:= FHintColor;
  FHintFont.Style:= [fsItalic];
  FHintText:= FHintText;

  InCreate:= False;
end;

procedure TrkEdit.CreateWnd;
begin
  inherited;

end;

destructor TrkEdit.Destroy;
begin
  FHintFont.Free;
  FBmp.Free;
  inherited;
end;

function TrkEdit.InButton(x, y: Integer): Boolean;
begin
  Result:= (x > (ClientWidth - FBtnsWidth)) and (x < ClientWidth);
  Result:= Result and (y > 0) and (y < ClientHeight);
end;

procedure TrkEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FBtnPressed:= InButton(x, y) and (Button = mbLeft);
  Repaint;
end;

procedure TrkEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  b: Boolean;
begin
  inherited;
  b:= FInBtn;
  FInBtn:= InButton(x, y);
  if FInBtn <> b then
    Repaint;
end;

procedure TrkEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FBtnPressed:= False;
  Repaint;
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

procedure TrkEdit.PaintCtrl;
var
  dc: HDC;
  bmp: TBitmap;
  i: Integer;
  x, y: Integer;
  R, EditRect: TRect;
begin
  if InCreate then
    Exit;

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

  BtnRect:= R;//ClientRect;

  EditRect:= R;
  EditRect.Left:= EditRect.Left + FBtnsWidth;
  EditRect.Right:=  EditRect.Right - FBtnsWidth;

  BtnRect.Left:=BtnRect.Right-FBtnsWidth;
  i:= CurImg;
  if Assigned(FImages) then
  begin
    //if Self.Text = '' then
    //  i := FImgInactive
    //else
    begin
      if FInBtn then
      begin
        if FBtnPressed then
          i:= FImgActiveDown
        else
          i:= FImgActiveHot;
      end
      else
        i:= FImgActive;
    end;
  end;

  FHintText:= IntToStr(i);

  //if (CurImg <> i) then
  begin
    CurImg:= i;
//    if CurImg <> -1 then
//    begin
      x:= BtnRect.Left + ((FBtnsWidth - FImages.Width) shr 1);
      y:= (ClientHeight - FImages.Height) shr 1;
      FImages.Draw(FBmp.Canvas, x, y, i, True);
//    end;
  end;

  {
  if FBtnPressed and FInBtn then
  begin
    //DrawFrameControl(DC,BtnRect,DFC_BUTTON,DFCS_BUTTONPUSH+DFCS_PUSHED);
    //DrawText(DC,PChar(fBtnCaption),Length(fBtnCaption),BtnRect,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end
  else
  begin
    //DrawFrameControl(DC,BtnRect,DFC_BUTTON,DFCS_BUTTONPUSH);
    //DrawText(DC,PChar(fBtnCaption),Length(fBtnCaption),BtnRect,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
  }

  FBmp.Canvas.Font.Assign(FHintFont);
  //FBmp.Canvas.Font.Color:= clRed;
  //if {(Self.Text <> '') and} (FHintActive) or (Focused) then
    DrawText(FBmp.Canvas.Handle, PChar(FHintText), Length(FHintText), EditRect, DT_SINGLELINE or DT_VCENTER);

  BitBlt(Canvas.Handle, 0, 0, FBmp.Width, FBmp.Height, FBmp.Canvas.Handle, 0, 0,
    SRCCOPY);
end;

procedure TrkEdit.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      FBmp.Canvas.Font.Assign(Font);
      PaintCtrl;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;

end;

procedure TrkEdit.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TrkEdit.SetBtnCaption(const Value: String);
begin
  if Value <> FBtnCaption then
  begin
    FBtnCaption:=Value;
    Invalidate;
  end;
end;

procedure TrkEdit.SetBtnWidth(const Value: Integer);
begin
  if Value <> FBtnsWidth then
  begin
    FBtnsWidth:= Value;
    Perform(EM_SETMARGINS,EC_RIGHTMARGIN,(FBtnsWidth) shl 16);
    BtnRect.Left:=BtnRect.Right-FBtnsWidth;
    Invalidate;
  end;
end;

procedure TrkEdit.SetFramed(const Value: Boolean);
begin
  FFramed := Value;
  Invalidate;
end;

procedure TrkEdit.SetHintActive(const Value: Boolean);
begin
  FHintActive := Value;
  Repaint;
end;

procedure TrkEdit.SetHintColor(const Value: TColor);
begin
  FHintColor := Value;
  Repaint;
end;

procedure TrkEdit.SetHintFont(const Value: TFont);
begin
  FHintFont.Assign(Value);
  Repaint;
end;

procedure TrkEdit.SetHintText(const Value: String);
begin
  if Value <> FHintText then
  begin
    FHintText := Value;
    Repaint;
  end;
end;

procedure TrkEdit.SetImgActive(const Value: Integer);
begin
  FImgActive := Value;
  Invalidate;
end;

procedure TrkEdit.SetImgActiveDown(const Value: Integer);
begin
  FImgActiveDown := Value;
  Invalidate;
end;

procedure TrkEdit.SetImgActiveHot(const Value: Integer);
begin
  FImgActiveHot := Value;
  Invalidate;
end;

procedure TrkEdit.SetImgInactive(const Value: Integer);
begin
  FImgInactive := Value;
  Invalidate;
end;

procedure TrkEdit.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TrkEdit.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkEdit.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  // Answer Delphi that this component wants to handle its own arrow key press:
  message.result := DLGC_WANTARROWS;
end;

procedure TrkEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

end.
