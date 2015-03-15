unit rkFilterPreviewDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Math;

type
  TOnCtrlEvent = procedure(Sender: TObject; AIdx, AValue: Integer) of object;
  TOnProcessEvent = procedure(Sender: TObject; ABitmap: TBitmap) of object;
  TrkFilterPreviewDialog = class(TComponent)
  private
    { Private declarations }
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCaption: TCaption;
    FCtrls: TStrings;
    FPicture: TBitmap;
    FPosition: TPosition;
    FStart: Integer;
    FEnd: Integer;
    FUseBeginEnd: Boolean;
    FOnCtrlChg: TOnCtrlEvent;
    FOnProcess: TOnProcessEvent;
    procedure DoProcessing;
    procedure MakePreviewImage;
    procedure SetCtrls(const Value: TStrings);
    procedure SetPicture(const Value: TBitmap);
    procedure SetPosition(const Value: TPosition);
  protected
    { Protected declarations }
    ImageLink: TImage;
    img: TBitmap;
    procedure FormResize(Sender: TObject);
    procedure PositionChange(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure SetRadioBtn(AIndex: Integer);
    procedure SetTrackbarPos(AIndex, AValue: Integer);
  published
    { Published declarations }
    property Caption: TCaption read FCaption write FCaption;
    property Controls: TStrings read FCtrls write SetCtrls;
    property Picture: TBitmap read FPicture write SetPicture;
    property Position: TPosition read FPosition write SetPosition;
    property DlgLeft: Integer read FLeft write FLeft default 40;
    property DlgTop: Integer read FTop write FTop default 40;
    property DlgWidth: Integer read FWidth write FWidth default 600;
    property DlgHeight: Integer read FHeight write FHeight default 400;
    property CtrlBegin: Integer read FStart write FStart default 0;
    property CtrlEnd: Integer read FEnd write FEnd default 0;
    property UseBeginEnd: Boolean read FUseBeginEnd write FUseBeginEnd default False;
    property OnControlChange: TOnCtrlEvent read FOnCtrlchg write FOnCtrlChg;
    property OnImageProcess: TOnProcessEvent read FOnProcess write FOnProcess;
  end;

type
  PRGB24 = ^TRGB24;
  TRGB24 = record B, G, R: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGB24;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('rmklever', [TrkFilterPreviewDialog]);
end;

type
  TrkFilterDialogForm = class(TCustomForm)
  private
    BtnOk: TButton;
    BtnCancel: TButton;
    CtrlPanel: TPanel;
    ImgDest: TImage;
  protected
    procedure AfterConstruction; override;
    procedure DoShow; override;
  public
  end;

function CalcImgSize(w, h, tw, th: integer): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if (w < tw) and (h < th) then
  begin
    Result.X := w;
    Result.Y := h;
  end
  else if (w = 0) or (h = 0) then
    Exit
  else
  begin
    if w > h then
    begin
      if w < tw then
        tw := w;
      Result.X := tw;
      Result.Y := Trunc(tw * h / w);
      if Result.Y > th then
      begin
        Result.Y := th;
        Result.X := Trunc(th * w / h);
      end;
    end
    else
    begin
      if h < th then
        th := h;
      Result.Y := th;
      Result.X := Trunc(th * w / h);
      if Result.X > tw then
      begin
        Result.X := tw;
        Result.Y := Trunc(tw * h / w);
      end;
    end;
  end;
end;

procedure SmoothReSize(Src, Dest: TBitmap);
var
  x, y, px, py: Integer;
  i, x1, x2, z, z2, iz2: Integer;
  w1, w2, w3, w4: Integer;
  Ratio: Integer;
  sDst, sDstOff: Integer;
  PScanLine: array of PRGBArray;
  Src1, Src2: PRGBArray;
  C, C1, C2: TRGB24;
begin
  if (Dest.Width < 2) or (Dest.Height < 2) then
  begin
    Dest.Assign(Src);
  end;
  SetLength(PScanLine, Src.Height);
  PScanLine[0]:= (Src.Scanline[0]);
  i := Integer(Src.Scanline[1]) - Integer(PScanLine[0]);
  for y := 1 to Src.Height - 1 do
    PScanLine[y]:= PRGBArray(Integer(PScanLine[y - 1]) + i);
  sDst := Integer(Dest.ScanLine[0]);
  sDstOff := Integer(Dest.ScanLine[1]) - sDst;
  Ratio := ((Src.Width - 1) shl 15) div Dest.Width;
  py := 0;
  for y := 0 to Dest.Height - 1 do
  begin
    i := py shr 15;
    if i > src.Height - 1 then
      i := src.Height - 1;
    Src1 := PScanline[i];
    if i < src.Height - 1 then
      Src2 := PScanline[i + 1]
    else
      Src2 := Src1;
    z2 := py and $7FFF;
    iz2 := $8000 - z2;
    px := 0;
    for x := 0 to Dest.Width - 1 do
    begin
      x1 := px shr 15;
      x2 := x1 + 1;
      C1 := Src1[x1];
      C2 := Src2[x1];
      z := px and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      C.R := (C1.R * w1 + Src1[x2].R * w2 + C2.R * w3 + Src2[x2].R * w4) shr 15;
      C.G := (C1.G * w1 + Src1[x2].G * w2 + C2.G * w3 + Src2[x2].G * w4) shr 15;
      C.B := (C1.B * w1 + Src2[x2].B * w2 + C2.B * w3 + Src2[x2].B * w4) shr 15;
      PRGBArray(sDst)[x] := C;
      Inc(px, Ratio);
    end;
    sDst := sDst + SDstOff;
    Inc(py, Ratio);
  end;
  SetLength(PScanline, 0);
end;

{ TrkFilterPreviewDialog }

constructor TrkFilterPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  FTop:= 40;
  FLeft:= 40;
  FWidth:= 600;
  FHeight:= 400;
  Position:= poDefault;
  FCtrls:= TStringList.Create;
  FPicture:= TBitmap.Create;
  img:= TBitmap.Create;
  img.PixelFormat := pf24Bit;
  FStart:= 0;
  FEnd:= 0;
  FUseBeginend:= False;
end;

destructor TrkFilterPreviewDialog.Destroy;
begin
  img.Free;
  FPicture.Free;
  FCtrls.Free;
  inherited;
end;

procedure TrkFilterPreviewDialog.DoProcessing;
var
  tmp, bmp: TBitmap;
  pt: TPoint;
begin
  if not Assigned(OnImageProcess) then
    Exit;
  bmp := TBitmap.Create;
  bmp.Assign(img);
  bmp.PixelFormat := pf24Bit;
  tmp := TBitmap.Create;
  tmp.PixelFormat := pf24Bit;
  pt := CalcImgSize(bmp.Width, bmp.Height, ImageLink.Width, ImageLink.Height);
  tmp.Width := pt.X;
  tmp.Height := pt.Y;
  SmoothReSize(bmp, tmp);
  FOnProcess(Self, tmp);
  ImageLink.Picture.Bitmap.Assign(tmp);
  ImageLink.Refresh;
  bmp.Free;
  tmp.Free;
end;

function TrkFilterPreviewDialog.Execute: Boolean;
var
  Form: TrkFilterDialogForm;
  b, e, i, t, y: Integer;
  ctrlList: TStringList;
begin
  Form := TrkFilterDialogForm.CreateNew(Application);
  try
    Form.Caption:= Caption;
    Form.BorderStyle:= bsSizeToolWin;
    Form.Left:= FLeft;
    Form.Top:= FTop;
    Form.Width:= FWidth;
    Form.Height:= FHeight;
    Form.Position:= Position;
    Form.OnResize:= FormResize;
    Form.DoubleBuffered:= True;
    ImageLink:= Form.ImgDest;

    t:= 1;
    y:= 8;

    b:= 0;
    e:= FCtrls.Count - 1;

    if FUseBeginEnd then
    begin
      b:= FStart;
      if (b < 0) then
        b:= 0
      else
      if (b > FCtrls.Count - 1) then
        b:= FCtrls.Count - 1;
      e:= FEnd;
      if (e < b) then
        e:= b
      else
      if (e > FCtrls.Count - 1) then
        e:= FCtrls.Count - 1;
    end;

    ctrlList:= TStringList.Create;
    for i := b to e do
    begin
      ctrlList.Text:=Trim(StringReplace(FCtrls[i], ':', #13#10, [rfReplaceAll]));
      if ctrlList.Count > 1 then
      begin
        if ctrlList[0] = 'R' then // Radiobutton
        begin
          if ctrlList.Count = 3 then
          begin
            with TRadioButton.Create(Form) do
            begin
              Parent:= Form.CtrlPanel;
              Left:= 25;
              Top:= y;
              Width:= 154;
              Caption:= ctrlList[1];
              if ctrlList.Count > 2 then
                Checked:= ctrlList[2] = '1'
              else
                Checked:= True;
              Tag:= t;
              t:= t + 1;
              OnClick:= PositionChange;
            end;
            y:= y + 23;
          end;
        end;
        if ctrlList[0] = 'T' then // Trackbar
        begin
          if ctrlList.Count = 5 then
          begin
            with TTrackBar.Create(Form) do
            begin
              Parent:= Form.CtrlPanel;
              Left:= 17;
              Top:= y;
              Width:= 154;
              Min:= StrToIntDef(ctrlList[1], 0);
              Max:= StrToIntDef(ctrlList[2], 100);
              Position:= StrToIntDef(ctrlList[3], 50);
              Frequency:= StrToIntDef(ctrlList[4], 10);
              TickMarks:= tmTopLeft;
              ShowSelRange:= False;
              Tag:= t;
              t:= t + 1;
              OnChange:= PositionChange;
            end;
            y:= y + 36;
          end;
        end;
        if ctrlList[0] = 'L' then // Label
        begin
          if ctrlList.Count = 2 then
          begin
            with TLabel.Create(Form) do
            begin
              Parent:= Form.CtrlPanel;
              Left:= 24;
              Top:= y;
              Caption:= ctrlList[1];
            end;
            y:= y + 12;
          end;
        end;
        if ctrlList[0] = 'S' then // Space
          if ctrlList.Count = 2 then
            y:= y + StrToIntDef(ctrlList[1], 10);
      end;
    end;
    ctrlList.Free;
    Result := Form.ShowModal = mrOk;
    FLeft:= Form.Left;
    FTop:= Form.Top;
    FWidth:= Form.Width;
    FHeight:= Form.Height;
  finally
    Form.Free;
  end;
end;

procedure TrkFilterPreviewDialog.FormResize(Sender: TObject);
begin
  MakePreviewImage;
end;

procedure TrkFilterPreviewDialog.MakePreviewImage;
var
  pt: TPoint;
begin
  pt := CalcImgSize(FPicture.Width, FPicture.Height, ImageLink.Width, ImageLink.Height);
  if (pt.X < 2) or (pt.Y < 2) then
    Exit;
  img.Width := pt.X;
  img.Height := pt.Y;
  FPicture.PixelFormat := pf24Bit;
  SmoothReSize(FPicture, img);
  DoProcessing;
end;

procedure TrkFilterPreviewDialog.PositionChange(Sender: TObject);
var
  i, p: Integer;
begin
  i:= -1;
  p:= -1;
  if Sender is TTrackBar then
  begin
    i:= (Sender as TTrackBar).Tag;
    p:= (Sender as TTrackBar).Position;
  end
  else
  if Sender is TRadioButton then
  begin
    i:= (Sender as TRadioButton).Tag;
    p:= 1;
  end;
  if Assigned(FOnCtrlChg) then
    FOnCtrlChg(Sender, i, p);
  DoProcessing;
end;

procedure TrkFilterPreviewDialog.SetCtrls(const Value: TStrings);
begin
  FCtrls.Assign(Value);
end;

procedure TrkFilterPreviewDialog.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TrkFilterPreviewDialog.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

procedure TrkFilterPreviewDialog.SetTrackbarPos(AIndex, AValue: Integer);
var
  b, e, i, j: Integer;
  ctrlList: TStringList;
begin
  if AIndex > FCtrls.Count then
    Exit;
  b:= 0;
  e:= FCtrls.Count - 1;
  if FUseBeginEnd then
  begin
    b:= FStart;
    if (b < 0) then
      b:= 0
    else
    if (b > FCtrls.Count - 1) then
      b:= FCtrls.Count - 1;
    e:= FEnd;
    if (e < b) then
      e:= b
    else
    if (e > FCtrls.Count - 1) then
      e:= FCtrls.Count - 1;
  end;
  i:= b;
  j:= 0;
  repeat
    if Uppercase(FCtrls[i][1]) = 'T' then
      j:= j + 1;
    i:= i + 1;
  until (i = e + 1) or (j = AIndex);
  if j = Aindex then
  begin
    i:= i - 1;
    ctrlList:= TStringList.Create;
    ctrlList.Text:= Trim(StringReplace(FCtrls[i], ':', #13#10, [rfReplaceAll]));
    if ctrlList.Count = 5 then
      ctrlList[3]:= IntToStr(AValue);
    ctrlList.Text:= Trim(StringReplace(ctrlList.Text, #13#10, ':', [rfReplaceAll]));
    FCtrls[i]:= ctrlList.Text;
    ctrlList.Free;
  end;
end;

procedure TrkFilterPreviewDialog.SetRadioBtn(AIndex: Integer);
var
  b, e, i, j: Integer;
  ctrlList: TStringList;
begin
  if AIndex > FCtrls.Count then
    Exit;
  b:= 0;
  e:= FCtrls.Count - 1;
  if FUseBeginEnd then
  begin
    b:= FStart;
    if (b < 0) then
      b:= 0
    else
    if (b > FCtrls.Count - 1) then
      b:= FCtrls.Count - 1;
    e:= FEnd;
    if (e < b) then
      e:= b
    else
    if (e > FCtrls.Count - 1) then
      e:= FCtrls.Count - 1;
  end;

  j:= 0;
  ctrlList:= TStringList.Create;
  for i:= b to e do
  begin
    if Uppercase(FCtrls[i][1]) = 'R' then
    begin
      j:= j + 1;
      ctrlList.Text:= Trim(StringReplace(FCtrls[i], ':', #13#10, [rfReplaceAll]));
      if ctrlList.Count = 3 then
        if j = AIndex then
          ctrlList[2]:= '1'
        else
          ctrlList[2]:= '0';
      ctrlList.Text:= Trim(StringReplace(ctrlList.Text, #13#10, ':', [rfReplaceAll]));
      FCtrls[i]:= ctrlList.Text;
    end;
  end;
  ctrlList.Free;

  i:= b;
  j:= 0;
  repeat
    if Uppercase(FCtrls[i][1]) = 'R' then
      j:= j + 1;
    i:= i + 1;
  until (i = e + 1) or (j = AIndex);
  if j = Aindex then
  begin
    i:= i - 1;
    ctrlList:= TStringList.Create;
    ctrlList.Text:= Trim(StringReplace(FCtrls[i], ':', #13#10, [rfReplaceAll]));
    if ctrlList.Count = 3 then
      ctrlList[2]:= '1';
    ctrlList.Text:= Trim(StringReplace(ctrlList.Text, #13#10, ':', [rfReplaceAll]));
    FCtrls[i]:= ctrlList.Text;
    ctrlList.Free;
  end;
end;

{ TrkFilterDialogForm }

procedure TrkFilterDialogForm.AfterConstruction;
begin
  CtrlPanel:= TPanel.Create(Self);
  CtrlPanel.Parent:= Self;
  CtrlPanel.Width:= 185;
  CtrlPanel.Height:= ClientHeight;
  CtrlPanel.Align:= alRight;
  CtrlPanel.BevelOuter:= bvNone;
  CtrlPanel.Color:= clWhite;
  CtrlPanel.ParentBackground:= False;
  CtrlPanel.TabOrder:= 0;
  ImgDest:= TImage.Create(Self);
  ImgDest.Parent:= Self;
  ImgDest.Align:= alClient;
  ImgDest.Center:= True;
  ImgDest.Proportional:= True;
  BtnOk:= TButton.Create(Self);
  BtnOk.Parent:= CtrlPanel;
  BtnOk.Left:= 16;
  BtnOk.Top:= ClientHeight - 33;
  BtnOk.Width:= 75;
  BtnOk.Height:= 25;
  BtnOk.Anchors:= [akLeft, akBottom];
  BtnOk.Default:= True;
  BtnOk.Caption:= '&Ok';
  BtnOk.TabOrder:= 2;
  BtnOk.ModalResult:= mrOk;
  BtnCancel:= TButton.Create(Self);
  BtnCancel.Parent:= CtrlPanel;
  BtnCancel.Left:= 97;
  BtnCancel.Top:= ClientHeight - 33;
  BtnCancel.Width:= 75;
  BtnCancel.Height:= 25;
  BtnCancel.Anchors:= [akLeft, akBottom];
  BtnCancel.Cancel:= True;
  BtnCancel.Caption:= '&Cancel';
  BtnCancel.TabOrder:= 1;
  BtnCancel.ModalResult:= mrCancel;
end;

procedure TrkFilterDialogForm.DoShow;
begin
  inherited;

end;

end.
