unit MsgList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls;

type
  TMsgList = class;
  
  TMsgScreen = class (TGraphicControl)
  private
    procedure do_Draw;
    function GetMsgList: TMsgList;
  protected
    procedure Paint; override;
  public
    property MsgList : TMsgList read GetMsgList;
  end;

  TMsgList = class (TCustomPanel)
  private
    FList : TStringList;
    FMsgScroll : TScrollBar;
    FMsgScreen : TMsgScreen;
    procedure set_ScrollPosition;
    procedure on_Resize(Sender:TObject);
    procedure on_ScrollChanged(Sender:TObject);
  private
    FTabWidth: integer;
    FMargins: TRect;
    FImageList: TImageList;
    function GetCount: integer;
    procedure SetTabWidth(const Value: integer);
    procedure SetMargins(const Value: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Msg:string);
    procedure AddMsgList(List:TStringList);
    procedure Delete(Index:integer);
  published
    property Align;
    property Alignment;
    property Anchors;
    property Constraints;
    property Visible;

    // Todo : Left, Top, Right, Bottom Margin 주기
    property Margins : TRect read FMargins write SetMargins;
    property TabWidth : integer read FTabWidth write SetTabWidth;
    property ImageList : TImageList read FImageList write FImageList;
    property Count : integer read GetCount;
  end;
  
procedure Register;

implementation

uses
  RyuGraphics;

procedure Register;
begin
  RegisterComponents('RyuLib', [TMsgList]);
end;

{ TMsgList }

procedure TMsgList.Add(Msg: string);
begin
  FList.Add(Msg);

  FMsgScroll.Max := FList.Count;
  set_ScrollPosition;

  FMsgScreen.Paint;
end;

procedure TMsgList.AddMsgList(List: TStringList);
var
  Loop : Integer;
begin
  for Loop := 0 to List.Count-1 do FList.Add(List[Loop]);

  FMsgScroll.Max := FList.Count;
  set_ScrollPosition;

  FMsgScreen.Paint;
end;

procedure TMsgList.Clear;
begin
  FList.Clear;
  
  FMsgScroll.Max := 0;
  FMsgScroll.Position := 0;
end;

constructor TMsgList.Create(AOwner: TComponent);
begin
  inherited;

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FTabWidth := 16;
  FMargins.Left := 3;
  FMargins.Top := 3;
  FMargins.Right := 3;
  FMargins.Bottom := 3;

  OnResize := on_Resize;

  FList := TStringList.Create;

  FMsgScreen := TMsgScreen.Create(Self);
  FMsgScreen.Align := alClient;
  FMsgScreen.Parent := Self;

  FMsgScroll := TScrollBar.Create(Self);
  FMsgScroll.Kind := sbVertical;
  FMsgScroll.Align := alRight;
  FMsgScroll.Parent := Self;
  FMsgScroll.Max := 0;
  FMsgScroll.OnChange := on_ScrollChanged;
end;

procedure TMsgList.Delete(Index: integer);
begin
  FList.Delete(Index);

  FMsgScroll.Max := FList.Count;

  FMsgScreen.Paint;
end;

destructor TMsgList.Destroy;
begin
  FList.Free;
  FMsgScroll.Free;
  FMsgScreen.Free;

  inherited;
end;

procedure TMsgList.on_Resize(Sender: TObject);
begin
  FMsgScreen.Paint;
end;

procedure TMsgList.on_ScrollChanged(Sender: TObject);
begin  
  FMsgScreen.Paint;
end;

function TMsgList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TMsgList.SetMargins(const Value: TRect);
begin
  FMargins := Value;

  FMsgScreen.Paint;
end;

procedure TMsgList.SetTabWidth(const Value: integer);
begin
  FTabWidth := Value;

  FMsgScreen.Paint;
end;

procedure TMsgList.set_ScrollPosition;
var
  sLine, sPart : string;
  Loop, iPosition, iSegment, iLeft, iHeight : Integer;
begin
  iHeight := Margins.Top;
  iSegment := 0;

  for Loop := FList.Count-1 downto 0 do begin
    sLine := FList[Loop];
    iLeft := Margins.Left;

    while sLine <> '' do begin
      sPart := GetStringOfWidth(FMsgScreen.Canvas, FMsgScreen.Width-iLeft-Margins.Right, sLine);

      iLeft := Margins.Left + TabWidth;
      iHeight := iHeight + FMsgScreen.Canvas.TextHeight(sPart + ' ');
    end;

    if iHeight >= (FMsgScreen.Height-Margins.Bottom) then Break;

    iSegment := iSegment + 1;      
  end;

  iPosition := FList.Count - iSegment;
  if iPosition < 0 then iPosition := 0;
  
  FMsgScroll.Position := iPosition;
end;

{ TMsgScreen }

procedure TMsgScreen.do_Draw;
var
  Bitmap : TBitmap;
  sLine, sPart : string;
  iImageIndex, iIndex, iTop, iLeft : Integer;
begin
  iIndex := MsgList.FMsgScroll.Position;
  if iIndex < 0 then iIndex := 0;

  iTop := MsgList.Margins.Top;
  while iIndex < MsgList.FMsgScroll.Max  do begin
    iLeft := MsgList.Margins.Left;

    sLine := MsgList.FList[iIndex];

    // Todo :
    iImageIndex := 0;
    if Pos('<쪽지>', sLine) > 0  then begin
      sLine := StringReplace(sLine, '<쪽지>', '', [rfReplaceAll]);

      if MsgList.ImageList <> nil then begin
        Bitmap := TBitmap.Create;
        try
          MsgList.ImageList.GetBitmap(iImageIndex, Bitmap);
          Canvas.Draw(2, iTop, Bitmap);
        finally
          Bitmap.Free;
        end;
      end;
    end;

    while sLine <> '' do begin
      sPart := GetStringOfWidth(Canvas, Width-iLeft-MsgList.Margins.Right, sLine);
      Canvas.TextOut(iLeft, iTop, sPart);

      iLeft := MsgList.Margins.Left + MsgList.TabWidth;

      // Todo : 행간 간격 설정 속성 추가 
      iTop := iTop + Canvas.TextHeight(sPart + ' ') + 3;
    end;

    if iTop > (Height-MsgList.Margins.Bottom) then Break;

    iIndex := iIndex + 1;
  end;
end;

function TMsgScreen.GetMsgList: TMsgList;
begin
  Result := Pointer(Owner);
end;

procedure TMsgScreen.Paint;
begin
  if Width*Height = 0 then Exit;

  if csDesigning in ComponentState then
     with inherited Canvas do begin
       Pen.Style :=   psDash;
       Brush.Style := bsClear;
       Rectangle(0, 0, Width, Height);
     end;

  // Todo : 폰트를 속성으로
  Canvas.Font.Size := 10;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  do_Draw;
end;

end.
