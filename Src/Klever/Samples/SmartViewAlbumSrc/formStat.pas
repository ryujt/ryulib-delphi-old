unit formStat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, rkView, rmkUtil, StdCtrls, ExtCtrls;

type
  PStrItem = ^TStrItem;
  TStrItem = record str1, str2: String end;
  TfrmStat = class(TForm)
    viewStat: TrkView;
    pnlBottom: TPanel;
    imgBottom: TImage;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure viewStatListPaint(Sender: TObject; Canvas: TCanvas; Cell: TRect;
      IdxA, Idx: Integer; State: TsvItemState; Columns: array of Integer);
  private
    { Private declarations }
    Items: TList;
    procedure AddL(s1, s2: string);
    procedure GetStat(All: Boolean);
    procedure ItemPaintBasic(Canvas: TCanvas; R: TRect; State: TsvItemState);
  public
    { Public declarations }
  end;

var
  frmStat: TfrmStat;

implementation

uses
  Main;

{$R *.dfm}

function BytesToStr(const i64Size: Int64): string;
const
  i64GB = 1024 * 1024 * 1024;
  i64MB = 1024 * 1024;
  i64KB = 1024;
begin
  if i64Size div i64GB > 0 then
    Result := Format('%.1f GB', [i64Size / i64GB])
  else if i64Size div i64MB > 0 then
    Result := Format('%.2f MB', [i64Size / i64MB])
  else if i64Size div i64KB > 0 then
    Result := Format('%.0f kB', [i64Size / i64KB])
  else Result := IntToStr(i64Size) + ' byte';
end;

procedure TfrmStat.ItemPaintBasic(Canvas: TCanvas; R: TRect; State: TsvItemState);
var
  C: TColor;
begin
  Canvas.Brush.Style := bsClear;
  if (State = svSelected) or (State = svHot) then begin
    if (viewStat.Focused) and (State = svSelected) then begin
      Canvas.Pen.Color:= frmMain.cSelected;
      WinGradient(Canvas.Handle, R, cGSelectedStart, cGSelectedEnd);
    end else
    if (State = svHot) then begin
      Canvas.Pen.Color:= frmMain.cHot;
      WinGradient(Canvas.Handle, R, cGHotStart, cGHotEnd);
    end else begin
      Canvas.Pen.Color:= frmMain.cDisabled;
      WinGradient(Canvas.Handle, R, cGDisabledStart, cGDisabledEnd);
    end;
    Canvas.Rectangle(R);
    if (viewStat.Focused) then C:= frmMain.cShadeSelect else C:= frmMain.cShadeDisabled;
    Canvas.Pen.Color:= C;
    Canvas.MoveTo(R.Left + 1, R.Top + 2);
    Canvas.LineTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Pen.Style:= psSolid;
    Canvas.Pixels[R.Left, R.Top]:= C;
    Canvas.Pixels[R.Left, R.Bottom - 1]:= C;
    Canvas.Pixels[R.Right - 1, R.Top]:= C;
    Canvas.Pixels[R.Right - 1, R.Bottom -1]:= C;
  end;
end;

procedure TfrmStat.viewStatListPaint(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState;
  Columns: array of Integer);
var
  R, T: TRect;
  i: Integer;
  Txt: string;
  Item: PStrItem;
begin
  Item:= Items.Items[idx];
  R:= Cell;
  if ((IdxA and 1) = 1) then begin
    Canvas.Pen.Style:= psSolid;
    Canvas.Pen.Color:= Canvas.Brush.Color;
    Canvas.Brush.Color:= frmMain.cLineHighLight;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(Cell);
  end;
  ItemPaintBasic(Canvas, Cell, State);
  if High(Columns) > 0 then begin
    R.Top:= R.Top;
    for i := Low(Columns) to High(Columns) do begin
      R.Right:= R.Left + Columns[i];
      T:= R;
      case i of
        0  : begin
            Canvas.Font.Color:= $00905422;
            T.Left:= T.Left + 3;
            txt:= Item.str1;
            DrawText(Canvas.Handle, PChar(txt), Length(txt), T,
              DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_LEFT or DT_VCENTER);
          end;
        1  : begin
              Canvas.Font.Color:= clBlack;
              T.Left:= T.Left + 3;
              txt:= Item.str2;
              DrawText(Canvas.Handle, PChar(txt), Length(txt), T,
                DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_LEFT or DT_VCENTER);
          end;
      else
      end;
      R.Left:= R.Right;
    end;
  end;
end;

procedure TfrmStat.AddL(s1, s2: string);
var
  NewItem: PStrItem;
begin
  New(NewItem);
  NewItem.str1:= s1;
  Newitem.str2:= s2;
  viewStat.Items.Add(Items.Add(NewItem));
end;

procedure TfrmStat.GetStat(All: Boolean);
var
  MinFn, MaxFn: string;
  i, j, k, l, n, z, MinFs, MaxFs: integer;
  Entry: PThumbData;
begin
  viewStat.BeginUpdate;
  k:= frmMain.Items.Count - 1;
  AddL('Total number of pictures', IntToStr(k + 1));
  j:= 0;
  l:= 0;
  if k > 0 then
    MinFs:= MaxInt
  else
    MinFs:= 0;
  MaxFs:= 0;
  for i:= 0 to k do
  begin
    z:= i;
    Entry:= PThumbData(frmMain.Items[z]);
    if Entry.Image <> nil then j:= j + (Entry.Image as TMemoryStream).Size;
    n:= Entry.Size;
    l:= l + n;
    if n < MinFs then begin
      MinFs:= n;
      MinFn:= Entry.Name;
    end;
    if n > MaxFs then begin
      MaxFs:= n;
      MaxFn:= Entry.Name;
    end;
  end;
  AddL('Total picture files size', BytesToStr(l));
  AddL('', '');
  AddL('Average picture file size', BytesToStr(l div (k - 1)));
  AddL('Smallest picture file', MinFn);
  AddL('Size of smallest file', BytesToStr(MinFs));
  AddL('Largest picture file', MaxFn);
  AddL('Size of largest file', BytesToStr(MaxFs));
  AddL('', '');
  AddL('Total thumbnails size', BytesToStr(j));
  AddL('Average thumbnail size', BytesToStr(j div (k - 1)));
  AddL('', '');
  AddL('Keywords', IntToStr(frmMain.KeyWords.Count));
  AddL('Paths', IntToStr(frmMain.Paths.Count));
  AddL('', '');
  AddL('Cache count', IntToStr(ThumbsPool.Count));
  AddL('Cache size', BytesToStr(PoolSize));
  AddL('Average cache size', BytesToStr(PoolSize div ThumbsPool.Count));
  viewStat.EndUpdate;
end;

procedure TfrmStat.FormCreate(Sender: TObject);
begin
  Items:= TList.Create;
  viewStat.UseAsList:= True;
end;

procedure TfrmStat.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i:= Items.Count - 1 downto 0 do Dispose(Items[i]);
  Items.Free;
end;

procedure TfrmStat.FormShow(Sender: TObject);
begin
  frmMain.ShadeGradient(imgBottom, $00F8F8F8, $00E0E0E0, False);
  GetStat(True);
end;

end.
