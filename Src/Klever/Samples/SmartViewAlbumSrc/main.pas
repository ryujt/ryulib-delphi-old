unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, rkIntegerList, Jpeg, ImageEnIO, ImageEnView,
  hyieutils, hyiedefs, Math, ComCtrls, StdCtrls, ShellAPI,
  SHFolder, ImgList, Menus, AppEvnts, IniFiles, rkVistaProBar,
  rkSupFileExt, rkGlassButton, rkVistaPanel, rkSmartView, XPMan;

const
  AppName = 'Mini Photo Album';
  AppVersion = '0.1';
  AppCompany = 'rmKlever';
  IniFileName = '\FileSettings.ini';
  Noname = 'Untitled.bpa';
  CellText: Cardinal = DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX;

type
  PThumbItem = ^TThumbItem;

  TThumbItem = record
    Idx: Integer;
    Size: Integer;
    Age: TDateTime;
    Scale: Integer;
    Bmp: TBitmap;
  end;

  PThumbData = ^TThumbData;

  TThumbData = record
    Name: string;
    Caption: string;
    Note: string;
    InfoID: SmallInt;
    PathID: SmallInt;
    CustID: SmallInt;
    FileID: SmallInt;
    AuthorID: SmallInt;
    FilmID: SmallInt;
    PlaceID: SmallInt;
    MediaID: SmallInt;
    Modified: TDateTime;
    IWidth, IHeight: Word;
    Size: Integer;
    ThumbWidth: Word;
    ThumbHeight: Word;
    Data: Byte;
    SimSort: Integer;
    SimData: Byte;
    BitDepth: Byte;
    Selected, Checked, GotThumb, Grey: Boolean;
    Image: TObject;
    KeyWords: TIntList;
    ColorDom: TRGB24;
    ColorAvg: TRGB24;
    Col1, Col2, Col3: TRGB24;
    // Metrics: array[0..344] of Byte;
  end;

  TfrmMain = class(TForm)
    pnlMain: TPanel;
    svMain: TrkSmartView;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    editView: TEdit;
    appMain: TApplicationEvents;
    timerSearch: TTimer;
    pnlTop: TPanel;
    imgGrd: TImage;
    labAlbum: TLabel;
    editTitle: TEdit;
    pnlBottom: TPanel;
    imgBottom: TImage;
    labInfo: TLabel;
    Panel1: TPanel;
    tbSize: TTrackBar;
    pnlThumbs: TPanel;
    pnlInfo: TPanel;
    pnlImgInfo: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    labAddTags: TLabel;
    imgInfoSplit: TImage;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    labFName: TLabel;
    labFDate: TLabel;
    labFSize: TLabel;
    labISize: TLabel;
    labCamera: TLabel;
    labAuthor: TLabel;
    labExp: TLabel;
    labBlender: TLabel;
    labISO: TLabel;
    imgRating: TImage;
    Label3: TLabel;
    labCap: TLabel;
    editInfo: TEdit;
    svKeyWords: TrkSmartView;
    editKeywords: TEdit;
    proOpen: TVistaProBar;
    Image1: TImage;
    fxSupport: TrkSupFileExt;
    popMain: TPopupMenu;
    popGroup: TPopupMenu;
    popSort: TPopupMenu;
    popViewThumbs: TPopupMenu;
    popClose: TMenuItem;
    popOpen: TMenuItem;
    popSave: TMenuItem;
    N1: TMenuItem;
    popAdd: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    grpNone: TMenuItem;
    grpPath: TMenuItem;
    grpRating: TMenuItem;
    grpOrientation: TMenuItem;
    grpHue: TMenuItem;
    grpMatch: TMenuItem;
    popInfoPnl: TMenuItem;
    N3: TMenuItem;
    viewThumbnails: TMenuItem;
    viewModified: TMenuItem;
    viewFileSize: TMenuItem;
    viewImageSize: TMenuItem;
    viewText: TMenuItem;
    viewFileName: TMenuItem;
    viewDetails: TMenuItem;
    sortAuto: TMenuItem;
    N4: TMenuItem;
    sortDate: TMenuItem;
    sortSize: TMenuItem;
    sortPSize: TMenuItem;
    sortRating: TMenuItem;
    sortText: TMenuItem;
    sortName: TMenuItem;
    sortPath: TMenuItem;
    sortType: TMenuItem;
    sortHue: TMenuItem;
    sortMatch: TMenuItem;
    N5: TMenuItem;
    sortAsc: TMenuItem;
    sortDes: TMenuItem;
    viewRating: TMenuItem;
    btnShow: TrkGlassButton;
    btnStyle: TrkGlassButton;
    rkVistaPanel1: TrkVistaPanel;
    btnMain: TrkGlassButton;
    btnGroup: TrkGlassButton;
    btnSort: TrkGlassButton;
    btnView: TrkGlassButton;
    editFilter: TEdit;
    Image2: TImage;
    Panel2: TPanel;
    ilGUImain: TImageList;
    XPManifest1: TXPManifest;
    procedure ShowStat;
    procedure MakeHueNew;
    procedure MakeHue;
    procedure CalcThumbColors;
    procedure GenCellColors;
    procedure MakeFileList(All: Boolean; Files: TStrings);
    procedure GetFilesOfPath(Idx: Integer; Files: TStrings);
    procedure UpdateStatus;
    function ThumbBmp(Idx: Integer): TBitmap;
    procedure FilterView(f: String);
    procedure ClearThumbsPool;
    procedure ClearAlbum;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure FormCreate(Sender: TObject);
    procedure MakeThumbNail(src: TIEBitmap; dst: TBitmap; Sharpen: Boolean);
    function ThumbImage(Filename: string): TBitmap;
    procedure DeleteImages(DelFiles: Boolean);
    procedure AddImages(Filelist: TStringlist);
    procedure GetImages;
    procedure DoSort(Idx: Integer);
    procedure btnAddImagesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetThumbSize(Value: Integer; UpdateTrackbar: Boolean);
    procedure SetThumbStyle(Value: Byte);
    procedure ItemPaintBasic(Canvas: TCanvas; R: TRect; State: TsvItemState);
    procedure ThumbsCellPaintStyle1(Sender: TObject; Canvas: TCanvas;
      Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);
    procedure ThumbsCellPaintStyle2(Sender: TObject; Canvas: TCanvas;
      Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);
    procedure svMainDividerPaint(Sender: TObject; Canvas: TCanvas; Cell: TRect;
      Group: PSmartGroup; State: TsvItemState);
    procedure FormResize(Sender: TObject);
    procedure OpenAlbum(name: string);
    procedure SaveAlbum(name: string);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure svMainSelecting(Sender: TObject; Count: Integer);
    procedure btnStyleClick(Sender: TObject);
    procedure tbSizeChange(Sender: TObject);
    procedure DoPreview(dlg: Boolean);
    procedure btnSlideshowClick(Sender: TObject);
    procedure SetGrouping(Idx: Integer);
    procedure grpNoneClick(Sender: TObject);
    procedure svMainDblClick(Sender: TObject);
    procedure svMainCellHit(Sender: TObject; Canvas: TCanvas;
      Idx, x, y: Integer; var Selected: Boolean);
    procedure svMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure editViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editViewKeyPress(Sender: TObject; var Key: Char);
    procedure editViewExit(Sender: TObject);
    procedure appMainDeactivate(Sender: TObject);
    procedure UpdateInfoPnl;
    procedure labAlbumClick(Sender: TObject);
    procedure editTitleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editTitleExit(Sender: TObject);
    procedure editTitleKeyPress(Sender: TObject; var Key: Char);
    procedure labAlbumMouseEnter(Sender: TObject);
    procedure labAlbumMouseLeave(Sender: TObject);
    procedure Image1MouseEnter(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
    procedure mruAlbumClick(Sender: TObject; const Filename: string);
    procedure editFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure timerSearchTimer(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure sortNameClick(Sender: TObject);
    procedure subSaveClick(Sender: TObject);
    procedure btnDelImagesClick(Sender: TObject);
    procedure viewThumbnailsClick(Sender: TObject);
    procedure popSortPopup(Sender: TObject);
    procedure svMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure labCapClick(Sender: TObject);
    procedure editInfoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure labAddTagsClick(Sender: TObject);
    function AddKeyword(Keyword: String): Integer;
    procedure editKeywordsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure svKeyWordsListPaint(Sender: TObject; Canvas: TCanvas;
      Cell: TRect; IdxA, Idx: Integer; State: TsvItemState;
      Columns: array of Integer);
    procedure svKeyWordsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure popInfoPnlClick(Sender: TObject);
    procedure popViewThumbsPopup(Sender: TObject);
    procedure imgGrdMouseEnter(Sender: TObject);
    procedure imgGrdMouseLeave(Sender: TObject);
    procedure imgGrdClick(Sender: TObject);
    procedure svMainDividerHit(Sender: TObject; Canvas: TCanvas;
      Grp, x: Integer; var Selected: Boolean);
  private
    { Private declarations }
    AppTitle: String;
    AlbumName: String;
    AlbumPath: String;
    AlbumVersion: Byte;
    AlbumModified: Boolean;
    AlbumText: String;
    IniFile: TIniFile;
    IniName: String;
    // Colors
    CellBkgColor: TColor;
    CellBrdColor: array [Boolean, Boolean] of TColor;
    CellTxtColor: array [Boolean, Boolean] of TColor;
    CellShdColor: array [Boolean, Boolean, 0 .. 4] of TColor;
    CellJPEG: TJpegImage;
    CellDetail: Boolean;
    CellStyle: Byte;
    CellScale: Byte;
    SimpleScale: Byte;
    DetailScale: Byte;
    CellDateFormat: string;
    procedure FindFiles(FilesList: TStringlist; StartDir: string);
    procedure SetTitleVisible(Value: Boolean);
  public
    { Public declarations }
    ThumbSizeW: Word;
    ThumbSizeH: Word;
    Items: TList;
    UndoList: TList;
    RedoList: TList;
    Authors: TStringlist;
    Paths: TStringlist;
    KeyWords: TStringlist;
    FileInfo: TStringlist;
    Location: TStringlist;
    MediaID: TStringlist;
    FilmNr: TStringlist;
    ShowMusic: String;
    GroupedOn: Integer;
    SortedOn: Integer;
    // Colors part 2
    cGSelectedStart, cGSelectedEnd, cGHotStart, cGHotEnd, cGDisabledStart,
      cGDisabledEnd, cGHeaderStart, cGHeaderEnd, cGHeaderHotStart,
      cGHeaderHotEnd, cGHeaderSelStart, cGHeaderSelEnd, cHot, cSelected,
      cDisabled, cBackground, cLineHighLight: TColor;
    cShadeSelect: TColor;
    cShadeDisabled: TColor;
    procedure GetSelection(s: TStrings);
    procedure ShowImgInfo;
    procedure ShadeGradient(Img: TImage; Back, Color: TColor; Reverse: Boolean);
  end;

var
  frmMain: TfrmMain;
  ThumbIO: TImageEnIO;
  WI, HI, TW, TH: Integer;
  Success: Boolean;
  AutoSort: Byte;
  GrpSortKind: Integer;
  GrpSortAsc: Boolean;
  GrpColors: Array [1 .. 8] of TColor;
  DblClicked: Boolean;
  HotStars, HSX, HSY: Integer;
  CellTextOff: Integer;
  CellEditIdx: Integer;
  EditIdx, EditId: Integer;
  Rows: Integer;
  ThumbsPool: TList;
  PoolSize, MaxPool: Integer;
  txtFind: String;
  txtCount: Integer;
  vGrp, vIdx, vCol: Integer;
  Color1, Color2: TRGB24;
  InfoPnlIdx: Integer;

implementation

uses
  ColorUtils, rmkUtil, formPro, formStat, BrowseForFolderU,
  formPreview, formImginfo;
{$R *.dfm}

function CalcThumbSize(w, h, TW, TH: Integer): TPoint;
begin
  Result.x := 0;
  Result.y := 0;
  if (w < TW) and (h < TH) then
  begin
    Result.x := w;
    Result.y := h;
  end
  else if (w = 0) or (h = 0) then
    Exit
  else
  begin
    if w > h then
    begin
      if w < TW then
        TW := w;
      Result.x := TW;
      Result.y := Trunc(TW * h / w);
      if Result.y > TH then
      begin
        Result.y := TH;
        Result.x := Trunc(TH * w / h);
      end;
    end
    else
    begin
      if h < TH then
        TH := h;
      Result.y := TH;
      Result.x := Trunc(TH * w / h);
      if Result.x > TW then
      begin
        Result.x := TW;
        Result.y := Trunc(TW * h / w);
      end;
    end;
  end;
end;

function DynBackCol(Bmp: TBitmap): TColor;
var
  R, g, b: Cardinal;
  p, o, t, x, y: Integer;
begin
  R := 0;
  g := 0;
  b := 0;
  p := Integer(Bmp.Scanline[0]);
  o := Integer(Bmp.Scanline[1]) - p;
  for y := 0 to Bmp.Height - 1 do
  begin
    for x := 0 to Bmp.Width - 1 do
    begin
      R := R + PRGB(p + x * 3).R;
      g := g + PRGB(p + x * 3).g;
      b := b + PRGB(p + x * 3).b;
    end;
    p := p + o;
  end;
  t := Bmp.Width * Bmp.Height;
  if R > 0 then
    R := Trunc(R / t);
  if g > 0 then
    g := Trunc(g / t);
  if b > 0 then
    b := Trunc(b / t);
  Result := R or (g shl 8) or (b shl 16);
end;

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
  else
    Result := IntToStr(i64Size) + ' byte';
end;

function ToMixCase(InString: string): string;
var
  I: Integer;
begin
  Result := LowerCase(InString);
  Result[1] := UpCase(Result[1]);
  for I := 1 to Length(InString) - 1 do
  begin
    if (Result[I] = ' ') or (Result[I] = '''') or (Result[I] = '"') or
      (Result[I] = '-') or (Result[I] = '.') or (Result[I] = '(') or
      (Result[I] = '\') or (Result[I] = '_') or (Result[I] = '&') then
      Result[I + 1] := UpCase(Result[I + 1]);
  end;
end;

procedure TfrmMain.ShadeGradient(Img: TImage; Back, Color: TColor; Reverse: Boolean);
type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 0] of TRGB24;
var
  X, Y, w, h, j, w1: Integer;
  Row: PRGBArray;
  slMain, slSize: Integer;
  R, G, B, a: Byte;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Canvas.Brush.Color:= Back;
  w := Img.Width;
  h := Img.Height;
  bmp.Width := w;
  bmp.Height := h;
  Color := ColorToRGB(Color);
  R := Byte(Color);
  G := Byte(Color shr 8);
  B := Byte(Color shr 16);
  w1 := w - 1;
  w := (w shr 1) + (w and 1);
  slMain := Integer(bmp.ScanLine[0]);
  slSize := Integer(bmp.ScanLine[1]) - slMain;
  for Y := 0 to h - 1 do
  begin
    if Reverse then
      j := 255 - MulDiv(255, Y, h)
    else
      j := MulDiv(255, Y + 1, h);
    for X := 0 to w - 1 do
    begin
      a := 255 - MulDiv(j, X, w);
      Row := PRGBArray(slMain);
      Row[X].R := a * (Row[X].R - R) shr 8 + R;
      Row[X].G := a * (Row[X].G - G) shr 8 + G;
      Row[X].B := a * (Row[X].B - B) shr 8 + B;
      if (X < (w1 - X)) then
      begin
        Row[w1 - X].R := a * (Row[w1 - X].R - R) shr 8 + R;
        Row[w1 - X].G := a * (Row[w1 - X].G - G) shr 8 + G;
        Row[w1 - X].B := a * (Row[w1 - X].B - B) shr 8 + B;
      end;
    end;
    slMain := slMain + slSize;
  end;
  Img.Picture.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure TfrmMain.ShowImgInfo;
begin
  begin
    if (svMain.Selection.Count > 0) then
    begin
      frmInfo := TfrmInfo.Create(self);
      try
        frmInfo.ShowModal;
      finally
        frmInfo.Free;
      end;
    end;
  end
end;

procedure TfrmMain.ShowStat;
begin
  frmStat := TfrmStat.Create(nil);
  try
    frmStat.ShowModal;
  finally
    frmStat.Free;
  end;
end;

procedure TfrmMain.MakeHueNew;
type
  PRGB = ^TRGB;

  TRGB = record
    b, g, R: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 0] of TRGB;
var
  Thumb: PThumbData;
  HueJPEG: TJpegImage;
  Bmp: TBitmap;
  I, n, v, x, y: Integer;
  c: TColor;
  Row: PRGBArray;
  slMain, slSize, slPtr: Integer;
  HueCnt: Array [0 .. 5] of Integer;
  HueTab: Array [0 .. 5, 0 .. 3] of Integer;
  R, g, b, h, s, l: Byte;
begin
  HueJPEG := TJpegImage.Create;
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  for I := 0 to Items.Count - 1 do
  begin
    Thumb := Items[I];
    if Thumb.Image <> nil then
    begin
      // Get image
      TMemoryStream(Thumb.Image).Position := 0;
      HueJPEG.LoadFromStream(TMemoryStream(Thumb.Image));
      HueJPEG.Scale := jsEighth;
      Bmp.Width := HueJPEG.Width;
      Bmp.Height := HueJPEG.Height;
      Bmp.Canvas.Draw(0, 0, HueJPEG);
      // Clear counters
      for n := 0 to 5 do
      begin
        HueCnt[n] := 0;
        HueTab[n, 0] := 0;
        HueTab[n, 1] := 0;
        HueTab[n, 2] := 0;
        HueTab[n, 3] := 0;
      end;
      // Init scanline accsess
      slMain := Integer(Bmp.Scanline[0]);
      slSize := Integer(Bmp.Scanline[1]) - slMain;
      slPtr := slMain;
      for y := 0 to Bmp.Height - 1 do
      begin
        Row := PRGBArray(slPtr);
        for x := 0 to Bmp.Width - 1 do
        begin
          R := Row[x].R;
          g := Row[x].g;
          b := Row[x].b;
          RGBtoHSL(R, g, b, h, s, l);
          // l:= 128;
          n := h;
          if n > 0 then
            n := (n div 60)
          else
            n := 0;
          Inc(HueCnt[n]);
          if (s > 127) then
            Inc(HueCnt[n]);
          if (l > 64) and (l < 192) then
            Inc(HueCnt[n]);
          Inc(HueTab[n, 0]);
          Inc(HueTab[n, 1], h);
          Inc(HueTab[n, 2], s);
          Inc(HueTab[n, 3], l);
        end;
        slPtr := slPtr + slSize;
      end;
      x := 0;
      v := 0;
      for n := 0 to 5 do
      begin
        if HueCnt[n] > x then
        begin
          x := HueCnt[n];
          v := n;
        end;
      end;
      if HueTab[v, 1] > 0 then
        h := HueTab[v, 1] div HueTab[v, 0];
      if HueTab[v, 2] > 0 then
        s := HueTab[v, 2] div HueTab[v, 0];
      if HueTab[v, 3] > 0 then
        l := HueTab[v, 3] div HueTab[v, 0];
      HSLtoRGB(h, s, l, R, g, b);
      c := (b shl 16) + (g shl 8) + R;
      Thumb.SimData := h;
      Thumb.SimSort := (h shl 24) + (s shl 8) + l;
    end;
  end;
  Bmp.Free;
  HueJPEG.Free;
end;

procedure TfrmMain.MakeHue;
var
  Bmp: TBitmap;
  I: Integer;
  Thumb: PThumbData;
  c: TColor;
  ThumbIO: TImageEnView;
  h, s, l: Byte;
  // rgb: TRGB24;
begin
  ThumbIO := TImageEnView.Create(nil);
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  for I := 0 to Items.Count - 1 do
  begin
    Thumb := Items[I];
    if Thumb.Image <> nil then
    begin
      TMemoryStream(Thumb.Image).Position := 0;
      CellJPEG.LoadFromStream(TMemoryStream(Thumb.Image));
      Bmp.Width := CellJPEG.Width;
      Bmp.Height := CellJPEG.Height;
      Bmp.Canvas.Draw(0, 0, CellJPEG);
      c := DynBackCol(Bmp);
      RGBtoHSL(Byte(c), Byte(c shr 8), Byte(c shr 16), h, s, l);
      Thumb.SimData := h;
      Thumb.SimSort := (h shl 24) + (s shl 8) + l;
      // rgb.r:= Byte(c);
      // rgb.g:= Byte(c shr 8);
      // rgb.b:= Byte(c shr 16);
      // Thumb.ColorAvg:= rgb;
    end;
  end;
  Bmp.Free;
  ThumbIO.Free;
end;

function ColorDistance(c1, c2: TRGB24): Double;
var
  rmean, R, g, b: Integer;
begin
  rmean := c1.R + c2.R shr 1;
  R := c1.R - c2.R;
  g := c1.g - c2.g;
  b := c1.b - c2.b;
  Result := sqrt((((512 + rmean) * R * R) shr 8) + 4 * g * g +
      (((767 - rmean) * b * b) shr 8));
end;

// RMK Get thumb colors...
procedure TfrmMain.CalcThumbColors;
var
  Bmp: TBitmap;
  I, j, col, swap, Idx: Integer;
  f: hyiedefs.TRGB;
  Thumb: PThumbData;
  ThumbIO: TImageEnView;
  R, g, b: Cardinal;
  p, o, t, x, y: Integer;
  Colors, CCount: Array [0 .. 9] of Integer;
  dummy, temp: TRGB24;
begin
  ThumbIO := TImageEnView.Create(nil);
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  for Idx := 0 to Items.Count - 1 do
  begin
    Thumb := Items[Idx];
    if Thumb.Image <> nil then
    begin
      TMemoryStream(Thumb.Image).Position := 0;
      CellJPEG.LoadFromStream(TMemoryStream(Thumb.Image));
      Bmp.Width := CellJPEG.Width;
      Bmp.Height := CellJPEG.Height;
      Bmp.Canvas.Draw(0, 0, CellJPEG);
      ThumbIO.Proc.AttachedBitmap := Bmp;
      ThumbIO.Proc.GetDominantColor(f);
      Thumb.ColorDom.R := f.R;
      Thumb.ColorDom.g := f.g;
      Thumb.ColorDom.b := f.b;
      R := 0;
      g := 0;
      b := 0;
      p := Integer(Bmp.Scanline[0]);
      o := Integer(Bmp.Scanline[1]) - p;
      for y := 0 to Bmp.Height - 1 do
      begin
        for x := 0 to Bmp.Width - 1 do
        begin
          R := R + PRGB(p + x * 3).R;
          g := g + PRGB(p + x * 3).g;
          b := b + PRGB(p + x * 3).b;
        end;
        p := p + o;
      end;
      t := Bmp.Width * Bmp.Height;
      if R > 0 then
        R := Trunc(R / t);
      if g > 0 then
        g := Trunc(g / t);
      if b > 0 then
        b := Trunc(b / t);
      Thumb.ColorAvg.R := b;
      Thumb.ColorAvg.g := g;
      Thumb.ColorAvg.b := R;
    end;

    ThumbIO.Proc.ConvertTo(10, ieOrdered);
    // if idx = 0 then ThumbIO.IO.SaveToFileJpeg('d:\Testimg.jpg');

    for I := 0 to 9 do
    begin
      Colors[I] := -1;
      CCount[I] := 0;
    end;
    p := Integer(Bmp.Scanline[0]);
    o := Integer(Bmp.Scanline[1]) - p;
    for y := 0 to Bmp.Height - 1 do
    begin
      for x := 0 to Bmp.Width - 1 do
      begin
        col := (PRGB(p + x * 3).R shl 16) + (PRGB(p + x * 3).g shl 8) + PRGB
          (p + x * 3).b;
        I := 9;
        // repeat
        // i:= i + 1;
        // until (Col = Colors[i]) or (i = 9);
        while (col <> Colors[I]) and (I > 0) and (Colors[I] <> -1) do
          Dec(I);
        // Caption:= IntToStr(i);
        Colors[I] := col;
        CCount[I] := CCount[I] + 1;
      end;
      p := p + o;
    end;

    // if idx = 0 then for i:= 0 to 9 do memo1.Lines.Add(IntToStr(Colors[i]) + ' Count: ' + IntToStr(CCount[i]));
    // Gnome sort (look it up if you care)
    I := 1;
    j := 2;
    while (I < Length(Colors)) do
      if (CCount[I - 1] - CCount[I] <= 0) then
      begin
        I := j;
        Inc(j);
      end
      else
      begin
        // Swap hue values
        p := CCount[I];
        CCount[I] := CCount[I - 1];
        CCount[I - 1] := p;

        // Swap colors
        swap := Colors[I];
        Colors[I] := Colors[I - 1];
        Colors[I - 1] := swap;

        Dec(I);
        if (I = 0) then
          I := 1;
      end;
    Thumb.Col1.R := Byte(Colors[9]);
    Thumb.Col1.g := Byte(Colors[9] shr 8);
    Thumb.Col1.b := Byte(Colors[9] shr 16);

    temp.R := Byte(Colors[9]);
    temp.g := Byte(Colors[9] shr 8);
    temp.b := Byte(Colors[9] shr 16);
    for I := 0 to 1 do
    begin
      dummy.R := Byte(Colors[8 - I]);
      dummy.g := Byte(Colors[8 - I] shr 8);
      dummy.b := Byte(Colors[8 - I] shr 16);
      j := 8 - I;
      while (ColorDistance(temp, dummy) < 75) and (j > 0) do
        j := j - 1;
      if I = 0 then
        while (ColorDistance(Thumb.Col1, dummy) < 75) and (j > 0) do
          j := j - 1;
      if I = 1 then
        while (ColorDistance(Thumb.Col2, dummy) < 75) and (j > 0) do
          j := j - 1;
      temp.R := Byte(Colors[j]);
      temp.g := Byte(Colors[j] shr 8);
      temp.b := Byte(Colors[j] shr 16);
      if I = 0 then
        Thumb.Col2 := temp;
      if I = 1 then
        Thumb.Col3 := temp;
    end;
  end;

  Bmp.Free;
  ThumbIO.Free;
end;

{
procedure TfrmMain.VistaGradient(const Img: TImage; const ARect: TRect;
  const c1, c2: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);
type
  PRGB = ^TRGB;

  TRGB = record
    b, g, R: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 0] of TRGB;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3: Integer;
  x, y, w, h, j: Integer;
  I, w1: Integer;
  Row: PRGBArray;
  c: TRGB;
  slMain, slSize, slPtr: Integer;
  Color: Integer;
  QCol: Array of TRGB;
  g: Byte;
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  Bmp.Width := (ARect.Right - ARect.Left) - 1;
  Bmp.Height := (ARect.Bottom - ARect.Top) - 1;
  h := Bmp.Height;
  w := Bmp.Width;
  Color := ColorToRGB(c3);
  rc1 := Byte(Color);
  gc1 := Byte(Color shr 8);
  bc1 := Byte(Color shr 16);
  Color := ColorToRGB(c4);
  rc2 := Byte(Color);
  gc2 := Byte(Color shr 8);
  bc2 := Byte(Color shr 16);
  SetLength(QCol, h);
  for I := 0 to h - 1 do
  begin
    if Reverse then
    begin
      QCol[I].R := Byte(rc1 + (((rc2 - rc1) * I) div h));
      QCol[I].g := Byte(gc1 + (((gc2 - gc1) * I) div h));
      QCol[I].b := Byte(bc1 + (((bc2 - bc1) * I) div h));
    end
    else
    begin
      QCol[I].R := Byte(rc2 + (((rc1 - rc2) * I) div h));
      QCol[I].g := Byte(gc2 + (((gc1 - gc2) * I) div h));
      QCol[I].b := Byte(bc2 + (((bc1 - bc2) * I) div h));
    end;
  end;
  w1 := w - 1;
  if Center then
    w := (w shr 1) + (w and 1);
  slMain := Integer(Bmp.Scanline[0]); // Init scanline accsess
  slSize := Integer(Bmp.Scanline[1]) - slMain;
  for x := 0 to w - 1 do
  begin // Paint gradient
    j := (255 - c1) + MulDiv(c1, x, w);
    slPtr := slMain;
    for y := 0 to h - 1 do
    begin
      Row := PRGBArray(slPtr);
      Row[x].R := j * (QCol[y].R - rc1) shr 8 + rc1;
      Row[x].g := j * (QCol[y].g - gc1) shr 8 + gc1;
      Row[x].b := j * (QCol[y].b - bc1) shr 8 + bc1;
      if (Center) and (x < (w1 - x)) then
      begin
        Row[w1 - x].R := Row[x].R;
        Row[w1 - x].g := Row[x].g;
        Row[w1 - x].b := Row[x].b;
      end;
      slPtr := slPtr + slSize;
    end;
  end;
  QCol := nil;
  Img.Picture.Bitmap.Assign(Bmp);
  Bmp.Free;
end;
}

procedure TfrmMain.GenCellColors;
var
  c: TColor;
begin
  cHot := $00FDDE99;
  cGHotStart := $00FDF5E6;
  cGHotEnd := $00FDFBF6;
  cSelected := $00FDCE99;
  cGSelectedStart := $00FCEFC4;
  cGSelectedEnd := $00FDF8EF;
  cShadeSelect := $00F8F3EA;
  cDisabled := $00D9D9D9;
  cGDisabledStart := $00EAE9E9;
  cGDisabledEnd := $00FCFBFB;
  cShadeDisabled := $00F6F5F5;
  cGHeaderStart := $00F9F9F9;
  cGHeaderEnd := $00FEFEFE;
  cGHeaderHotStart := $00FFEDBD;
  cGHeaderHotEnd := $00FFF7E3;
  cGHeaderSelStart := $00FCEABA;
  cGHeaderSelEnd := $00FCF4E0;
  cBackground := clWindow; ;
  cLineHighLight := $00FEFBF6;

  CellBkgColor := clWindow;
  CellTxtColor[False, False] := clGray;
  CellTxtColor[False, True] := clSilver;
  CellTxtColor[True, False] := clBlack;
  CellTxtColor[True, True] := clHighlightText;
  CellBrdColor[False, False] := cDisabled; // clSilver;
  CellBrdColor[False, True] := cDisabled; // clGray;
  CellBrdColor[True, False] := $00B5B5B5;
  CellBrdColor[True, True] := cSelected; // clHighlight;
  c := CellBkgColor;
  CellShdColor[False, False, 0] := c;
  CellShdColor[False, False, 1] := Blend(c, clBlack, 60);
  CellShdColor[False, False, 2] := Blend(c, clBlack, 75);
  CellShdColor[False, False, 3] := Blend(c, clBlack, 85);
  CellShdColor[False, False, 4] := Blend(c, clBlack, 93);
  c := Blend(CellBrdColor[False, True], CellBkgColor, 85);
  CellShdColor[False, True, 0] := c;
  CellShdColor[False, True, 1] := Blend(c, clBlack, 60);
  CellShdColor[False, True, 2] := Blend(c, clBlack, 75);
  CellShdColor[False, True, 3] := Blend(c, clBlack, 85);
  CellShdColor[False, True, 4] := Blend(c, clBlack, 93);
  c := CellBkgColor;
  CellShdColor[True, False, 0] := c;
  CellShdColor[True, False, 1] := Blend(c, clBlack, 60); // 60 $00e0e0e0;
  CellShdColor[True, False, 2] := Blend(c, clBlack, 75); // 75 $00eaeaea;
  CellShdColor[True, False, 3] := Blend(c, clBlack, 85); // 85 $00f2f2f2;
  CellShdColor[True, False, 4] := Blend(c, clBlack, 93); // 93 $00fafafa;
  c := Blend(CellBrdColor[True, True], CellBkgColor, 85);
  // C:= clRed;//$00FDCE99;
  CellShdColor[True, True, 0] := c;
  CellShdColor[True, True, 1] := $00E4D0C0; // Blend(C, clWhite, 80);
  CellShdColor[True, True, 2] := $00EED9CA; // Blend(C, clWhite, 88);
  CellShdColor[True, True, 3] := $00F4E0D2; // Blend(C, clWhite, 94);
  CellShdColor[True, True, 4] := $00FFE8DA; // Blend(C, clWhite, 99);
end;

procedure TfrmMain.MakeFileList(All: Boolean; Files: TStrings);
var
  I: Integer;
  p: PThumbData;
begin
  Files.Clear;
  if All then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      p := Items[I];
      Files.Add(Paths[p.PathID] + p.Name);
    end;
  end
  else
  begin
    for I := 0 to svMain.Selection.Count - 1 do
    begin
      p := Items[svMain.Selection[I]];
      Files.Add(Paths[p.PathID] + p.Name);
    end;
  end;
end;

procedure TfrmMain.GetFilesOfPath(Idx: Integer; Files: TStrings);
var
  I: Integer;
  p: PThumbData;
begin
  Files.Clear;
  for I := 0 to Items.Count - 1 do
  begin
    p := Items[I];
    if (p.PathID = Idx) then
      Files.Add(Paths[p.PathID] + p.Name);
  end;
end;

procedure TfrmMain.UpdateStatus;
var
  I: Integer;
  n: Int64;
  item: PThumbData;
  s: String;
begin
  n := 0;
  for I := 0 to svMain.Selection.Count - 1 do
  begin
    item := Items[svMain.Selection[I]];
    n := n + item.Size;
  end;
  s := IntToStr(svMain.Items.Count) + ' items';
  if svMain.Selection.Count > 0 then
    s := s + ', ' + IntToStr(svMain.Selection.Count)
      + ' selected (' + BytesToStr(n) + ')';
  labInfo.Caption := s;
  UpdateInfoPnl;
end;

procedure TfrmMain.viewThumbnailsClick(Sender: TObject);
var
  I: Integer;
begin
  I := (Sender as TMenuItem).Tag;
  CellDetail := (I = 7);
  viewThumbnails.Checked := (I = 0);
  viewModified.Checked := (I = 1);
  viewFileSize.Checked := (I = 2);
  viewImageSize.Checked := (I = 3);
  viewRating.Checked := (I = 4);
  viewText.Checked := (I = 5);
  viewFileName.Checked := (I = 6);
  SetThumbStyle(I);
  svMain.Invalidate;
end;

procedure TfrmMain.appMainDeactivate(Sender: TObject);
begin
  if editView.Visible then
    editView.Visible := False;
  svMain.IsEditing := editView.Visible;
  svMain.Invalidate;
end;

procedure TfrmMain.btnAddImagesClick(Sender: TObject);
begin
  GetImages;
  svMain.SetFocus;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  ClearAlbum;
end;

procedure TfrmMain.btnDelImagesClick(Sender: TObject);
begin
  DeleteImages(False);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    // mruAlbum.Add(dlgOPen.FileName);
    OpenAlbum(dlgOpen.Filename);
    //CalcThumbColors;
    svMain.SetFocus;
  end;
end;

procedure TfrmMain.btnSaveAsClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    SaveAlbum(dlgSave.Filename);
    // mruAlbum.Add(dlgSave.FileName);
  end;
end;

procedure TfrmMain.DoPreview(dlg: Boolean);
var
  n: Integer;
  s: String;
  p: PThumbData;
  w, h, x, y: Integer;
  pt: TPoint;
begin
  n := svMain.Selection.Count;
  if n = 0 then
    Exit;
  p := Items[svMain.Selection[0]];
  s := Paths[p.PathID] + p.Name;
  if FileExists(s) or (n > 1) then
  begin

    // Open preview form and show images...

    // p := ClickedItem(s);
    // if p = nil then
    // Exit;
    // if FileExists(s) then
    // begin
    frmView := TfrmView.Create(nil);
    try
      frmView.Multiple := True;
      frmView.SlideShow := False; // n > 1;
      frmView.ShowProgress := p.Size > 512000;
      frmView.ColorMode := True;
      frmView.ColorDom := False;
      frmView.Zoom := 2; // FZoom;
      frmView.Dialog := False; // UseDialog;
      frmView.iePreview.IO.ParamsFromFile(s);
      x := frmView.iePreview.IO.Params.Width;
      y := frmView.iePreview.IO.Params.Height;
      if (x > Screen.DesktopWidth) or (y > Screen.DesktopHeight) then
      begin
        w := MulDiv(Screen.DesktopWidth, 90, 100);
        h := MulDiv(Screen.DesktopHeight, 90, 100);
        pt := CalcThumbSize(x, y, w, h);
        w := pt.x;
        h := pt.y;
      end
      else
      begin
        w := 256;
        h := 256;
        if (x > w) and (y > h) then
        begin
          w := x;
          h := y;
        end;
      end;
      frmView.Width := w + 2;
      frmView.Height := h + 2;
      frmView.UseTransitions := True;
      frmView.ShowModal;
      // FZoom:= frmView.Zoom;
    finally
      frmView.Free;
    end;
  end;

end;

procedure TfrmMain.editFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
  begin
    timerSearch.Enabled := False;
    editFilter.Text := '';
    FilterView('');
    svMain.SetFocus;
  end
  else
  begin
    txtCount := 4;
    timerSearch.Enabled := True;
  end;
end;

procedure TfrmMain.editInfoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  p: PThumbData;
  done: Boolean;
  txt: String;
begin
  // done:= False;
  if Key = VK_Escape then
    done := True
  else if Key = VK_Return then
  begin
    done := True;
    txt := editInfo.Text;
    p := Items[InfoPnlIdx];
    if editInfo.Tag = 2 then
    begin
      if p.Note <> txt then
      begin
        p.Note := txt;
        svMain.Invalidate;
      end;
    end
    else if editInfo.Tag = 1 then
    begin
      if p.Name <> txt then
      begin
        p.Name := txt;
        svMain.Invalidate;
      end;
    end;
  end
  else
    done := False;
  editInfo.Visible := not done;
  // if done then svMain.SetFocus;
  if done then
    UpdateInfoPnl
end;

function TfrmMain.AddKeyword(Keyword: String): Integer;
begin
  Result := KeyWords.IndexOf(Keyword);
  if Result = -1 then
    Result := KeyWords.Add(Keyword);
end;

procedure TfrmMain.editKeywordsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  p: PThumbData;
  done: Boolean;
  txt: String;
  str: TStringlist;
  I: Integer;
begin
  // done:= False;
  if Key = VK_Escape then
    done := True
  else if Key = VK_Return then
  begin
    done := True;
    p := Items[InfoPnlIdx];
    str := TStringlist.Create;
    txt := editKeywords.Text;
    str.Text := Trim(UpperCase(StringReplace(txt, ',', #13, [rfReplaceAll])));
    for I := 0 to str.Count - 1 do
    begin
      str[I] := Trim(str[I]);
      p.KeyWords.Add(AddKeyword(str[I]));
    end;
  end
  else
    done := False;
  editKeywords.Visible := not done;
  if done then
    UpdateInfoPnl;
end;

procedure TfrmMain.editTitleExit(Sender: TObject);
begin
  editTitle.Visible := False;
end;

procedure TfrmMain.editTitleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  done: Boolean;
begin
  // done:= False;
  if Key = VK_Escape then
    done := True
  else if Key = VK_Return then
  begin
    done := True;
    AlbumText := editTitle.Text;
    labAlbum.Caption := AlbumText;
  end
  else
    done := False;
  editTitle.Visible := not done;
end;

procedure TfrmMain.editTitleKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TfrmMain.editViewExit(Sender: TObject);
begin
  editView.Visible := False;
  svMain.IsEditing := False;
  svMain.SetFocus;
  svMain.Invalidate;
end;

procedure TfrmMain.editViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  p: PThumbData;
  done: Boolean;
  txt: String;
begin
  // done:= False;
  if Key = VK_Escape then
    done := True
  else if Key = VK_Return then
  begin
    done := True;
    txt := editView.Text;
    p := Items[EditIdx];
    if CellDetail then
    begin
      if (EditId = 6) then
      begin
        if p.Note <> txt then
        begin
          p.Note := txt;
          svMain.Invalidate;
        end;
      end
      else if (EditId = 1) then
      begin
        if p.Name <> txt then
        begin
          p.Name := txt;
          svMain.Invalidate;
        end;
      end;
    end
    else
    begin
      if (EditId = 1) then
      begin
        if p.Note <> txt then
        begin
          p.Note := txt;
          svMain.Invalidate;
        end;
      end
      else if (EditId = 2) then
      begin
        if p.Name <> txt then
        begin
          p.Name := txt;
          svMain.Invalidate;
        end;
      end;
    end;
  end
  else
    done := False;
  editView.Visible := not done;
  svMain.IsEditing := editView.Visible;
  if done then
    svMain.SetFocus;
end;

procedure TfrmMain.editViewKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnSlideshowClick(Sender: TObject);
begin
  DoPreview(True);
end;

procedure TfrmMain.btnStyleClick(Sender: TObject);
begin
  if CellDetail then
  begin
    CellDetail := False;
    SetThumbStyle(CellStyle);
    btnStyle.Tag := 0;
    btnView.Tag := 0;
  end
  else
  begin
    CellDetail := True;
    SetThumbStyle(7);
    btnStyle.Tag := 1;
    btnView.Tag := 0;
  end;
  svMain.IsEditing := False;
  svMain.SetFocus;
  svMain.CalcView(False);
  svMain.UpdateView;
  svMain.Invalidate;
end;

// Fast resamplers and we need it :)
procedure MakeThumbNailBMP(src, dst: TBitmap);
var
  x, y, ix, iy, w, h, dx, dy: Integer;
  x1, x2, x3: Integer;
  RowDest, RowSource, RowSourceStart: Integer;
  iRatio: Integer;
  Ratio: Single;
  iRed, iGrn, iBlu: Integer;
  pt: PRGB24;
  iSrc, iDst: Integer;
  lutW, lutH: array of Integer;
begin
  if (src.Width <= dst.Width) and (src.Height <= dst.Height) then
  begin
    dst.Assign(src);
    Exit;
  end;
  w := dst.Width;
  h := dst.Height;
  Ratio := 1 / (w / src.Width);
  SetLength(lutW, w);
  x1 := 0;
  x2 := Trunc(Ratio);
  for x := 0 to w - 1 do
  begin
    lutW[x] := x2 - x1;
    x1 := x2;
    x2 := Trunc((x + 2) * Ratio);
  end;
  Ratio := 1 / (h / src.Height);
  SetLength(lutH, h);
  x1 := 0;
  x2 := Trunc(Ratio);
  for x := 0 to h - 1 do
  begin
    lutH[x] := x2 - x1;
    x1 := x2;
    x2 := Trunc((x + 2) * Ratio);
  end;
  RowDest := Integer(dst.Scanline[0]);
  RowSourceStart := Integer(src.Scanline[0]);
  RowSource := RowSourceStart;
  iDst := ((w * 24 + 31) and not 31) shr 3;
  iSrc := ((src.Width * 24 + 31) and not 31) shr 3;
  for y := 0 to h - 1 do
  begin
    dy := lutH[y];
    x1 := 0;
    x3 := 0;
    for x := 0 to w - 1 do
    begin
      dx := lutW[x];
      iRed := 0;
      iGrn := 0;
      iBlu := 0;
      RowSource := RowSourceStart;
      for iy := 1 to dy do
      begin
        pt := PRGB24(RowSource + x1);
        for ix := 1 to dx do
        begin
          iRed := iRed + pt.R;
          iGrn := iGrn + pt.g;
          iBlu := iBlu + pt.b;
          Inc(pt);
        end;
        RowSource := RowSource - iSrc;
      end;
      iRatio := $00FFFFFF div (dx * dy);
      pt := PRGB24(RowDest + x3);
      pt.R := (iRed * iRatio) shr 24;
      pt.g := (iGrn * iRatio) shr 24;
      pt.b := (iBlu * iRatio) shr 24;
      x1 := x1 + 3 * dx;
      Inc(x3, 3);
    end;
    RowDest := RowDest - iDst;
    RowSourceStart := RowSource;
  end;
end;

procedure BiReSample(src, dest: TBitmap); // RMK
type
  TRGB24 = record
    b, g, R: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 0] of TRGB24;
var
  x, y, px, py: Integer;
  I, x1, x2, z, z2, iz2: Integer;
  w1, w2, w3, w4: Integer;
  Ratio: Integer;
  sDst, sDstOff: Integer;
  sScanLine: Array [0 .. 255] of PRGBArray;
  Src1, Src2: PRGBArray;
  c, c1, c2: TRGB24;
  y1, y2, y3, x3, iRed, iGrn, iBlu: Integer;
  p1, p2, p3, p4, p5: PRGB24;
begin
  // ScanLine buffer for Source
  sDst := Integer(src.Scanline[0]);
  sDstOff := Integer(src.Scanline[1]) - Integer(sDst);
  for I := 0 to src.Height - 1 do
  begin
    sScanLine[I] := PRGBArray(sDst);
    sDst := sDst + sDstOff;
  end;
  // ScanLine for Destiantion
  sDst := Integer(dest.Scanline[0]);
  y1 := sDst; // only for sharpening...
  sDstOff := Integer(dest.Scanline[1]) - sDst;
  // Ratio is same for width and height
  Ratio := ((src.Width - 1) shl 15) div dest.Width;
  py := 0;
  for y := 0 to dest.Height - 1 do
  begin
    Src1 := sScanLine[py shr 15];
    if py shr 15 < src.Height - 1 then
      Src2 := sScanLine[py shr 15 + 1]
    else
      Src2 := Src1;
    z2 := py and $7FFF;
    iz2 := $8000 - z2;
    px := 0;
    for x := 0 to dest.Width - 1 do
    begin
      x1 := px shr 15;
      x2 := x1 + 1;
      c1 := Src1[x1];
      c2 := Src2[x1];
      z := px and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      c.R := (c1.R * w1 + Src1[x2].R * w2 + c2.R * w3 + Src2[x2].R * w4) shr 15;
      c.g := (c1.g * w1 + Src1[x2].g * w2 + c2.g * w3 + Src2[x2].g * w4) shr 15;
      c.b := (c1.b * w1 + Src2[x2].b * w2 + c2.b * w3 + Src2[x2].b * w4) shr 15;
      // Set destination pixel
      PRGBArray(sDst)[x] := c;
      Inc(px, Ratio);
    end;
    sDst := sDst + sDstOff;
    Inc(py, Ratio);
  end;

  // Sharpening...
  y2 := y1 + sDstOff;
  y3 := y2 + sDstOff;
  for y := 1 to dest.Height - 2 do
  begin
    for x := 0 to dest.Width - 3 do
    begin
      x1 := x * 3;
      x2 := x1 + 3;
      x3 := x1 + 6;

      p1 := PRGB24(y1 + x1);
      p2 := PRGB24(y1 + x3);
      p3 := PRGB24(y2 + x2);
      p4 := PRGB24(y3 + x1);
      p5 := PRGB24(y3 + x3);

      // -15 -11                       // -17 - 13
      iRed := (p1.R + p2.R + (p3.R * -15) + p4.R + p5.R) div -11;
      iGrn := (p1.g + p2.g + (p3.g * -15) + p4.g + p5.g) div -11;
      iBlu := (p1.b + p2.b + (p3.b * -15) + p4.b + p5.b) div -11;

      if iRed < 0 then
        iRed := 0
      else if iRed > 255 then
        iRed := 255;
      if iGrn < 0 then
        iGrn := 0
      else if iGrn > 255 then
        iGrn := 255;
      if iBlu < 0 then
        iBlu := 0
      else if iBlu > 255 then
        iBlu := 255;

      PRGB24(y2 + x2).R := iRed;
      PRGB24(y2 + x2).g := iGrn;
      PRGB24(y2 + x2).b := iBlu;
    end;
    Inc(y1, sDstOff);
    Inc(y2, sDstOff);
    Inc(y3, sDstOff);
  end;
end;

function TfrmMain.ThumbBmp(Idx: Integer): TBitmap;
var
  i, n, sf: Integer;
  p: PThumbItem;
  t: PThumbData;
  Bmp, tmp: TBitmap;
  pt: TPoint;
  Oldest: TDateTime;
begin
  Result := nil;
  // if we have thumbs, see if we can find it...
  if ThumbsPool.Count > 0 then
  begin
    i := ThumbsPool.Count - 1;
    while (i >= 0) and (PThumbItem(ThumbsPool[i]).Idx <> Idx) do
      i := i - 1;
    if i <> -1 then
    begin
      p := ThumbsPool[i];
      if (p.Idx = Idx) then
      begin
        if (p.Scale = CellScale) then
        begin
          p.Age := Now;
          Result := p.Bmp
        end
        else
        begin
          PoolSize := PoolSize - p.Size;
          p.Bmp.Free;
          Dispose(p);
          ThumbsPool.Delete(i);
        end;
      end;
    end;
  end;
  // if we dont have a thumb, make one...
  if Result = nil then
  begin
    t := Items[Idx];
    if t.Image <> nil then
    begin
      TMemoryStream(t.Image).Position := 0;
      sf := Trunc(Min(t.ThumbWidth / CellScale, t.ThumbHeight / CellScale));
      if sf < 0 then
        sf := 0;
      case sf of
        0 .. 1:
          CellJpeg.Scale := jsFullSize;
        2 .. 3:
          CellJpeg.Scale := jsHalf;
        4 .. 7:
          CellJpeg.Scale := jsQuarter;
      else
        CellJpeg.Scale := jsEighth;
      end;
      CellJpeg.LoadFromStream(TMemoryStream(t.Image));
      Bmp := TBitmap.Create;
      Bmp.PixelFormat := pf24bit;
      pt := CalcThumbSize(CellJpeg.width, CellJpeg.Height, CellScale,
        CellScale);
      if pt.X <> CellJpeg.width then
      begin
        tmp := TBitmap.Create;
        tmp.PixelFormat := pf24bit;
        tmp.width := CellJpeg.width;
        tmp.Height := CellJpeg.Height;
        tmp.Canvas.Draw(0, 0, CellJpeg);
        Bmp.width := pt.X;
        Bmp.Height := pt.Y;
        if (Bmp.width > 4) and (Bmp.Height > 4) then
          {MakeThumbNailBMP(tmp, Bmp)} BiResample(tmp, Bmp)
        else
          Bmp.Canvas.StretchDraw(Rect(0, 0, pt.X, pt.Y), tmp);
        tmp.Free;
      end
      else
      begin
        Bmp.width := CellJpeg.width;
        Bmp.Height := CellJpeg.Height;
        Bmp.Canvas.Draw(0, 0, CellJpeg);
      end;
      New(p);
      p.Idx := Idx;
      p.Size := (Bmp.width * Bmp.Height) * 3;
      p.Age := Now;
      p.Scale := CellScale;
      p.Bmp := Bmp;
      ThumbsPool.Add(p);
      PoolSize := PoolSize + p.Size;
      Result := p.Bmp;
      // Purge thumbs if needed
      while (PoolSize > MaxPool) and (ThumbsPool.Count > 0) do
      begin
        Oldest := Now;
        n := 0;
        for i := 0 to ThumbsPool.Count - 1 do
        begin
          p := ThumbsPool[i];
          if p.Age <= Oldest then
          begin
            Oldest := p.Age;
            n := i;
          end;
        end;
        Assert(n >= 0);
        p := ThumbsPool[n];
        PoolSize := PoolSize - p.Size;
        p.Bmp.Free;
        Dispose(p);
        ThumbsPool.Delete(n);
      end;
    end;
  end;
end;

procedure TfrmMain.FilterView(f: String);
var
  I, j: Integer;
  p: PThumbData;
  s0: String;
begin
  svMain.Items.Clear;
  if f = '' then
    for I := 0 to Items.Count - 1 do
      svMain.Items.Add(I)
    else
    begin
      f := UpperCase(f);
      for I := 0 to Items.Count - 1 do
      begin
        p := Items[I];
        s0 := p.Name;
        if p.Note <> '' then
          s0 := s0 + ';' + p.Note;
        if p.KeyWords.Count > 0 then
          for j := 0 to p.KeyWords.Count - 1 do
            s0 := s0 + ';' + KeyWords[p.KeyWords[j]];
        s0 := UpperCase(s0);
        if Pos(f, s0) <> 0 then
          svMain.Items.Add(I);
      end;
    end;
  if svMain.Grouped then
    SetGrouping(GroupedOn);
  DoSort(GrpSortKind);
  svMain.CalcView(False);
  svMain.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.ClearThumbsPool;
var
  I: Integer;
  Thumb: PThumbItem;
begin
  for I := ThumbsPool.Count - 1 downto 0 do
  begin
    Thumb := ThumbsPool[I];
    if Thumb.Bmp <> nil then
      Thumb.Bmp.Free;
    Dispose(Thumb);
  end;
  ThumbsPool.Clear;
  PoolSize := 0;
end;

procedure TfrmMain.ClearAlbum;
var
  I: Integer;
  item: PThumbData;
  s: AnsiString;
begin
  AlbumName := Noname;
  AlbumPath := '';
  AlbumText := '';
  s := AlbumName;
  if Pos('.', s) <> 0 then
    s := Copy(s, 1, Pos('.', s) - 1);
  Caption := s + ' - ' + AppTitle;
  Forms.Application.Title := Caption;
  if AlbumText = '' then
    labAlbum.Caption := '< Add Title >'
  else
    labAlbum.Caption := AlbumText;
  svMain.ClearSmartGroups;
  for I := Items.Count - 1 downto 0 do
  begin
    item := Items[I];
    if item.Size <> 0 then
      item.Image.Free;
    if item.KeyWords <> nil then
      item.KeyWords.Free;
    Dispose(item);
  end;
  Items.Clear;
  ClearThumbsPool;
  Paths.Clear;
  FileInfo.Clear;
  Authors.Clear;
  KeyWords.Clear;
  Location.Clear;
  MediaID.Clear;
  FilmNr.Clear;
  svMain.Clear;
  svMain.Grouped := False;
  UpdateStatus;
end;

function GetSpecialFolderPath(folder: Integer): String;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0 .. MAX_PATH] of Char;
begin
  if SUCCEEDED(SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @path[0])) then
    Result := path
  else
    Result := '';
end;

procedure TfrmMain.SaveSettings;
begin
  IniFile := TIniFile.Create(IniName);
  try
    IniFile.WriteInteger('Placement', 'Top', Top);
    IniFile.WriteInteger('Placement', 'Left', Left);
    IniFile.WriteInteger('Placement', 'Width', Width);
    IniFile.WriteInteger('Placement', 'Height', Height);
    // s:= '"' + StringReplace(MruAlbum.Items.Text, #13#10, '$0D$0A', [rfReplaceAll]) + '"';
    // IniFile.WriteString('MruAlbum', 'Items', s);//'"' + s + '"');
    IniFile.WriteInteger('Thumbs', 'Size', CellScale);
    IniFile.WriteInteger('Thumbs', 'Style', CellStyle);
    IniFile.WriteBool('Thumbs', 'Detail', CellDetail);
    IniFile.WriteInteger('Thumbs', 'SimpleScale', SimpleScale);
    IniFile.WriteInteger('Thumbs', 'DetailScale', DetailScale);
    IniFile.WriteString('Show', 'Music', ShowMusic);
    IniFile.WriteBool('Info', 'Panel', pnlInfo.Visible);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.LoadSettings;
var
  s: String;
begin
  IniFile := TIniFile.Create(IniName);
  try
    Top := IniFile.ReadInteger('Placement', 'Top', Top);
    Left := IniFile.ReadInteger('Placement', 'Left', Left);
    Width := IniFile.ReadInteger('Placement', 'Width', Width);
    Height := IniFile.ReadInteger('Placement', 'Height', Height);
    // s:= Inifile.ReadString('MruAlbum', 'Items', '');
    // MruAlbum.Items.Text:= StringReplace(s, '$0D$0A', #13#10, [rfReplaceAll]);
    CellScale := IniFile.ReadInteger('Thumbs', 'Size', 96);
    CellStyle := IniFile.ReadInteger('Thumbs', 'Style', 1);
    CellDetail := IniFile.ReadBool('Thumbs', 'Detail', False);
    SimpleScale := IniFile.ReadInteger('Thumbs', 'SimpleScale', 96);
    DetailScale := IniFile.ReadInteger('Thumbs', 'DetailScale', 96);
    s := IniFile.ReadString('Show', 'Music', '');
    pnlInfo.Visible := IniFile.ReadBool('Info', 'Panel', True);
  finally
    IniFile.Free;
  end;
  SetThumbSize(CellScale, True);
  if CellDetail then
    SetThumbStyle(7)
  else
    SetThumbStyle(CellStyle);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  path: String;
begin
  path := IncludeTrailingPathDelimiter(GetSpecialFolderPath(CSIDL_APPDATA));
  path := path + 'rmKlever\Album2009\1.0\';
  ForceDirectories(PChar(path));
  IniName := path + 'Settings.ini';
  Randomize;
  GenCellColors;
  GrpColors[1] := $00FEEEAE;
  GrpColors[2] := $0000EEDE;
  GrpColors[3] := $00DECE8E;
  GrpColors[4] := $0000CE00;
  GrpColors[5] := $00EEAEFE;
  GrpColors[6] := $00009EEE;
  GrpColors[7] := $00CE8EDE;
  GrpColors[8] := $00007E00;
  ThumbSizeW := 255;
  ThumbSizeH := 255;
  PoolSize := 0;
  MaxPool := Round(((Screen.Width * Screen.Height) * 3) * 1.5);
  Items := TList.Create;
  ThumbsPool := TList.Create;
  Paths := TStringlist.Create;
  FileInfo := TStringlist.Create;
  Authors := TStringlist.Create;
  KeyWords := TStringlist.Create;
  Location := TStringlist.Create;
  MediaID := TStringlist.Create;
  FilmNr := TStringlist.Create;
  AppTitle := 'Blaise photo album';
  HotStars := -1;
  svMain.CellSelect := False;
  CellJPEG := TJpegImage.Create;
  CellJPEG.Performance := jpBestSpeed;
  LoadSettings;
  svMain.ScrollMode := False;
  ShadeGradient(imgGrd, clWhite, clSilver, True);
  svKeyWords.UseAsList := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  CellJPEG.Free;
  ClearAlbum;
  Items.Free;
  ThumbsPool.Free;
  Paths.Free;
  FileInfo.Free;
  Authors.Free;
  KeyWords.Free;
  Location.Free;
  MediaID.Free;
  FilmNr.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  ShadeGradient(imgGrd, clWhite, clSilver, True);
  ShadeGradient(imgBottom, $00F8F8F8, $00E0E0E0, False);
  ShadeGradient(imgInfoSplit, clWhite, $00F0F0F0, True);
end;

procedure TfrmMain.MakeThumbNail(src: TIEBitmap; dst: TBitmap;
  Sharpen: Boolean);
var
  x, y: Integer;
  iRGB: TRGB24;
  s0, a0, incS, incA, alpha: Integer;
begin
  if src.PixelFormat <> ie24RGB then
    src.PixelFormat := ie24RGB;
  if dst.PixelFormat <> pf24Bit then
    dst.PixelFormat := pf24Bit;
  if src.HasAlphaChannel then
  begin
    s0 := Integer(src.Scanline[0]);
    incS := Integer(src.Scanline[1]) - s0;
    a0 := Integer(src.AlphaChannel.Scanline[0]);
    incA := Integer(src.AlphaChannel.Scanline[1]) - a0;
    for y := 0 to src.Height - 1 do
    begin
      for x := 0 to src.Width - 1 do
      begin
        alpha := 255 - PLine8(a0 + x)^;
        iRGB := PRGB24(s0 + x * 3)^;
        iRGB.R := (alpha * ($FF - iRGB.R)) shr 8 + iRGB.R;
        iRGB.g := (alpha * ($FF - iRGB.g)) shr 8 + iRGB.g;
        iRGB.b := (alpha * ($FF - iRGB.b)) shr 8 + iRGB.b;
        PRGB24(s0 + x * 3)^ := iRGB;
      end;
      Inc(s0, incS);
      Inc(a0, incA);
    end;
  end;
  if (src.Width <= dst.Width) and (src.Height <= dst.Height) then
  begin
    src.CopyToTBitmap(dst);
    Exit;
  end
  else
    src.RenderToTBitmapEx(dst, 0, 0, dst.Width, dst.Height, 0, 0, src.Width,
      src.Height, 255, rfLanczos3, ielNormal);
end;

procedure TfrmMain.mruAlbumClick(Sender: TObject; const Filename: string);
begin
  OpenAlbum(Filename);
end;

function TfrmMain.ThumbImage(Filename: string): TBitmap;
var
  sf: Integer;
  s, ext: string;
  tmp: TBitmap;
  tSize: TPoint;
  IEBitmap: TIEBitmap;
  newW, newH: Integer;
begin
  Result := nil;
  Success := False;
  tmp := nil;
  IEBitmap := TIEBitmap.Create;
  if ThumbIO <> nil then
  begin
    ThumbIO.AttachedIEBitmap := IEBitmap;
    s := Filename;
    if s[1] = '*' then
      Delete(s, 1, 1);
    if Filename <> '' then
    begin
      ThumbIO.Params.JPEG_Scale := ioJPEG_FULLSIZE;
      ext := LowerCase(ExtractFileExt(s));
      if (ext = '.jpg') or (ext = '.jpeg') then
      begin
        ThumbIO.ParamsFromFile(s);
        WI := ThumbIO.Params.JPEG_OriginalWidth;
        HI := ThumbIO.Params.JPEG_OriginalHeight;
        if WI < 1 then
          WI := 1;
        if HI < 1 then
          HI := 1;
        sf := Trunc(Min(WI / TW, HI / TH));
        if sf < 0 then
          sf := 0;
        case sf of
          0 .. 1:
            ThumbIO.Params.JPEG_Scale := ioJPEG_FULLSIZE;
          2 .. 3:
            ThumbIO.Params.JPEG_Scale := ioJPEG_HALF;
          4 .. 7:
            ThumbIO.Params.JPEG_Scale := ioJPEG_QUARTER;
        else
          ThumbIO.Params.JPEG_Scale := ioJPEG_EIGHTH;
        end;
        ThumbIO.LoadFromFile(s);
      end
      else
      begin
        ThumbIO.LoadFromFile(s);
        WI := IEBitmap.Width;
        HI := IEBitmap.Height;
      end;
      Success := not ThumbIO.Aborting;
    end;
    if Success then
    begin
      try
        tmp := TBitmap.Create;
        tSize := CalcThumbSize(IEBitmap.Width, IEBitmap.Height, TW, TH);
        newW := tSize.x;
        newH := tSize.y;
        if newW <= 0 then
          newW := 1;
        if newH <= 0 then
          newH := 1;
        tmp.PixelFormat := pf24Bit;
        tmp.Width := newW;
        tmp.Height := newH;
        MakeThumbNail(IEBitmap, tmp, False);
      except
        tmp.Free;
        raise ;
      end;
      Result := tmp;
    end;
    IEBitmap.Free;
  end;
end;

function DelSort(List: TIntList; Item1, Item2: Integer): Integer;
begin
  Result := Sign(Item1 - Item2);
end;

procedure TfrmMain.DeleteImages(DelFiles: Boolean);
var
  I: Integer;
  Idx: Integer;
  p: PThumbData;
begin
  svMain.Selection.CustomSort(DelSort);
  for I := svMain.Selection.Count - 1 downto 0 do
  begin
    Idx := svMain.Selection[I];
    if Idx < Items.Count then
    begin
      p := Items[Idx];
      if p.KeyWords <> nil then
        p.KeyWords.Free;
      if p.Image <> nil then
        p.Image.Free;
      Dispose(p);
      Items.Delete(Idx);
    end
    else
      Caption := IntToStr(Idx);
  end;
  svMain.Selection.Clear;
  ClearThumbsPool;
  FilterView(editFilter.Text);
end;

procedure TfrmMain.AddImages(Filelist: TStringlist);
var
  Entry: PThumbData;
  Dir: string;
  SR: TSearchRec;
  j, p, n: Integer;
  FName: string;
  Jpeg: TJpegImage;
  MS: TMemoryStream;
  ThumbBmp: TBitmap;
  SFI: TSHFileInfo;
  PV: Single;
  // UndoImages: TIntList;
  // UndoStream: TMemoryStream;
begin
  MS := nil;
  frmPro := TfrmPro.Create(frmMain);
  try
    frmPro.proDlg.Position := 0;
    frmPro.Show;
    frmMain.Enabled := False;
    try
      Forms.Application.ProcessMessages;
      PV := 100 / Filelist.Count;
      Jpeg := TJpegImage.Create;
      Jpeg.CompressionQuality := 80;
      TW := frmMain.ThumbSizeW;
      TH := frmMain.ThumbSizeH;
      ThumbIO := TImageEnIO.Create(nil);
      j := 0;
      while (j < Filelist.Count) and (not frmPro.Abort) do
      begin
        FName := Filelist[j];
        Dir := LowerCase(ExtractFilePath(FName));
        if FindFirst(FName, faAnyFile, SR) = 0 then
        begin
          New(Entry);
          Entry.Name := ExtractFileName(FName);
          n := Paths.IndexOf(Dir);
          if (n = -1) then
            Entry.PathID := Paths.Add(Dir)
          else
            Entry.PathID := n;
          SHGetFileInfo(PChar(FName), FILE_ATTRIBUTE_NORMAL, SFI,
            SizeOf(TSHFileInfo), SHGFI_TYPENAME or SHGFI_SYSICONINDEX);
          n := FileInfo.IndexOf(SFI.szTypeName);
          if (n = -1) then
            Entry.InfoID := FileInfo.Add(SFI.szTypeName)
          else
            Entry.InfoID := n;
          Entry.Size := SR.Size;
          Entry.Modified := FileDateToDateTime(SR.Time);
          Entry.IWidth := 0;
          Entry.IHeight := 0;
          Entry.ThumbWidth := 0;
          Entry.ThumbHeight := 0;
          Entry.Selected := False;
          Entry.GotThumb := False;
          Entry.Checked := False;
          Entry.Data := 0;
          ThumbBmp := ThumbImage(FName);
          if ThumbBmp <> nil then
          begin
            Jpeg.Assign(ThumbBmp);
            Jpeg.Compress;
            MS := TMemoryStream.Create;
            try
              Jpeg.SaveToStream(MS);
            except
              MS.Free;
              raise ;
            end;
            Entry.Image := MS;
            Entry.IWidth := WI;
            Entry.IHeight := HI;
            Entry.ThumbWidth := ThumbBmp.Width;
            Entry.ThumbHeight := ThumbBmp.Height;
            Entry.GotThumb := True;
            ThumbBmp.Free;
            n := Items.Add(Entry);
            if n <> -1 then
              svMain.Items.Add(n);
            AlbumModified := True;
            Entry.KeyWords := TIntList.Create;
          end
          else
            Dispose(Entry);
        end;
        FindClose(SR);
        p := Trunc(j * PV);
        if p <> frmPro.proDlg.Position then
          frmPro.proDlg.Position := p;
        if MS <> nil then
        begin
          MS.Position := 0;
          frmPro.ieThumb.IO.LoadFromStream(MS);
        end;
        frmPro.ieThumb.Update;
        Forms.Application.ProcessMessages;
        Inc(j);
      end;
      {
        UndoStream := TMemoryStream.Create;
        try
        if UndoImages.Count > 0 then
        begin
        UndoStream.Position := 0;
        UndoImages.SaveToStream(UndoStream);
        Thumbs.SaveUndo('Image(s) added', 1, UndoStream);
        end;
        except
        raise;
        end;
        UndoImages.Free;
        }
      Jpeg.Free;
      ThumbIO.Free;
    finally
      frmMain.Enabled := True;
    end;
  finally
    frmPro.Free;
  end;
  svMain.UpdateView;
  svMain.CalcView(True);
end;

// Recursive procedure to build a list of files
procedure TfrmMain.FindFiles(FilesList: TStringlist; StartDir: string);
var
  SR: TSearchRec;
  DirList: TStringlist;
  IsFound: Boolean;
  I: Integer;
begin
  if StartDir[Length(StartDir)] <> '\' then
    StartDir := StartDir + '\';
  IsFound := FindFirst(StartDir + '*.*', faAnyFile - faDirectory, SR) = 0;
  while IsFound do
  begin
    if fxSupport.ValidFile(SR.Name) then
      FilesList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);
  // Build a list of subdirectories
  DirList := TStringlist.Create;
  IsFound := FindFirst(StartDir + '*.*', faAnyFile, SR) = 0;
  while IsFound do
  begin
    if ((SR.Attr and faDirectory) <> 0) and (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);
  // Scan the list of subdirectories
  for I := 0 to DirList.Count - 1 do
    FindFiles(FilesList, DirList[I]);
  DirList.Free;
end;

procedure TfrmMain.GetImages;
var
  fl: TStringlist;
  Dir: String;
begin
  Dir := BrowseForFolder('Add images folder', '', False);
  if Dir = '' then
    Exit;
  fl := TStringlist.Create;
  FindFiles(fl, Dir);
  AddImages(fl);
  fl.Clear;
  fl.Free;
  UpdateStatus;
end;

procedure TfrmMain.GetSelection(s: TStrings);
var
  FName: string;
  item: PThumbData;
  Idx, Cnt, I: Integer;
  bool: Boolean;
begin
  Cnt := svMain.Selection.Count;
  if Cnt > 0 then
  begin
    s.Clear;
    for I := 0 to Cnt - 1 do
    begin
      Idx := svMain.Selection[I];
      item := Items[Idx];
      FName := Paths[item.PathID] + item.Name;
      s.Add(FName);
    end;
  end;
end;

function CompareColor(Item1, Item2: Integer): Integer;
var
  I, j: Byte;
begin
  I := Byte(Item1 Shr 16);
  j := Byte(Item2 shr 16);
  Result := Sign(I - j);
  if Result = 0 then
  begin
    I := Byte(Item1 Shr 8);
    j := Byte(Item2 shr 8);
  end;
  Result := Sign(I - j);
  if Result = 0 then
  begin
    I := Byte(Item1);
    j := Byte(Item2);
  end;
  Result := Sign(I - j);
end;

function CompareInt(Item1, Item2: Integer): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

function CompareColorMatch(i1, i2: PThumbData): Integer;
begin
  Result := Sign(ColorDistance(i1.Col1, Color1) - ColorDistance(i2.Col1,
      Color1));
  if Result = 0 then
    Result := Sign(ColorDistance(i1.Col2, Color1) - ColorDistance(i2.Col2,
        Color1));
  if Result = 0 then
    Result := Sign(ColorDistance(i1.Col3, Color1) - ColorDistance(i2.Col3,
        Color1));
end;

function CompareNatural(s1, s2: String): Integer; // by Roy M Klever

  function ExtractNr(n: Integer; var txt: String): Int64;
  begin
    while (n <= Length(txt)) and (txt[n] in ['0' .. '9']) do
      n := n + 1;
    Result := StrToInt64Def(Copy(txt, 1, (n - 1)), 0);
    Delete(txt, 1, (n - 1));
  end;

var
  b: Boolean;
begin
  Result := 0;
  s1 := LowerCase(s1);
  s2 := LowerCase(s2);
  if (s1 <> s2) and (s1 <> '') and (s2 <> '') then
  begin
    b := False;
    while (not b) do
    begin
      if ((s1[1] in ['0' .. '9']) and (s2[1] in ['0' .. '9'])) then
        Result := Sign(ExtractNr(1, s1) - ExtractNr(1, s2))
      else
        Result := Sign(Integer(s1[1]) - Integer(s2[1]));
      b := (Result <> 0) or (Min(Length(s1), Length(s2)) < 2);
      if not b then
      begin
        Delete(s1, 1, 1);
        Delete(s2, 1, 1);
      end;
    end;
  end;
end;

function GroupSort(List: TIntList; Item1, Item2: Integer): Integer;
var
  temp, Idx: Integer;
  Thumb1, Thumb2: PThumbData;
begin
  Result := 0;
  if (not GrpSortAsc) then
  begin
    temp := Item1;
    Item1 := Item2;
    Item2 := temp;
  end;
  Thumb1 := frmMain.Items[List[Item1]];
  Thumb2 := frmMain.Items[List[Item2]];
  if GrpSortKind = -1 then
    Idx := AutoSort
  else
    Idx := GrpSortKind;
  case Idx of
    0:
      Result := CompareNatural(Thumb1.Name, Thumb2.Name);
    1:
      Result := CompareInt(Thumb1.Size, Thumb2.Size);
    2:
      Result := Sign(Thumb1.Modified - Thumb2.Modified);
    3:
      Result := CompareColor(Thumb1.SimSort, Thumb1.SimSort);
    4:
      Result := CompareColorMatch(Thumb1, Thumb2);
    5:
      Result := CompareText(Thumb1.Note, Thumb2.Note);
    6:
      Result := Sign(Thumb1.Data - Thumb2.Data);
    7:
      Result := Sign((Thumb1.IWidth * Thumb1.IHeight) -
          (Thumb2.IWidth * Thumb2.IHeight));
    8:
      Result := CompareNatural(ExtractFileExt(Thumb1.Name),
        ExtractFileExt(Thumb2.Name));
    9:
      Result := CompareNatural(frmMain.Paths[Thumb1.PathID],
        frmMain.Paths[Thumb2.PathID]);
  end;
  if (Result = 0) and (Idx <> 0) then
    Result := CompareNatural(Thumb1.Name, Thumb2.Name);
  if (Result = 0) and (Idx = 0) then
    Result := CompareNatural(frmMain.Paths[Thumb1.PathID],
      frmMain.Paths[Thumb2.PathID]);
end;

procedure TfrmMain.SetGrouping(Idx: Integer);
var
  I, j, n, index: Integer;
  s: String;
  item: PThumbData;
  Group: PSmartGroup;
  c: TRGB24;
  f1, f2: Double;
begin
  svMain.ClearSmartGroups;
  GrpSortAsc := True;
  AutoSort := 0;
  case Idx of
    0:
      svMain.Grouped := False;
    1:
      begin
        for I := 0 to Paths.Count - 1 do
        begin
          s := Paths[I];
          Delete(s, Length(s), 1);
          svMain.AddSmartGroup(ToMixCase(s));
        end;
        for I := 0 to svMain.Items.Count - 1 do
        begin
          index := svMain.Items[I];
          item := PThumbData(Items[index]);
          n := item.PathID;
          Group := PSmartGroup(svMain.SmartGroups[n]);
          Group.Items.Add(index);
        end;
      end;
    2:
      begin
        svMain.AddSmartGroup('5 stars');
        svMain.AddSmartGroup('4 stars');
        svMain.AddSmartGroup('3 stars');
        svMain.AddSmartGroup('2 stars');
        svMain.AddSmartGroup('1 star');
        svMain.AddSmartGroup('Not rated');
        for I := 0 to svMain.Items.Count - 1 do
        begin
          index := svMain.Items[I];
          item := PThumbData(Items[index]);
          n := item.Data;
          if n = 0 then
            n := 5
          else
            n := 5 - n;
          Group := PSmartGroup(svMain.SmartGroups[n]);
          Group.Items.Add(index);
        end;
      end;
    3:
      begin
        svMain.AddSmartGroup('Horizontal');
        svMain.AddSmartGroup('Vertical');
        for I := 0 to Items.Count - 1 do
        begin
          index := svMain.Items[I];
          item := PThumbData(Items[index]);
          if item.IWidth > item.IHeight then
            n := 0
          else
            n := 1;
          Group := PSmartGroup(svMain.SmartGroups[n]);
          Group.Items.Add(index);
        end;
      end;
    4:
      begin // Hue or Color Tone
        // MakeHueNew;
        MakeHue;
        svMain.AddSmartGroup('Gray');
        svMain.AddSmartGroup('Red');
        svMain.AddSmartGroup('Yellow');
        svMain.AddSmartGroup('Green');
        svMain.AddSmartGroup('Cyan');
        svMain.AddSmartGroup('Blue');
        svMain.AddSmartGroup('Magenta');
        for I := 0 to svMain.Items.Count - 1 do
        begin
          index := svMain.Items[I];
          item := PThumbData(Items[index]);
          n := Round(item.SimData / 51);
          //j := Byte(item.SimSort Shr 8);
          // if ((item.SimData) <> 0) and (j <> 255) then
          // n := n + 1;
          n := n + 1;
          if n = 1 then
          begin
            j := Byte(item.SimSort Shr 8);
            if j = 0 then
              n := 0;
          end;

          {
            n:= 0;
            c.r:= 128; c.g:= 128; c.b:= 128;
            f1:= ColorDistance(item.ColorAvg, c);

            c.r:= 255; c.g:= 0; c.b:= 0;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f2:= f1;
            n:= 1;
            end;

            c.r:= 255; c.g:= 255; c.b:= 0;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f1:= f2;
            n:= 2;
            end;

            c.r:= 0; c.g:= 128; c.b:= 0;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f1:= f2;
            n:= 3;
            end;

            c.r:= 0; c.g:= 255; c.b:= 255;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f1:= f2;
            n:= 4;
            end;

            c.r:= 0; c.g:= 0; c.b:= 255;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f1:= f2;
            n:= 5;
            end;

            c.r:= 255; c.g:= 0; c.b:= 255;
            f2:= ColorDistance(item.ColorAvg, c);
            if f2 < f1 then begin
            f1:= f2;
            n:= 6;
            end;
            }

          Group := PSmartGroup(svMain.SmartGroups[n]);
          Group.Items.Add(index);
        end;
        AutoSort := 3;
      end;
    5:
      begin
        svMain.AddSmartGroup('Best match');
        svMain.AddSmartGroup('Good match');
        svMain.AddSmartGroup('Some match');
        svMain.AddSmartGroup('No match');
        for I := 0 to svMain.Items.Count - 1 do
        begin
          index := svMain.Items[I];
          item := PThumbData(Items[index]);
          f2 := ColorDistance(item.Col1, c);
          if f2 < 50 then
            n := 0
          else if f2 < 100 then
            n := 1
          else if f2 < 150 then
            n := 2
          else
            n := 3;
          Group := PSmartGroup(svMain.SmartGroups[n]);
          Group.Items.Add(index);
        end;
        AutoSort := 4;
      end;
  end;
  // Delete empty groups
  for I := svMain.SmartGroups.Count - 1 downto 0 do
  begin
    Group := PSmartGroup(svMain.SmartGroups[I]);
    Group.Expanded := True;
    if Group.Items.Count = 0 then
      svMain.DeleteSmartGroup(I);
  end;
  // Sort groups content if needed
  svMain.Grouped := (Idx <> 0);
  DoSort(GrpSortKind);
  GroupedOn := Idx;
end;

procedure TfrmMain.DoSort(Idx: Integer);
var
  I: Integer;
begin
  if (Idx = 99) or (Idx = 100) then
    GrpSortAsc := (Idx = 99)
  else
    GrpSortKind := Idx;
  if (Idx = -1) and (not svMain.Grouped) then
    AutoSort := 0;
  if svMain.Grouped then
    for I := 0 to svMain.SmartGroups.Count - 1 do
      PSmartGroup(svMain.SmartGroups[I]).Items.CustomSort(GroupSort)
    else
      svMain.Items.CustomSort(GroupSort);
  svMain.CalcView(False);
  svMain.Invalidate;
end;

procedure TfrmMain.grpNoneClick(Sender: TObject);
var
  I: Integer;
begin
  I := (Sender as TMenuItem).Tag;
  grpNone.Checked := (I = 0);
  grpPath.Checked := (I = 1);
  grpRating.Checked := (I = 2);
  grpOrientation.Checked := (I = 3);
  grpHue.Checked := (I = 4);
  grpMatch.Checked := (I = 5);
  SetGrouping(I);
  svMain.SetFocus;
end;

procedure TfrmMain.SetThumbSize(Value: Integer; UpdateTrackbar: Boolean);
var
  w, h, I: Integer;
begin
  if CellDetail then
    DetailScale := Value
  else
    SimpleScale := Value;
  case Value of
    32 .. 63:
      CellJPEG.Scale := jsQuarter;
    64 .. 127:
      CellJPEG.Scale := jsHalf;
    128 .. 255:
      CellJPEG.Scale := jsFullSize;
  else
    CellJPEG.Scale := jsEighth;
  end;
  w := Value;
  h := Value;
  if CellDetail then
  begin
    I := (h + 1) div 16;
    if I > 6 then
      I := 6;
    CellTextOff := (h - (I * 16)) div 2;
    Rows := I;
    HSX := w + 20;
    HSY := CellTextOff + 19;
    w := w + 160;
    h := h + 20;
    CellTextOff := CellTextOff + 10;
  end
  else
  begin
    w := w + 20;
    If CellStyle = 0 then
      h := h + 20
    else
      h := h + 40;
    HSX := (w - 70) shr 1;
  end;
  svMain.CellWidth := w;
  svMain.CellHeight := h;
  if CellDetail then
    CellScale := DetailScale
  else
    CellScale := SimpleScale;
  if UpdateTrackbar then
  begin
    tbSize.OnChange := nil;
    tbSize.Position := CellScale;
    tbSize.OnChange := tbSizeChange;
  end;
  svMain.CalcView(False);
  if not UpdateTrackbar then
    svMain.SetAtTop(vGrp, vIdx);
end;

procedure TfrmMain.SetThumbStyle(Value: Byte);
begin
  if Value < 7 then
    CellStyle := Value;
  case Value of
    0 .. 6:
      svMain.OnCellPaint := ThumbsCellPaintStyle1;
    7:
      svMain.OnCellPaint := ThumbsCellPaintStyle2;
  end;
  CellDetail := (Value = 7);
  if CellDetail then
  begin
    btnStyle.Tag := 1;
    btnView.Tag := 1;
  end
  else
  begin
    btnStyle.Tag := 0;
    btnView.Tag := 0;
  end;
  btnStyle.ImageIndex := 4 + btnStyle.Tag;
  btnView.ImageIndex := 4 + btnView.Tag;
  if CellDetail then
    SetThumbSize(DetailScale, True)
  else
    SetThumbSize(SimpleScale, True);
end;

procedure TfrmMain.sortNameClick(Sender: TObject);
begin
  DoSort((Sender as TMenuItem).Tag);
end;

procedure TfrmMain.subSaveClick(Sender: TObject);
begin
  if AlbumName = Noname then
    btnSaveAsClick(self)
  else
    SaveAlbum(AlbumPath + AlbumName);
end;

procedure TfrmMain.svKeyWordsClick(Sender: TObject);
var
  Idx: Integer;
  I: Integer;
  p: PThumbData;
begin
  svMain.Items.Clear;
  if svKeyWords.Selection.Count = 1 then
  begin
    Idx := svKeyWords.Selection[0];
    for I := 0 to Items.Count - 1 do
    begin
      p := Items[I];
      if p.KeyWords.IndexOf(Idx) <> -1 then
        svMain.Items.Add(I);
    end;
  end
  else
    FilterView(editFilter.Text);
  svMain.CalcView(False);
  svMain.Invalidate;
  svMain.SetFocus;
  UpdateStatus;
end;

procedure TfrmMain.svKeyWordsListPaint(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState;
  Columns: array of Integer);
var
  txt: String;
  c: TColor;
  R: TRect;
begin
  R := Cell;
  Canvas.Brush.Style := bsClear;
  if (State = svSelected) or (State = svHot) then
  begin
    if (svKeyWords.Focused) and (State = svSelected) then
    begin
      Canvas.Pen.Color := cSelected;
      WinGradient(Canvas.Handle, R, cGSelectedStart, cGSelectedEnd);
    end
    else if (State = svHot) then
    begin
      Canvas.Pen.Color := cHot;
      WinGradient(Canvas.Handle, R, cGHotStart, cGHotEnd);
    end
    else
    begin
      Canvas.Pen.Color := cDisabled;
      WinGradient(Canvas.Handle, R, cGDisabledStart, cGDisabledEnd);
    end;
    Canvas.Rectangle(R);
    if (svKeyWords.Focused) then
      c := cShadeSelect
    else
      c := cShadeDisabled;
    Canvas.Pen.Color := c;
    Canvas.MoveTo(R.Left + 1, R.Top + 2);
    Canvas.LineTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Pen.Style := psSolid;
    Canvas.Pixels[R.Left, R.Top] := c;
    Canvas.Pixels[R.Left, R.Bottom - 1] := c;
    Canvas.Pixels[R.Right - 1, R.Top] := c;
    Canvas.Pixels[R.Right - 1, R.Bottom - 1] := c;
  end;
  txt := ToMixCase(KeyWords[Idx]);
  R.Left := R.Left + 3;
  Canvas.Font.Color:= clBlack;
  DrawText(Canvas.Handle, PChar(txt), Length(txt), R,
    DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
end;

procedure TfrmMain.svMainCellHit(Sender: TObject; Canvas: TCanvas;
  Idx, x, y: Integer; var Selected: Boolean);
var
  I: Integer;
begin
  I := -1;
  if CellDetail then
  begin
    // Check if over a txtline
    if (x > HSX) and (y > CellTextOff) then
    begin
      I := Rows - ((y - CellTextOff) shr 4);
      if I <> CellEditIdx then
      begin
        CellEditIdx := I;
        svMain.Invalidate;
      end;
    end
    else
    begin
      if CellEditIdx <> -1 then
        svMain.Invalidate;
      CellEditIdx := -1;
    end;
    // Check if over rating, stars
    if (x > HSX) and (x < HSX + 70) and (I = 5) then
    begin
      I := 1 + (x - HSX) div 14;
      if I <> HotStars then
      begin
        HotStars := I;
        svMain.Invalidate;
      end;
    end
    else
    begin
      if HotStars <> -1 then
        svMain.Invalidate;
      HotStars := -1;
    end;
  end
  else
  begin
    if (y < 20) then
    begin
      I := 0;
      if (CellStyle = 5) and (y < 20) then
        I := 1;
      if (CellStyle = 6) and (y < 20) then
        I := 2;
      if I <> CellEditIdx then
      begin
        CellEditIdx := I;
        svMain.Invalidate;
      end;
    end
    else
    begin
      if CellEditIdx <> -1 then
        svMain.Invalidate;
      CellEditIdx := -1;
    end;
    if CellStyle <> 4 then
      Exit;
    // Check if over rating stars
    if (x > HSX) and (x < HSX + 70) and (y < 20) then
    begin
      CellEditIdx := 3;
      I := 1 + (x - HSX) div 14;
      if I <> HotStars then
      begin
        HotStars := I;
        svMain.Invalidate;
      end;
    end
    else
    begin
      if HotStars <> -1 then
        svMain.Invalidate;
      HotStars := -1;
    end;
  end;
end;

procedure TfrmMain.svMainDblClick(Sender: TObject);
begin
  DblClicked := True;
  DoPreview(True);
end;

procedure TfrmMain.svMainDividerHit(Sender: TObject; Canvas: TCanvas;
  Grp, x: Integer; var Selected: Boolean);
begin
  Selected := (x > (24 + svMain.CellOffset)) and (not Selected);
end;

procedure TfrmMain.svMainDividerPaint(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; Group: PSmartGroup; State: TsvItemState);
var
  R: TRect;
  txt: string;
  x: Integer;
begin
  R := Cell;
  ItemPaintBasic(Canvas, Cell, State);
  if Group.Expanded then
    ilGUImain.Draw(Canvas, R.Left + 8, R.Top + 6, 0)
  else
    ilGUImain.Draw(Canvas, R.Left + 9, R.Top + 6, 1);

  Canvas.Pen.Color := $00F0F0F0;
  Canvas.MoveTo(R.Left + 24, R.Top + 2);
  Canvas.LineTo(R.Left + 24, R.Bottom - 3);

  Canvas.Pen.Width := 1;
  Canvas.Brush.Style := bsClear;
  R.Left := R.Left + 30;
  Canvas.Font.Size := 11;
  Canvas.Font.Color := $00905422;
  txt := Group.Caption;
  DrawText(Canvas.Handle, PChar(txt), Length(txt), R,
    DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
  R.Top := R.Top + 1;
  R.Bottom := R.Bottom + 1;
  x := Canvas.TextWidth(txt);
  R.Left := R.Left + x;
  Canvas.Font.Size := 9;
  Canvas.Font.Color := clGray;
  txt := '  (' + IntToStr(Group.Items.Count) + ' items)';
  x := x + Canvas.TextWidth(txt);
  DrawText(Canvas.Handle, PChar(txt), Length(txt), R,
    DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
  Canvas.Pen.Color := $00F0F0F0;
  Canvas.MoveTo(x + 48, R.Top + 10);
  Canvas.LineTo(R.Right - 8, R.Top + 10);
end;

procedure TfrmMain.svMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F8 then
    ShowStat;
end;

procedure TfrmMain.svMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  Idx: Integer;
  pt: TPoint;
  p: PThumbData;
  R: TRect;
begin
  vGrp := svMain.ViewGrp;
  vIdx := svMain.ViewIdx;

  if svMain.Grouped then
    Idx := svMain.GrpIdxAtXY(Point(x, y), True).y
  else
    Idx := svMain.IdxAtXY(Point(x, y), True);

  if Idx = -1 then
    Exit;

  if (ssAlt in Shift) then
    ShowImgInfo
  else
  if (Shift = [ssLeft]) and not(svMain.Selecting) then
  begin
    if (CellDetail) then
    begin
      if (CellEditIdx <= Rows) then
      begin
        if (CellEditIdx = 5) then
        begin // Stars...
          if (HotStars <> -1) and (Idx <> -1) then
          begin
            p := Items[Idx];
            if p.Data = HotStars then
              p.Data := 0
            else
              p.Data := HotStars;
          end;
        end
        else if (CellEditIdx = 1) or (CellEditIdx = 6) then
        begin
          EditId := CellEditIdx;
          EditIdx := Idx;
          p := Items[Idx];
          // Needed since IdxAtXY can not be used with GetItemRect ...
          if svMain.Grouped then
            pt := svMain.GrpItemAtXY(Point(x, y), True)
          else
            pt.y := svMain.ItemAtXY(Point(x, y), True);
          R := svMain.GetItemRect(pt.x, pt.y);
          editView.Left := R.Left + HSX - 3;
          if CellEditIdx = 6 then
          begin
            editView.Text := p.Note;
            editView.Top := ((R.Bottom - CellTextOff) - 17)
              - svMain.sbVert.Position;
          end
          else
          begin
            editView.Text := p.Name;
            editView.Top := ((R.Top + CellTextOff) - 3)
              - svMain.sbVert.Position;
          end;
          editView.Visible := True;
          editView.SetFocus;
          svMain.IsEditing := True;
        end;
      end;
    end
    else
    begin
      if (CellEditIdx = 3) then
      begin // Stars...
        if (HotStars <> -1) and (Idx <> -1) then
        begin
          p := Items[Idx];
          if p.Data = HotStars then
            p.Data := 0
          else
            p.Data := HotStars;
        end;
      end
      else if (CellEditIdx = 1) or (CellEditIdx = 2) then
      begin
        EditId := CellEditIdx;
        EditIdx := Idx;
        p := Items[Idx];
        // Needed since IdxAtXY can not be used with GetItemRect ...
        if svMain.Grouped then
          pt := svMain.GrpItemAtXY(Point(x, y), True)
        else
          pt.y := svMain.ItemAtXY(Point(x, y), True);
        if CellEditIdx = 1 then
          editView.Text := p.Note
        else
          editView.Text := p.Name;
        R := svMain.GetItemRect(pt.x, pt.y);
        editView.Left := R.Left;
        editView.Width := R.Right - R.Left;
        editView.Top := ((R.Bottom - 20)) - svMain.sbVert.Position;
        editView.Visible := True;
        editView.SetFocus;
        svMain.IsEditing := True;
      end;
    end;
  end;
end;

procedure TfrmMain.svMainSelecting(Sender: TObject; Count: Integer);
begin
  UpdateStatus;
end;

procedure TfrmMain.Image1MouseEnter(Sender: TObject);
begin
  if AlbumText = '' then
    labAlbum.Caption := '< Add Title >'
  else
    labAlbum.Caption := AlbumText;
  // MakeTitle(Image1, AlbumText, $00C08000, clWhite);
end;

procedure TfrmMain.Image1MouseLeave(Sender: TObject);
begin
  if AlbumText = '' then
    labAlbum.Caption := '< Add Title >'
  else
    labAlbum.Caption := AlbumText;
  // MakeTitle(Image1, AlbumText, clBlack, clWhite);
end;

procedure TfrmMain.SetTitleVisible(Value: Boolean);
begin
  if Value then
  begin
    editTitle.Visible := False;
    labAlbum.Visible := False;
    imgRating.Visible := False;
    pnlTop.Height := 36
  end
  else
  begin
    pnlTop.Height := 62;
    imgRating.Visible := True;
    labAlbum.Visible := True;
  end;
end;

procedure TfrmMain.imgGrdClick(Sender: TObject);
begin
  SetTitleVisible(pnlTop.Height = 62)
end;

procedure TfrmMain.imgGrdMouseEnter(Sender: TObject);
begin
  ShadeGradient(imgGrd, clWhite, $00C08000, True);
end;

procedure TfrmMain.imgGrdMouseLeave(Sender: TObject);
begin
  ShadeGradient(imgGrd, clWhite, clSilver, True);
end;

procedure TfrmMain.ItemPaintBasic(Canvas: TCanvas; R: TRect;
  State: TsvItemState);
var
  c: TColor;
begin
  Canvas.Brush.Style := bsClear;
  if (State = svSelected) or (State = svHot) then
  begin
    if (svMain.Focused) and (State = svSelected) then
    begin
      Canvas.Pen.Color := cSelected;
      WinGradient(Canvas.Handle, R, cGSelectedStart, cGSelectedEnd);
    end
    else if (State = svHot) then
    begin
      Canvas.Pen.Color := cHot;
      WinGradient(Canvas.Handle, R, cGHotStart, cGHotEnd);
    end
    else
    begin
      Canvas.Pen.Color := cDisabled;
      WinGradient(Canvas.Handle, R, cGDisabledStart, cGDisabledEnd);
    end;
    Canvas.Rectangle(R);
    if (svMain.Focused) then
      c := cShadeSelect
    else
      c := cShadeDisabled;
    Canvas.Pen.Color := c;
    Canvas.MoveTo(R.Left + 1, R.Top + 2);
    Canvas.LineTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Pen.Style := psSolid;
    Canvas.Pixels[R.Left, R.Top] := c;
    Canvas.Pixels[R.Left, R.Bottom - 1] := c;
    Canvas.Pixels[R.Right - 1, R.Top] := c;
    Canvas.Pixels[R.Right - 1, R.Bottom - 1] := c;
  end;
end;

procedure TfrmMain.UpdateInfoPnl;
var
  I, j, Idx: Integer;
  txt: String;
  info: PThumbData;
begin
  if svMain.Selection.Count = 1 then
  begin
    labAddTags.Caption := 'Add tags';
    InfoPnlIdx := svMain.Selection[0];
    Idx := InfoPnlIdx;
    if Idx <> -1 then
    begin
      info := Items[Idx];
      txt := info.Note;
      if txt = '' then
        txt := 'Add text';
      labCap.Caption := txt;
      labFName.Caption := info.Name;
      DateTimeToString(txt, 'dd.mmm.yyyy hh:mm', info.Modified);
      labFDate.Caption := txt;
      labFSize.Caption := BytesToStr(info.Size);
      labISize.Caption := IntToStr(info.IWidth) + ' x ' + IntToStr
        (info.IHeight);
      imgRating.Canvas.Brush.Style := bsSolid;
      imgRating.Canvas.Brush.Color := clWindow;
      imgRating.Canvas.FillRect(Rect(0, 0, imgRating.Width, imgRating.Height));
      for I := 0 to 4 do
      begin
        if I < (info.Data and $F) then
          j := 3
        else
          j := 2;
        ilGUImain.Draw(imgRating.Canvas, (I * 14), 0, j, svMain.Focused);
      end;
      // Keywords...
      svKeyWords.Items.Clear;
      for I := 0 to info.KeyWords.Count - 1 do
        svKeyWords.Items.Add(info.KeyWords[I]);
      svKeyWords.UpdateView;
      svKeyWords.Invalidate;
    end;
  end
  else
  begin
    txt := 'Not available';
    if svMain.Selection.Count > 1 then
      labAddTags.Caption := 'Add tags to selected images'
    else
      labAddTags.Caption := txt;
    labCap.Caption := txt;
    labFName.Caption := txt;
    labFDate.Caption := txt;
    labFSize.Caption := txt;
    labISize.Caption := txt;
    imgRating.Canvas.Brush.Style := bsSolid;
    imgRating.Canvas.Brush.Color := clWindow;
    imgRating.Canvas.FillRect(Rect(0, 0, imgRating.Width, imgRating.Height));
    for I := 0 to 4 do
      ilGUImain.Draw(imgRating.Canvas, (I * 14), 0, 2, svMain.Focused);
  end;
end;

procedure TfrmMain.labAddTagsClick(Sender: TObject);
begin
  editKeywords.Left := 3;
  editKeywords.Top := 43;
  editKeywords.Text := '';
  editKeywords.Visible := True;
  editKeywords.SetFocus;
end;

procedure TfrmMain.labAlbumClick(Sender: TObject);
begin
  editTitle.Text := AlbumText;
  editTitle.Top := 34;
  editTitle.Left := 16;
  editTitle.Width := pnlThumbs.Width - 32;
  editTitle.Visible := True;
  editTitle.SetFocus;
end;

procedure TfrmMain.labAlbumMouseEnter(Sender: TObject);
begin (Sender as TLabel)
  .Font.Color := $00C08000;
end;

procedure TfrmMain.labAlbumMouseLeave(Sender: TObject);
begin (Sender as TLabel)
  .Font.Color := clBlack;
end;

procedure TfrmMain.labCapClick(Sender: TObject);
begin
  editInfo.Tag := 2;
  editInfo.Left := 3;
  editInfo.Top := 43;
  editInfo.Text := PThumbData(Items[InfoPnlIdx]).Note;
  editInfo.Visible := True;
  editInfo.SetFocus;
end;

procedure TfrmMain.ThumbsCellPaintStyle1(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);
var
  x, y, I, j: Integer;
  f, s: Boolean;
  R, t: TRect;
  TW, TH: Integer;
  txt: string;
  Thumb: PThumbData;
  pt: TPoint;
  CEdit: Boolean;
begin
  Thumb := PThumbData(Items[Idx]);
  pt := CalcThumbSize(Thumb.ThumbWidth, Thumb.ThumbHeight, CellScale,
    CellScale);
  TW := pt.x;
  TH := pt.y;
  ItemPaintBasic(Canvas, Cell, State);
  f := svMain.Focused;
  s := State = svSelected;

  x := Cell.Left + ((Cell.Right - (Cell.Left + TW)) shr 1);
  y := Cell.Top + ((Cell.Bottom - (Cell.Top + TH)) shr 1);
  if CellStyle <> 0 then
    y := y - 10;

  if Thumb.Image <> nil then
    Canvas.Draw(x, y, ThumbBmp(Idx));

  R.Left := x;
  R.Top := y;
  R.Right := x + TW;
  R.Bottom := y + TH;

  Canvas.Pen.Color := CellBrdColor[f, s];
  InflateRect(R, 2, 2);
  Canvas.Rectangle(R);
  Canvas.Pen.Color := clWhite;
  InflateRect(R, -1, -1);
  Canvas.Rectangle(R);

  if CellStyle = 0 then
    Exit;

  Canvas.Font.Color := clBlack;
  t := Cell;
  t.Top := t.Bottom - 20;

  case CellStyle of
    1:
      DateTimeToString(txt, CellDateFormat, Thumb.Modified);
    2:
      txt := BytesToStr(Thumb.Size);
    3:
      txt := IntToStr(Thumb.IWidth) + ' x ' + IntToStr(Thumb.IHeight);
    4:
      txt := '';
    5:
      begin
        txt := Thumb.Note;
        if txt = '' then
          txt := '< Add text >';
      end;
    6:
      txt := Thumb.Name;
  end;

  CEdit := (CellEditIdx = 1) or (CellEditIdx = 2);
  if CellStyle <> 4 then
  begin
    if (CEdit) and (State = svHot) then
      Canvas.Font.Color := $00C08000
    else
      Canvas.Font.Color := clBlack;
    DrawText(Canvas.Handle, PChar(txt), Length(txt), t,
      DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_CENTER or
        DT_VCENTER);
  end
  else
  begin
    if svMain.Focused then
      ilGUImain.DrawingStyle:= dsNormal
    else
      ilGUImain.DrawingStyle:= dsFocus;
    t.Left := Cell.Left + ((Cell.Right - (Cell.Left + 70)) shr 1);
    if (HotStars <> -1) and (State = svHot) then
    begin
      for I := 0 to 4 do
      begin
        if I < HotStars then
          j := 3
        else
          j := 2;
        ilGUImain.Draw(Canvas, t.Left + (I * 14), t.Top, j, True);
      end;
    end
    else
      for I := 0 to 4 do
      begin
        if I < (Thumb.Data and $F) then
          j := 3
        else
          j := 2;
        ilGUImain.Draw(Canvas, t.Left + (I * 14), t.Top, j, True);
      end;
    ilGUImain.DrawingStyle:= dsNormal
  end;

  {
    // New color values
    T.Left:= T.Left + 10;
    T.Right:= T.Left + 16;
    T.Bottom:= T.Top + 16;
    Canvas.Pen.Color:= clBlack;
    Canvas.Brush.Color:= (Thumb.ColorAvg.B shl 16) + (Thumb.ColorAvg.G shl 8) + Thumb.ColorAvg.R;
    Canvas.Rectangle(t);

    T.Left:= T.Right + 1;
    T.Right:= T.Left + 16;
    Canvas.Pen.Color:= clBlack;
    Canvas.Brush.Color:= (Thumb.ColorDom.B shl 16) + (Thumb.ColorDom.G shl 8) + Thumb.ColorDom.R;
    Canvas.Rectangle(t);

    T.Left:= T.Right + 1;
    T.Right:= T.Left + 16;
    Canvas.Pen.Color:= clBlack;
    Canvas.Brush.Color:= (Thumb.Col1.B shl 16) + (Thumb.Col1.G shl 8) + Thumb.Col1.R;
    Canvas.Rectangle(t);

    T.Left:= T.Right + 1;
    T.Right:= T.Left + 16;
    Canvas.Pen.Color:= clBlack;
    Canvas.Brush.Color:= (Thumb.Col2.B shl 16) + (Thumb.Col2.G shl 8) + Thumb.Col2.R;
    Canvas.Rectangle(t);

    T.Left:= T.Right + 1;
    T.Right:= T.Left + 16;
    Canvas.Pen.Color:= clBlack;
    Canvas.Brush.Color:= (Thumb.Col3.B shl 16) + (Thumb.Col3.G shl 8) + Thumb.Col3.R;
    Canvas.Rectangle(t);
  }
end;

procedure TfrmMain.ThumbsCellPaintStyle2(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState);
var
  x, y: Integer;
  f, s: Boolean;
  R, t: TRect;
  TW, TH, I, j: Integer;
  txt: string;
  Thumb: PThumbData;
  pt: TPoint;
  C: TColor;
begin
  Thumb := PThumbData(Items[Idx]);
  pt := CalcThumbSize(Thumb.ThumbWidth, Thumb.ThumbHeight, CellScale,
    CellScale);
  TW := pt.x;
  TH := pt.y;
  ItemPaintBasic(Canvas, Cell, State);
  f := svMain.Focused;
  s := (State = svSelected) or (State = svHot);
  x := Cell.Left + ((Cell.Right - ((Cell.Left + 140) + TW)) shr 1);
  y := Cell.Top + ((Cell.Bottom - (Cell.Top + TH)) shr 1);
  Canvas.Draw(x, y, ThumbBmp(Idx));

  R.Left := x;
  R.Top := y;
  R.Right := x + TW;
  R.Bottom := y + TH;

  Canvas.Pen.Color := CellBrdColor[f, s];
  InflateRect(R, 2, 2);
  Canvas.Rectangle(R);
  Canvas.Pen.Color := clWhite;
  InflateRect(R, -1, -1);
  Canvas.Rectangle(R);
  Canvas.Font.Color := clBlack;

  {
    x := Cell.Left + ((Cell.Right - ((Cell.Left + 140) + TW)) shr 1);
    y := Cell.Top + ((Cell.Bottom - (Cell.Top + TH)) shr 1);
    R.Left := X;
    R.Top := Y;
    R.Right := X + TW;
    R.Bottom := Y + TH;
    InflateRect(R, 2, 2);
    R.Left := R.Left + 4;
    R.Top := R.Top + 3;
    Canvas.Pen.Color := CellShdColor[F, S, 1];
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Right, R.Top);
    Canvas.Pixels[R.Left - 1, R.Bottom] := CellShdColor[F, S, 2];
    Canvas.Pixels[R.Left - 2, R.Bottom] := CellShdColor[F, S, 3];
    Canvas.Pixels[R.Left - 3, R.Bottom] := CellShdColor[F, S, 4];
    Canvas.Pixels[R.Right, R.Top] := CellShdColor[F, S, 2];
    Canvas.Pixels[R.Right, R.Top - 1] := CellShdColor[F, S, 3];
    Canvas.Pixels[R.Right, R.Top - 2] := CellShdColor[F, S, 4];
    Canvas.Pixels[R.Right, R.Bottom] := CellShdColor[F, S, 3];
  }

  t := Cell;
  t.Left := t.Right - 140;
  t.Right := t.Right - 10;
  t.Top := t.Top + CellTextOff;
  txt := Thumb.Caption;
  if (txt = '') then
    txt := Thumb.Name;
  if (CellEditIdx = 1) and (State = svHot) then
    Canvas.Font.Color := $00C08000;
  DrawText(Canvas.Handle, PChar(txt), Length(txt), t,
    DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX);
  Canvas.Font.Color := clBlack;
  t.Top := t.Top + 16;
  DateTimeToString(txt, 'dd.mmm.yyyy hh:mm', Thumb.Modified);
  DrawText(Canvas.Handle, PChar(txt), Length(txt), t, CellText);
  if Rows > 2 then
  begin
    t.Top := t.Top + 16;
    txt := BytesToStr(Thumb.Size);
    DrawText(Canvas.Handle, PChar(txt), Length(txt), t, CellText);
  end;
  if Rows > 3 then
  begin
    t.Top := t.Top + 16;
    txt := IntToStr(Thumb.IWidth) + ' x ' + IntToStr(Thumb.IHeight);
    DrawText(Canvas.Handle, PChar(txt), Length(txt), t, CellText);
  end;
  if Rows > 4 then
  begin
    if svMain.Focused then
      ilGUImain.DrawingStyle:= dsNormal
    else
      ilGUImain.DrawingStyle:= dsFocus;
    t.Top := t.Top + 16;
    if (HotStars <> -1) and (State = svHot) then
    begin
      for I := 0 to 4 do
      begin
        if I < HotStars then
          j := 3
        else
          j := 2;
        ilGUImain.Draw(Canvas, t.Left + (I * 14), t.Top, j, True);//svMain.Focused);
      end;
    end
    else
      for I := 0 to 4 do
      begin
        if I < (Thumb.Data and $F) then
          j := 3
        else
          j := 2;
        ilGUImain.Draw(Canvas, t.Left + (I * 14), t.Top, j, True);//svMain.Focused);
      end;
    ilGUImain.DrawingStyle:= dsNormal;
  end;
  if Rows > 5 then
  begin
    Canvas.Brush.Style:= bsClear;
    t.Top := t.Top + 16;
    txt := Thumb.Note;
    if txt = '' then
      txt := '< Add text >';
    if (CellEditIdx = 6) and (State = svHot) then
      Canvas.Font.Color := $00C08000
    else
      Canvas.Font.Color := clBlack;
    DrawText(Canvas.Handle, PChar(txt), Length(txt), t, CellText);
  end;

  {
    t.Top := t.Top + 16;
    t.Bottom:= T.Top + 16;
    //Canvas.Brush.Color:= ColorFromHSL(Thumb.SimSort);
    C:= (Thumb.ColorAvg.R Shl 16) + (Thumb.ColorAvg.G Shl 8) + (Thumb.ColorAvg.B);
    Canvas.Brush.Color:= C;
    Canvas.FillRect(t);
  }
end;

procedure TfrmMain.timerSearchTimer(Sender: TObject);
begin
  txtCount := txtCount - 1;
  if txtCount = 0 then
    FilterView(editFilter.Text);
  timerSearch.Enabled := txtCount <> 0;
end;

procedure TfrmMain.tbSizeChange(Sender: TObject);
begin
  SetThumbSize(tbSize.Position, False);
end;

procedure TfrmMain.OpenAlbum(name: string);
var
  Int, I, j: Integer;
  s, id: AnsiString;
  b: Byte;
  FStr: TFileStream;
  MS: TMemoryStream;
  w: Word;

  w1: Word;
  l, p: Integer;
  Entry: PThumbData;
  js: TMemoryStream;

begin
  ClearAlbum;

  FStr := TFileStream.Create(name, fmOpenRead);
  FStr.Position := 0;

  SetLength(s, 3);
  FStr.Read(PChar(s)^, 3);

  // Read version
  FStr.Read(b, SizeOf(Byte));
  AlbumVersion := b;

  // version 1 have no BitDepth
  // Thumbs.LoadDepth := (b <> 1);
  // version 3 have no Group.Data
  // Thumbs.LoadGroupData := (b > 3);
  // version < 5 have no Thumb.Data
  // Thumbs.LoadDataByte := (b > 5);
  // version2 have no Thumb Size info or Tree Info
  if AlbumVersion > 2 then
  begin
    FStr.Read(Int, SizeOf(LongInt));
    ThumbSizeW := (Int and $FFFF);
    ThumbSizeH := ((Int shr 16) and $FFFF);
  end
  else
  begin
    ThumbSizeW := 160;
    ThumbSizeH := 120;
  end;

  FStr.Read(Int, SizeOf(LongInt));
  FStr.Read(w, SizeOf(Word));
  SetLength(s, w);
  FStr.Read(PChar(s)^, w);
  AlbumText := s;

  if AlbumVersion < 3 then
  begin
    SetLength(s, 4);
    FStr.Read(PChar(s)^, 4);
    FStr.Read(Int, SizeOf(LongInt));
    if s = 'TREG' then
    begin // TREG = TreeGroups
      FStr.Read(w, SizeOf(Word));
      if w > 0 then
      begin
        MS := TMemoryStream.Create;
        MS.Position := 0;
        MS.CopyFrom(FStr, w);
        MS.Position := 0;
        MS.Free;
      end;
    end
    else
      FStr.Position := FStr.Position - 8;

    SetLength(s, 4);
    FStr.Read(PChar(s)^, 4);
    FStr.Read(Int, SizeOf(LongInt));
    if s = 'TRES' then
    begin // TRES = TreeSearch
      FStr.Read(w, SizeOf(Word));
      if w > 0 then
      begin
        MS := TMemoryStream.Create;
        MS.Position := 0;
        MS.CopyFrom(FStr, w);
        MS.Position := 0;
        MS.Free;
      end;
    end
    else
      FStr.Position := FStr.Position - 8;
  end;

  MS := TMemoryStream.Create;
  try
    SetLength(s, 4);
    FStr.Read(PChar(s)^, 4);
    FStr.Read(Int, SizeOf(LongInt));
    id := s;
    if s = 'THMG' then
    begin
      // Load Groups
      FStr.Read(w, SizeOf(Word));
      for I := 0 to w - 1 do
      begin
        FStr.Read(b, SizeOf(Byte));
        SetLength(s, b);
        FStr.Read(PChar(s)^, b);
        FStr.Read(b, SizeOf(Byte));
        SetLength(s, b);
        FStr.Read(PChar(s)^, b);
        if (AlbumVersion > 3) then
          FStr.Read(w1, SizeOf(Word));
        FStr.Read(Int, SizeOf(LongInt));
        for j := 0 to Int - 1 do
        begin
          FStr.Read(w1, SizeOf(Word));
        end;
      end;
    end;

    if id <> 'LIST' then
    begin
      SetLength(s, 4);
      FStr.Read(PChar(s)^, 4);
      FStr.Read(Int, SizeOf(LongInt));
    end;

    if s = 'LIST' then
    begin
      // read number of lists
      FStr.Read(b, SizeOf(Byte));
      I := b;
      for j := 0 to I - 1 do
      begin
        FStr.Read(b, SizeOf(Byte));
        FStr.Read(w, SizeOf(Word));
        if w <> 0 then
        begin
          MS.Clear;
          MS.CopyFrom(FStr, w);
          MS.Position := 0;
          case b of
            1:
              Paths.LoadFromStream(MS); // Paths
            2:
              FileInfo.LoadFromStream(MS); // FileInfo
            3:
              Authors.LoadFromStream(MS); // Author
            4:
              Location.LoadFromStream(MS); // Location
            5:
              MediaID.LoadFromStream(MS); // Media ID
            6:
              FilmNr.LoadFromStream(MS); // Film Number
            7:
              KeyWords.LoadFromStream(MS); // KeyWords
          else
          end;
        end;
      end;
    end;

    SetLength(s, 4);
    FStr.Read(PChar(s)^, 4);
    FStr.Read(Int, SizeOf(LongInt));

    proOpen.Left := (svMain.ClientWidth - proOpen.Width) shr 1;
    proOpen.Top := (svMain.ClientHeight - proOpen.Height) shr 1;
    proOpen.Visible := True;

    FStr.Read(Int, SizeOf(Integer));
    I := Int;
    Items.Capacity := I;
    for j := 0 to I - 1 do
    begin
      New(Entry);
      Entry.Data := 0;
      if (AlbumVersion > 5) then
        FStr.Read(Entry.Data, SizeOf(Byte));

      // Div Identifiers
      FStr.Read(Entry.PathID, SizeOf(SmallInt));
      FStr.Read(Entry.InfoID, SizeOf(SmallInt));
      FStr.Read(Entry.AuthorID, SizeOf(SmallInt));
      FStr.Read(Entry.FilmID, SizeOf(SmallInt));
      FStr.Read(Entry.MediaID, SizeOf(SmallInt));
      FStr.Read(Entry.CustID, SizeOf(SmallInt));

      // Image Size
      FStr.Read(Entry.IWidth, SizeOf(Word));
      FStr.Read(Entry.IHeight, SizeOf(Word));

      // Thumb Size
      FStr.Read(Entry.ThumbWidth, SizeOf(Word));
      FStr.Read(Entry.ThumbHeight, SizeOf(Word));

      // Image Size
      FStr.Read(Entry.Size, SizeOf(Integer));

      // Bitdepth
      if (AlbumVersion <> 1) then
        FStr.Read(Entry.BitDepth, SizeOf(Byte));

      // Filename
      FStr.Read(b, SizeOf(Byte));
      SetLength(s, b);
      FStr.Read(PChar(s)^, b);
      Entry.Name := s;

      // Caption
      FStr.Read(b, SizeOf(Byte));
      SetLength(s, b);
      FStr.Read(PChar(s)^, b);
      Entry.Caption := s;

      // Note
      FStr.Read(w, SizeOf(Word));
      SetLength(s, w);
      FStr.Read(PChar(s)^, w);
      Entry.Note := s;

      // Modified date
      FStr.Read(Entry.Modified, SizeOf(TDateTime));

      js := TMemoryStream.Create;
      js.Position := 0;
      FStr.Read(Int, SizeOf(LongWord));
      js.CopyFrom(FStr, Int);
      Entry.Image := js;
      Entry.GotThumb := Int <> 0;

      Items.Add(Entry);
      Entry.KeyWords := TIntList.Create;
      FStr.Read(w, SizeOf(Word));
      if w > 0 then
      begin
        for l := 0 to w - 1 do
        begin
          FStr.Read(w1, SizeOf(Word));
          Entry.KeyWords.Add(w1);
        end;
      end;

      if j = 0 then
        p := 0
      else
        p := Round((100 / I) * j);
      proOpen.Position := p;
    end;
  finally
    Screen.Cursor := crDefault;
    MS.Free;
  end;
  FStr.Free;

  proOpen.Visible := False;
  for I := 0 to Items.Count - 1 do
    svMain.Items.Add(I);
  svMain.CalcView(True);
  svMain.UpdateView;
  AlbumName := ExtractFileName(name);
  AlbumPath := ExtractFilePath(name);
  s := AlbumName;
  if Pos('.', s) <> 0 then
    s := Copy(s, 1, Pos('.', s) - 1);
  Caption := s + ' - ' + AppTitle;
  Forms.Application.Title := Caption;
  if AlbumText = '' then
    labAlbum.Caption := '< Add Title >'
  else
    labAlbum.Caption := AlbumText;
  SetThumbSize(tbSize.Position, False);
  SetGrouping(GroupedOn);
  UpdateStatus;
  svMain.CalcView(False);
  svMain.Invalidate;
end;

procedure TfrmMain.popInfoPnlClick(Sender: TObject);
begin
  popInfoPnl.Checked := not popInfoPnl.Checked;
  pnlInfo.Visible := popInfoPnl.Checked;
end;

procedure TfrmMain.popSortPopup(Sender: TObject);
begin
  sortAuto.Checked := (GrpSortKind = -1);
  sortName.Checked := (GrpSortKind = 0);
  sortSize.Checked := (GrpSortKind = 1);
  sortDate.Checked := (GrpSortKind = 2);
  sortHue.Checked := (GrpSortKind = 3);
  sortMatch.Checked := (GrpSortKind = 4);
  sortText.Checked := (GrpSortKind = 5);
  sortRating.Checked := (GrpSortKind = 6);
  sortPSize.Checked := (GrpSortKind = 7);
  sortType.Checked := (GrpSortKind = 8);
  sortPath.Checked := (GrpSortKind = 9);
  sortAsc.Checked := GrpSortAsc;
  sortDes.Checked := not(GrpSortAsc);
end;

procedure TfrmMain.popViewThumbsPopup(Sender: TObject);
begin
  popInfoPnl.Checked:= pnlInfo.Visible;
  viewDetails.Checked := CellDetail;
  viewThumbnails.Checked := (CellStyle = 0) and (not CellDetail);
  viewModified.Checked := (CellStyle = 1) and (not CellDetail);
  viewFileSize.Checked := (CellStyle = 2) and (not CellDetail);
  viewImageSize.Checked := (CellStyle = 3) and (not CellDetail);
  viewRating.Checked := (CellStyle = 4) and (not CellDetail);
  viewText.Checked := (CellStyle = 5) and (not CellDetail);
  viewFileName.Checked := (CellStyle = 6) and (not CellDetail);
end;

procedure TfrmMain.SaveAlbum(name: string);
var
  FStr: TFileStream;
  MS: TMemoryStream;
  I: LongInt;
  w: Word;
  b: Byte;
  id, s: AnsiString;
  j, k, p: Integer;
  Entry: PThumbData;
  Group: PSmartGroup;
  Int: Integer;
  SavePos, PosSize: Int64;
begin
  AlbumVersion := 7;
  FStr := TFileStream.Create(Name, fmCreate);
  try
    FStr.Position := 0;
    // Save ID and version nr
    id := 'BPA'; // Blaise Photo Album
    FStr.write(PChar(id)^, 3);
    b := AlbumVersion; // Version nr: 1 - 255
    FStr.write(b, SizeOf(Byte));

    // Save thumb size
    I := ThumbSizeW + (ThumbSizeH shl 16);
    FStr.write(I, SizeOf(LongInt));

    w := Length(AlbumText);
    I := w + 6;
    FStr.write(I, SizeOf(LongInt));

    // Save album description
    FStr.write(w, SizeOf(Word));
    s := AlbumText;
    FStr.write(PAnsiChar(s)^, Length(s));

    // Save Album
    Screen.Cursor := crHourglass;
    try
      id := 'LIST'; // Thumbs Groups
      FStr.write(PChar(id)^, Length(id));
      PosSize := FStr.Position;
      FStr.Position := FStr.Position + 4;
      b := 7; // write number of lists
      FStr.write(b, SizeOf(Byte));
      b := 1; // Paths
      FStr.write(b, SizeOf(Byte));
      w := Length(Paths.Text);
      FStr.write(w, SizeOf(Word));
      s := Paths.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 2; // FileInfo
      FStr.write(b, SizeOf(Byte));
      w := Length(FileInfo.Text);
      FStr.write(w, SizeOf(Word));
      s := FileInfo.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 3; // Author
      FStr.write(b, SizeOf(Byte));
      w := Length(Authors.Text);
      FStr.write(w, SizeOf(Word));
      s := Authors.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 4; // Location
      FStr.write(b, SizeOf(Byte));
      w := Length(Location.Text);
      FStr.write(w, SizeOf(Word));
      s := Location.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 5; // Media Id
      FStr.write(b, SizeOf(Byte));
      w := Length(MediaID.Text);
      FStr.write(w, SizeOf(Word));
      s := MediaID.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 6; // FilmNr
      FStr.write(b, SizeOf(Byte));
      w := Length(FilmNr.Text);
      FStr.write(w, SizeOf(Word));
      s := FilmNr.Text;
      FStr.write(PAnsiChar(s)^, w);
      b := 7; // KeyWords
      FStr.write(b, SizeOf(Byte));
      w := Length(KeyWords.Text);
      FStr.write(w, SizeOf(Word));
      s := KeyWords.Text;
      FStr.write(PAnsiChar(s)^, w);
      // write size of lists [LIST]
      SavePos := FStr.Position;
      Int := FStr.Position - PosSize;
      FStr.Position := PosSize;
      FStr.Write(Int, SizeOf(LongInt));
      FStr.Position := SavePos;

      id := 'THMB'; // Thumbnails
      FStr.write(PAnsiChar(id)^, Length(id));
      PosSize := FStr.Position;
      FStr.Position := FStr.Position + 4;
      FStr.write(Items.Count, SizeOf(Integer));
      for k := 0 to Items.Count - 1 do
      begin
        Entry := Items[k];
        // Data rating and more...
        FStr.write(Entry.Data, SizeOf(Byte));
        // Div Identifiers
        FStr.write(Entry.PathID, SizeOf(SmallInt));
        FStr.write(Entry.InfoID, SizeOf(SmallInt));
        FStr.write(Entry.AuthorID, SizeOf(SmallInt));
        FStr.write(Entry.FilmID, SizeOf(SmallInt));
        FStr.write(Entry.MediaID, SizeOf(SmallInt));
        FStr.write(Entry.CustID, SizeOf(SmallInt));
        // Image Size
        FStr.write(Entry.IWidth, SizeOf(Word));
        FStr.write(Entry.IHeight, SizeOf(Word));
        // Thumb Size
        FStr.write(Entry.ThumbWidth, SizeOf(Word));
        FStr.write(Entry.ThumbHeight, SizeOf(Word));
        // Image Size
        FStr.write(Entry.Size, SizeOf(Integer));
        // Bitdepth
        FStr.write(Entry.BitDepth, SizeOf(Byte));
        // Filename
        b := Length(Entry.Name);
        FStr.write(b, SizeOf(Byte));
        s := Entry.Name;
        FStr.write(PAnsiChar(s)^, b);
        // Caption
        b := Length(Entry.Caption);
        FStr.write(b, SizeOf(Byte));
        s := Entry.Caption;
        FStr.write(PAnsiChar(s)^, b);
        // Note
        w := Length(Entry.Note);
        FStr.write(w, SizeOf(Word));
        s := Entry.Note;
        FStr.write(PAnsiChar(s)^, w);
        // Modified date
        FStr.write(Entry.Modified, SizeOf(TDateTime));
        // rmk Save Thumb
        Int := TMemoryStream(Entry.Image).Size;
        FStr.Write(Int, SizeOf(Integer));
        TMemoryStream(Entry.Image).Position := 0;
        FStr.CopyFrom(TMemoryStream(Entry.Image), Int);
        if Entry.KeyWords <> nil then
        begin
          w := Entry.KeyWords.Count;
          FStr.write(w, SizeOf(Word));
          for j := 0 to Entry.KeyWords.Count - 1 do
          begin
            w := Entry.KeyWords[j];
            FStr.write(w, SizeOf(Word));
          end;
        end
        else
        begin
          w := 0;
          FStr.write(w, SizeOf(Word));
        end;
      end;
      // write size of lists [THMD]
      SavePos := FStr.Position;
      Int := FStr.Position - PosSize;
      FStr.Position := PosSize;
      FStr.Write(Int, SizeOf(LongInt));
      FStr.Position := SavePos;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    FStr.Free;
  end;

  AlbumName := ExtractFileName(name);
  AlbumPath := ExtractFilePath(name);
  s := AlbumName;
  if Pos('.', s) <> 0 then
    s := Copy(s, 1, Pos('.', s) - 1);
  Caption := s + ' - ' + AppTitle;
  Forms.Application.Title := Caption;
  UpdateStatus;
end;

end.
