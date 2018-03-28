unit formImgInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, rkView, ImageEnIO, StdCtrls, ExtCtrls, Math,
  hyieutils, hyiedefs;

type
  PStrItem = ^TStrItem;
  TStrItem = record str1, str2: string
  end;
  TfrmInfo = class(TForm)
    viewStat: TrkView;
    pnlBottom: TPanel;
    imgBottom: TImage;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure viewStatListPaint(Sender: TObject; Canvas: TCanvas; Cell: TRect;
      IdxA, Idx: Integer; State: TsvItemState; Columns: array of Integer);
    procedure viewStatDblClick(Sender: TObject);
    procedure viewStatHeaderPaint(Sender: TObject; Canvas: TCanvas;
      Header: TRect; Offset, Active: Integer; State: TsvItemState;
      Columns: array of Integer);
  private
    { Private declarations }
    Items: TList;
    procedure AddL(s1, s2: string);
    procedure GetExif(FName: string);
    procedure ItemPaintBasic(Canvas: TCanvas; R: TRect; State: TsvItemState);
  public
    { Public declarations }
  end;

var
  frmInfo: TfrmInfo;
  IdSmall, IdLarge: Integer;

implementation

uses
  rmkUtil, main;

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
  else
    Result := IntToStr(i64Size) + ' byte';
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor; const Vertical: Boolean);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;
  TGradientColors = array[0..255] of TRGBTriple;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3, y1, i, GSize: Integer;
  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
  GradientBmp: TBitmap;
begin
  rc2 := GetRValue(ColorToRGB(StartColor));
  gc2 := GetGValue(ColorToRGB(StartColor));
  bc2 := GetBValue(ColorToRGB(StartColor));
  rc1 := GetRValue(ColorToRGB(EndColor));
  gc1 := GetGValue(ColorToRGB(EndColor));
  bc1 := GetBValue(ColorToRGB(EndColor));
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
  for i := 0 to y1 - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    GradCol.rgbtRed := Byte(rc1 + (((rc2 - rc1) * (i)) div y1));
    GradCol.rgbtGreen := Byte(gc1 + (((gc2 - gc1) * (i)) div y1));
    GradCol.rgbtBlue := Byte(bc1 + (((bc2 - bc1) * (i)) div y1));
    Row[0] := GradCol;
  end;
  for i := y1 to GSize - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    GradCol.rgbtRed := Byte(rc3 + (((rc2 - rc3) * (i)) div GSize));
    GradCol.rgbtGreen := Byte(gc3 + (((gc2 - gc3) * (i)) div GSize));
    GradCol.rgbtBlue := Byte(bc3 + (((bc2 - bc3) * (i)) div GSize));
    Row[0] := GradCol;
  end;
  Canvas.StretchDraw(ARect, GradientBmp);
  GradientBmp.Free;
end;

procedure TfrmInfo.ItemPaintBasic(Canvas: TCanvas; R: TRect; State:
  TsvItemState);
var
  C: TColor;
begin
  Canvas.Brush.Style := bsClear;
  if (State = svSelected) or (State = svHot) then
  begin
    if (viewStat.Focused) and (State = svSelected) then
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
      WinGradient(Canvas.Handle, R, cGDisabledStart,cGDisabledEnd);
    end;
    Canvas.Rectangle(R);
    if (viewStat.Focused) then
      C := cShadeSelect
    else
      C := cShadeDisabled;
    Canvas.Pen.Color := C;
    Canvas.MoveTo(R.Left + 1, R.Top + 2);
    Canvas.LineTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Bottom - 2);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Pen.Style := psSolid;
    Canvas.Pixels[R.Left, R.Top] := C;
    Canvas.Pixels[R.Left, R.Bottom - 1] := C;
    Canvas.Pixels[R.Right - 1, R.Top] := C;
    Canvas.Pixels[R.Right - 1, R.Bottom - 1] := C;
  end;
end;

procedure TfrmInfo.viewStatDblClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := viewStat.Selected;
  if idx = 4 then
  begin
    frmMain.svMain.SetInView(-1, IdSmall);
    frmMain.svMain.Selection.Clear;
    frmMain.svMain.Selection.Add(IdSmall);
    frmMain.svMain.Invalidate;
  end;
  if idx = 6 then
  begin
    frmMain.svMain.SetInView(-1, IdLarge);
    frmMain.svMain.Selection.Clear;
    frmMain.svMain.Selection.Add(IdLarge);
    frmMain.svMain.Invalidate;
  end;
end;

procedure TfrmInfo.viewStatHeaderPaint(Sender: TObject; Canvas: TCanvas;
  Header: TRect; Offset, Active: Integer; State: TsvItemState;
  Columns: array of Integer);
const
  ColumnsName: array[0..1] of string = ('Name', 'Value');
var
  R, H, S, T: TRect;
  i: Integer;
  Txt: string;
begin
  Canvas.Brush.Style:= bsClear;
  R := Header;
  S := R;
  R.Right := Offset;
  FillGradient(Canvas, R, cGHeaderStart, cGHeaderEnd, True);
  R.Left := R.Right + 1;
  for i := Low(Columns) to High(Columns) do
  begin
    R.Right := R.Right + Columns[i];
    H := R;
    if i > 0 then
      H.Left := H.Left + 1;
    FillGradient(Canvas, H, cGHeaderStart, cGHeaderEnd, True);
    Canvas.Pen.Color := cDisabled;
    Canvas.MoveTo(H.Left - 1, H.Top);
    Canvas.LineTo(H.Left - 1, H.Bottom);
    Canvas.Font.Color := clBlack;
    T := H;
    if (State = svSelected) and (i = Active) then
    begin
      T.Top := T.Top + 1;
      T.Left := T.Left + 7;
    end
    else
      T.Left := T.Left + 6;
    txt := ColumnsName[i];
    DrawText(Canvas.Handle, PChar(txt), Length(txt), T,
      DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_LEFT or DT_VCENTER);
    R.Left := R.Right;
  end;
  Canvas.Pen.Color := cDisabled;
  Canvas.MoveTo(R.Left, H.Top);
  Canvas.LineTo(R.Left, H.Bottom);
  R := Header;
  R.Left := H.Right + 1;
  FillGradient(Canvas, R, cGHeaderStart, cGHeaderEnd, True);
  R := Header;
  Canvas.Pen.Color := cDisabled;
  Canvas.Rectangle(R);
end;

procedure TfrmInfo.viewStatListPaint(Sender: TObject; Canvas: TCanvas;
  Cell: TRect; IdxA, Idx: Integer; State: TsvItemState;
  Columns: array of Integer);
var
  R, T: TRect;
  i: Integer;
  Txt: string;
  Item: PStrItem;
begin
  Item := Items.Items[idx];
  R := Cell;
  if ((IdxA and 1) = 1) then
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Brush.Color := cLineHighLight;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(Cell);
  end;
  ItemPaintBasic(Canvas, Cell, State);
  if High(Columns) > 0 then
  begin
    R.Top := R.Top;
    if Item.str1 = '' then
    begin
      Canvas.Pen.Color := $00B07442;
      i := Cell.Top + ((Cell.Bottom - Cell.Top) shr 1);
      Canvas.MoveTo(Cell.Left, i);
      Canvas.LineTo(Cell.Right, i);
    end
    else
      for i := Low(Columns) to High(Columns) do
      begin
        R.Right := R.Left + Columns[i];
        T := R;
        case i of
          0:
            begin
              Canvas.Font.Color := $00905422;
              T.Left := T.Left + 3;
              txt := Item.str1;
              DrawText(Canvas.Handle, PChar(txt), Length(txt), T,
                DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_LEFT or
                DT_VCENTER);
            end;
          1:
            begin
              Canvas.Font.Color := clBlack;
              T.Left := T.Left + 3;
              txt := Item.str2;
              DrawText(Canvas.Handle, PChar(txt), Length(txt), T,
                DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX or DT_LEFT or
                DT_VCENTER);
            end;
        else
        end;
        R.Left := R.Right;
      end;
  end;
end;

procedure TfrmInfo.AddL(s1, s2: string);
var
  NewItem: PStrItem;
begin
  New(NewItem);
  NewItem.str1 := s1;
  Newitem.str2 := s2;
  viewStat.Items.Add(Items.Add(NewItem));
end;

function FileSize(fileName: wideString): Int64;
// returns file size in bytes or -1 if not found.
var
  sr: TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr) = 0 then
    result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) +
      Int64(sr.FindData.nFileSizeLow)
  else
    result := -1;
  FindClose(sr);
end;

function ExifDateToDateTime(dstr: string): TDateTime;
// Convert ExifDate To DateTime
type
  TConvert = packed record
    year: array[1..4] of char;
    f1: char;
    mon: array[1..2] of char;
    f2: char;
    day: array[1..2] of char;
    f3: char;
    hr: array[1..2] of char;
    f4: char;
    Min: array[1..2] of char;
    f5: char;
    sec: array[1..2] of char;
  end;
  PConvert = ^TConvert;
begin
  try
    if dstr <> '' then
      with PConvert(@dstr[1])^ do
        Result := EncodeDate(SysUtils.StrToInt(year), SysUtils.StrToInt(mon),
          SysUtils.StrToInt(day)) + EncodeTime(SysUtils.StrToInt(hr),
          SysUtils.StrToInt(Min), SysUtils.StrToInt(sec), 0)
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function ExtractDate(TimeIn: TDateTime): string;
// Convert DateTime to long date string
begin
  Result := FormatDateTime('d. mmmm yyyy', TimeIn);
end;

function ExtractTime(TimeIn: TDateTime): string;
// Convert DateTime to am/pm time string
begin
  Result := FormatDateTime('hh:mm:ss', TimeIn);
end;

function Dec2Frac(f: Double): String;
var
  df: Double;
  lUpperPart: Integer;
  lLowerPart: Integer;
begin
  lUpperPart := 1;
  lLowerPart := 1;
  df := lUpperPart / lLowerPart;
  While (df <> f) do
  begin
    If (df < f) Then
      lUpperPart := lUpperPart + 1
    Else
    begin
      lLowerPart := lLowerPart + 1;
      lUpperPart := Trunc(f * lLowerPart);
    end;
    df := lUpperPart / lLowerPart;
  end;
  if lLowerPart = 1 then
    result := IntToStr(lUpperPart)
  else
    result := '1/' + IntToStr(lLowerPart div lUpperPart);
    //result := IntToStr(lUpperPart) + '/' + IntToStr(lLowerPart);
end;

procedure TfrmInfo.GetExif(FName: string);
const
  Orientation: array[1..9] of string = ('Normal', 'Mirrored', 'Rotated 180',
    'Rotated 180, mirrored', 'Rotated 90 left, mirrored', 'Rotated 90 right',
    'Rotated 90 right, mirrored', 'Rotated 90 left', 'Unknown');
  ExpProg: array[0..8] of string = ('Unknown', 'Manual control', 'Normal',
    'Aperture priority', 'Shutter priority', 'Creative (slow program)',
    'Action(high-speed program', 'Portrait mode', 'Landscape mode');
  Exif: array[0..47] of string = (
    ('Make'), // EXIF_Make
    ('Model'), // EXIF_Model
    ('Filename'), // Filename
    ('Filesize'), // Filesize
    ('Image width'), // EXIF Image Width
    ('Image height'), // EXIF Image Height
    ('Date/time orignal'), // EXIF_DateTimeOriginal
    ('Date/time digitized'), // EXIF_DateTimeDigitized
    ('Software'), // EXIF_Software
    ('ISO speed rating'), // EXIF_ISOSpeedRatings
    ('F number'), // EXIF_FNumber
    ('Exposure time'), // EXIF_Exposure Time
    ('Focal length'), // EXIF_FocalLength
    ('Exposure programme'), // EXIF_ExposureProgram
    ('Light source'), // EXIF_LightSource
    ('Flash'), // EXIF_Flash
    ('Max aperture value'), // EXIF_MaxApertureValue
    ('Metering mode'), // EXIF_MeteringMode
    ('Exposure bias value'), // EXIF_ExposureBiasValue
    ('Brightness value'), // EXIF_BrightnessValue
    ('Sensing method'), // EXIF Sensing Method
    ('White Point'), // EXIF_WhitePoint
    ('Reference Black White'), // EXIF_ReferenceBlackWhite
    ('YCbCr Coefficients'), // EXIF_YCbCrCoefficients
    ('FlashPix Version'), // EXIF_FlashPixVersion
    ('Color Space'), // EXIF_ColorSpace
    ('Focal Plane X Resolution'), // EXIF_FocalPlaneXResolution
    ('Focal Plane Y Resolution'), // EXIF_FocalPlaneYResolution
    ('Focal Plane Resolution Unit'), // EXIF_FocalPlaneResolutionUnit
    ('Primary Chromaticities'), // EXIF_PrimaryChromaticities
    ('Exif version'), // EXIF_ExifVersion
    ('Compressed BitsPerPixel'), // EXIF_CompressedBitsPerPixel
    ('Resolution Unit'), // EXIF_ResolutionUnit
    ('YCbCr Positioning'), // EXIF_YCbCrPositioning
    ('Orientation'), // EXIF_Orientation
    ('X Resolution'), // EXIF_XResolution
    ('Y Resolution'), // EXIF_YResolution
    ('Image Description'), // EXIF_ImageDescription
    ('Scene Type'), // EXIF_SceneType
    ('Subject Distance'), // EXIF_SubjectDistance
    ('Subsec Time'), // EXIF_SubsecTime
    ('Subsec Time Original'), // EXIF_SubsecTimeOriginal
    ('Related Sound File'), // EXIF_RelatedSoundFile
    ('Exposure Index'), // EXIF_ExposureIndex
    ('File Source'), // EXIF_FileSource
    ('Copyright'), // EXIF_Copyright
    ('User Comment Code'), // EXIF_UserCommentCode
    ('User Comment')); // EXIF_UserComment
var
  MeteringMode: string;
  LightSource: string;
  Flash: string;
  SensingMethod: string;
  dt: TDateTime;
  d: string;
  t: string;
  ImageIO: TImageEnIO;
  MS: TMemoryStream;
  Entry: PThumbData;
begin
  Entry:= PThumbData(frmMain.Items[frmMain.svMain.Selection[0]]);
  if FileExists(FName) then
  begin
    AddL(exif[2], ExtractFilename(FName));
    AddL(exif[3], BytesToStr(FileSize(FName)));
    //AddL('Image type', frmMain.svMain.GetItemType(Entry));
    AddL(exif[4], SysUtils.IntToStr(Entry.IWidth));
    AddL(exif[5], SysUtils.IntToStr(Entry.IHeight));
  end;
  ImageIO := TImageEnIO.Create(nil);
  try
    if frmMain.svMain.Selection.Count > 0 then
    begin
      MS := TMemoryStream(Entry.Image);
      MS.Position := 0;
      ImageIO.LoadFromStreamJpeg(MS);
    end;

    if ImageIO.Params.EXIF_HasEXIFData then
    begin
      //viewStat.BeginUpdate;
      AddL('X resolution', floattostr(ImageIO.Params.EXIF_XResolution));
      AddL('Y resolution', floattostr(ImageIO.Params.EXIF_YResolution));
      AddL('', '');
      if ImageIO.Params.EXIF_Software <> '' then
        AddL(exif[8], Trim(string(ImageIO.Params.EXIF_Software)));

      if ImageIO.Params.EXIF_DateTime <> '' then
      begin
        dt := ExifDateToDateTime(string(ImageIO.Params.EXIF_DateTime));
        d := ExtractDate(dt);
        t := ExtractTime(dt);
        AddL('Date/time', d + ' at ' + t);
      end;

      if ImageIO.Params.EXIF_DateTimeOriginal <> '' then
      begin
        dt := ExifDateToDateTime(string(ImageIO.Params.EXIF_DateTimeOriginal));
        d := ExtractDate(dt);
        t := ExtractTime(dt);
        AddL(exif[6], d + ' at ' + t);
      end;

      if ImageIO.Params.EXIF_DateTimeDigitized <> '' then
      begin
        dt := ExifDateToDateTime(string(ImageIO.Params.EXIF_DateTimeDigitized));
        d := ExtractDate(dt);
        t := ExtractTime(dt);
        AddL(exif[7], d + ' at ' + t);
      end;

      AddL('Image description',
        Trim(string(ImageIO.Params.EXIF_ImageDescription)));

      AddL('Author', Trim(string(ImageIO.Params.EXIF_Artist)));

      AddL('Copyright', Trim(string(ImageIO.Params.EXIF_Copyright)));

      AddL('', '');

      if ImageIO.Params.EXIF_Make <> '' then
        AddL(exif[0], Trim(string(ImageIO.Params.EXIF_Make)));

      if ImageIO.Params.EXIF_Model <> '' then
        AddL(exif[1], Trim(string(ImageIO.Params.EXIF_Model)));

      if ImageIO.Params.EXIF_FNumber > -1 then
        AddL(exif[10], 'f/' + FloatToStr(ImageIO.Params.EXIF_FNumber));

      if ImageIO.Params.EXIF_ExposureTime > 0 then
        AddL(exif[11], Dec2Frac(ImageIO.Params.EXIF_ExposureTime) + ' s');

      if ImageIO.Params.EXIF_ISOSpeedRatings[0] = 0 then
        AddL(exif[9], 'Unknown - Camera Specific EXIF')
      else
        AddL(exif[9], 'ISO ' + IntToStr(ImageIO.Params.EXIF_ISOSpeedRatings[0]));

      if ImageIO.Params.EXIF_ExposureBiasValue > -1 then
        AddL(Exif[18], 'EV ' +
          Format('%-8.2f', [ImageIO.Params.EXIF_ExposureBiasValue]));
          //FloatToStr(ImageIO.Params.EXIF_ExposureBiasValue));

      if ImageIO.Params.EXIF_FocalLength > -1 then
        AddL(exif[12], FloatToStr(ImageIO.Params.EXIF_FocalLength) + ' mm');

      if ImageIO.Params.EXIF_MaxApertureValue > 0 then
        AddL(Exif[16], FloattoStr(ImageIO.Params.EXIF_MaxApertureValue));

      case ImageIO.Params.EXIF_MeteringMode of
        0:
          MeteringMode := 'Unknown';
        1:
          MeteringMode := 'Average';
        2:
          MeteringMode := 'Center weighted average';
        3:
          MeteringMode := 'Spot';
        4:
          MeteringMode := 'MultiSpot';
        5:
          MeteringMode := 'Pattern';
        6:
          MeteringMode := 'Partial';
        255:
          MeteringMode := 'Other';
      end; // case
      AddL(Exif[17], MeteringMode);

      if ImageIO.Params.EXIF_SubjectDistance > -1 then
        AddL(Exif[39], Format('%-8.2f', [ImageIO.Params.EXIF_SubjectDistance]));

      case ImageIO.Params.EXIF_Flash of
        0:
          Flash := 'No flash';
        1:
          Flash := 'Flash';
        5:
          Flash := 'Flash no strobe return';
        7:
          Flash := 'Flash strobe return';
        9:
          Flash := 'Flash, compulsory';
        13:
          Flash := 'Flash, compulsory, no strobe return';
        15:
          Flash := 'Flash compulsory, strobe return';
        16:
          Flash := 'No flash, compulsory';
        24:
          Flash := 'No flash, auto';
        25:
          Flash := 'Flash, auto';
        65:
          Flash := 'Flash, red-eye';
        69:
          Flash := 'Flash, red_eye, no strobe return';
        71:
          Flash := 'Flash, red_eye, strobe return';
        73:
          Flash := 'Flash, compulsory, red_eye';
        77:
          Flash := 'Flash, compulsory, red_eye, no strobe return';
        79:
          Flash := 'Flash, compulsory, red_eye, strobe return';
        89:
          Flash := 'Flash, auto, red_eye';
        93:
          Flash := 'Flash, auto, red_eye, no strobe return';
        95:
          Flash := 'Flash, auto, red_eye, strobe return';
      else
        Flash := 'Unknown';
      end; // case
      AddL(Exif[15], Flash);

      AddL('Aperture value', FloatToStr(ImageIO.Params.EXIF_ApertureValue));

      AddL(Exif[13], ExpProg[ImageIO.Params.EXIF_ExposureProgram]);

      if ImageIO.Params.EXIF_ExifVersion <> '' then
        AddL('EXIF version', Trim(string(ImageIO.Params.EXIF_ExifVersion)));

      // EXIF_LightSource
      case ImageIO.Params.EXIF_LightSource of
        0:
          LightSource := 'Unknown';
        1:
          LightSource := 'Daylight';
        2:
          LightSource := 'Fluorescent';
        3:
          LightSource := 'Tungsten';
        10:
          LightSource := 'Flash';
        17:
          LightSource := 'Standard light A';
        18:
          LightSource := 'Standard light B';
        19:
          LightSource := 'Standard light C';
        20:
          LightSource := 'D55';
        21:
          LightSource := 'D65';
        22:
          LightSource := 'D75';
        255:
          LightSource := 'Other';
      else
        LightSource := 'Unknown';
      end; // case
      AddL(Exif[14], LightSource);

      if ImageIO.Params.EXIF_Orientation in [1..8] then
        AddL('Orientation', Orientation[ImageIO.Params.EXIF_Orientation])
      else
        AddL('Orientation', 'Unknown');

      if ImageIO.Params.EXIF_BrightnessValue > -1 then
        AddL(Exif[19], Format('%-8.2f', [ImageIO.Params.EXIF_BrightnessValue]));

      case ImageIO.Params.EXIF_SensingMethod of
        0:
          SensingMethod := 'Unknown';
        2:
          SensingMethod := '1 chip color area sensor';
      end; // case

      //AddL(Exif[20], SensingMethod);

      {
      if ImageEnView1.IO.Params.EXIF_WhitePoint[0] > -1 then
        ListView2.Items.Item[21].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_WhitePoint[0]]));
      // EXIF_ReferenceBlackWhite
      if ImageEnView1.IO.Params.EXIF_ReferenceBlackWhite[0] > -1 then
        ListView2.Items.Item[22].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_ReferenceBlackWhite[0]
          ]));
      // EXIF_YCbCrCoefficients
      if ImageEnView1.IO.Params.EXIF_YCbCrCoefficients[0] <> -1 then
        ListView2.Items.Item[23].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_YCbCrCoefficients[0]]
          )); // was [1]
      // EXIF_FlashPixVersion
      if ImageEnView1.IO.Params.EXIF_FlashPixVersion <> '' then
        ListView2.Items.Item[24].SubItems.Add(string(ImageEnView1.IO.Params.EXIF_FlashPixVersion));
      // EXIF_ColorSpace
      case ImageEnView1.IO.Params.EXIF_ColorSpace of
        0:
          ColorSpace := 'Inknown';
        1:
          ColorSpace := 'sRGB';
        65535:
          ColorSpace := 'Uncalibrated'
      end; // case
      ListView2.Items.Item[25].SubItems.Add(ColorSpace);
      // EXIF_FocalPlaneXResolution
      if ImageEnView1.IO.Params.EXIF_FocalPlaneXResolution <> -1 then
        ListView2.Items.Item[26].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_FocalPlaneXResolution])
          );
      // EXIF_FocalPlaneYResolution
      if ImageEnView1.IO.Params.EXIF_FocalPlaneYResolution <> -1 then
        ListView2.Items.Item[27].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_FocalPlaneYResolution])
          );
      // EXIF_FocalPlaneResolutionUnit
      case ImageEnView1.IO.Params.EXIF_FocalPlaneResolutionUnit of
        0:
          FocalPlaneResolutionUnit := 'Unknown';
        1:
          FocalPlaneResolutionUnit := 'No-unit';
        2:
          FocalPlaneResolutionUnit := 'Inches';
        3:
          FocalPlaneResolutionUnit := 'Centimeter';
      end; // case
      ListView2.Items.Item[28].SubItems.Add(FocalPlaneResolutionUnit);
      // EXIF_PrimaryChromaticities
      if ImageEnView1.IO.Params.EXIF_PrimaryChromaticities[0] <> -1 then
        ListView2.Items.Item[29].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_PrimaryChromaticities[0
          ]]));
      // EXIF_CompressedBitsPerPixel
      if ImageEnView1.IO.Params.EXIF_CompressedBitsPerPixel <> -1 then
        ListView2.Items.Item[31].SubItems.Add(Format('%-8.1f',
          [ImageEnView1.IO.Params.EXIF_CompressedBitsPerPixel]
          ));
      // EXIF_ResolutionUnit
      case ImageEnView1.IO.Params.EXIF_ResolutionUnit of
        0:
          ResolutionUnit := 'Unknown';
        1:
          ResolutionUnit := 'No-unit';
        2:
          ResolutionUnit := 'Inches';
        3:
          ResolutionUnit := 'Centimeter';
      end; // case
      ListView2.Items.Item[32].SubItems.Add(ResolutionUnit);
      // EXIF_YCbCrPositioning
      case ImageEnView1.IO.Params.EXIF_YCbCrPositioning of
        0:
          YCbCrPositioning := 'Unknown';
        1:
          YCbCrPositioning := 'Center of pixel array';
        2:
          YCbCrPositioning := 'Datum point';
      end; // case
      ListView2.Items.Item[33].SubItems.Add(YCbCrPositioning);

      // EXIF_XResolution
      if ImageEnView1.IO.Params.EXIF_XResolution <> -1 then
        ListView2.Items.Item[35].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_XResolution]));
      // EXIF_YResolution
      if ImageEnView1.IO.Params.EXIF_YResolution <> -1 then
        ListView2.Items.Item[36].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_YResolution]));
      // EXIF_ImageDescription
      if ImageEnView1.IO.Params.EXIF_ImageDescription <> '' then
        ListView2.Items.Item[37].SubItems.Add(Trim(string(ImageEnView1.IO.Params.EXIF_ImageDescription)));
      // EXIF_SceneType
      if ImageEnView1.IO.Params.EXIF_SceneType <> -1 then
        ListView2.Items.Item[38].SubItems.Add(IntToStr(ImageEnView1.IO.Params.EXIF_SceneType));
      // EXIF_SubsecTime
      if ImageEnView1.IO.Params.EXIF_SubsecTime <> '' then
        ListView2.Items.Item[40].SubItems.Add(Trim(string(ImageEnView1.IO.Params.EXIF_SubsecTime)));
      // EXIF_SubsecTimeOriginal
      if ImageEnView1.IO.Params.EXIF_SubsecTimeOriginal <> '' then
        ListView2.Items.Item[41].SubItems.Add(Trim(string(ImageEnView1.IO.Params.EXIF_SubsecTimeOriginal)));
      // EXIF_RelatedSoundFile
      if ImageEnView1.IO.Params.EXIF_RelatedSoundFile <> '' then
        ListView2.Items.Item[42].SubItems.Add(string(ImageEnView1.IO.Params.EXIF_RelatedSoundFile));
      // EXIF_ExposureIndex
      if ImageEnView1.IO.Params.EXIF_ExposureIndex > -1 then
        ListView2.Items.Item[43].SubItems.Add(Format('%-8.2f',
          [ImageEnView1.IO.Params.EXIF_ExposureIndex]));
      // EXIF_FileSource
      if ImageEnView1.IO.Params.EXIF_FileSource = 3 then
        FileSource := 'Digital Still Camera'
      else
        FileSource := '';
      ListView2.Items.Item[44].SubItems.Add(FileSource);
      // EXIF_Copyright
      if ImageEnView1.IO.Params.EXIF_Copyright <> '' then
        ListView2.Items.Item[45].SubItems.Add(Trim(string(ImageEnView1.IO.Params.EXIF_Copyright)));
      // EXIF_UserCommentCode
      if ImageEnView1.IO.Params.EXIF_UserCommentCode <> '' then
        ListView2.Items.Item[46].SubItems.Add(Trim(string(ImageEnView1.IO.Params.EXIF_UserCommentCode)));
      // EXIF_UserComment
      if ImageEnView1.IO.Params.EXIF_UserComment <> '' then
        ListView2.Items.Item[47].SubItems.Add(Trim(ImageEnView1.IO.Params.EXIF_UserComment));
      }

    end;
  finally
    ImageIO.Free;
  end;
  viewStat.EndUpdate;
  viewStat.CalcView(True);
end;

procedure TfrmInfo.FormCreate(Sender: TObject);
begin
  Items := TList.Create;
  viewStat.UseAsList := True;
end;

procedure TfrmInfo.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := Items.Count - 1 downto 0 do
    Dispose(Items[i]);
  Items.Free;
end;

procedure VistaGradient(const Img: TImage; const ARect: TRect;
  const c1, c2: Byte; const c3, c4: TColor; const Center, Reverse: Boolean);
type
  PRGB = ^TRGB;
  TRGB = record b, g, r: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBARRAY = array[0..0] of TRGB;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3: Integer;
  x, y, w, h, j: Integer;
  i, w1: Integer;
  Row: PRGBArray;
  C: TRGB;
  slMain, slSize, slPtr: Integer;
  Color: Integer;
  QCol: array of TRGB;
  g: Byte;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Width := (ARect.Right - ARect.Left) - 1;
  bmp.Height := (ARect.Bottom - ARect.Top) - 1;
  h := bmp.Height;
  w := bmp.Width;
  Color := ColorToRGB(c3);
  rc1 := Byte(Color);
  gc1 := Byte(Color shr 8);
  bc1 := Byte(Color shr 16);
  Color := ColorToRGB(c4);
  rc2 := Byte(Color);
  gc2 := Byte(Color shr 8);
  bc2 := Byte(Color shr 16);
  SetLength(QCol, h);
  for i := 0 to h - 1 do
  begin
    if Reverse then
    begin
      QCol[i].r := Byte(rc1 + (((rc2 - rc1) * i) div h));
      QCol[i].g := Byte(gc1 + (((gc2 - gc1) * i) div h));
      QCol[i].b := Byte(bc1 + (((bc2 - bc1) * i) div h));
    end
    else
    begin
      QCol[i].r := Byte(rc2 + (((rc1 - rc2) * i) div h));
      QCol[i].g := Byte(gc2 + (((gc1 - gc2) * i) div h));
      QCol[i].b := Byte(bc2 + (((bc1 - bc2) * i) div h));
    end;
  end;
  w1 := w - 1;
  if Center then
    w := (w shr 1) + (w and 1);
  slMain := Integer(bmp.ScanLine[0]); // Init scanline accsess
  slSize := Integer(bmp.ScanLine[1]) - slMain;
  for x := 0 to w - 1 do
  begin // Paint gradient
    j := (255 - c1) + MulDiv(c1, x, w);
    slPtr := slMain;
    for y := 0 to h - 1 do
    begin
      Row := PRGBArray(slPtr);
      Row[x].r := j * (QCol[y].r - rc1) shr 8 + rc1;
      Row[x].g := j * (QCol[y].g - gc1) shr 8 + gc1;
      Row[x].b := j * (QCol[y].b - bc1) shr 8 + bc1;
      if (Center) and (x < (w1 - x)) then
      begin
        Row[w1 - x].r := Row[x].r;
        Row[w1 - x].g := Row[x].g;
        Row[w1 - x].b := Row[x].b;
      end;
      slPtr := slPtr + slSize;
    end;
  end;
  QCol := nil;
  Img.Picture.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure TfrmInfo.FormShow(Sender: TObject);
var
  s: String;
  p: PThumbData;
begin
  VistaGradient(imgBottom, Rect(0, 0, ClientWidth, 4), 192, 0, $00F8F8F8,
    $00E0E0E0, True, False);
  p := frmMain.Items[frmMain.svMain.Selection[0]];
  s := frmMain.Paths[p.PathID] + p.Name;
  GetExif(s);
end;

end.

