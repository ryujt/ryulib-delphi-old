unit ChatCanvas;

interface

uses
  GlassContols, Strg,
  Windows, Messages, SysUtils, Variants, Classes, Graphics;

type
  TChatCanvas = class (TGlassContols)
  private
    FOldWidth, FOldHeight : integer;
    FSource : TStringList;
    FLines : TStringList;
    function cut_Text(var AText:string):string;
    procedure do_Resizing;
  private
    FLineMargine: integer;
    FPosition: integer;
    FLeftMargine: integer;
    procedure SetLineMargine(const Value: integer);
    procedure SetPosition(const Value: integer);
    function GetCount: integer;
    procedure SetLeftMargine(const Value: integer);
  protected
    procedure do_Repaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddText(AText:string);
  published
    property Position : integer read FPosition write SetPosition;
    property Count : integer read GetCount;
    property LeftMargine : integer read FLeftMargine write SetLeftMargine;
    property LineMargine : integer read FLineMargine write SetLineMargine;
  end;

implementation

{ TChatCanvas }

procedure TChatCanvas.AddText(AText: string);
var
  sLine : string;
begin
  FSource.Add(AText);

  AText := StringReplace(AText, #13#10, #13, [rfReplaceAll]);
  AText := StringReplace(AText, #10, #13, [rfReplaceAll]);
  AText := StringReplace(AText, #10#13, #13, [rfReplaceAll]);

  sLine := cut_Text(AText);
  while sLine <> '' do begin
    FLines.Add(sLine);

    if AText = '' then sLine := ''
    else begin
      AText := Strg.CharString(' ', LeftMargine) + AText;
      sLine := cut_Text(AText);
    end;
  end;
  
  FPosition := Count-1;

  Repaint;
end;

procedure TChatCanvas.Clear;
begin
  FSource.Clear;
  FLines.Clear;
end;

constructor TChatCanvas.Create(AOwner: TComponent);
begin
  inherited;

  Font.Color := clBlack;
  Font.Size := 10;
  Brush.Style := bsClear;
  FPosition := 0;

  // 공백문자 단위이다.
  FLeftMargine := 5;

  // 픽셀 단위이다.
  FLineMargine := 2;

  FSource := TStringList.Create;
  FLines := TStringList.Create;
end;

function TChatCanvas.cut_Text(var AText: string): string;
var
  Loop : integer;
  sLine, sCopy, sTemp : widestring;
begin
  sLine := AText;
  sCopy := '';
  sTemp := '';
  Result := '';

  for Loop := 1 to Length(sLine) do begin
    if Copy(sLine, Loop, 1) = #13 then begin
      Delete(sLine, Loop, 1);
      Break;
    end;
    
    sTemp := sTemp + Copy(sLine, Loop, 1);
    if get_TextWidth(sTemp) > Width then Break;

    sCopy := sTemp;
  end;

  Delete(sLine, 1, Length(sCopy));
  AText := sLine;
  Result := sCopy;
end;

destructor TChatCanvas.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FLines);

  inherited;
end;

procedure TChatCanvas.do_Repaint;
var
  Loop, iStartPosition, iTextHeight, iLineCount, iLineSize, iHeight : integer;
begin
  if Count = 0 then Exit;

  do_EraseBackGround;

  do_Resizing;

  iTextHeight := GlassForm.Bitmap.Canvas.TextHeight('1y') + LineMargine;

  iLineSize := Height div iTextHeight;

  iStartPosition := Position;
  if (Count-iStartPosition) < iLineSize then begin
    iStartPosition := iStartPosition - (iLineSize - (Count-iStartPosition) + 1);
    if iStartPosition < 0 then iStartPosition := 0;    
  end;

  iLineCount := 0;
  for Loop := iStartPosition to Count-1 do begin
    iHeight := Top+(iTextHeight*iLineCount);
    Inc(iLineCount);

    GlassForm.Bitmap.Canvas.TextOut(Left, iHeight, FLines[Loop]);

    if iHeight > Height then Break;
  end;
end;

procedure TChatCanvas.do_Resizing;
var
  iTextHeight : integer;
begin
  if (Width = FOldWidth) and (Height = FOldHeight) then Exit;

  FOldWidth  := Width;
  FOldHeight := Height;

  iTextHeight := GlassForm.Bitmap.Canvas.TextHeight('1y') + LineMargine;

  // Todo : 
end;

function TChatCanvas.GetCount: integer;
begin
  Result := FLines.Count;
end;

procedure TChatCanvas.SetLeftMargine(const Value: integer);
begin
  FLeftMargine := Value;
  Repaint;
end;

procedure TChatCanvas.SetLineMargine(const Value: integer);
begin
  FLineMargine := Value;
  Repaint;
end;

procedure TChatCanvas.SetPosition(const Value: integer);
begin
  FPosition := Value;
  Repaint;
end;

end.
