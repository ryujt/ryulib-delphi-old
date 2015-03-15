unit CodeDiff;

interface

uses
  SysUtils, Classes, Dialogs;

type
  TCodeLines = class(TStringList)
  private
    FPosition : integer;
    FBookMark : integer;
    function GetEOF: boolean;
  public
    procedure First;
    procedure Next;
    procedure SaveBookMark;
    procedure GoToBookMark;
    function IndexOfFrom(Text:string; Index:integer):integer;
  published
    property EOF : boolean read GetEOF;
    property Position : integer read FPosition;
  end;

  TCodeDiff = class(TComponent)
  private
    FSaveDropLine: boolean;
    FDifferent: boolean;
    procedure SetSaveDropLine(const Value: boolean);
  public
    CodeSrc : TCodeLines;
    CodeDst : TCodeLines;
    CodeOut : TStringList;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure Execute;
  published
    property SaveDropLine : boolean read FSaveDropLine write SetSaveDropLine;
             // 원본에서 사라진 부분도 표시할 것인가?
    property Different : boolean read FDifferent;
  end;

implementation

{ TCodeDiff }

constructor TCodeDiff.Create(AOwner: TComponent);
begin
  inherited;

  FSaveDropLine:= true;

  CodeSrc:= TCodeLines.Create;
  CodeDst:= TCodeLines.Create;
  CodeOut:= TStringList.Create;
end;

destructor TCodeDiff.Destroy;
begin
  CodeSrc.Free;
  CodeDst.Free;
  CodeOut.Free;

  inherited;
end;

procedure TCodeDiff.Execute;
var
  iIndex, iIndexNext : integer;
begin
  FDifferent:= false;
  iIndex:= -1;
  CodeSrc.First;
  CodeDst.First;
  CodeOut.Clear;
  while CodeSrc.EOF = false do begin
    iIndex:= CodeDst.IndexOfFrom(CodeSrc.Strings[CodeSrc.Position], CodeDst.Position);
    if CodeSrc.Position = (CodeSrc.Count-1) then iIndexNext:= -1
    else iIndexNext:= CodeDst.IndexOfFrom(CodeSrc.Strings[CodeSrc.Position+1], CodeDst.Position);

    // Case 1 : 아예 없을 때
    if iIndex = -1 then begin
      if SaveDropLine then CodeOut.Add('-' + CodeSrc.Strings[CodeSrc.Position]);
      FDifferent:= true;
      CodeSrc.Next;
      Continue;
    end;

    // Case 2 : 둘 다 현재 라인에서 같을 때
    if CodeDst.Position = iIndex then begin
      CodeOut.Add(' ' + CodeDst.Strings[CodeDst.Position]);
      CodeSrc.Next;
      CodeDst.Next;
      Continue;
    end;

    // Case 3 마지막 라인의 경우 아직까지 결말이 안났다면?
    if CodeSrc.Position = (CodeSrc.Count-1) then Break;

    // Case 4 : 자신의 이후의 라인이 먼저 발견되면
    if iIndex > iIndexNext then begin
      if SaveDropLine then CodeOut.Add('-' + CodeSrc.Strings[CodeSrc.Position]);
      FDifferent:= true;
      CodeSrc.Next;
      Continue;
    end;

    // Case 5 :
    while CodeDst.Position < iIndex do begin
      CodeOut.Add('+' + CodeDst.Strings[CodeDst.Position]);
      FDifferent:= true;
      CodeDst.Next;
    end;
    CodeOut.Add(' ' + CodeSrc.Strings[CodeSrc.Position]);
    CodeSrc.Next;
    CodeDst.Next;
  end;

  while CodeDst.EOF = false do begin
    if (iIndex <> -1) and (iIndex = CodeDst.Position) then begin
      CodeOut.Add(' ' + CodeDst.Strings[CodeDst.Position]);
      iIndex:= -1;
      CodeDst.Next;
      Continue;
    end;

    CodeOut.Add('+' + CodeDst.Strings[CodeDst.Position]);
    FDifferent:= true;

    CodeDst.Next;
  end;
end;

procedure TCodeDiff.SetSaveDropLine(const Value: boolean);
begin
  FSaveDropLine := Value;
end;

{ TCodeLines }

procedure TCodeLines.SaveBookMark;
begin
  FBookMark:= FPosition;
end;

procedure TCodeLines.First;
begin
  FPosition:= 0;
  FBookMark:= -1;
end;

function TCodeLines.GetEOF: boolean;
begin
  Result:= FPosition >= Count;
end;

procedure TCodeLines.GoToBookMark;
begin
  if FBookMark = -1 then Exit;

  FPosition:= FBookMark;
  FBookMark:= -1;
end;

function TCodeLines.IndexOfFrom(Text: string; Index: integer): integer;
var
  Loop : integer;
begin
  Result:= -1;

  for Loop := Index to Count - 1 do
    if Text = Strings[Loop] then begin
      Result:= Loop;
      Break;
    end;
end;

procedure TCodeLines.Next;
begin
  FPosition:= FPosition + 1;
end;

end.
