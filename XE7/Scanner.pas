unit Scanner;

interface

uses
  Windows, Classes, SysUtils;

const
  SPECIAL_CHAR : string = '`-=[]\;,.~!@#$%^&*()+|:<>?';

type
  TTokenType = (
    ttNone, ttWhiteSpace, ttSpecialChar, ttString, ttNumber, ttIdentifier, ttComment
  );

  TToken = record
  strict private
    FOriginalText : string;
  private
    FLowerCaseText: string;
    procedure SetText(const AText:string);
    function GetText: string;
    function GetLowerCaseText: string;
  public
    Line :integer;
    Col : integer;
    TokenType : TTokenType;
  public
    property LowerCaseText : string read GetLowerCaseText;
    property Text : string read GetText;
    property OriginalText : string read FOriginalText;
  end;
  PToken = ^TToken;

  TScanner = class
  strict private
    FState : TObject;
    FText : string;
    FIndex : integer;
  private
    FLine :integer;
    FCol : integer;
    FToken : TToken;
  private
    // NextChar 처리
    FStateNormal,

    // CurrentChar 처리, 이전에 스택에 푸시해야 할 처리 내용이 있다.
    FStatePushed,

    FStateSlash, FStateSlashComment,
    FStateBraceComment,
    FStateSlashAsteriskComment, FSTateAsteriskInComment,
    FStateStringBegin, FStateStringEscape,
    FStateNumber,
    FStateIdentifier : TObject;
    procedure SetState(AState:TObject);
  private
    function CurrentChar:Char;
    function NextChar:Char;
  private
    FPascalStyle: boolean;
    FUseStringEscape: boolean;
    procedure set_Token(ALine,ACol:integer; ATokenType:TTokenType; const AText:string);
    procedure SetPascalStyle(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // TODO: FileStream 이용하는 메소드 추가

    procedure SetText(AText:string);
    function GetNextToken:TToken;

    function IsEOF:boolean;
  public
    property PascalStyle : boolean read FPascalStyle write SetPascalStyle;
    property Token : TToken read FToken;

    /// 문자열 중에 Escape를 처리할 지 여부
    property UseStringEscape :boolean read FUseStringEscape;
 end;

implementation

type
  TState = class
  private
    FLine : integer;
    FCol : integer;
    FBuffer : string;
    FScanner : TScanner;
  public
    constructor Create(AScanner:TScanner); reintroduce;

    procedure ActionIn(AOld:TState); virtual;
    procedure Scan; virtual; abstract;
  end;

  TStateNormal = class (TState)
  private
  public
    procedure Scan; override;
  end;

  TStatePushed = class (TState)
  private
  public
    procedure Scan; override;
  end;

  TStateSlash = class (TState)
  private
  public
    procedure Scan; override;
  end;

  TStateSlashComment = class (TState)
  private
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

  TStateBraceComment = class (TState)
  private
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

  TStateSlashAsteriskComment = class (TState)
  private
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

  TSTateAsteriskInComment = class (TState)
  private
  public
    procedure Scan; override;
  end;

  TStateStringBegin = class (TState)
  private
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

  TStateStringEscape = class (TState)
  private
  public
    procedure Scan; override;
  end;

  TStateNumber = class (TState)
  private
    FHasDot : boolean;
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

  TStateIdentifier = class (TState)
  private
  public
    procedure ActionIn(AOld:TState); override;
    procedure Scan; override;
  end;

{ TScanner }

constructor TScanner.Create;
begin
  inherited;

  FPascalStyle := true;
  FUseStringEscape := false;

  FStateNormal               := TStateNormal.Create(Self);
  FStatePushed               := TStatePushed.Create(Self);
  FStateSlash                := TStateSlash.Create(Self);
  FStateSlashComment         := TStateSlashComment.Create(Self);
  FStateBraceComment         := TStateBraceComment.Create(Self);
  FStateSlashAsteriskComment := TStateSlashAsteriskComment.Create(Self);
  FSTateAsteriskInComment    := TSTateAsteriskInComment.Create(Self);
  FStateStringBegin          := TStateStringBegin.Create(Self);
  FStateStringEscape         := TStateStringEscape.Create(Self);
  FStateNumber               := TStateNumber.Create(Self);
  FStateIdentifier           := TStateIdentifier.Create(Self);

  FState := FStateNormal;
end;

function TScanner.CurrentChar: Char;
begin
  if FIndex = 0 then Result := #0
  else Result := FText[FIndex];

  {$IFDEF DEBUG}
//  OutputDebugString(PChar(Format(
//    'TScanner.CurrentChar: FIndex=%d, Length(FText)=%d, Result=%s', [FIndex, Length(FText), Result]
//  )));
  {$ENDIF}
end;

destructor TScanner.Destroy;
begin
  FreeAndNil(FStateNormal);
  FreeAndNil(FStatePushed);
  FreeAndNil(FStateSlash);
  FreeAndNil(FStateSlashComment);
  FreeAndNil(FStateSlashAsteriskComment);
  FreeAndNil(FSTateAsteriskInComment);
  FreeAndNil(FStateStringBegin);
  FreeAndNil(FStateStringEscape);
  FreeAndNil(FStateNumber);
  FreeAndNil(FStateIdentifier);

  inherited;
end;

procedure TScanner.set_Token(ALine, ACol: integer; ATokenType: TTokenType; const AText: string);
begin
  FToken.Line      := ALine;
  FToken.Col       := ACol;
  FToken.TokenType := ATokenType;

  FToken.SetText( AText );
end;

function TScanner.GetNextToken:TToken;
begin
  FToken.TokenType := ttNone;

  while not IsEOF do begin
    TState(FState).Scan;
    if FToken.TokenType <> ttNone then Break;
  end;

  Result := FToken;
end;

function TScanner.IsEOF: boolean;
begin
  Result := FIndex > Length(FText);
end;

function TScanner.NextChar: Char;
begin
  Inc(FIndex);
  if FIndex > Length(FText) then Result := #0
  else Result := FText[FIndex];

  if Result <> #10 then FCol := FCol + 1;
  if Result = #13 then begin
    FLine := FLine + 1;
    FCol := 0;
  end;

  {$IFDEF DEBUG}
//  OutputDebugString(PChar(Format(
//    'TScanner.NextChar: FLine=%d, FCol=%d, Result=%s', [FLine, FCol, Result]
//  )));
  {$ENDIF}
end;

procedure TScanner.SetPascalStyle(const Value: boolean);
begin
  FPascalStyle := Value;
  FUseStringEscape := not Value;
end;

procedure TScanner.SetState(AState: TObject);
var
  Old : TObject;
begin
  Old := FState;
  FState := AState;
  TState(FState).ActionIn(TState(Old));

  {$IFDEF DEBUG}
//  OutputDebugString(PChar(Format(
//    'TScanner.SetState: New=%s, Old=%s', [AState.ClassName, Old.ClassName]
//  )));
  {$ENDIF}
end;

procedure TScanner.SetText(AText: string);
begin
  FIndex := 0;
  FText := AText;

  FLine := 1;
  FCol := 0;

  FToken.TokenType := ttNone;

  FState := FStateNormal;
end;

{ TState }

procedure TState.ActionIn(AOld:TState);
begin
  FLine := FScanner.FLine;
  FCol := FScanner.FCol;
end;

constructor TState.Create(AScanner: TScanner);
begin
  inherited Create;

  FScanner := AScanner;
end;

{ TStateNormal }

procedure TStateNormal.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  if Pos(Ch, SPECIAL_CHAR) > 0 then begin
    FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);
    Exit;
  end;

  case Ch of
    '''': if FScanner.PascalStyle then FScanner.SetState(FScanner.FStateStringBegin)
          else FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);

    '"': if not FScanner.PascalStyle then FScanner.SetState(FScanner.FStateStringBegin)
          else FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);

    '{': if FScanner.PascalStyle then FScanner.SetState(FScanner.FStateBraceComment)
         else FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);

    '/': FScanner.SetState(FScanner.FStateSlash);
    '0'..'9': FScanner.SetState(FScanner.FStateNumber);
    '_', 'a'..'z', 'A'..'Z': FScanner.SetState(FScanner.FStateIdentifier);
    else FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttWhiteSpace, Ch);
  end;
end;

{ TStatePushed }

procedure TStatePushed.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.CurrentChar;

  if Pos(Ch, SPECIAL_CHAR) > 0 then begin
    FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);
    FScanner.SetState(FScanner.FStateNormal);
    Exit;
  end;

  case Ch of
    '''': if FScanner.PascalStyle then FScanner.SetState(FScanner.FStateStringBegin)
          else begin
            FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);
            FScanner.SetState(FScanner.FStateNormal);
          end;

    '"': if not FScanner.PascalStyle then FScanner.SetState(FScanner.FStateStringBegin)
          else begin
            FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);
            FScanner.SetState(FScanner.FStateNormal);
          end;

    '{': if FScanner.PascalStyle then FScanner.SetState(FScanner.FStateBraceComment)
         else begin
           FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttSpecialChar, Ch);
           FScanner.SetState(FScanner.FStateNormal);
         end;

    '/': FScanner.SetState(FScanner.FStateSlash);
    '0'..'9': FScanner.SetState(FScanner.FStateNumber);
    'a'..'z', 'A'..'Z': FScanner.SetState(FScanner.FStateIdentifier);

    else begin
      FScanner.SetState(FScanner.FStateNormal);
      FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttWhiteSpace, Ch);
    end;
  end;
end;

{ TStateSlash }

procedure TStateSlash.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '/': FScanner.SetState(FScanner.FStateSlashComment);
    '*': FScanner.SetState(FScanner.FStateSlashAsteriskComment);

    else begin
      FScanner.set_Token(FLine, FCol, ttSpecialChar, '/');
      FScanner.SetState(FScanner.FStatePushed);
    end;
  end;
end;

{ TStateSlashComment }

procedure TStateSlashComment.ActionIn(AOld: TState);
begin
  inherited;
  FBuffer := '';
end;

procedure TStateSlashComment.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    #10: begin
      FScanner.SetState(FScanner.FStateNormal);
      FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttComment, '//' + FBuffer + #13#10);
    end;

    else FBuffer := FBuffer + Ch;
  end;
end;

{ TStateBraceComment }

procedure TStateBraceComment.ActionIn(AOld: TState);
begin
  inherited;
  FBuffer := '';
end;

procedure TStateBraceComment.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '}': begin
      // TODO: {$}- 컴파일러 지시자 처리
      FScanner.SetState(FScanner.FStateNormal);
      FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttComment, '{' + FBuffer + '}');
    end;

    else FBuffer := FBuffer + Ch;
  end;
end;

{ TStateSlashAsteriskComment }

procedure TStateSlashAsteriskComment.ActionIn(AOld: TState);
begin
  inherited;
  FBuffer := '';
end;

procedure TStateSlashAsteriskComment.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '*': FScanner.SetState(FScanner.FSTateAsteriskInComment);
    else FBuffer := FBuffer + Ch;
  end;
end;

{ TSTateAsteriskInComment }

procedure TSTateAsteriskInComment.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '/': begin
      FScanner.SetState(FScanner.FStateNormal);
      FScanner.set_Token(FScanner.FLine, FScanner.FCol, ttComment, '/*' + FBuffer + '*/');
    end;

    else begin
      FBuffer := FBuffer + Ch;
      FScanner.SetState(FScanner.FStateSlashAsteriskComment);
    end;
  end;
end;

{ TStateStringBegin }

procedure TStateStringBegin.ActionIn(AOld:TState);
begin
  if AOld = FScanner.FStateStringEscape then begin
    case FScanner.CurrentChar of
      'n': FBuffer := FBuffer + #13#10;
      else FBuffer := FBuffer + FScanner.CurrentChar;
    end;
  end else begin
    inherited;
    FBuffer := '';
  end;
end;

procedure TStateStringBegin.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '''': if FScanner.PascalStyle then begin
      FScanner.set_Token(FLine, FCol, ttString, FBuffer);
      FScanner.SetState(FScanner.FStateNormal);
    end else begin
      FBuffer := FBuffer + Ch;
    end;

    '"': if not FScanner.PascalStyle then begin
      FScanner.set_Token(FLine, FCol, ttString, FBuffer);
      FScanner.SetState(FScanner.FStateNormal);
    end else begin
      FBuffer := FBuffer + Ch;
    end;

    '\': if FScanner.FUseStringEscape then FScanner.SetState(FScanner.FStateStringEscape)
         else FBuffer := FBuffer + Ch;

    else begin
      FBuffer := FBuffer + Ch;
    end;
  end;
end;

{ TStateStringEscape }

procedure TStateStringEscape.Scan;
begin
  FScanner.NextChar;
  FScanner.SetState(FScanner.FStateStringBegin);
end;

{ TStateNumber }

procedure TStateNumber.ActionIn(AOld: TState);
begin
  inherited;

  FHasDot := false;
  FBuffer := FScanner.CurrentChar;
end;

procedure TStateNumber.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    '0'..'9': FBuffer := FBuffer + Ch;

    '.': if FHasDot then begin
      FScanner.set_Token(FLine, FCol, ttNumber, FBuffer);
      FScanner.SetState(FScanner.FStatePushed);
    end else begin
      FHasDot := true;
      FBuffer := FBuffer + Ch;
    end;

    else begin
      FScanner.set_Token(FLine, FCol, ttNumber, FBuffer);
      FScanner.SetState(FScanner.FStatePushed);
    end;
  end;
end;

{ TStateIdentifier }

procedure TStateIdentifier.ActionIn(AOld: TState);
begin
  inherited;

  FBuffer := FScanner.CurrentChar;
end;

procedure TStateIdentifier.Scan;
var
  Ch : Char;
begin
  Ch := FScanner.NextChar;

  case Ch of
    'a'..'z', 'A'..'Z': FBuffer := FBuffer + Ch;
    '0'..'9': FBuffer := FBuffer + Ch;
    '_': FBuffer := FBuffer + Ch;

    else begin
      FScanner.set_Token(FLine, FCol, ttIdentifier, FBuffer);
      FScanner.SetState(FScanner.FStatePushed);
    end;
  end;
end;

{ TToken }

function TToken.GetLowerCaseText: string;
begin
  Result := LowerCase( FOriginalText );
end;

function TToken.GetText: string;
begin
  case TokenType of
    ttNone: Result := '';
    ttString: Result := '''' + FOriginalText + '''';
    else Result := FOriginalText;
  end;
end;

procedure TToken.SetText(const AText: string);
begin
  FOriginalText := AText;
end;

end.
