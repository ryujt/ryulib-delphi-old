unit JsonFormatter;

interface

uses
  Strg,
  Classes, SysUtils;

type
  TJsonFormatter = class
  strict private
    FState : TObject;
  private
    FTabs : integer;
    FIndex : integer;
    FSource : string;
    FResultText : string;
    function read_Char:char;
    function tabs:string;
  private
    FStateBase,
    FStateWhiteSpace,
    FStateLeft,
    FStateRight,
    FStateComma,
    FStateStringBegin,
    FStateStringEscape,
    FStateStringEnd,
    FStateNormal,
    FStateEscape : TObject;
    procedure SetState(AState:TObject; AChar:Char);
  private
    FTabSize: integer;
    procedure SetSource(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property TabSize : integer read FTabSize write FTabSize;
    property Source : string read FSource write SetSource;
    property ResultText : string read FResultText;
  end;

function JsonFormat(AText:string; ATabSize:integer=4):string;

implementation

function JsonFormat(AText:string; ATabSize:integer):string;
var
  Formatter : TJsonFormatter;
begin
  Formatter := TJsonFormatter.Create;
  try
    Formatter.Source := AText;
    Result := Formatter.ResultText;
  finally
    Formatter.Free;
  end;
end;

type
  TStateBase = class
  private
    FFormatter : TJsonFormatter;
  public
    constructor Create(AJsonFormatter:TJsonFormatter); reintroduce;

    procedure ActionIn(ANewState:TStateBase; AChar:Char); virtual;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); virtual;

    procedure ReadNext; virtual;
  end;

  TStateWhiteSpace = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateLeft = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateRight = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateComma = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateStringBegin = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateStringEscape = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateStringEnd = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateNormal = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

  TStateEscape = class (TStateBase)
  private
  public
    procedure ActionIn(ANewState:TStateBase; AChar:Char); override;
    procedure ActionOut(AOldState:TStateBase; AChar:Char); override;

    procedure ReadNext; override;
  end;

{ TJsonFormatter }

constructor TJsonFormatter.Create;
begin
  inherited;

  FTabSize := 4;
  FTabs := 0;
  FIndex := 0;
  FSource := '';
  FResultText := '';

  FStateBase := TStateBase.Create(Self);
  FStateWhiteSpace := TStateWhiteSpace.Create(Self);
  FStateLeft := TStateLeft.Create(Self);
  FStateRight := TStateRight.Create(Self);
  FStateComma := TStateComma.Create(Self);
  FStateStringBegin := TStateStringBegin.Create(Self);
  FStateStringEscape := TStateStringEscape.Create(Self);
  FStateStringEnd := TStateStringEnd.Create(Self);
  FStateNormal := TStateNormal.Create(Self);
  FStateEscape := TStateEscape.Create(Self);

  FState := FStateBase;
end;

destructor TJsonFormatter.Destroy;
begin
  FreeAndNil(FStateBase);
  FreeAndNil(FStateWhiteSpace);
  FreeAndNil(FStateLeft);
  FreeAndNil(FStateRight);
  FreeAndNil(FStateComma);
  FreeAndNil(FStateStringBegin);
  FreeAndNil(FStateStringEscape);
  FreeAndNil(FStateStringEnd);
  FreeAndNil(FStateNormal);
  FreeAndNil(FStateEscape);

  inherited;
end;

function TJsonFormatter.read_Char: char;
begin
  if FIndex > Length(FSource) then Result := #0
  else begin
    Result := FSource[FIndex];
    Inc(FIndex);
  end;
end;

procedure TJsonFormatter.SetSource(const Value: string);
begin
  FSource := Value;

  FTabs := 0;
  FIndex := 1;
  while FIndex <= Length(FSource) do begin
    TStateBase(FState).ReadNext;
  end;
end;

procedure TJsonFormatter.SetState(AState: TObject; AChar:Char);
var
  OldState, NewState : TStateBase;
begin
  OldState := Pointer(FState);
  NewState := Pointer(AState);

  OldState.ActionOut(NewState, AChar);
  FState := NewState;

  NewState.ActionIn(OldState, AChar);
end;

function TJsonFormatter.tabs: string;
begin
  Result := CharString(' ', FTabs * FTabSize);
end;

{ TStateBase }

procedure TStateBase.ActionIn(ANewState: TStateBase; AChar:Char);
begin
end;

procedure TStateBase.ActionOut(AOldState: TStateBase; AChar:Char);
begin
end;

constructor TStateBase.Create(AJsonFormatter: TJsonFormatter);
begin
  inherited Create;

  FFormatter := AJsonFormatter;
end;

procedure TStateBase.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateWhiteSpace }

procedure TStateWhiteSpace.ActionIn(ANewState: TStateBase; AChar:Char);
begin

end;

procedure TStateWhiteSpace.ActionOut(AOldState: TStateBase; AChar:Char);
begin

end;

procedure TStateWhiteSpace.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateLeft }

procedure TStateLeft.ActionIn(ANewState: TStateBase; AChar:Char);
begin
  FFormatter.FResultText := FFormatter.FResultText + '{' + #13#10;
  Inc(FFormatter.FTabs);

  FFormatter.FResultText := FFormatter.FResultText + FFormatter.tabs;
end;

procedure TStateLeft.ActionOut(AOldState: TStateBase; AChar:Char);
begin

end;

procedure TStateLeft.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateRight }

procedure TStateRight.ActionIn(ANewState: TStateBase; AChar:Char);
begin
  Dec(FFormatter.FTabs);
  FFormatter.FResultText := FFormatter.FResultText + #13#10 + FFormatter.tabs + '}';
end;

procedure TStateRight.ActionOut(AOldState: TStateBase; AChar:Char);
begin

end;

procedure TStateRight.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateComma }

procedure TStateComma.ActionIn(ANewState: TStateBase; AChar:Char);
begin
  FFormatter.FResultText := FFormatter.FResultText + AChar + #13#10 + FFormatter.tabs;
end;

procedure TStateComma.ActionOut(AOldState: TStateBase; AChar:Char);
begin

end;

procedure TStateComma.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateStringBegin }

procedure TStateStringBegin.ActionIn(ANewState: TStateBase; AChar:Char);
begin
  if AChar <> #0 then
    FFormatter.FResultText := FFormatter.FResultText + AChar;
end;

procedure TStateStringBegin.ActionOut(AOldState: TStateBase; AChar:Char);
begin

end;

procedure TStateStringBegin.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    '"': FFormatter.SetState(FFormatter.FStateStringEnd, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.FResultText := FFormatter.FResultText + Ch;
  end;
end;

{ TStateStringEscape }

procedure TStateStringEscape.ActionIn(ANewState: TStateBase; AChar: Char);
begin

end;

procedure TStateStringEscape.ActionOut(AOldState: TStateBase; AChar: Char);
begin

end;

procedure TStateStringEscape.ReadNext;
begin
  FFormatter.FResultText := FFormatter.FResultText + FFormatter.read_Char;
  FFormatter.SetState(FFormatter.FStateStringBegin, #0);
end;

{ TStateStringEnd }

procedure TStateStringEnd.ActionIn(ANewState: TStateBase; AChar: Char);
begin
  FFormatter.FResultText := FFormatter.FResultText + AChar;
end;

procedure TStateStringEnd.ActionOut(AOldState: TStateBase; AChar: Char);
begin

end;

procedure TStateStringEnd.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

{ TStateNormal }

procedure TStateNormal.ActionIn(ANewState: TStateBase; AChar: Char);
begin
  FFormatter.FResultText := FFormatter.FResultText + AChar;
end;

procedure TStateNormal.ActionOut(AOldState: TStateBase; AChar: Char);
begin

end;

procedure TStateNormal.ReadNext;
var
  Ch : Char;
begin
  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.FResultText := FFormatter.FResultText + Ch;
  end;
end;

{ TStateEscape }

procedure TStateEscape.ActionIn(ANewState: TStateBase; AChar: Char);
begin

end;

procedure TStateEscape.ActionOut(AOldState: TStateBase; AChar: Char);
begin

end;

procedure TStateEscape.ReadNext;
var
  Ch : Char;
begin
  FFormatter.FResultText := FFormatter.FResultText + FFormatter.read_Char;

  Ch := FFormatter.read_Char;
  case Ch of
    #0..#32: FFormatter.SetState(FFormatter.FStateWhiteSpace, Ch);
    '{': FFormatter.SetState(FFormatter.FStateLeft, Ch);
    '}': FFormatter.SetState(FFormatter.FStateRight, Ch);
    ',': FFormatter.SetState(FFormatter.FStateComma, Ch);
    '"': FFormatter.SetState(FFormatter.FStateStringBegin, Ch);
    '\': FFormatter.SetState(FFormatter.FStateEscape, Ch);
    else FFormatter.SetState(FFormatter.FStateNormal, Ch);
  end;
end;

end.
