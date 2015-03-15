unit StripComments;

interface

uses
  Classes, SysUtils;

type
  TStripComments = class;

  TState = class (TComponent)
  private
    function StripComments:TStripComments;
    procedure do_Error;
  public
    procedure Transfer(Text:string);
    procedure ReadCh; virtual; abstract;
  end;

  TStateNormal = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateString = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateQuotation = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateSlash = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateSlashComment = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateBrace = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateBraceComment = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateParenthesis = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateAsteriskInParenthesisComment = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStateParenthesisComment = class (TState)
  private
  public
    procedure ReadCh; override;
  end;

  TStripComments = class (TComponent)
  private
    FInput : string;
    FResult : string;
    FIndex : integer;
    FState : TState;
    FStateNormal : TStateNormal;
    FStateString : TStateString;
    FStateQuotation : TStateQuotation;
    FStateSlash : TStateSlash;
    FStateSlashComment : TStateSlashComment;
    FStateBrace : TStateBrace;
    FStateBraceComment : TStateBraceComment;
    FStateParenthesis : TStateParenthesis;
    FStateAsteriskInParenthesisComment : TStateAsteriskInParenthesisComment;
    FStateParenthesisComment : TStateParenthesisComment;
    function get_Char:char;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(Text:string):string;
  end;

implementation

{ TState }

procedure TState.do_Error;
begin
  raise Exception.Create(Format('Parsing error index of (%d)', [StripComments.FIndex]));
end;

procedure TState.Transfer(Text: string);
begin
  StripComments.FResult := StripComments.FResult + Text;
  StripComments.FState := Self;
end;

function TState.StripComments: TStripComments;
begin
  Result := Pointer(Owner);
end;

{ TStateNormal }

procedure TStateNormal.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '''' : StripComments.FStateString.Transfer(ch);
       '/' : StripComments.FState := StripComments.FStateSlash;
       '{' : StripComments.FState := StripComments.FStateBrace;
       '(' : StripComments.FState := StripComments.FStateParenthesis;
       else
         StripComments.FResult := StripComments.FResult + ch;
  end
end;

{ TStateString }

procedure TStateString.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '''' : StripComments.FState := StripComments.FStateQuotation;
       #13, #10 : do_Error;
       else
         StripComments.FResult := StripComments.FResult + ch;
  end;
end;

{ TStateQuotation }

procedure TStateQuotation.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '''' : StripComments.FStateString.Transfer('''''');
       '/' : StripComments.FStateSlash.Transfer('''');
       '{' : StripComments.FStateBrace.Transfer('''');
       '(' : StripComments.FStateParenthesis.Transfer('''');
       else StripComments.FStateNormal.Transfer('''' + ch);
  end;
end;

{ TStateSlash }

procedure TStateSlash.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '/' : StripComments.FState := StripComments.FStateSlashComment;
       '''' : StripComments.FStateString.Transfer('/' + ch);
       '{' : StripComments.FStateBrace.Transfer('/');
       '(' : StripComments.FStateParenthesis.Transfer('/');
       else StripComments.FStateNormal.Transfer('/' + ch);
  end;
end;

{ TStateSlashComment }

procedure TStateSlashComment.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       #13, #10 : begin
         StripComments.FResult := StripComments.FResult + ch;
         StripComments.FState := StripComments.FStateNormal;
       end;
  end;
end;

{ TStateBrace }

procedure TStateBrace.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '$' : StripComments.FStateNormal.Transfer('{$');
       else
         StripComments.FState := StripComments.FStateBraceComment;
  end;
end;

{ TStateBraceComment }

procedure TStateBraceComment.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '}' : StripComments.FState := StripComments.FStateNormal;
  end;
end;

{ TStateParenthesis }

procedure TStateParenthesis.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '*' : StripComments.FState := StripComments.FStateParenthesisComment;
       '''' : StripComments.FStateString.Transfer('(' + ch);
       '/' : StripComments.FStateSlash.Transfer('(');
       '{' : StripComments.FStateBrace.Transfer('(');
       '(' : StripComments.FStateParenthesis.Transfer('(');
       else StripComments.FStateNormal.Transfer('(' + ch);
  end;
end;

{ TStateAsteriskInParenthesisComment }

procedure TStateAsteriskInParenthesisComment.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       ')' : StripComments.FState := StripComments.FStateNormal;
       '*' : ;
       else
         StripComments.FState := StripComments.FStateParenthesisComment;
  end;
end;

{ TStateParenthesisComment }

procedure TStateParenthesisComment.ReadCh;
var
  ch : char;
begin
  ch := StripComments.get_Char;
  case ch of
       '*' : StripComments.FState := StripComments.FStateAsteriskInParenthesisComment;
  end;
end;

{ TStripComments }

constructor TStripComments.Create(AOwner: TComponent);
begin
  inherited;

  FStateNormal := TStateNormal.Create(Self);
  FStateQuotation := TStateQuotation.Create(Self);
  FStateString := TStateString.Create(Self);
  FStateSlash := TStateSlash.Create(Self);
  FStateSlashComment := TStateSlashComment.Create(Self);
  FStateBrace := TStateBrace.Create(Self);
  FStateBraceComment := TStateBraceComment.Create(Self);
  FStateParenthesis := TStateParenthesis.Create(Self);
  FStateAsteriskInParenthesisComment := TStateAsteriskInParenthesisComment.Create(Self);
  FStateParenthesisComment := TStateParenthesisComment.Create(Self);

  FState := FStateNormal;
  FIndex := 0;
end;

destructor TStripComments.Destroy;
begin
  FStateNormal.Free;
  FStateString.Free;
  FStateQuotation.Free;
  FStateSlash.Free;
  FStateSlashComment.Free;
  FStateBrace.Free;
  FStateBraceComment.Free;
  FStateParenthesis.Free;
  FStateAsteriskInParenthesisComment.Free;
  FStateParenthesisComment.Free;

  inherited;
end;

function TStripComments.Execute(Text: string): string;
begin
  FIndex := 0;
  FInput := Text;
  FResult := '';
  FState := FStateNormal;

  repeat
    FState.ReadCh;
  until FIndex >= Length(FInput);
  
  Result := FResult;
end;

function TStripComments.get_Char: char;
begin
  if FInput = '' then begin
    Result := #0;
    Exit;
  end;

  FIndex := FIndex + 1;
  Result := FInput[FIndex];
end;

end.
