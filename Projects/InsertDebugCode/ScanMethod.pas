unit ScanMethod;

interface

uses
  DebugTools, Scanner,
  SysUtils, Classes;

type
  TState = (
    stNone, stMethodBegin, stIdentifier, stDot, stIdentifier2, stParenthesisLeft,
    stParenthesisRight, stColon, stResultType, stMethodDeclare, stMethodBody,
    stBlockBegin, stMethodEnd
  );

  TScanMethod = class
  private
    procedure skip_WhiteSpace;
    function get_ErrorMsg(AMsg:string):string;
  private
    procedure do_MethodBegin(var AState:TState; var AMethodName:string);
    procedure do_Identifier(var AState:TState; var AMethodName:string);
    procedure do_Dot(var AState:TState; var AMethodName:string);
    procedure do_Identifier2(var AState:TState; var AMethodName:string);
    procedure do_ParenthesisLeft(var AState:TState; var AMethodName:string);
    procedure do_ParenthesisRight(var AState:TState; var AMethodName:string);
    procedure do_Colon(var AState:TState; var AMethodName:string);
    procedure do_ResultType(var AState:TState; var AMethodName:string);
    procedure do_MethodDeclare(var AState:TState; var AMethodName:string; AMethodDelpth:integer);
    procedure do_MethodBody(var AState:TState; var AMethodName:string; AMethodDelpth:integer);
    procedure do_BlockBegin(var AState:TState; var AMethodName:string; ADelpth:integer);
  public
    constructor Create;
    destructor Destroy; override;

    class function Obj:TScanMethod;

    procedure Execute(ADelpth:integer);
  end;

implementation

uses
  ScanMgr;

{ TScanMethod }

var
  MyObject : TScanMethod = nil;

class function TScanMethod.Obj: TScanMethod;
begin
  if MyObject = nil then MyObject := TScanMethod.Create;
  Result := MyObject;
end;

procedure TScanMethod.skip_WhiteSpace;
begin
  TScanMgr.Obj.GetNextToken;
  while TScanMgr.Obj.CurrentToken.TokenType = ttWhiteSpace do begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
    TScanMgr.Obj.GetNextToken;
  end;
end;

constructor TScanMethod.Create;
begin
  inherited;

end;

destructor TScanMethod.Destroy;
begin

  inherited;
end;

procedure TScanMethod.do_BlockBegin;
begin
  if TScanMgr.Obj.isEndToken then begin
    if ADelpth = 0 then AState := stMethodBody;
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
  end else if TScanMgr.Obj.isBeginToken then begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;

    skip_WhiteSpace;
    do_BlockBegin(AState, AMethodName, ADelpth + 1);
  end else begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
  end;
end;

procedure TScanMethod.do_Colon;
begin
  if TScanMgr.Obj.CurrentToken.TokenType <> ttIdentifier then
    raise Exception.Create( get_ErrorMsg('Identifier expected. - ' + TScanMgr.Obj.CurrentToken.OriginalText) )
  else begin
    AState := stResultType;
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_Dot;
begin
  AMethodName := AMethodName + TScanMgr.Obj.CurrentToken.OriginalText;

  if TScanMgr.Obj.CurrentToken.TokenType <> ttIdentifier then
    raise Exception.Create( get_ErrorMsg('Identifier expected. - ' + TScanMgr.Obj.CurrentToken.OriginalText) )
  else begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
    AState := stIdentifier2;
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_Identifier;
begin
  if TScanMgr.Obj.CurrentToken.OriginalText = '.' then begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
    AState := stDot;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = '(' then begin
    AState := stParenthesisLeft;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = ':' then begin
    AState := stColon;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = ';' then begin
    AState := stMethodDeclare;
  end else begin
    raise Exception.Create( get_ErrorMsg('Method declaration error.') )
  end;
end;

procedure TScanMethod.do_Identifier2;
begin
  if TScanMgr.Obj.CurrentToken.OriginalText = '(' then begin
    AState := stParenthesisLeft;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = ':' then begin
    AState := stColon;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = ';' then begin
    AState := stMethodDeclare;
  end else begin
    raise Exception.Create( get_ErrorMsg('Method declaration error.') )
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_MethodBegin;
begin
  if TScanMgr.Obj.CurrentToken.TokenType <> ttIdentifier then
    raise Exception.Create( get_ErrorMsg('Identifier expected. - ' + TScanMgr.Obj.CurrentToken.OriginalText) )
  else begin
    AMethodName := TScanMgr.Obj.CurrentToken.OriginalText;
    AState := stIdentifier;
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_MethodBody;
begin
  if TScanMgr.Obj.isBeginToken then begin
    AState := stBlockBegin;
  end else if TScanMgr.Obj.isEndToken then begin
    // 디버그 코드 삽입
    if AMethodDelpth = 0 then begin
      TScanMgr.Obj.Source := TScanMgr.Obj.Source + Format(#13#10+'  CodeSite.Send(''%s - End'');'+#13#10, [AMethodName]);
    end;

    AState := stMethodEnd;
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_MethodDeclare;
begin
  if TScanMgr.Obj.isMethodBegin then begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
    Execute( AMethodDelpth + 1);
  end else if TScanMgr.Obj.isBeginToken then begin
    AState := stMethodBody;
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;

    // 디버그 코드 삽입
    if AMethodDelpth = 0 then begin
      TScanMgr.Obj.Source := TScanMgr.Obj.Source + Format(#13#10+'  CodeSite.Send(''%s - Begin'', Self);'+#13#10, [AMethodName]);
    end;
  end else begin
    TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
  end;
end;

procedure TScanMethod.do_ParenthesisLeft;
begin
  if TScanMgr.Obj.CurrentToken.OriginalText = ')' then begin
    AState := stParenthesisRight;
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_ParenthesisRight;
begin
  if TScanMgr.Obj.CurrentToken.OriginalText = ':' then begin
    AState := stColon;
  end else if TScanMgr.Obj.CurrentToken.OriginalText = ';' then begin
    AState := stMethodDeclare;
  end else begin
    raise Exception.Create( get_ErrorMsg('Method declaration error.') )
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.do_ResultType;
begin
  if TScanMgr.Obj.CurrentToken.OriginalText = ';' then begin
    AState := stMethodDeclare;
  end else begin
    raise Exception.Create( get_ErrorMsg('Method declaration error.') )
  end;

  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
end;

procedure TScanMethod.Execute(ADelpth:integer);
var
  State : TState;
  MethodName : string;
begin
  TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;

  State := stMethodBegin;
  MethodName := '';

  skip_WhiteSpace;
  while State <> stMethodEnd do begin
    case State of
      stMethodBegin: do_MethodBegin(State, MethodName);
      stIdentifier: do_Identifier(State, MethodName);
      stDot: do_Dot(State, MethodName);
      stIdentifier2: do_Identifier2(State, MethodName);
      stParenthesisLeft: do_ParenthesisLeft(State, MethodName);
      stParenthesisRight: do_ParenthesisRight(State, MethodName);
      stColon: do_Colon(State, MethodName);
      stResultType: do_ResultType(State, MethodName);
      stMethodDeclare: do_MethodDeclare(State, MethodName, ADelpth);
      stMethodBody: do_MethodBody(State, MethodName, ADelpth);
      stBlockBegin: do_BlockBegin(State, MethodName, 0);

      stMethodEnd: begin
        TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
        Break;
      end;
    end;

    if State <> stMethodEnd then skip_WhiteSpace;
  end;
end;

function TScanMethod.get_ErrorMsg(AMsg: string): string;
begin
  Result := Format(
    '%s (%d, %d)' + #13#10 + '  ' + AMsg ,
    [TScanMgr.Obj.FileName, TScanMgr.Obj.CurrentToken.Line, TScanMgr.Obj.CurrentToken.Col, AMsg]
  );
end;

initialization
  MyObject := TScanMethod.Create;
end.