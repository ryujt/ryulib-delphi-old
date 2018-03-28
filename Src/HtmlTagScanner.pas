unit HtmlTagScanner;

interface

uses
  RyuLibBase, Strg,
  SysUtils, Classes;

type
  /// ssElement = /, #, 0..9, A..Z
  TScannerState = (ssNormal, ssElement, ssSingleQuote, ssDoubleQuote, ssEOF);

  THtmlTagScanner = class
  private
    FBuffer : string;
    FState : TScannerState;
    procedure do_Normal(Ch:Char);
    procedure do_Element(Ch:Char);
    procedure do_SingleQuote(Ch:Char);
    procedure do_DoubleQuote(Ch:Char);
    procedure do_EOF(Ch:Char);
  private
    FElements : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const AText:string);
  public
    property Elements : TStringList read FElements;
  end;

implementation

{ THtmlTagScanner }

constructor THtmlTagScanner.Create;
begin
  inherited;

  FElements := TStringList.Create;
end;

destructor THtmlTagScanner.Destroy;
begin
  FreeAndNil(FElements);

  inherited;
end;

procedure THtmlTagScanner.do_DoubleQuote(Ch: Char);
begin
  if Ch = '"' then begin
    if FBuffer <> '' then FElements.Add(FBuffer);
    FBuffer := '';
    FState := ssNormal;
  end else begin
    FBuffer := FBuffer + Ch;
  end;
end;

procedure THtmlTagScanner.do_Element(Ch: Char);
begin
  if not (Ch in ['/', '#', '0'..'9', 'a'..'z', 'A'..'Z']) then begin
    if FBuffer <> '' then FElements.Add(FBuffer);
    FBuffer := '';
    do_Normal(Ch);
  end else begin
    FBuffer := FBuffer + Ch;
  end;
end;

procedure THtmlTagScanner.do_EOF(Ch: Char);
begin
  if FState in [ssSingleQuote, ssDoubleQuote] then
    raise Exception.Create('String is not Terminated: ' + FBuffer);

  if FBuffer <> '' then FElements.Add(FBuffer);
  FBuffer := '';
end;

procedure THtmlTagScanner.do_Normal(Ch: Char);
begin
  case Ch of
    '/', '#', '0'..'9', 'a'..'z', 'A'..'Z': begin
      FBuffer := FBuffer + Ch;
      FState := ssElement;
    end;

    '=': begin
      if FBuffer <> '' then FElements.Add(FBuffer);
      FBuffer := '';
      FElements.Add('=');
      FState := ssNormal;
    end;

    '''': FState := ssSingleQuote;
    '"': FState:= ssDoubleQuote;

    else FState := ssNormal;
  end;
end;

procedure THtmlTagScanner.do_SingleQuote(Ch: Char);
begin
  if Ch = '''' then begin
    if FBuffer <> '' then FElements.Add(FBuffer);
    FBuffer := '';
    FState := ssNormal;
  end else begin
    FBuffer := FBuffer + Ch;
  end;
end;

procedure THtmlTagScanner.Execute(const AText: string);
var
  Loop: Integer;
begin
  FElements.Clear;
  FBuffer := '';
  FState := ssNormal;

  for Loop := 1 to Length(AText) do begin
    case FState of
      ssNormal: do_Normal(AText[Loop]);
      ssElement: do_Element(AText[Loop]);
      ssSingleQuote: do_SingleQuote(AText[Loop]);
      ssDoubleQuote: do_DoubleQuote(AText[Loop]);
    end;
  end;

  do_EOF(#0);
end;

end.
