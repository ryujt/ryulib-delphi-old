unit HtmlScanner;

interface

uses
  RyuLibBase, Strg,
  SysUtils, Classes;

type
  TScannerState = (ssText, ssTag, ssEOF);

  THtmlScanner = class
  private
    FBuffer : string;
    FState : TScannerState;
    procedure do_Text(Ch:Char);
    procedure do_Tag(Ch:Char);
    procedure do_EOF(Ch:Char);
  private
    FOnText: TStringEvent;
    FOnTag: TStringEvent;
  public
    procedure Execute(const AText:string);
  public
    property OnText : TStringEvent read FOnText write FOnText;
    property OnTag : TStringEvent read FOnTag write FOnTag;
  end;

implementation

{ THtmlScanner }

procedure THtmlScanner.do_Tag(Ch: Char);
begin
  if Ch = '>' then begin
    FState := ssText;
    if FBuffer <> '' then FOnTag(Self, FBuffer);
    FBuffer := '';
  end else begin
    FBuffer := FBuffer + Ch;
  end;
end;

procedure THtmlScanner.do_EOF(Ch: Char);
begin
  if FBuffer = '' then Exit;

  if FState = ssText then FOnText(Self, FBuffer)
  else FOnTag(Self, FBuffer);

  FBuffer := '';
end;

procedure THtmlScanner.do_Text(Ch: Char);
begin
  if Ch = '<' then begin
    FState := ssTag;
    if FBuffer <> '' then FOnText(Self, FBuffer);
    FBuffer := '';
  end else begin
    FBuffer := FBuffer + Ch;
  end;
end;

procedure THtmlScanner.Execute(const AText: string);
var
  Loop: Integer;
begin
  if not Assigned(FOnTag) then Exit;

  FBuffer := '';
  FState := ssText;

  for Loop := 1 to Length(AText) do begin
    case FState of
      ssText: do_Text(AText[Loop]);
      ssTag: do_Tag(AText[Loop]);
    end;
  end;

  do_EOF(#0);
end;

end.
