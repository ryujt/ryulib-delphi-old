unit PrintString;

interface

uses
  Scanner, Disk,
  SysUtils, Classes;

type
  TPrintString = class
  private
    FScanner : TScanner;
  public
    constructor Create;
    destructor Destroy; override;

    class function Obj:TPrintString;

    procedure Execute(AFileName:string);
  end;

implementation

{ TPrintString }

var
  MyObject : TPrintString = nil;

class function TPrintString.Obj: TPrintString;
begin
  if MyObject = nil then MyObject := TPrintString.Create;
  Result := MyObject;
end;

constructor TPrintString.Create;
begin
  inherited;

  FScanner := TScanner.Create;
end;

destructor TPrintString.Destroy;
begin
  FreeAndNil(FScanner);

  inherited;
end;

procedure TPrintString.Execute(AFileName: string);
var
  Token : TToken;
begin
  FScanner.SetText( LoadFileAsText(AFileName) );

  repeat
    Token := FScanner.GetNextToken;

    if Token.TokenType = ttString then
      WriteLn(
        Format('* %4d:%3d - ''%s''', [Token.Line, Token.Col, Token.Text])
      );
  until FScanner.IsEOF;
end;

end.
