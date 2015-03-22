unit ScanMgr;

interface

uses
  Scanner,
  SysUtils, Classes;

type
  TScanMgr = class
  private
    FSource : string;
    FCurrentToken : TToken;
    FScanner : TScanner;
    FFileName: string;
    function GetIsEOF: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    class function Obj:TScanMgr;

    procedure SetText(AFileName,AText:string);
    procedure GetNextToken;

    procedure SaveCurrentToken;

    procedure GetNextTokenAndSkipWhiteSpace;

    function isUses:boolean;
    function isType:boolean;
    function isVar:boolean;
    function isClass:boolean;
    function isRecord:boolean;
    function isObject:boolean;
    function isImplementation:boolean;
    function isMethodBegin:boolean;
    function isBeginToken:boolean;
    function isEndToken:boolean;
  public
    property IsEOF : boolean read GetIsEOF;
    property FileName : string read FFileName write FFileName;
    property Source : string read FSource write FSource;
    property CurrentToken : TToken read FCurrentToken;
  end;

implementation

{ TScanMgr }

var
  MyObject : TScanMgr = nil;

class function TScanMgr.Obj: TScanMgr;
begin
  if MyObject = nil then MyObject := TScanMgr.Create;
  Result := MyObject;
end;

procedure TScanMgr.SaveCurrentToken;
begin
  FScanner.SaveCurrentToken;
end;

procedure TScanMgr.SetText(AFileName,AText: string);
begin
  FFileName := AFileName;
  FSource := '';
  FScanner.SetText(AText);
end;

constructor TScanMgr.Create;
begin
  inherited;

  FFileName := '';

  FScanner := TScanner.Create;
end;

destructor TScanMgr.Destroy;
begin
  FreeAndNil(FScanner);

  inherited;
end;

function TScanMgr.GetIsEOF: boolean;
begin
  Result := FScanner.IsEOF;
end;

procedure TScanMgr.GetNextToken;
begin
  FCurrentToken := FScanner.GetNextToken;
end;

procedure TScanMgr.GetNextTokenAndSkipWhiteSpace;
begin
  GetNextToken;
  while FCurrentToken.TokenType = ttWhiteSpace do begin
    if FCurrentToken.Text <> #0 then FSource := FSource + FCurrentToken.OriginalText;
    GetNextToken;
  end;
end;

function TScanMgr.isBeginToken: boolean;
begin
  Result := (FCurrentToken.TokenType = ttIdentifier) and (FCurrentToken.LowerCaseText = 'begin');
end;

function TScanMgr.isClass: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'class');
end;

function TScanMgr.isEndToken: boolean;
begin
  Result := (FCurrentToken.TokenType = ttIdentifier) and (FCurrentToken.LowerCaseText = 'end');
end;

function TScanMgr.isImplementation: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'implementation');
end;

function TScanMgr.isMethodBegin: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    ((FCurrentToken.LowerCaseText = 'procedure') or (FCurrentToken.LowerCaseText = 'function'));
end;

function TScanMgr.isObject: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'object');
end;

function TScanMgr.isRecord: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'record');
end;

function TScanMgr.isType: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'type');
end;

function TScanMgr.isUses: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'uses');
end;

function TScanMgr.isVar: boolean;
begin
  Result :=
    (FCurrentToken.TokenType = ttIdentifier) and
    (FCurrentToken.LowerCaseText = 'var');
end;

initialization
  MyObject := TScanMgr.Create;
end.