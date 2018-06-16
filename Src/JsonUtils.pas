unit JsonUtils;

interface

uses
  DebugTools,
  JsonData, IdHttp,
  Classes, SysUtils;

type
  TStateType = (stNone, stStarted, stString, stEscape, stEnd);

  TphpArrayToStringList = class
  private
    FText : string;
    FList : TStringList;
    FLine : string;
    FIndex : integer;
    FLastChar : char;
    FCurrentChar : char;
    FState : TStateType;
    function GetChar:Char;
  private
    procedure do_None;
    procedure do_Started;
    procedure do_String;
    procedure do_Escape;
    procedure do_End;
    procedure do_Execute;
  public
    function StringToStringList(const AText:string; AList:TStringList):boolean;
  end;

(** PHP JSON array to StringList
  This function will return JSON list to AList.
  When you use json_encode() in PHP, it will return string like below.
  "[{SJON #1}, {JSON #2} ....]"
  This function will cut JSONs and put into StringList one by one.
*)
function phpArrayToStringList(const AText:string; AList:TStringList):boolean;

/// Get JSON data from AURL
function GetJsonFromHTTP(const AURL:string):TJsonData;

implementation

function phpArrayToStringList(const AText:string; AList:TStringList):boolean;
var
  ToStringList : TphpArrayToStringList;
begin
  ToStringList := TphpArrayToStringList.Create;
  try
    Result := ToStringList.StringToStringList(AText, AList);
  finally
    ToStringList.Free;
  end;
end;

function GetJsonFromHTTP(const AURL:string):TJsonData;
var
  http : TIdHTTP;
  sResult : string;
begin
  Result := TJsonData.Create;

  http := TIdHTTP.Create(nil);
  try
    sResult := http.Get(AURL);

    if Trim(sResult) <> '' then Result.Text := sResult;

    {$IFDEF DEBUG}
    Trace( Format('JsonUtils.GetJsonFromHTTP - %s', [sResult]) );
    {$ENDIF}
  finally
    http.Free;
  end;
end;

{ TphpArrayToStringList }

procedure TphpArrayToStringList.do_End;
begin
  FList.Add(FLine);
  FState := stNone;
end;

procedure TphpArrayToStringList.do_Escape;
begin
  case FCurrentChar of
    'b': FLine := FLine + #8;
    't': FLine := FLine + #9;
    'n': FLine := FLine + #13#10;
    else
      FLine := FLine + FCurrentChar;
  end;

  FState := stString;
end;

procedure TphpArrayToStringList.do_Execute;
begin
  case FState of
    stNone: do_None;
    stStarted: do_Started;
    stString: do_String;
    stEscape: do_Escape;
    stEnd: do_End;
  end;
end;

procedure TphpArrayToStringList.do_None;
begin
  case FCurrentChar of
    '{': begin
      FLine := '{';
      FState := stStarted;
    end;

    else ;
  end;
end;

procedure TphpArrayToStringList.do_Started;
begin
  case FCurrentChar of
    '}': begin
      FLine := FLine + FCurrentChar;
      FState := stEnd;
    end;

    '"': begin
      FLine := FLine + FCurrentChar;
      FState := stString;
    end;

    #0: ;

    else FLine := FLine + FCurrentChar;
  end;
end;

procedure TphpArrayToStringList.do_String;
begin
  case FCurrentChar of
    '\': FState := stEscape;

    '"': begin
      FLine := FLine + FCurrentChar;
      FState := stStarted;
    end;

    #0: ;

    else FLine := FLine + FCurrentChar;
  end;
end;

function TphpArrayToStringList.GetChar: Char;
begin
  FLastChar := FCurrentChar;

  FIndex := FIndex + 1;
  if FIndex <= Length(FText) then FCurrentChar := FText[FIndex]
  else FCurrentChar := #0;

  Result := FCurrentChar;
end;

function TphpArrayToStringList.StringToStringList(const AText: string;
  AList: TStringList): boolean;
begin
  // TODO: Error detect
  Result := true;

  FText := AText;
  FList := AList;

  FLine := '';
  FIndex := 0;
  FLastChar := #0;
  FCurrentChar := #0;
  FState := stNone;

  while GetChar <> #0 do begin
    do_Execute;
  end;

  do_Execute;
end;

end.
