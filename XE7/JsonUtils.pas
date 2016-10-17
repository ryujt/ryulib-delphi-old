unit JsonUtils;

interface

uses
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

/// PHP JSON array to StringList
function phpArrayToStringList(const AText:string; AList:TStringList):boolean;

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

{ TphpArrayToStringList }

procedure TphpArrayToStringList.do_End;
begin
  FList.Add(FLine);
  FState := stNone;
end;

procedure TphpArrayToStringList.do_Escape;
begin
  FLine := FLine + FCurrentChar;
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
  // TODO: 에러 처리
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
