unit Para;

interface

uses
  Classes, SysUtils;


function GetParameterCount:integer;
function GetParameter(AIndex:integer):string;

function GetSwitchCount:integer;
function GetSwitch(AIndex:integer):string;
function GetSwitchName(AIndex:integer):string;
function GetSwitchValue(AName:string):string;
function GetSwitchValueFromIndex(AIndex:integer):string;
function FindSwitchName(AName:string):boolean;

implementation

type
  TPara = class
  private
    FParameters : TStringList;
    FSwitchs : TStringList;
    procedure add_Parameter(AText:string);
    procedure add_Switch(AText:string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MyObject : TPara;

function GetParameterCount:integer;
begin
  Result := MyObject.FParameters.Count;
end;

function GetParameter(AIndex:integer):string;
begin
  Result := '';
  if AIndex >= GetParameterCount then Exit;

  Result := MyObject.FParameters[AIndex];
end;

function GetSwitchCount:integer;
begin
  Result := MyObject.FSwitchs.Count;
end;

function GetSwitch(AIndex:integer):string;
begin
  Result := '';
  if AIndex >= GetSwitchCount then Exit;

  Result := MyObject.FSwitchs[AIndex];
end;

function GetSwitchName(AIndex:integer):string;
begin
  Result := '';
  if AIndex >= GetSwitchCount then Exit;

  Result := MyObject.FSwitchs.Names[AIndex];
end;

function GetSwitchValue(AName:string):string;
begin
  Result := MyObject.FSwitchs.Values[AName];
end;

function GetSwitchValueFromIndex(AIndex:integer):string;
begin
  Result := '';
  if AIndex >= GetSwitchCount then Exit;

  Result := MyObject.FSwitchs.ValueFromIndex[AIndex];
end;

function FindSwitchName(AName:string):boolean;
var
  Loop: Integer;
begin
  Result := false;

  AName := LowerCase(AName);

  for Loop := 0 to MyObject.FSwitchs.Count-1 do
    if LowerCase(MyObject.FSwitchs.Names[Loop]) = AName then begin
      Result := true;
      Exit;
    end;
end;

{ TPara }

procedure TPara.add_Parameter(AText: string);
begin
  FParameters.Add(AText);
end;

procedure TPara.add_Switch(AText: string);
begin
  if Pos('=', AText) = 0 then AText := AText + '=';
  FSwitchs.Add(Copy(AText, 2, Length(AText)));
end;

constructor TPara.Create;
var
  Loop: Integer;
  sParamStr : string;
begin
  inherited;

  FParameters := TStringList.Create;
  FSwitchs := TStringList.Create;

  for Loop := 1 to ParamCount do begin
    sParamStr := ParamStr(Loop);
    if Copy(sParamStr, 1, 1) = '-' then add_Switch(sParamStr)
    else add_Parameter(sParamStr);
  end;
end;

destructor TPara.Destroy;
begin
  FreeAndNil(FParameters);
  FreeAndNil(FSwitchs);

  inherited;
end;

initialization
  MyObject := TPara.Create;
finalization
  FreeAndNil(MyObject);
end.
