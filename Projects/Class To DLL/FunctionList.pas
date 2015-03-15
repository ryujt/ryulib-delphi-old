unit FunctionList;

interface

uses
  SysUtils, Classes;

type
  TFunctionList = class
  private
    FList : TList;
    function get_FunctionParameters(AStringList:TStringList):string;
  private
    FClassName: string;
    function GetCount: integer;
    function GetText: string;
    function GetFunctionTexts(Index: integer): string;
    function GetExportFunctionTexts(Index: integer): string;
    procedure SetClassName(const Value: string);
    function GetDllText: string;
    function GetExportsText: string;
    function GetFunctionNames(Index: integer): string;
    function GetExternalsText: string;
    function GetExternalFunctionTexts(Index: integer): string;
    function GetMethodInterfaceText: string;
    function GetMethodImplementationText: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AStringList:TStringList);

    property ClassName : string read FClassName write SetClassName;

    property Count : integer read GetCount;

    property FunctionNames [Index:integer] : string read GetFunctionNames;
    property FunctionTexts [Index:integer] : string read GetFunctionTexts;

    property ExportFunctionTexts [Index:integer] : string read GetExportFunctionTexts;
    property DllText : string read GetDllText;
    property ExportsText : string read GetExportsText;

    property MethodInterfaceText : string read GetMethodInterfaceText;

    property ExternalFunctionTexts [Index:integer] : string read GetExternalFunctionTexts;
    property ExternalsText : string read GetExternalsText;

    property MethodImplementationText : string read GetMethodImplementationText;

    property Text : string read GetText;
  end;

implementation

{ TFunctionList }

procedure TFunctionList.Add(AStringList: TStringList);
var
  StringList : TStringList;
begin
  StringList := TStringList.Create;
  StringList.Text := AStringList.Text;

  FList.Add(StringList);
end;

procedure TFunctionList.Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do TObject(FList[Loop]).Free;
  FList.Clear;
end;

constructor TFunctionList.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TFunctionList.Destroy;
begin
  Clear;

  FreeAndNil(FList);

  inherited;
end;

function TFunctionList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TFunctionList.get_FunctionParameters(AStringList: TStringList): string;
var
  Loop : Integer;
  iLeftParentheseCount : integer;
begin
  Result := '';
  iLeftParentheseCount := 0;

  for Loop := 0 to AStringList.Count-1 do begin
    if AStringList[Loop] = '(' then begin
      iLeftParentheseCount := iLeftParentheseCount + 1;
      if iLeftParentheseCount > 1 then
        raise Exception.Create('TFunctionList.get_FunctionParameters: iLeftParentheseCount > 1');

      Continue;
    end else if AStringList[Loop] = ')' then begin
      iLeftParentheseCount := iLeftParentheseCount - 1;
      if iLeftParentheseCount < 0 then
        raise Exception.Create('TFunctionList.get_FunctionParameters: iLeftParentheseCount < 0');

      Continue;
    end;

    if iLeftParentheseCount = 1 then begin
      Result := Result + AStringList[Loop];
      if AStringList[Loop] = ';' then Result := Result + ' ';
    end;
  end;
end;

function TFunctionList.GetExportFunctionTexts(Index: integer): string;
var
  Loop: Integer;
  sParameters : string;
  StringList : TStringList;
begin
  StringList := TStringList(FList[Index]);

  if StringList.Count < 3 then
    raise Exception.Create('TFunctionList.GetExportFunctionTexts: StringList.Count < 3');

  sParameters := get_FunctionParameters(StringList);

  if sParameters <> '' then
    Result := Format('%s _%s(AHandle:pointer; ', [StringList[0], StringList[1]])
  else
    Result := Format('%s _%s(AHandle:pointer)', [StringList[0], StringList[1]]);

  for Loop := 2 to StringList.Count-1 do begin
    if StringList[Loop] = '(' then Continue;

    Result := Result + StringList[Loop];

    if StringList[Loop] = ';' then Result := Result + ' ';
  end;

  Result := Result + 'export;' + #13#10;

  Result := Result + 'begin' + #13#10;

  if sParameters <> '' then
    Result := Result + Format('  %s.%s(%s);', [FClassName, StringList[1], sParameters]) + #13#10
  else
    Result := Result + Format('  %s.%s;', [FClassName, StringList[1]]) + #13#10;

  Result := Result + 'end;';
end;

function TFunctionList.GetDllText: string;
var
  Loop: Integer;
begin
  Result := '';
  for Loop := 0 to FList.Count-1 do
    Result := Result + ExportFunctionTexts[Loop] + #13#10 + #13#10;
end;

function TFunctionList.GetExportsText: string;
var
  Loop: Integer;
begin
  Result := 'exports' + #13#10 + '  _CreateObject, _DestroyObject, ' + #13#10;

  for Loop := 0 to FList.Count-1 do begin
    if Loop < FList.Count-1 then
      Result := Result + '_' + FunctionNames[Loop] + ', ' + #13#10
    else
      Result := Result + '_' + FunctionNames[Loop] + ';' + #13#10
  end;
end;

function TFunctionList.GetExternalFunctionTexts(Index: integer): string;
var
  Loop: Integer;
  sDllName, sParameters : string;
  StringList : TStringList;
begin
  StringList := TStringList(FList[Index]);

  sDllName := 'lib' + Copy(FClassName, 2, Length(FClassName));

  if StringList.Count < 3 then
    raise Exception.Create('TFunctionList.GetExternalFunctionTexts: StringList.Count < 3');

  sParameters := get_FunctionParameters(StringList);

  if sParameters <> '' then
    Result := Format('%s _%s(AHandle:pointer; ', [StringList[0], StringList[1]])
  else
    Result := Format('%s _%s(AHandle:pointer)', [StringList[0], StringList[1]]);

  for Loop := 2 to StringList.Count-1 do begin
    if StringList[Loop] = '(' then Continue;

    Result := Result + StringList[Loop];

    if StringList[Loop] = ';' then Result := Result + ' ';
  end;

  Result := Result + Format('external ''%s.dll'';', [sDllName]);
end;

function TFunctionList.GetExternalsText: string;
var
  Loop: Integer;
begin
  Result := '';
  for Loop := 0 to FList.Count-1 do
    Result := Result + ExternalFunctionTexts[Loop] + #13#10;
end;

function TFunctionList.GetFunctionNames(Index: integer): string;
var
  Loop: Integer;
  sParameters : string;
  StringList : TStringList;
begin
  StringList := TStringList(FList[Index]);

  if StringList.Count < 2 then
    raise Exception.Create('TFunctionList.GetDllFunctionTexts: StringList.Count < 3');

  Result := StringList[1];
end;

function TFunctionList.GetFunctionTexts(Index: integer): string;
var
  Loop: Integer;
  StringList : TStringList;
begin
  StringList := TStringList(FList[Index]);

  Result := '';
  for Loop := 0 to StringList.Count-1 do
    Result := Result + StringList[Loop] + ' ';
end;

function TFunctionList.GetMethodImplementationText: string;
var
  Loop: Integer;
  sParameters : string;
  StringList : TStringList;
begin
  Result := '';

  for Loop := 0 to FList.Count-1 do begin
    StringList := TStringList(FList[Loop]);

    if StringList.Count < 3 then
      raise Exception.Create('TFunctionList.GetMethodImplementationText: StringList.Count < 3');

    sParameters := get_FunctionParameters(StringList);

    if sParameters <> '' then
      Result := Result  + Format('%s %s.%s(%s);', [StringList[0], FClassName, StringList[1], sParameters]) + #13#10
    else
      Result := Result  + Format('%s %s.%s;', [StringList[0], FClassName, StringList[1]]) + #13#10;

      Result := Result + 'begin' + #13#10;

      if sParameters <> '' then
        Result := Result + Format('  _%s(FHandle, %s);', [StringList[1], sParameters]) + #13#10
      else
        Result := Result + Format('  _%s(FHandle);', [StringList[1]]) + #13#10;

      Result := Result + 'end;' + #13#10;

      if Loop <> FList.Count-1 then Result := Result + #13#10;
  end;
end;

function TFunctionList.GetMethodInterfaceText: string;
var
  Loop: Integer;
  sParameters : string;
  StringList : TStringList;
begin
  Result := '';

  for Loop := 0 to FList.Count-1 do begin
    StringList := TStringList(FList[Loop]);

    if StringList.Count < 3 then
      raise Exception.Create('TFunctionList.GetMethodInterfaceText: StringList.Count < 3');

    sParameters := get_FunctionParameters(StringList);

    if sParameters <> '' then
      Result := Result  + Format('    %s(%s);', [StringList[1], sParameters])
    else
      Result := Result  + Format('    %s;', [StringList[1]]);

    if Loop <> FList.Count-1 then Result := Result + #13#10;
  end;
end;

function TFunctionList.GetText: string;
var
  Loop: Integer;
begin
  Result := '';
  for Loop := 0 to FList.Count-1 do
    Result := Result + FunctionTexts[Loop] + #13#10;
end;

procedure TFunctionList.SetClassName(const Value: string);
begin
  FClassName := Value;
end;

end.
