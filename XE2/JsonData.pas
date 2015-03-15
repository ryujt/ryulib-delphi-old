unit JsonData;

interface

uses
  Data.DBXJSON,
  Windows, Classes, SysUtils;

type
  TJsonValueClass = class of TJsonValue;
  TJsonValueHelper = class helper for TJsonValue
  private
    function GetValueAsBoolean(AName: string): boolean;
    function GetValueAsString(AName: string): string;
    function GetValueAsInt(AName: string): integer;
    function GetValueAsJson(AName: string): TJSONValue;
    procedure SetValueAsBoolean(AName: string; const Value: boolean);
    procedure SetValueAsInt(AName: string; const Value: integer);
    procedure SetValueAsJson(AName: string; const Value: TJSONValue);
    procedure SetValueAsString(AName: string; const Value: string);
  public
    class function LoadFromFile(AFileName:string):TJSONValue;
    procedure SaveToFile(AFileName:string);
  public
    property ValueAsJson[AName:string]:TJSONValue read GetValueAsJson write SetValueAsJson;
    property ValueAsInt[AName:string]:integer read GetValueAsInt write SetValueAsInt;
    property ValueAsString[AName:string]:string read GetValueAsString write SetValueAsString;
    property ValueAsBoolean[AName:string]:boolean read GetValueAsBoolean write SetValueAsBoolean;
  end;

  TJsonData = class
  private
    FJSONObject: TJSONObject;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetBooleans(AName: string): Boolean;
    function GetDoubles(AName: string): double;
    function GetInt64s(AName: string): int64;
    function GetIntegers(AName: string): integer;
    procedure SetBooleans(AName: string; const Value: Boolean);
    procedure SetDoubles(AName: string; const Value: double);
    procedure SetInt64s(AName: string; const Value: int64);
    procedure SetIntegers(AName: string; const Value: integer);
    function GetValues(AName: string): string;
    procedure SetValues(AName: string; const Value: string);
    function GetCount: integer;
    function GetNames(Index: integer): string;
  public
    constructor Create; overload;
    constructor Create(AJsonObject:TJSONObject); overload;
    destructor Destroy; override;

    procedure LoadFromFile(AFileName:string);
    procedure SaveToFile(AFileName:string);

    function GetJsonData(AName:string):TJsonData;
    function GetJsonText(AName:string):string;
    procedure SetJsonData(AName,AText:string);

    procedure Delete(AIndex:integer); overload;
    procedure Delete(AName:string); overload;

    property Names[Index : integer] : string read GetNames;
    property Values[AName:string] : string read GetValues write SetValues;

    property Integers[AName:string] : integer read GetIntegers write SetIntegers;
    property Int64s[AName:string] : int64 read GetInt64s write SetInt64s;
    property Doubles[AName:string] : double read GetDoubles write SetDoubles;
    property Booleans[AName:string] : Boolean read GetBooleans write SetBooleans;

    property Count : integer read GetCount;
    property Text : string read GetText write SetText;
  end;

implementation

{ TResponseData }

constructor TJsonData.Create;
begin
  inherited;

  FJSONObject := nil;
end;

constructor TJsonData.Create(AJsonObject: TJSONObject);
begin
  inherited Create;

  FJSONObject := TJSONObject(AJsonObject.Clone);
end;

procedure TJsonData.Delete(AIndex: integer);
begin
  FJSONObject.RemovePair(GetNames(AIndex));
end;

procedure TJsonData.Delete(AName: string);
begin
  FJSONObject.RemovePair(AName);
end;

destructor TJsonData.Destroy;
begin
  if FJSONObject <> nil then FreeAndNil(FJSONObject);

  inherited;
end;

function TJsonData.GetBooleans(AName: string): Boolean;
var
  Pair: TJSONPair;
begin
  Result := False;
  if FJSONObject = nil then Exit;

  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result := TJSONNumber(Pair.JsonValue).AsInt = 1;
end;

function TJsonData.GetDoubles(AName: string): double;
var
  Pair: TJSONPair;
begin
  Result := 0;
  if FJSONObject = nil then Exit;

  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result := TJSONNumber(Pair.JsonValue).AsDouble;
end;

function TJsonData.GetInt64s(AName: string): int64;
var
  Pair: TJSONPair;
begin
  Result := 0;
  if FJSONObject = nil then Exit;

  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result := TJSONNumber(Pair.JsonValue).AsInt64;
end;

function TJsonData.GetIntegers(AName: string): integer;
var
  Pair: TJSONPair;
begin
  Result := 0;
  if FJSONObject = nil then Exit;

  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result := TJSONNumber(Pair.JsonValue).AsInt;
end;

function TJsonData.GetJsonData(AName: string): TJsonData;
var
  Pair: TJSONPair;
begin
  Result := TJsonData.Create;
  if FJSONObject = nil then Exit;

  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result.Text := TJSONObject(Pair.JsonValue).ToString;
end;

function TJsonData.GetJsonText(AName:string):string;
var
  JsonData : TJsonData;
begin
  JsonData := GetJsonData(AName);
  try
    Result := JsonData.Text;
  finally
    JsonData.Free;
  end;
end;

function TJsonData.GetNames(Index: integer): string;
begin
  Result := '';
  if FJSONObject = nil then Exit;

  Result := TJSONPair(FJSONObject.Get(Index)).JsonString.Value;
end;

function TJsonData.GetCount: integer;
begin
  Result := 0;
  if FJSONObject = nil then Exit;

  Result := FJSONObject.Size
end;

function TJsonData.GetText: string;
begin
  Result := '';
  if FJSONObject = nil then Exit;

  Result := FJSONObject.ToString
end;

function TJsonData.GetValues(AName: string): string;
var
  Pair: TJSONPair;
begin
  Result := '';
  if FJSONObject = nil then Exit;
  
  Pair := FJSONObject.Get(AName);
  if Pair <> nil then Result := Pair.JsonValue.Value;
end;

procedure TJsonData.LoadFromFile(AFileName: string);
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    if FileExists(AFileName) then begin
      List.LoadFromFile(AFileName);
      SetText(List.Text);
    end else begin
      SetText('');
    end;
  finally
    List.Free;
  end;
end;

procedure TJsonData.SaveToFile(AFileName: string);
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := Text;
    List.SaveToFile(AFileName);
  finally
    List.Free;
  end;
end;

procedure TJsonData.SetBooleans(AName: string; const Value: Boolean);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, TJSONNumber.Create(Integer(Value)));
    Exit;
  end;

  Pair := FJSONObject.Get(AName);
  if Pair = nil then begin
    FJSONObject.AddPair(AName, TJSONNumber.Create(Integer(Value)));
  end else begin
    if Pair.JsonValue <> nil then Pair.JsonValue.Free;
    Pair.JsonValue := TJSONNumber.Create(Integer(Value));
  end;
end;

procedure TJsonData.SetDoubles(AName: string; const Value: double);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
    Exit;
  end;

  Pair := FJSONObject.Get(AName);
  if Pair = nil then begin
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
  end else begin
    if Pair.JsonValue <> nil then Pair.JsonValue.Free;
    Pair.JsonValue := TJSONNumber.Create(Value);
  end;
end;

procedure TJsonData.SetInt64s(AName: string; const Value: int64);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
    Exit;
  end;

  Pair := FJSONObject.Get(AName);
  if Pair = nil then begin
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
  end else begin
    if Pair.JsonValue <> nil then Pair.JsonValue.Free;
    Pair.JsonValue := TJSONNumber.Create(Value);
  end;
end;

procedure TJsonData.SetIntegers(AName: string; const Value: integer);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
    Exit;
  end;

  Pair := FJSONObject.Get(AName);
  if Pair = nil then begin
    FJSONObject.AddPair(AName, TJSONNumber.Create(Value));
  end else begin
    if Pair.JsonValue <> nil then Pair.JsonValue.Free;
    Pair.JsonValue := TJSONNumber.Create(Value);
  end;
end;

procedure TJsonData.SetJsonData(AName,AText:string);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, TJSONObject(TJSONObject.ParseJSONValue(AText)));
  end else begin
    Pair := FJSONObject.Get(AName);
    if Pair = nil then begin
      FJSONObject.AddPair(AName, TJSONObject(TJSONObject.ParseJSONValue(AText)));
    end else begin
      if Pair.JsonValue <> nil then Pair.JsonValue.Free;
      Pair.JsonValue := TJSONObject(TJSONObject.ParseJSONValue(AText));
    end;
  end;
end;

procedure TJsonData.SetText(const Value: string);
var
  OldObject : TJSONObject;
begin
  if FJSONObject <> nil then OldObject := FJSONObject
  else OldObject := nil;

  FJSONObject := TJSONObject(TJSONObject.ParseJSONValue(Value));

  if OldObject <> nil then FreeAndNil(OldObject);
end;

procedure TJsonData.SetValues(AName: string; const Value: string);
var
  Pair: TJSONPair;
begin
  if FJSONObject = nil then begin
    FJSONObject := TJSONObject.Create;
    FJSONObject.AddPair(AName, Value);
    Exit;
  end;

  Pair := FJSONObject.Get(AName);
  if Pair = nil then begin
    FJSONObject.AddPair(AName, Value);
  end else begin
    if Pair.JsonValue <> nil then Pair.JsonValue.Free;
    Pair.JsonValue := TJSONString.Create(Value);
  end;
end;

{ TJsonValueHelper }

function TJsonValueHelper.GetValueAsJson(AName: string): TJSONValue;
var
  pair:TJSONPair;
begin
  Result:= nil;
  if Self is TJSONObject then begin
    try
      pair:= TJSONObject(Self).Get(AName);
      if pair <> nil then
      begin
        Result:= pair.JsonValue;
      end;
    except
    end;
  end;
end;

function TJsonValueHelper.GetValueAsBoolean(AName: string): boolean;
var
  pair:TJSONPair;
begin
  Result:= False;
  if Self is TJSONObject then begin
    try
      pair:= TJSONObject(Self).Get(AName);
      if pair <> nil then
      begin
        if  pair.JsonValue is TJSONNumber  then
          Result:= TJSONNumber(pair.JsonValue).AsInt = 1
        else
          Result:= pair.JsonValue is TJSONTrue;
      end;
    except
    end;
  end;
end;

function TJsonValueHelper.GetValueAsInt(AName: string): integer;
var
  pair:TJSONPair;
begin
  Result:= 0;

  if Self is TJSONObject then begin
    try
      pair:= TJSONObject(Self).Get(AName);
      if (pair <> nil) then Result:= StrToIntDef(pair.JsonValue.Value, 0);
    except
    end;
  end;
end;

function TJsonValueHelper.GetValueAsString(AName: string): string;
var
  pair:TJSONPair;
begin
  Result:= '';

  if Self is TJSONObject then begin
    try
      pair:= TJSONObject(Self).Get(AName);
      if pair <> nil then Result:= pair.JsonValue.Value;
    except
    end;
  end;
end;

class function TJsonValueHelper.LoadFromFile(AFileName: string): TJSONValue;
var
  Buffer: TStringList;
begin
  Result:= nil;

  if FileExists(AFileName) then
  begin
    Buffer:= TStringList.Create;
    try
      Buffer.LoadFromFile(AFileName);
      Result:= TJSONObject.ParseJSONValue(Buffer.GetText);
    finally
      Buffer.Free;
    end;
  end;
end;

procedure TJsonValueHelper.SaveToFile(AFileName: string);
var
  Buffer: TStringList;
begin
  Buffer:= TStringList.Create;
  try
    Buffer.Text:= ToString;
    Buffer.SaveToFile(AFileName);
  finally
    Buffer.Free;
  end;
end;

procedure TJsonValueHelper.SetValueAsBoolean(AName: string;
  const Value: boolean);
var
  Pair: TJSONPair;
  NewValueClass: TJsonValueClass;
begin
  if Self is TJSONObject then
  begin
    if Value then NewValueClass:= TJSONTrue
             else NewValueClass:= TJSONFalse;

    with TJSONObject(Self) do
    begin
      Pair:= RemovePair(AName);
      if Pair <> nil then Pair.Free;
      AddPair(AName, NewValueClass.Create);
    end;
  end;
end;

procedure TJsonValueHelper.SetValueAsInt(AName: string; const Value: integer);
var
  Pair: TJSONPair;
  NewValue: TJSONValue;
begin
  if Self is TJSONObject then
  begin
    NewValue:= TJSONNumber.Create(Value);
    with TJSONObject(Self) do
    begin
      Pair:= RemovePair(AName);
      if Pair <> nil then Pair.Free;
      AddPair(AName, NewValue);
    end;
  end;
end;

procedure TJsonValueHelper.SetValueAsJson(AName: string;
  const Value: TJSONValue);
var
  Pair: TJSONPair;
  NewValue: TJSONValue;
begin
  if Self is TJSONObject then
  begin
    NewValue:= Value;
    with TJSONObject(Self) do
    begin
      Pair:= RemovePair(AName);
      if Pair <> nil then Pair.Free;
      AddPair(AName, NewValue);
    end;
  end;
end;

procedure TJsonValueHelper.SetValueAsString(AName: string;
  const Value: string);
var
  Pair: TJSONPair;
  NewValue: TJSONValue;
begin
  if Self is TJSONObject then
  begin
    NewValue:= TJSONString.Create( Value );
    with TJSONObject(Self) do
    begin
      Pair:= RemovePair(AName);
      if Pair <> nil then Pair.Free;
      AddPair(AName, NewValue);
    end;
  end;
end;

end.
