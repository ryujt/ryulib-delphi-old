unit ValueList;

interface

uses
  Windows, Classes, SysUtils, IniFiles;

type
  TValueList = class(TStringList)
  private
    function GetIntegers(Name: string): integer;
    procedure SetIntegers(Name: string; const Value: integer);
    function GetInt64s(Name: string): int64;
    procedure SetInt64s(Name: string; const Value: int64);
    function GetReals(Name: string): real;
    procedure SetReals(Name: string; const Value: real);
    function GetDoubles(Name: string): double;
    procedure SetDoubles(Name: string; const Value: double);
    function GetTotalDouble: double;
    function GetTotalInt64: int64;
    function GetTotalInteger: integer;
    function GetTotalReal: real;
    function GetPointers(Name: string): pointer;
    procedure SetPointers(Name: string; const Value: pointer);
    function GetBooleans(Name: string): Boolean;
    procedure SetBooleans(Name: string; const Value: Boolean);
    function GetRect(Name: string): TRect;
    procedure SetRect(Name: string; const Value: TRect);
  public
    constructor Create;

    procedure LoadFromBuffer(Data:pointer; DataSize:integer);
    procedure SaveToBuffer(var Data:pointer; var DataSize:integer);
    function Exists(Name: string): Boolean;

    property Integers[Name:string] : integer read GetIntegers write SetIntegers;
    property Int64s[Name:string] : int64 read GetInt64s write SetInt64s;
    property Reals[Name:string] : real read GetReals write SetReals;
    property Doubles[Name:string] : double read GetDoubles write SetDoubles;
    property Pointers[Name:string] : pointer read GetPointers write SetPointers;
    property Booleans[Name:string] : Boolean read GetBooleans write SetBooleans;
    property Rects[Name:string]: TRect read GetRect write SetRect;

    property TotalInteger : integer read GetTotalInteger;
    property TotalInt64 : int64 read GetTotalInt64;
    property TotalReal : real read GetTotalReal;
    property TotalDouble : double read GetTotalDouble;
  end;

implementation

{ TValueList }

constructor TValueList.Create;
begin
  inherited;

  LineBreak := '<rYu>';
  StrictDelimiter := true;
end;

function TValueList.Exists(Name: string): Boolean;
begin
  Result := IndexOfName(Name) <> -1;
end;

function TValueList.GetBooleans(Name: string): Boolean;
begin
  Result := Values[Name] = '1';
end;

function TValueList.GetDoubles(Name: string): double;
begin
  Result:= StrToFloatDef(Values[Name], 0);
end;

function TValueList.GetInt64s(Name: string): int64;
begin
  Result:= StrToInt64Def(Values[Name], 0);
end;

function TValueList.GetIntegers(Name: string): integer;
begin
  Result:= StrToIntDef(Values[Name], 0);
end;

function TValueList.GetPointers(Name: string): pointer;
begin
  Result:= Pointer(StrToIntDef(Values[Name], 0));
end;

function TValueList.GetReals(Name: string): real;
begin
  Result:= StrToFloatDef(Values[Name], 0);
end;

function TValueList.GetRect(Name: string): TRect;
var
  SL: TValueList;
begin
  Result := Rect(0,0,0,0);
  if not Exists(Name) then Exit;
  
  SL := TValueList.Create;
  try
    SL.CommaText := Values[Name];
    Result.Left := SL.Integers['Left'];
    Result.Top := SL.Integers['Top'];
    Result.Right := SL.Integers['Right'];
    Result.Bottom := SL.Integers['Bottom'];
  finally
    SL.Free;
  end;
end;

function TValueList.GetTotalDouble: double;
var
  Name : string;
  Loop : integer;
begin
  Result:= 0;
  for Loop := 0 to Count - 1 do begin
    Name:= Names[Loop];
    if Name = '' then Continue;

    Result:= Result + Doubles[Name];
  end;
end;

function TValueList.GetTotalInt64: int64;
var
  Name : string;
  Loop : integer;
begin
  Result:= 0;
  for Loop := 0 to Count - 1 do begin
    Name:= Names[Loop];
    if Name = '' then Continue;

    Result:= Result + Int64s[Name];
  end;
end;

function TValueList.GetTotalInteger: integer;
var
  Name : string;
  Loop : integer;
begin
  Result:= 0;
  for Loop := 0 to Count - 1 do begin
    Name:= Names[Loop];
    if Name = '' then Continue;

    Result:= Result + Integers[Name];
  end;
end;

function TValueList.GetTotalReal: real;
var
  Name : string;
  Loop : integer;
begin
  Result:= 0;
  for Loop := 0 to Count - 1 do begin
    Name:= Names[Loop];
    if Name = '' then Continue;

    Result:= Result + Reals[Name];
  end;
end;

procedure TValueList.LoadFromBuffer(Data: pointer; DataSize: integer);
var
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    msData.Write(Data^, DataSize);
    msData.Position:= 0;
    LoadFromStream(msData);
  finally
    msData.Free;
  end;          
end;

procedure TValueList.SaveToBuffer(var Data: pointer; var DataSize: integer);
var
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    SaveToStream(msData);
    msData.Position := 0;
    DataSize := msData.Size;

    if DataSize > 0 then begin
      GetMem(Data, DataSize);
      msData.Read(Data^, DataSize);
    end else
      Data := nil;
  finally
    msData.Free;
  end;
end;

procedure TValueList.SetBooleans(Name: string; const Value: Boolean);
begin
  case Value of
    True : Values[Name]:= IntToStr(1);
    False: Values[Name]:= IntToStr(0);
  end;
end;

procedure TValueList.SetDoubles(Name: string; const Value: double);
begin
  Values[Name]:= FloatToStr(Value);
end;

procedure TValueList.SetInt64s(Name: string; const Value: int64);
begin
  Values[Name]:= IntToStr(Value);
end;

procedure TValueList.SetIntegers(Name: string; const Value: integer);
begin
  Values[Name]:= IntToStr(Value);
end;

procedure TValueList.SetPointers(Name: string; const Value: pointer);
begin
  Values[Name]:= IntToStr(Integer(Value));
end;

procedure TValueList.SetReals(Name: string; const Value: real);
begin
  Values[Name]:= FloatToStr(Value);
end;

procedure TValueList.SetRect(Name: string; const Value: TRect);
begin
  Values[Name] := Format('Left=%d,Top=%d,Right=%d,Bottom=%d', [Value.Left, Value.Top, Value.Right, Value.Bottom]);
end;

end.
