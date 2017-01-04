unit WebUtils;

interface

uses
  JsonData,
  IdHttp,
  SysUtils, Classes;

function GetHTTP(const AURL:string):string;
function GetHTTP_JSON_Value(const AURL,AName:string):string;

implementation

function GetHTTP(const AURL:string):string;
var
  http : TIdHTTP;
begin
  Result := '';

  http := TIdHTTP.Create(nil);
  try
    Result := http.Get(AURL);
  finally
    http.Free;
  end;
end;

function GetHTTP_JSON_Value(const AURL,AName:string):string;
var
  http : TIdHTTP;
  JsonData : TJsonData;
begin
  Result := '';

  http := TIdHTTP.Create(nil);
  JsonData := TJsonData.Create;
  try
    JsonData.Text := http.Get(AURL);
    Result := JsonData.Values[AName];
  finally
    JsonData.Free;
    http.Free;
  end;
end;

end.
