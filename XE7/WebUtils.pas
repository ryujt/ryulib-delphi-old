unit WebUtils;

interface

uses
  JsonData,
  WinInet,
  Windows, SysUtils, Classes;

function GetHTTP(const AURL:string):string;
function GetHTTP_JSON_Value(const AURL,AName:string):string;

implementation

function GetHTTP(const AURL:string):string;
var
  NetHandle : HINTERNET;
  UrlHandle : HINTERNET;
  BytesRead : DWord;
  Buffer : array[0..1024] of AnsiChar;
  ResultLine : AnsiString;
begin
  Result := '';

  NetHandle := InternetOpen('Delphi', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(NetHandle) then Exit;

  try
    UrlHandle := InternetOpenUrl(NetHandle, PChar(AURL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not Assigned(UrlHandle) then Exit;

    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        ResultLine := ResultLine + Buffer;
        FillChar(Buffer, SizeOf(Buffer), 0);
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
      until BytesRead = 0;
    finally
      InternetCloseHandle(UrlHandle);
    end;
  finally
    InternetCloseHandle(NetHandle);
  end;

  Result := ResultLine;
end;

function GetHTTP_JSON_Value(const AURL,AName:string):string;
var
  JsonData : TJsonData;
begin
  Result := '';

  JsonData := TJsonData.Create;
  try
    JsonData.Text := GetHTTP(AURL);
    Result := JsonData.Values[AName];
  finally
    JsonData.Free;
  end;
end;

end.
