unit NetTools;

interface

uses
  Windows, Classes, SysUtils, IdStack, WinInet;

function GetLocalIP:string;
function HttpGet(const AURL:string):string;

implementation

function GetLocalIP:string;
var
  IdStack : TIdStack;
begin
  IdStack := TIdStack.Create;
  try
    Result := IdStack.LocalAddress;
  finally
    IdStack.Free;
  end;
end;

function HttpGet(const AURL:string):string;
var
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..4096] of Char;
  BytesRead: dWord;
  ssData : TStringStream;
begin
  Result := '';

  NetHandle := InternetOpen('Mozilla/5.0', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(NetHandle) then begin
    UrlHandle := InternetOpenUrl(NetHandle, PChar(AURL), nil, 0, INTERNET_FLAG_RELOAD, 0);

    if Assigned(UrlHandle) then begin
      ssData := TStringStream.Create;
      try
        repeat
          InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
          ssData.Write( Buffer, BytesRead );
        until BytesRead = 0;

        Result := ssData.DataString;
      finally
        ssData.Free;
      end;

      InternetCloseHandle(UrlHandle);

    end else
      raise Exception.CreateFmt('NetTools.HttpGet - Cannot open URL %s', [AURL]);

    InternetCloseHandle(NetHandle);
  end else
    raise Exception.Create('NetTools.HttpGet - Unable to initialize Wininet');
end;

end.
