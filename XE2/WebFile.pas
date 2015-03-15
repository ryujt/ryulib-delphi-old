unit WebFile;

interface

uses
  ExtActns;

implementation

end.


uses
  URLMon, ShellApi;

function DownloadFile(SourceFile, DestFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  // URL Location
  SourceFile = 'http://www.google.com/intl/de/images/home_title.gif';
  // Where to save the file
  DestFile = 'c:\temp\google-image.gif';
begin
  if DownloadFile(SourceFile, DestFile) then
  begin
    ShowMessage('Download succesful!');
    // Show downloaded image in your browser
    ShellExecute(Application.Handle, PChar('open'), PChar(DestFile),
      PChar(''), nil, SW_NORMAL)
  end
  else
    ShowMessage('Error while downloading ' + SourceFile)
end;

// Minimum availability: Internet Explorer 3.0
// Minimum operating systems Windows NT 4.0, Windows 95

{********************************************************}

{2.}


uses
  Wininet;

function DownloadURL(const aUrl: string): Boolean;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  lpBuffer: array[0..1024 + 1] of Char;
  dwBytesRead: DWORD;
begin
  Result := False;
  // hSession := InternetOpen( 'MyApp', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  hSession := InternetOpen('MyApp', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if Assigned(hSession) then
    begin
      hService := InternetOpenUrl(hSession, PChar(aUrl), nil, 0, 0, 0);
      if Assigned(hService) then
        try
          while True do
          begin
            dwBytesRead := 1024;
            InternetReadFile(hService, @lpBuffer, 1024, dwBytesRead);
            if dwBytesRead = 0 then break;
            lpBuffer[dwBytesRead] := #0;
            Form1.Memo1.Lines.Add(lpBuffer);
          end;
          Result := True;
        finally
          InternetCloseHandle(hService);
        end;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

{********************************************************}

{3. Forces a download of the requested file, object, or directory listing from the origin server,
    not from the cache
}

function DownloadURL_NOCache(const aUrl: string; var s: String): Boolean;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  lpBuffer: array[0..1024 + 1] of Char;
  dwBytesRead: DWORD;
begin
  Result := False;
  s := '';
  // hSession := InternetOpen( 'MyApp', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  hSession := InternetOpen('MyApp', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if Assigned(hSession) then
    begin
      hService := InternetOpenUrl(hSession, PChar(aUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
      if Assigned(hService) then
        try
          while True do
          begin
            dwBytesRead := 1024;
            InternetReadFile(hService, @lpBuffer, 1024, dwBytesRead);
            if dwBytesRead = 0 then break;
            lpBuffer[dwBytesRead] := #0;
            s := s + lpBuffer;
          end;
          Result := True;
        finally
          InternetCloseHandle(hService);
        end;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

//aufrufen
var
  s: String;
begin
 if DownloadURL('http://www.swissdelphicenter.ch/', s) then
   ShowMessage(s);
end;