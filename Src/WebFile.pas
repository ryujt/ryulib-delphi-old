unit WebFile;

interface

uses
  Disk, IdComponent, IdHTTP,
  Windows, SysUtils, Classes, Wininet;

type
  TDownloadingEvent = procedure (Sender:TObject; AFileSize,ACurrent:integer) of object;

  TWebFile = class (TComponent)
  private
    FIdHTTP : TIdHTTP;
    procedure on_WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure on_Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  private
    FCurrentSize: integer;
    FFileSize: integer;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloading: TDownloadingEvent;
    FOnDownloadEnd: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Download(const AURL,AFileName:string);
    procedure Stop;
  published
    property CurrentSize : integer read FCurrentSize;
    property FileSize : integer read FFileSize;
    property OnDownloadBegin : TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloading : TDownloadingEvent read FOnDownloading write FOnDownloading;
    property OnDownloadEnd : TNotifyEvent read FOnDownloadEnd write FOnDownloadEnd;
  end;

function DownloadFromURL(const AURL,ADst:string):boolean;

implementation

function DownloadFromURL(const AURL,ADst:string):boolean;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  Buffer: array[0..4096] of Char;
  dwBytesRead: DWORD;
  fsData : TFileStream;
begin
  Result := False;

  hSession := InternetOpen('Delphi', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then Exit;

  try
    hService := InternetOpenUrl(hSession, PChar(aUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not Assigned(hService) then Exit;

    fsData := TFileStream.Create(ADst, fmCreate);
    try
      while True do begin
        dwBytesRead := 4096;
        InternetReadFile(hService, @Buffer, 4096, dwBytesRead);
        if dwBytesRead = 0 then break;

        fsData.Write(Buffer, dwBytesRead);
      end;

      Result := True;
    finally
      InternetCloseHandle(hService);
      fsData.Free;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

{ TWebFile }

constructor TWebFile.Create(AOwner: TComponent);
begin
  inherited;

  FIdHTTP := TIdHTTP.Create(Self);
  FIdHTTP.OnWorkBegin := on_WorkBegin;
  FIdHTTP.OnWork := on_Work;
end;

destructor TWebFile.Destroy;
begin
  FreeAndNil(FIdHTTP);

  inherited;
end;

procedure TWebFile.Download(const AURL,AFileName: string);
var
  fsDst : TFileStream;
  Expires : TDateTime;
begin
  FFileSize := 0;
  FCurrentSize := 0;

  fsDst := TFileStream.Create(AFileName, fmCreate);
  try
    try
      FIdHTTP.Disconnect;
    except
    end;

    try
      Expires := 0;
      FIdHTTP.Request.CacheControl := 'no-cache';
      FIdHTTP.Request.Pragma := 'no-cache';
      FIdHTTP.Request.Expires := Expires;

      FIdHTTP.Get(AURL, fsDst);
    except
      on E: Exception do begin
        OutputDebugString(PChar(Format('TWebFile.Download: %s', [E.Message])));
      end;
    end;
  finally
    fsDst.Free;
  end;

  if Assigned(FOnDownloadEnd) then FOnDownloadEnd(Self);
end;

procedure TWebFile.on_Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if AWorkMode <> wmRead then Exit;
  if not FIdHTTP.Connected then Exit;

  FCurrentSize := AWorkCount;

  if Assigned(FOnDownloading) then FOnDownloading(Self, FFileSize, FCurrentSize);
end;

procedure TWebFile.on_WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  if AWorkMode <> wmRead then Exit;

  FFileSize := AWorkCountMax;

  if Assigned(FOnDownloadBegin) then FOnDownloadBegin(Self);
end;

procedure TWebFile.Stop;
begin
  FIdHTTP.Disconnect;
end;

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