unit HttpDownload;

interface

uses
  Windows, SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, ComCtrls, Forms, ZLibUtils;

type
  TProgressEvent = procedure (Sender:TObject; Percent:integer) of object;

  THttpDownload = class (TComponent)
  private
    FTotalSize : int64;
    FCurrentSize : int64;
    FStoped : boolean;
    FIdHTTP : TIdHTTP;
    procedure do_SaveFile(Stream:TStream);
    procedure on_WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
    procedure on_Work(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
  private
    FFileName: string;
    FURL: string;
    FOnProgress: TProgressEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  published
    property URL : string read FURL write FURL;
    property FileName : string read FFileName write FFileName;
    property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ THttpDownload }

constructor THttpDownload.Create(AOwner: TComponent);
begin
  inherited;

  FIdHTTP := TIdHTTP.Create(Self);
  FIdHTTP.OnWorkBegin := on_WorkBegin;
  FIdHTTP.OnWork := on_Work;
end;

destructor THttpDownload.Destroy;
begin
  FIdHTTP.Free;

  inherited;
end;

procedure THttpDownload.do_SaveFile(Stream: TStream);
var
  fsData : TFileStream;
begin
  fsData := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Position := 0;
    ExpandStream(Stream, fsData);
  finally
    fsData.Free;
  end;
end;

procedure THttpDownload.on_Work(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  FCurrentSize := FCurrentSize + AWorkCount;

  if Assigned(FOnProgress) then
    FOnProgress(Self, (FCurrentSize*100) div FTotalSize);

  Application.ProcessMessages;
  if FStoped then
    raise Exception.Create('THttpDownload.on_Work: Stoped!');
end;

procedure THttpDownload.on_WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  FTotalSize := AWorkCountMax;
end;

procedure THttpDownload.Start;
var
  msData : TMemoryStream;
begin
  FStoped := false;
  FTotalSize := 0;
  FCurrentSize := 0;

  msData := TMemoryStream.Create;
  try
    try
      FIdHTTP.Get(URL, msData);
      do_SaveFile(msData);
    except
      // Todo : 
    end;
  finally
    msData.Free;
  end;
end;

procedure THttpDownload.Stop;
begin
  FStoped := true;
end;

end.
