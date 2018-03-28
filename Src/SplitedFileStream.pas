unit SplitedFileStream;

interface

uses
  SearchDir, Disk,
  SysUtils, Classes;

type
  {*
    정해진 크기(SplitSize)로 파일을 나눠서 저장하고, 이를 하나의 파일처럼 읽는 클래스
  }
  TSplitedFileStream = class
  private
    FMode : word;
    FFileName : string;
    FFileStream : TFileStream;
  private
    FCount : integer;
    FIsEOF : boolean;
    FSplitSize: integer;
    FSize: int64;
    FPosition: int64;
    function GetIsEOF: boolean;
    function GetIsOpened: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenWrite(AFileName:string);
    procedure OpenRead(AFileName:string);
    procedure Close;

    function Read(var Buffer; Count: Longint): Longint;
    function Write(const ABuffer; ACount: Longint): Longint;
  public
    property SplitSize : integer read FSplitSize write FSplitSize;
    property FileName : string read FFileName;
    property IsOpened : boolean read GetIsOpened;
    property IsEOF : boolean read GetIsEOF;
    property Count : integer read FCount;
    property Position : int64 read FPosition;
    property Size : int64 read FSize;
  end;

implementation

{ TSplitedFileStream }

procedure TSplitedFileStream.Close;
begin
  FIsEOF := false;
  FPosition := 0;
  FSize := 0;

  if FFileStream <> nil then FreeAndNil(FFileStream);
end;

constructor TSplitedFileStream.Create;
begin
  inherited;

  FSplitSize := 0;

  FCount := 0;
  FFileName := '';
  FMode := 0;

  FFileStream := nil;

  Close;
end;

destructor TSplitedFileStream.Destroy;
begin
  FreeAndNil(FFileStream);

  inherited;
end;

function TSplitedFileStream.GetIsEOF: boolean;
begin
  Result := (FMode = fmCreate) or FIsEOF;
end;

function TSplitedFileStream.GetIsOpened: boolean;
begin
  Result := FFileStream <> nil;
end;

procedure TSplitedFileStream.OpenRead(AFileName: string);
var
  FileNameOnly : string;
begin
  FIsEOF := false;
  FPosition := 0;
  FSize := 0;

  FCount := 1;
  FFileName := AFileName;
  FMode := fmOpenRead;

  FFileStream := TFileStream.Create(FFileName + Format('.%d', [FCount]), FMode);

  FileNameOnly := ExtractFileName(AFileName);

  SearchFiles(
    ExtractFilePath(FFileName), false,
    procedure(Path:string; SearchRec:TSearchRec; var NeedStop:boolean) begin
      if Pos(LowerCase(FileNameOnly), LowerCase(SearchRec.Name)) = 1 then
        FSize := FSize + File_Size(Path+SearchRec.Name);
    end
  );
end;

procedure TSplitedFileStream.OpenWrite(AFileName: string);
begin
  FCount := 1;
  FFileName := AFileName;
  FMode := fmCreate;
  FFileStream := TFileStream.Create(FFileName + Format('.%d', [FCount]), FMode);
end;

function TSplitedFileStream.Read(var Buffer; Count: Integer): Longint;
var
  msData : TMemoryStream;
begin
  if FMode <> fmOpenRead then
   raise Exception.Create('TSplitedFileStream.Read - FMode <> fmOpenRead');

  Result := FFileStream.Read(Buffer, Count);
  FPosition := FPosition + Result;

  if Result < Count then begin
    FreeAndNil(FFileStream);
    FCount := FCount + 1;
    try
      FFileStream := TFileStream.Create(FFileName + Format('.%d', [FCount]), FMode);
    except
      FIsEOF := true;
      Exit;
    end;

    msData := TMemoryStream.Create;
    try
      msData.Write(Buffer, Result);

      Result := FFileStream.Read(Buffer, Count - Result);
      FPosition := FPosition + Result;

      msData.Write(Buffer, Result);

      msData.Position := 0;
      Result := msData.Size;

      msData.Read(Buffer, Result);
    finally
      msData.Free;
    end;
  end;
end;

function TSplitedFileStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  if FMode <> fmCreate then
   raise Exception.Create('TSplitedFileStream.Write - FMode <> fmCreate');

  if (FSplitSize > 0) and (FFileStream.Size >= FSplitSize) then begin
    FreeAndNil(FFileStream);
    FCount := FCount + 1;
    FFileStream := TFileStream.Create(FFileName + Format('.%d', [FCount]), FMode);
  end;

  Result := FFileStream.Write(ABuffer, ACount);
end;

end.
