unit VersionList;

interface

uses
  Classes, SysUtils, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

type
  TListItem = class (TStringList)
  private
    function GetFileName: string;
    function GetFileSize: int64;
    function GetPath: string;
    function GetVersion: int64;
  public
    property Version : int64 read GetVersion;
    property FileName : string read GetFileName;
    property Path : string read GetPath;
    property FileSize : int64 read GetFileSize;
  end;

  TVersionList = class
  private
    FList : TStringList;
    function GetItems(Index: integer): TListItem;
    function GetFileNames(Index: integer): string;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream:TStream);
    procedure LoadFromFile(FileName:string);
    procedure LoadFromURL(URL:string);

    function IndexOfFileName(FileName:string):integer;

    property Count : integer read GetCount;
    property FileNames[Index:integer] : string read GetFileNames;
    property Items[Index:integer] : TListItem read GetItems;
  end;

implementation

{ TVersionList }

constructor TVersionList.Create;
begin
  inherited;

  FList := TStringList.Create;
end;

destructor TVersionList.Destroy;
begin
  FList.Free;

  inherited;
end;

function TVersionList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TVersionList.GetFileNames(Index: integer): string;
begin
  Result := FList[Index];
end;

function TVersionList.GetItems(Index: integer): TListItem;
begin
  Result := Pointer(FList.Objects[Index]);
end;

function TVersionList.IndexOfFileName(FileName: string): integer;
begin
  Result := FList.IndexOfName(FileName);
end;

procedure TVersionList.LoadFromFile(FileName: string);
var
  List : TStringList;
  Item : TListItem;
  Loop : Integer;
begin
  // 파일명=버전정보;원래파일명;폴더;사이즈;

  FList.Clear;

  if not FileExists(FileName) then Exit;  

  List := TStringList.Create;
  try
    List.LoadFromFile(FileName);

    for Loop := 0 to List.Count-1 do begin
      Item := TListItem.Create;
      Item.Delimiter := ';';
      Item.DelimitedText := List.ValueFromIndex[Loop];

      FList.AddObject(List.Names[Loop], Item);
    end;
  finally
    List.Free;
  end;  
end;

procedure TVersionList.LoadFromStream(Stream: TStream);
var
  List : TStringList;
  Item : TListItem;
  Loop : Integer;
begin
  // 파일명=버전정보;원래파일명;폴더;사이즈;

  List := TStringList.Create;
  try
    List.LoadFromStream(Stream);

    for Loop := 0 to List.Count-1 do begin
      Item := TListItem.Create;
      Item.Delimiter := ';';
      Item.DelimitedText := List.ValueFromIndex[Loop];

      FList.AddObject(List.Names[Loop], Item);
    end;
  finally
    List.Free;
  end;  
end;

procedure TVersionList.LoadFromURL(URL: string);
var
  IdHTTP : TIdHTTP;
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  IdHTTP := TIdHTTP.Create(nil);
  try
    IdHTTP.Get(URL, msData);
    msData.Position := 0;

    LoadFromStream(msData);
  finally
    IdHTTP.Free;
    msData.Free;
  end;
end;

{ TListItem }

function TListItem.GetFileName: string;
begin
  Result := Strings[1];
end;

function TListItem.GetFileSize: int64;
begin
  Result := StrToIntDef(Strings[3], 0);
end;

function TListItem.GetPath: string;
begin
  Result := Strings[2];
end;

function TListItem.GetVersion: int64;
begin
  Result := StrToIntDef(Strings[0], 0);
end;

end.
