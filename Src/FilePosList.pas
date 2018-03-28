unit FilePosList;

interface

uses
  Classes, SysUtils;

type
  TFilePosInfo = class
  private
  public
    Key : string;
    Size : int64;
    Position : int64;
  end;

  TFilePosList = class
  strict private
    FList : TList;
  private
    function GetCount: integer;
    function GetItemsByIndex(AIndex: integer): TFilePosInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddByKey(AKey:string; APosition,ASize:int64);
    procedure AddByIndex(AIndex:integer; APosition,ASize:int64);

    // Todo :
//    property Items[AKey:string] : TFilePosInfo read GetItemsByKey;

    property Items[AIndex:integer] : TFilePosInfo read GetItemsByIndex;
    property Count : integer read GetCount;
  end;

implementation

{ TFilePosList }

procedure TFilePosList.AddByIndex(AIndex:integer; APosition,ASize: int64);
var
  FilePosInfo : TFilePosInfo;
begin
  if AIndex >= FList.Count then FList.Count := AIndex+1;

  FilePosInfo := TFilePosInfo.Create;
  FilePosInfo.Size := ASize;
  FilePosInfo.Position := APosition;

  FList[AIndex] := FilePosInfo;
end;

procedure TFilePosList.AddByKey(AKey: string; APosition,ASize: int64);
begin
  // Todo : 
  raise Exception.Create('ÁØºñ ¾ÈµÊ');
end;

procedure TFilePosList.Clear;
var
  Loop : Integer;
begin
  for Loop := 0 to FList.Count-1 do TObject(FList[Loop]).Free;
  FList.Clear;
end;

constructor TFilePosList.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TFilePosList.Destroy;
begin
  Clear;

  FreeAndNil(FList);

  inherited;
end;

function TFilePosList.GetCount: integer;
begin
  Result := FList.Count; 
end;

function TFilePosList.GetItemsByIndex(AIndex: integer): TFilePosInfo;
begin
  if AIndex >= FList.Count then begin
    Result := nil;
    Exit;
  end;

  Result := Pointer(FList[AIndex]);
end;

end.

