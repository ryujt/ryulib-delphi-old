unit MixedPacket;


interface

uses
  Classes, SysUtils, Contnrs, Windows;

{$region 'MixedPacket 유닛 설명'}{

  * 개요 : 다양한 데이터를 하나로 묶어서 전송하거나 저장하기 위한 라이브리러 유닛

  * TMixedPacket : 다양한 데이터를 하나로 묶어서 전송하거나 저장하기 위한 클래스

  * TMixedPacket.Clear : 묶여진 데이터를 모두 지운다.

  * TMixedPacket.AddString : 문자열 데이터를 포함시킨다.

  * TMixedPacket.AddStream : Stream 데이터를 포함시킨다.

  * TMixedPacket.AddData : 바이너리(포인터) 데이터를 포함시킨다.

  * TMixedPacket.AddFile : 파일을 포함시킨다.

  * TMixedPacket.SetData : 이미 묶여져 있는 데이터를 입력하여, 이것을 패킷 단위로 조각낸다.

  * TMixedPacket.GetData : 묶여진 데이터를 한꺼번에 반환한다.

  * TMixedPacket.GetPacket : 묶여진 데이터에서 패킷을 따로 분리하여 꺼낸다.

  * TMixedPacket.Count : 묶여진 데이터의 갯수를 반환한다.

}{$endregion}

type
  TMixedPacket = class
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetSize: Int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddString(AText:string);
    procedure AddStream(AStream:TStream);
    procedure AddData(AData:pointer; ASize:int64);
    procedure AddFile(AFileName:string);

    procedure SetData(AData:pointer; ASize:int64);
    function GetData(var AData:pointer; var ASize:int64):boolean; overload;
    function GetData(var AData:pointer; var ASize:integer):boolean; overload;

    function GetPacket(Index:integer; var AText:string):boolean; overload;
    function GetPacket(Index:integer; AStream:TStream):boolean; overload;
    function GetPacket(Index:integer; var AData:pointer; var ASize:int64):boolean; overload;
    function GetPacket(Index:integer; var AData:pointer; var ASize:integer):boolean; overload;

    property Count : integer read GetCount;
  end;

implementation

type
  TPacket = class
  private
  public
    Data: Pointer;
    Size: Int64;
    constructor Create(AData: Pointer; ASize: Int64);
    destructor Destroy; override;
  end;

{ TMixedPacket }

procedure TMixedPacket.AddData(AData: pointer; ASize: int64);
begin
  FList.Add(TPacket.Create(AData, ASize));
end;

procedure TMixedPacket.AddFile(AFileName: string);
var
  Size : int64;
  Data: Pointer;
  fsData : TFileStream;
  msData : TMemoryStream;
begin
  fsData := TFileStream.Create(AFileName, fmOpenRead);
  try
    msData := TMemoryStream.Create;
    try
      Size := fsData.Size;

      if Size <= 0 then
      begin
        Data := nil;
        Size := 0;
      end else
      begin
        fsData.Position := 0;
        msData.CopyFrom(fsData, Size);
        Data := msData.Memory;
      end;

      FList.Add(TPacket.Create(Data, Size));
    finally
      msData.Free;
    end;
  finally
    fsData.Free;
  end;
end;

procedure TMixedPacket.AddStream(AStream: TStream);
var
  Size : int64;
  Data: Pointer;
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    Size := AStream.Size;

    if Size <= 0 then
    begin
      Data := nil;
      Size := 0;
    end else
    begin
      AStream.Position := 0;
      msData.CopyFrom(AStream, Size);
      Data := msData.Memory;
    end;

    FList.Add(TPacket.Create(Data, Size));
  finally
    msData.Free;
  end;
end;

procedure TMixedPacket.AddString(AText: string);
var
  Size : int64;
  Data: Pointer;
  ssData : TStringStream;
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    ssData := TStringStream.Create(AText);
    try
      Size := ssData.Size;

      if Size <= 0 then
      begin
        Data := nil;
        Size := 0;
      end else
      begin
        ssData.Position := 0;
        msData.CopyFrom(ssData, Size);
        Data := msData.Memory;
      end;

      FList.Add(TPacket.Create(Data, Size));
    finally
      ssData.Free;
    end;
  finally
    msData.Free;
  end;
end;

procedure TMixedPacket.Clear;
begin
  FList.Clear;
end;

constructor TMixedPacket.Create;
begin
  inherited;

  FList := TObjectList.Create;
end;

destructor TMixedPacket.Destroy;
begin
  if Assigned(FList) then
    FreeAndNil(FList);

  inherited;
end;

function TMixedPacket.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMixedPacket.GetData(var AData: pointer; var ASize: integer): boolean;
var
  Size64: Int64;
begin
  Result := GetData(AData, Size64);
  ASize := Size64;
end;

function TMixedPacket.GetData(var AData: pointer; var ASize: int64): boolean;
var
  i: Integer;
  Packet: TPacket;
  PData: ^byte;
begin
  AData := nil;
  ASize := GetSize;
  Result := False;

  if ASize <= 0 then Exit;

  GetMem(AData, ASize);

  PData := AData;
  for i := 0 to FList.Count - 1 do
  begin
    Packet := TPacket(FList[i]);

    Move(Packet.Size, PData^, SizeOf(Packet.Size));
    Inc(PData, SizeOf(Packet.Size));

    if Packet.Size > 0 then
    begin
      Move(Packet.Data^, PData^, Packet.Size);
      Inc(PData, Packet.Size);
    end;
  end;

  Result := True;
end;

function TMixedPacket.GetPacket(Index: integer; AStream: TStream): boolean;
var
  Packet: TPacket;
begin
  if (Index < 0) or (Index >= FList.Count) then
  begin
    Result := False;
    Exit;
  end;

  if AStream = nil then
  begin
    Result := False;
    Exit;
  end;

  Packet := TPacket(FList[Index]);

  AStream.Size := 0;
  AStream.Position := 0;
  if (Packet.Size > 0) and (Packet.Data <> nil) then
  begin
    AStream.Write(Packet.Data^, Packet.Size);
    AStream.Position := 0;
  end;

  Result := True;
end;

function TMixedPacket.GetPacket(Index: integer; var AData: pointer;
  var ASize: int64): boolean;
var
  Packet: TPacket;
begin
  AData := nil;
  ASize := 0;

  if (Index < 0) or (Index >= FList.Count) then
  begin
    Result := False;
    Exit;
  end;

  Packet := TPacket(FList[Index]);

  ASize := Packet.Size;

  if ASize > 0 then
  begin
    GetMem(AData, ASize);
    Move(Packet.Data^, AData^, ASize);
  end;

  Result := True;
end;

function TMixedPacket.GetSize: Int64;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to FList.Count - 1 do
    Result := Result + SizeOf(TPacket(FList[i]).Size) + TPacket(FList[i]).Size;
end;

function TMixedPacket.GetPacket(Index: integer; var AText: string): boolean;
var
  Size : int64;
  Packet: TPacket;
  ssData : TStringStream;
begin
  if (Index < 0) or (Index >= FList.Count) then
  begin
    Result := False;
    Exit;
  end;

  Packet := TPacket(FList[Index]);

  Size := Packet.Size;

  if Size > 0 then
  begin
    ssData := TStringStream.Create('');
    try
      ssData.Write(Packet.Data^, Size);

      ssData.Position := 0;
      AText := ssData.DataString;
    finally
      ssData.Free;
    end;
  end else
  begin
    AText := '';
  end;

  Result := True;
end;

procedure TMixedPacket.SetData(AData: pointer; ASize: int64);
var
  msData: TMemoryStream;
  Size: Int64;
  Data: Pointer;
begin
  Clear;

  msData := TMemoryStream.Create;
  try
    msData.Write(AData^, ASize);
    msData.Position := 0;

    while msData.Position < msData.Size do
    begin
      msData.Read(Size, SizeOf(Size));
      if Size > 0 then
      begin
        GetMem(Data, Size);
        try
          msData.ReadBuffer(Data^, Size);
          FList.Add(TPacket.Create(Data, Size));
        finally
          FreeMem(Data);
        end;
      end else
      begin
        FList.Add(TPacket.Create(nil, 0));
      end;
    end;
  finally
    msData.Free;
  end;
end;

function TMixedPacket.GetPacket(Index: integer; var AData: pointer;
  var ASize: integer): boolean;
var
  Size64: Int64;
begin
  Result := GetPacket(Index, AData, Size64);
  ASize := Size64;
end;

{ TPacket }

constructor TPacket.Create(AData: Pointer; ASize: Int64);
begin
  inherited Create;

  Data := nil;
  Size := 0;

  if (AData <> nil) and (ASize > 0) then
  begin
    Size := ASize;

    GetMem(Data, Size);
    Move(AData^, Data^, Size);
  end;
end;

destructor TPacket.Destroy;
begin
  if Data <> nil then
  begin
    FreeMem(Data);
    Data := nil;
  end;

  inherited;
end;

end.
