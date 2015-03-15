{*
  멀티 스레드에서 사용 되는 변수가 있을 경우 사용하기 위해서 임계영역과 세트로
  작성돤 데이터 래핑 클래스이다.  단 번에 참조하지 않고 참조되는 동안 프로세스가
  진행되는 경우, 멀티 스레드 상에서 경합을 피하기 위해서 사용한다.
}
unit SyncValues;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TSyncValue = class
  private
    FCS : TCriticalSection;
  public
    Tag : Integer;
    TagPointer : pointer;
    TagObject : TObject;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
  end;

  TSyncInteger = class (TSyncValue)
  private
    FValue: integer;
  public
    procedure SetValue(AValue:integer);
  public
    property Value : integer read FValue write FValue;
  end;

  TSyncInt64 = class (TSyncValue)
  private
    FValue: int64;
  public
    function GetValue:int64;
    procedure SetValue(AValue:int64);
  public
    property Value : int64 read FValue write FValue;
  end;

  TSyncString = class (TSyncValue)
  private
    FValue: string;
  public
    function GetValue:string;
    procedure SetValue(AValue:string);
  public
    property Value : string read FValue write FValue;
  end;

  TSyncShortString = class (TSyncValue)
  private
    FValue: ShortString;
  public
    function GetValue:ShortString;
    procedure SetValue(AValue:ShortString);
  public
    property Value : ShortString read FValue write FValue;
  end;

  TSyncBoolean = class (TSyncValue)
  private
    FValue: boolean;
  public
    procedure SetValue(AValue:boolean);
  public
    property Value : boolean read FValue write FValue;
  end;

  TSyncSize = class (TSyncValue)
  private
    FX: integer;
    FY: integer;
  public
    procedure GetValue(var AX,AY:integer);
    procedure SetValue(AX,AY:integer);
  public
    property X : integer read FX write FX;
    property Y : integer read FY write FY;
  end;

  TSyncStringList = class (TSyncValue)
  private
    FStringList : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property StringList : TStringList read FStringList;
  end;

  TSyncData = class (TSyncValue)
  private
    FData : pointer;
    FSize : integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetValue(var AData:pointer):integer; overload;
    procedure GetValue(var AData:pointer; var ASize:integer); overload;

    procedure SetValue(AData:pointer; ASize:integer);
  end;

implementation

{ TSyncValue }

constructor TSyncValue.Create;
begin
  inherited;

  Tag := 0;;
  TagPointer := nil;
  TagObject := nil;

  FCS := TCriticalSection.Create;
end;

destructor TSyncValue.Destroy;
begin
  FCS.Free;

  inherited;
end;

procedure TSyncValue.Lock;
begin
  FCS.Acquire;
end;

procedure TSyncValue.Unlock;
begin
  FCS.Release;
end;

{ TSyncSize }

procedure TSyncSize.GetValue(var AX, AY: integer);
begin
  Lock;
  try
    AX := FX;
    AY := FY;
  finally
    Unlock;
  end;
end;

procedure TSyncSize.SetValue(AX, AY: integer);
begin
  Lock;
  try
    FX := AX;
    FY := AY;
  finally
    Unlock;
  end;
end;

{ TSyncInteger }

procedure TSyncInteger.SetValue(AValue: integer);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TSyncInt64 }

function TSyncInt64.GetValue: int64;
begin
  Lock;
  try
    Result := FValue;
  finally
    Unlock;
  end;
end;

procedure TSyncInt64.SetValue(AValue: int64);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TSyncString }

function TSyncString.GetValue: string;
begin
  Lock;
  try
    Result := FValue;
  finally
    Unlock;
  end;
end;

procedure TSyncString.SetValue(AValue: string);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TSyncShortString }

function TSyncShortString.GetValue: ShortString;
begin
  Lock;
  try
    Result := FValue;
  finally
    Unlock;
  end;
end;

procedure TSyncShortString.SetValue(AValue: ShortString);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TSyncBoolean }

procedure TSyncBoolean.SetValue(AValue: boolean);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TSyncStringList }

constructor TSyncStringList.Create;
begin
  inherited;

  FStringList := TStringList.Create;
end;

destructor TSyncStringList.Destroy;
begin
  FreeAndNil(FStringList);

  inherited;
end;

{ TSyncData }

constructor TSyncData.Create;
begin
  inherited;

  FData := nil;
  FSize := 0;
end;

destructor TSyncData.Destroy;
begin
  Lock;
  try
    if FData <> nil then FreeMem(FData);
  finally
    Unlock;
  end;

  inherited;
end;

function TSyncData.GetValue(var AData: pointer): integer;
begin
  Lock;
  try
    AData := nil;
    Result := FSize;

    if FSize <= 0 then Exit;

    GetMem( AData, FSize );
    Move( FData^, AData^, FSize );
  finally
    Unlock;
  end;
end;

procedure TSyncData.GetValue(var AData: pointer; var ASize: integer);
begin
  Lock;
  try
    AData := nil;
    ASize := FSize;

    if FSize <= 0 then Exit;

    GetMem( AData, FSize );
    Move( FData^, AData^, FSize );
  finally
    Unlock;
  end;
end;

procedure TSyncData.SetValue(AData: pointer; ASize: integer);
begin
  Lock;
  try
    if FData <> nil then begin
      FreeMem(FData);
      FData := nil;
    end;

    FSize := ASize;

    GetMem( FData, FSize );
    Move( AData^, FData^, FSize );
  finally
    Unlock;
  end;
end;

end.
