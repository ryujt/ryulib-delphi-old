unit ObjectPool;

interface

uses
  SysUtils, Classes, SyncObjs;

type
  TObjectClass = class of TObject;

  TObjectEvent = procedure (Sender:TObject; AObject:TObject) of object;

  {*
    불특정 Client가 발생하여 객체를 요구할 때, 객체를 생성하기 위한 코드를 캡슐화 한다.
    리스트(배열) 형태로 크기의 한계는 정해져 있지만, 사용하기 전에는 생성되지 않는다.

    ID는 Client를 구별하기 위해서 사용된다.

    음성채팅에서 활용되었다.
    불특정 사용자가 음성을 보내며 각 사용자 음성마다 구별 할 수 있는 ID가 있다.
    동시에 말하는 사용자마다 음성 출력 객체가 필요한데,
    현재 생성되었는 지를 알 필요도 없이 아래와 같이 사용한다.
      TSpeaker(TObjectPool.Objecs[ID]).Play( 수신 데이터 );
    이미 생성 되었을 경우에는 해당 객체를 계속 사용하고,
    처음으로 들어 온 ID의 경우에는 새로운 객체를 생성한다.
  }
  TObjectPool = class
  private
    FObjectClass: TObjectClass;
    FCS : TCriticalSection;
    FList : TList;
    FObjects : array [0..$FFFF] of TObject;
  private
    FOnObjectCreated: TObjectEvent;
    FOnObjectReleased: TObjectEvent;
    function GetCount: integer;
    function GetObjects(ID: word): TObject;
    function GetObjectByIndex(Index: integer): TObject;
  public
    constructor Create(AObjectClass:TObjectClass); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure Remove(AObject:TObject; AID:word);
  public
    property Objects[ID:word] : TObject read GetObjects;
    property ObjectByIndex[Index:integer] : TObject read GetObjectByIndex;
    property Count : integer read GetCount;
    property OnObjectCreated : TObjectEvent read FOnObjectCreated write FOnObjectCreated;
    property OnObjectReleased : TObjectEvent read FOnObjectReleased write FOnObjectReleased;
  end;

implementation

{ TObjectPool }

procedure TObjectPool.Clear;
var
  Loop: Integer;
begin
  FCS.Acquire;
  try
    for Loop := 0 to FList.Count-1 do begin
      if Assigned(FOnObjectReleased) then FOnObjectReleased(Self, TObject(FList[Loop]));
      TObject(FList[Loop]).Free;
    end;

    FList.Clear;

    FillChar( FObjects, SizeOf(FObjects), 0 );
  finally
    FCS.Release;
  end;
end;

constructor TObjectPool.Create(AObjectClass:TObjectClass);
begin
  inherited Create;

  FObjectClass := AObjectClass;

  FCS := TCriticalSection.Create;
  FList := TList.Create;

  Clear;
end;

destructor TObjectPool.Destroy;
begin
  Clear;

  FreeAndNil(FCS);
  FreeAndNil(FList);

  inherited;
end;

function TObjectPool.GetCount: integer;
begin
  FCS.Acquire;
  try
    Result := FList.Count;
  finally
    FCS.Release;
  end;
end;

function TObjectPool.GetObjectByIndex(Index: integer): TObject;
begin
  FCS.Acquire;
  try
    Result := TObject(FList[Index]);
  finally
    FCS.Release;
  end;
end;

function TObjectPool.GetObjects(ID: word): TObject;
var
  Instance: TComponent;
begin
  FCS.Acquire;
  try
    if FObjects[ID] = nil then begin
      {$IF DEFINED(CLR)}
      FObjects[ID] := FObjectClass.NewInstance;
      FObjects[ID].Create;
      {$ELSE}
      Instance := TComponent(FObjectClass.NewInstance);
      TComponent(FObjects[ID]) := Instance;
      try
        Instance.Create(nil);
      except
        TComponent(FObjects[ID]) := nil;
        Instance := nil;
        raise;
      end;
      {$IFEND}

      FList.Add(FObjects[ID]);

      if Assigned(FOnObjectCreated) then FOnObjectCreated(Self, FObjects[ID]);
    end;

    Result := FObjects[ID];
  finally
    FCS.Release;
  end;
end;

procedure TObjectPool.Remove(AObject: TObject; AID: word);
begin
  if AObject = nil then Exit;

  FCS.Acquire;
  try
    FList.Remove(AObject);
    FObjects[AID] := nil;

    if Assigned(FOnObjectReleased) then FOnObjectReleased(Self, AObject);

    AObject.Free;
  finally
    FCS.Release;
  end;
end;

end.
