unit SyncHole;

interface

uses
  RyuLibBase, SimpleThread, DynamicQueue,
  SysUtils, Classes, SyncObjs;

type
  TSyncDataEvent = procedure (Sender:TObject; ALayout:integer; AData:pointer; ASize:integer; ATag:pointer) of object;

  {*
    주 용도는 네트워크로 수신 한 음성과 비디오 데이터를 싱크하는 것이다.
    특정 데이터 처리 시간을 기준으로 다른 데이터를 특정 데이터에 맞춰서 이벤트로 발생시킨다.
  }
  TSyncHole = class
  private
    FCS : TCriticalSection;
    FWorks : TDynamicQueue;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
    procedure do_Work;
    function get_Work(var AWork:pointer):boolean;
  private
    FOnAskBaseIsBusy: TBooleanResultEvent;
    FOnData: TSyncDataEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    {*
      싱크되어야 할 데이터 입력
      @param ALayer 싱크의 기준이 되는 데이터를 구별한다.  이외의 데이터를 구별 할 때도 사용된다.
      @param AData 입력되는 데이터의 포인터 주소
      @param ASize 입력되는 데이터의 바이트 크기
      @param ATag 범용적으로 사용 한다.
    }
    procedure Add(ALayer:integer; AData:pointer; ASize:integer; ATag:pointer = nil);
  public
    /// 기준이 되는 데이터 처리가 완료되었는 지 확인한다.
    property OnAskBaseIsBusy : TBooleanResultEvent read FOnAskBaseIsBusy write FOnAskBaseIsBusy;

    property OnData : TSyncDataEvent read FOnData write FOnData;
  end;

implementation

type
  TWork = class
  private
    FIsBase : boolean;
    FLayer : integer;
    FData : pointer;
    FSize : integer;
    FTag : pointer;
  public
    constructor Create(AIsBase:boolean; ALayout:integer; AData:pointer; ASize:integer; ATag:pointer); reintroduce;
    destructor Destroy; override;
  end;

{ TWork }

constructor TWork.Create(AIsBase: boolean; ALayout: integer; AData: pointer;
  ASize: integer; ATag: pointer);
begin
  inherited Create;

  FIsBase := AIsBase;
  FLayer := ALayout;
  FSize := ASize;

  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  FTag := ATag;
end;

destructor TWork.Destroy;
begin
  if FData <> nil then FreeMem(FData);

  inherited;
end;

{ TSyncHole }

procedure TSyncHole.Add(ALayer: integer; AData: pointer; ASize: integer;
  ATag: pointer);
begin
  FCS.Acquire;
  try
    FWorks.Push( TWork.Create( false, ALayer, AData, ASize, ATag) );
    FSimpleThread.WakeUp;
  finally
    FCS.Release;
  end;
end;

procedure TSyncHole.Clear;
var
  Work : TWork;
begin
  FCS.Acquire;
  try
    while FWorks.Pop( Pointer(Work)) do Work.Free;
  finally
    FCS.Release;
  end;
end;

constructor TSyncHole.Create;
begin
  inherited;

  FCS := TCriticalSection.Create;
  FWorks := TDynamicQueue.Create(false);

  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

destructor TSyncHole.Destroy;
begin
  Clear;

  FSimpleThread.Terminate(1000);

  inherited;
end;

procedure TSyncHole.do_Work;
var
  Work : TWork;
begin
  while get_Work(Pointer(Work)) do begin
    try
      if Assigned(FOnData) then FOnData(Self, Work.FLayer, Work.FData, Work.FSize, Work.FTag);
    finally
      if Work <> nil then Work.Free;
    end;

    if FOnAskBaseIsBusy(Self) then Break;
  end;
end;

function TSyncHole.get_Work(var AWork: pointer): boolean;
begin
  FCS.Acquire;
  try
    Result := FWorks.Pop( AWork );
  finally
    FCS.Release;
  end;
end;

procedure TSyncHole.on_Repeat(Sender: TObject);
var
  SimpleThread : TSimpleThread absolute Sender;
begin
  while not SimpleThread.Terminated do begin
    // 이벤트가 설정되어 있지 않거나, Base 가 바쁘지 않다면, 저장 된 작업을 실행한다.
    if (not Assigned(FOnAskBaseIsBusy)) or (not FOnAskBaseIsBusy(Self)) then do_Work;

    SimpleThread.Sleep(1);
  end;

  Clear;

//  FreeAndNil(FCS);
//  FreeAndNil(FWorks);
end;

end.
