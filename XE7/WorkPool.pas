unit WorkPool;

interface

uses
  SimpleThread, PacketList,
  SysUtils, Classes;

type
  TWorkEvent = procedure (Sender:TObject; AContext,AWorkData:pointer) of object;

  TWorkPool = class (TComponent)
  private
    FWorks : TPacketList;
    FSimpleThread : TSimpleThread;
    FOnWorkRequest: TWorkEvent;
    procedure on_Execute(Sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddWork(AContext,AWorkData:pointer);
  published
    property OnWorkRequest : TWorkEvent read FOnWorkRequest write FOnWorkRequest;
  end;

implementation

type
  TWorkInfo = class
    Context : pointer;
    WorkData : pointer;
    constructor Create(AContext,AWorkData:pointer); reintroduce;
  end;

{ TWorkInfo }

constructor TWorkInfo.Create(AContext, AWorkData: pointer);
begin
  inherited Create;

  Context := AContext;
  WorkData := AWorkData;
end;

{ TWorkPool }

procedure TWorkPool.AddWork(AContext,AWorkData:pointer);
begin
  FWorks.Add(TWorkInfo.Create(AContext, AWorkData));
  FSimpleThread.WakeUp;
end;

constructor TWorkPool.Create(AOwner: TComponent);
begin
  inherited;

  FWorks := TPacketList.Create;
  FSimpleThread := TSimpleThread.Create(on_Execute);
end;

destructor TWorkPool.Destroy;
begin
  FSimpleThread.Terminate;

  FreeAndNil(FWorks);

  inherited;
end;

procedure TWorkPool.on_Execute(Sender: TObject);
var
  WorkInfo : TWorkInfo;
begin
  while not FSimpleThread.Terminated do begin
    while FWorks.Get(Pointer(WorkInfo)) do begin
      try
        if Assigned(FOnWorkRequest) then FOnWorkRequest(Self, WorkInfo.Context, WorkInfo.WorkData);
      finally
        WorkInfo.Free;
      end;
    end;

    FSimpleThread.SleepTight;
  end;
end;

end.
