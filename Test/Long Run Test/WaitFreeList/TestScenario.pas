unit TestScenario;

interface

uses
  DebugTools, WaitFreeList, SimpleThread,
  Windows, SysUtils, Classes;

const
  MAX_LIST_SIZE = $FF;

type
  TTestScenario = class
  private
    FList : TWaitFreeObjectList;
  private
    FThreadAdd : TSimpleThread;
    FThreadRemove : TSimpleThread;
    FThreadIterate : TSimpleThread;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
  end;

implementation

var
  SumOfID : integer;

{ TTestNode }

type
  TTestNode = class (TWaitFreeObject)
  private
  public
    IsDeleting : boolean;
    ID : integer;
  public
    constructor Create;

    function CompareWaitFreeObject(AObject:TObject):boolean; override;
    procedure WaitFreeObjectDuplicated; override;
  end;

function TTestNode.CompareWaitFreeObject(AObject: TObject): boolean;
begin
  Result := TTestNode(AObject).ID = ID;
end;

constructor TTestNode.Create;
begin
  inherited;

  IsDeleting := false;
  ID := Random(MAX_LIST_SIZE * 100);
end;

procedure TTestNode.WaitFreeObjectDuplicated;
begin
  Trace( Format('WaitFreeObjectDuplicated: %d', [ID]) );
end;

{ TTestScenario }

constructor TTestScenario.Create;
begin
  inherited;

  FList := TWaitFreeObjectList.Create(false);
end;

destructor TTestScenario.Destroy;
begin

  inherited;
end;

procedure TTestScenario.Run;
begin
  FThreadAdd := TSimpleThread.Create(
    'FThreadAdd',
    procedure (ASimpleThread:TSimpleThread)
    var
      Loop, iCount : integer;
    begin
      while True do begin
        iCount := MAX_LIST_SIZE - FList.Count;
        if iCount > 0 then begin
          for Loop := 1 to iCount do FList.Add( TTestNode.Create );
        end;
      end;
    end
  );

  FThreadRemove := TSimpleThread.Create(
    'FThreadRemove',
    procedure (ASimpleThread:TSimpleThread)
    var
      iIndex, Loop : integer;
      Target : IWaitFreeObject;
    begin
      while True do begin
        if FList.Count > (MAX_LIST_SIZE div 2) then begin
          iIndex := Random(MAX_LIST_SIZE div 2);

          Loop := 0;
          FList.Iterate(
            procedure (WaitFreeObject:IWaitFreeObject; var NeedStop:boolean) begin
              Target := WaitFreeObject;
              NeedStop := (Loop >= (MAX_LIST_SIZE div 3)) and (TTestNode(Target).IsDeleting = false);
            end
          );

          TTestNode(Target).IsDeleting := true;
          FList.Remove(Target);
        end;
      end;
    end
  );

  FThreadIterate := TSimpleThread.Create(
    'FThreadIterate',
    procedure (ASimpleThread:TSimpleThread)
    begin
      while True do begin
        SumOfID := 0;
        FList.Iterate(
          procedure (WaitFreeObject:IWaitFreeObject) begin
            SumOfID := SumOfID + TTestNode(WaitFreeObject).ID;
          end
        );
      end;
    end
  );
end;

end.




{$IFDEF DEBUG}
var
  Str : set of byte;
  List : TWaitFreeObjectList;
{$ENDIF}

initialization
{$IFDEF DEBUG}
  List := TWaitFreeObjectList.Create(false);
  try
    List.Add(TTestNode.Create(1));
    List.Add(TTestNode.Create(2));
    List.Add(TTestNode.Create(3));

    while List.IsBusy do Sleep(10);

    Str := [];
    List.Iterate(
      procedure (WaitFreeObject:IWaitFreeObject; var NeedStop:boolean) begin
        Str := Str + [TTestNode(WaitFreeObject).Data];
      end
    );
    Assert(Str = [1, 2, 3], 'Str = [1, 2, 3]');

  finally
    List.Free;
  end;
{$ENDIF}
