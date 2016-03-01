unit TestScenario;

interface

uses
  WaitFreeList,
  SysUtils, Classes;

type
  TTestScenario = class
  private
    FList : TWaitFreeObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
  end;

implementation

{ TTestNode }

type
  TTestNode = class (TWaitFreeObject)
  private
  public
    Data : integer;
    constructor Create(AData:integer); reintroduce;
  end;

constructor TTestNode.Create(AData: integer);
begin
  inherited Create;

  Data := AData;
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
