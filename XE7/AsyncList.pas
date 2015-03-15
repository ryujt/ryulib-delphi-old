unit AsyncList;

interface

uses
  RyuLibBase, DebugTools,
  SysUtils, Classes;

type
  IAsyncListObject = interface
    ['{C9CCDEF3-128D-4649-8D71-78F5ECDC19B0}']

    function IsDeleted:boolean;
    procedure SetDeleted(AValue:boolean);

    function CompareAsyncListObject(AObject:TObject):boolean;

    procedure AsyncListObjectAdded;
    procedure AsyncListObjectRemoved;
    procedure AsyncListObjectDuplicated;

    procedure SetLeftAsyncListObject(AObject:TObject; AInterface:IAsyncListObject);
    function GetLeftAsyncListObject:TObject;
    function GetLeftAsyncListInterface:IAsyncListObject;

    procedure SetRightAsyncListObject(AObject:TObject; AInterface:IAsyncListObject);
    function GetRightAsyncListObject:TObject;
    function GetRightAsyncListInterface:IAsyncListObject;
  end;

  TAsyncListObject = class (TInterfaceBase, IAsyncListObject)
  strict private
    FIsDeleted : boolean;
    function IsDeleted:boolean;
    procedure SetDeleted(AValue:boolean);
  strict private
    FObjectLeft : TObject;
    FObjectRight : TObject;

    FInterfaceLeft : IAsyncListObject;
    FInterfaceRight : IAsyncListObject;

    procedure SetLeftAsyncListObject(AObject:TObject; AInterface:IAsyncListObject);
    function GetLeftAsyncListObject:TObject;
    function GetLeftAsyncListInterface:IAsyncListObject;

    procedure SetRightAsyncListObject(AObject:TObject; AInterface:IAsyncListObject);
    function GetRightAsyncListObject:TObject;
    function GetRightAsyncListInterface:IAsyncListObject;
  protected
    function CompareAsyncListObject(AObject:TObject):boolean; virtual;

    procedure AsyncListObjectAdded; virtual;
    procedure AsyncListObjectRemoved; virtual;
    procedure AsyncListObjectDuplicated; virtual;
  public
    constructor Create; virtual;
  end;

  TSimpleIterateProcedure = reference to procedure(AObject:IAsyncListObject);
  TIterateProcedure = reference to procedure(AObject:IAsyncListObject; var ANeedStop:boolean);

  TAsyncList = class
  private
    FCount: integer;
    function GetIsEmpty: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AObject:IAsyncListObject);
    procedure Remove(AObject:IAsyncListObject);

    function GetFirst:IAsyncListObject;

    /// 락을 걸지 않고 탐색을 한다. 중간에 멈출 수 없다.
    procedure Iterate(AProcedure:TSimpleIterateProcedure); overload;

    /// 락을 걸지 않고 탐색을 한다.  중간에 멈출 수 있다.
    procedure Iterate(AProcedure:TIterateProcedure); overload;

    {*
      리스트의 변경은 지정된 스레드에서만 진행하여 락없이 처리하고 있다.
      따라서, 동기적인 동작은 해당 스레드에 의존해야 한다.
      Synchronize는 Iterate와 달리 락을 걸고 탐색을 한다.  중간에 멈출 수 없다.
    }
    procedure Synchronize(AProcedure:TSimpleIterateProcedure); overload;

    /// 락을 걸고 탐색을 한다.  중간에 멈출 수 없다.
    procedure Synchronize(AProcedure:TIterateProcedure); overload;

    property IsEmpty : boolean read GetIsEmpty;
    property Count : integer read FCount;
  end;

implementation


{ TAsyncListObject }

procedure TAsyncListObject.AsyncListObjectAdded;
begin

end;

procedure TAsyncListObject.AsyncListObjectDuplicated;
begin

end;

procedure TAsyncListObject.AsyncListObjectRemoved;
begin

end;

function TAsyncListObject.CompareAsyncListObject(AObject: TObject): boolean;
begin

end;

constructor TAsyncListObject.Create;
begin

end;

function TAsyncListObject.GetLeftAsyncListInterface: IAsyncListObject;
begin

end;

function TAsyncListObject.GetLeftAsyncListObject: TObject;
begin

end;

function TAsyncListObject.GetRightAsyncListInterface: IAsyncListObject;
begin

end;

function TAsyncListObject.GetRightAsyncListObject: TObject;
begin

end;

function TAsyncListObject.IsDeleted: boolean;
begin

end;

procedure TAsyncListObject.SetDeleted(AValue: boolean);
begin

end;

procedure TAsyncListObject.SetLeftAsyncListObject(AObject: TObject;
  AInterface: IAsyncListObject);
begin

end;

procedure TAsyncListObject.SetRightAsyncListObject(AObject: TObject;
  AInterface: IAsyncListObject);
begin

end;

{ TAsyncList }

procedure TAsyncList.Add(AObject: IAsyncListObject);
begin

end;

procedure TAsyncList.Clear;
begin

end;

constructor TAsyncList.Create;
begin

end;

destructor TAsyncList.Destroy;
begin

  inherited;
end;

function TAsyncList.GetFirst: IAsyncListObject;
begin

end;

function TAsyncList.GetIsEmpty: boolean;
begin

end;

procedure TAsyncList.Iterate(AProcedure: TIterateProcedure);
begin

end;

procedure TAsyncList.Iterate(AProcedure: TSimpleIterateProcedure);
begin

end;

procedure TAsyncList.Remove(AObject: IAsyncListObject);
begin

end;

procedure TAsyncList.Synchronize(AProcedure: TIterateProcedure);
begin

end;

procedure TAsyncList.Synchronize(AProcedure: TSimpleIterateProcedure);
begin

end;

end.
