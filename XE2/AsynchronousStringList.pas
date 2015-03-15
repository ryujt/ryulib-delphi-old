unit AsynchronousStringList;

interface

uses
  DebugTools, WaitFreeList, LazyRelease,
  Classes, SysUtils;

const
  LAZYRELEASE_FENDER_SIZE = 1024;

type
  TAsynchronousStringList = class
  private
    FList : TWaitFreeObjectList;
    FLazyRelease : TLazyDestroy;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AStr:string);
    procedure Remove(AStr:string);
    function Check(AStr:string):boolean;
  end;

implementation

type
  TStringItem = class (TWaitFreeObject)
  private
    FData : string;
    FList : TAsynchronousStringList;
  protected
    procedure WaitFreeObjectRemoved; override;
  public
    constructor Create(AList:TAsynchronousStringList; AData:string); reintroduce;
  end;

{ TStringItem }

constructor TStringItem.Create(AList: TAsynchronousStringList; AData: string);
begin
  inherited Create;

  FData := AData;
  FList := AList;
end;

procedure TStringItem.WaitFreeObjectRemoved;
begin
  FList.FLazyRelease.Release(Self);
end;

{ TAsynchronousStringList }

procedure TAsynchronousStringList.Add(AStr: string);
begin
  {$IFDEF DEBUG}
  Trace(Format('TAsynchronousStringList.Add: %s', [AStr]));
  {$ENDIF}

  FList.Add(TStringItem.Create(Self, AStr));
end;

function TAsynchronousStringList.Check(AStr: string): boolean;
var
  isFound : boolean;
  StringItem : TStringItem;
begin
  isFound := false;

  FList.Iterate(
    procedure(WaitFreeObject:IWaitFreeObject; var ANeedStop:boolean) begin
      StringItem := WaitFreeObject as TStringItem;

      {$IFDEF DEBUG}
      Trace(Format('TAsynchronousStringList.Check: %s=%s', [AStr, StringItem.FData]));
      {$ENDIF}

      if AStr = StringItem.FData then begin
        isFound := true;
        ANeedStop := true;
      end;
    end
  );

  Result := isFound;
end;

procedure TAsynchronousStringList.Clear;
begin
  FList.Clear;
end;

constructor TAsynchronousStringList.Create;
begin
  inherited;

  FList := TWaitFreeObjectList.Create(false);
  FList.Name := 'TAsynchronousStringList';

  FLazyRelease := TLazyDestroy.Create(LAZYRELEASE_FENDER_SIZE);
end;

destructor TAsynchronousStringList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLazyRelease);

  inherited;
end;

procedure TAsynchronousStringList.Remove(AStr: string);
var
  StringItem : TStringItem;
begin
  {$IFDEF DEBUG}
  Trace(Format('TAsynchronousStringList.Remove: %s', [AStr]));
  {$ENDIF}

  StringItem := nil;

  FList.Iterate(
   procedure(WaitFreeObject:IWaitFreeObject; var ANeedStop:boolean) begin
     if AStr = (WaitFreeObject as TStringItem).FData then begin
       StringItem := WaitFreeObject as TStringItem;
       ANeedStop := true;
     end;
   end
  );

  if StringItem <> nil then FList.Remove(StringItem);
end;

end.
