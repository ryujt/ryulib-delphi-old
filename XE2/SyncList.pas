unit SyncList;

interface

uses
  Classes, SysUtils, SyncObjs, ValueList;

type
  TSyncList = class
  private
    FList : TList;
    FCS : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Item:TObject);
    function Get:TObject;
  end;  

implementation

{ TSyncList }

procedure TSyncList.Add(Item: TObject);
begin
  FCS.Enter;
  try
    FList.Add(Item);
  finally
    FCS.Leave;
  end;
end;

procedure TSyncList.Clear;
var
  Item : TObject;
  Loop: Integer;
begin
  FCS.Enter;
  try
    for Loop := 0 to FList.Count - 1 do begin
      Item := FList[Loop];
      Item.Free;
    end;

    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TSyncList.Create;
begin
  inherited;

  FCS := TCriticalSection.Create;
  FList := TList.Create;
end;

destructor TSyncList.Destroy;
begin
  Clear;
  
  FList.Free;
  FCS.Free;

  inherited;
end;

function TSyncList.Get: TObject;
begin
  FCS.Enter;
  try
    if FList.Count = 0 then begin
      Result := nil;
      Exit;
    end;

    Result := FList[0];
    FList.Delete(0);
  finally
    FCS.Leave;
  end;
end;

end.
