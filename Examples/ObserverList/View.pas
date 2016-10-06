unit View;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils;

type
  TView = class (TObserverList)
  private
  public
    class function Obj:TView;

    procedure sp_CurrentVolume(AVolume:integer);
  end;

implementation

{ TView }

var
  MyObject : TView = nil;

class function TView.Obj: TView;
begin
  if MyObject = nil then MyObject := TView.Create(nil);
  Result := MyObject;
end;

procedure TView.sp_CurrentVolume(AVolume: integer);
var
  ValueList : TValueList;
begin
  ValueList := TValueList.Create;
  try
    ValueList.Values['Code'] := 'CurrentVolume';
    ValueList.Integers['Volume'] := AVolume;
    AsyncBroadcast(ValueList);
  finally
    ValueList.Free;
  end;
end;

end.
