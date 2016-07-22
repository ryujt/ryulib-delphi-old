unit @ClassName;

interface

uses
  SysUtils, Classes;

type
  T@ClassName = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    class function Obj:T@ClassName;
  end;

implementation

{ T@ClassName }

var
  MyObject : T@ClassName = nil;

class function T@ClassName.Obj: T@ClassName;
begin
  if MyObject = nil then MyObject := T@ClassName.Create;
  Result := MyObject;
end;

constructor T@ClassName.Create;
begin
  inherited;

end;

destructor T@ClassName.Destroy;
begin

  inherited;
end;

initialization
  MyObject := T@ClassName.Create;
end.