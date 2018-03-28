unit WaitFreeValues;

interface

uses
  LazyRelease,
  Windows, Classes, SysUtils;

const
  DEAFULT_FENDER_SIZE = 1024;

type
  TWaitFreeString = class
  private
    FCapacity : integer;
    FLazyRelease : TLazyRelease;
  private
    FValue : PString;
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(ACapacity:integer=0); reintroduce;
    destructor Destroy; override;

    property Value : string read GetValue write SetValue;
  end;

implementation

{ TWaitFreeString }

constructor TWaitFreeString.Create(ACapacity:integer);
begin
  inherited Create;

  FCapacity := ACapacity;
  if FCapacity = 0 then FCapacity := DEAFULT_FENDER_SIZE;

  FValue := nil;
  FLazyRelease := TLazyRelease.Create(FCapacity);
end;

destructor TWaitFreeString.Destroy;
begin
  FreeAndNil(FLazyRelease);

  inherited;
end;

function TWaitFreeString.GetValue: string;
var
  Temp : PString;
begin
  Temp := FValue;
  if Temp = nil then Result := ''
  else Result := Temp^;
end;

procedure TWaitFreeString.SetValue(const Value: string);
var
  Temp : PString;
begin
  New(Temp);
  Temp^ := Value;

  FLazyRelease.Release(Temp);

  FValue := Temp;
end;

end.
