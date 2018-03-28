unit Synchronizers;

interface

uses
  Classes, SysUtils, TypInfo;

type
  TBooleanSynchronizer = class(TObject)
  private
    FList : TStringList;
  private
    FValue: boolean;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(Obj:TObject; PropertyName:string; Reverse:boolean=false);
    procedure RemoveObject(Obj:TObject);

    property Value : boolean read FValue write SetValue;
    property OnChange : TNotifyEvent read FOnChange write FOnChange; 
  end;

implementation

{ TBooleanSynchronizer }

procedure TBooleanSynchronizer.AddObject(Obj: TObject; PropertyName: string;
  Reverse: boolean);
begin
  FList.AddObject(Format('%s=%s', [PropertyName, UpperCase(BoolToStr(Reverse, true))]), Obj);
end;

constructor TBooleanSynchronizer.Create;
begin
  inherited;

  FList := TStringList.Create;
end;

destructor TBooleanSynchronizer.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

procedure TBooleanSynchronizer.RemoveObject(Obj: TObject);
var
  Loop: Integer;
begin
  for Loop := FList.Count-1 downto 0 do
    if FList.Objects[Loop] = Obj then begin
      FList.Delete(Loop);
      Break;
    end;
end;

procedure TBooleanSynchronizer.SetValue(const Value: boolean);
var
  Loop : Integer;
  Obj : TObject;
  PropertyName : string;
  Reverse : boolean;
begin
  if FValue = Value then Exit;  
  FValue := Value;

  for Loop := 0 to FList.Count-1 do begin
    Obj := FList.Objects[Loop];
    PropertyName := FList.Names[Loop];
    Reverse := FList.ValueFromIndex[Loop] = 'TRUE';

    SetPropValue(Obj, PropertyName, Value xor Reverse);
  end;

  if Assigned(FOnChange) then FOnChange(Self);  
end;

end.
