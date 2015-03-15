unit CriticalValues;

interface

uses
  ValueList,
  Classes, SysUtils, SyncObjs;

type
  TCriticalValueBase = class(TObject)
  private
  protected
    FCS : TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
  end;

  TCriticalValues = class(TCriticalValueBase)
  private
    FLines : TValueList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetString(AName,AValue:string);
    procedure SetInteger(AName:string; AValue:integer);
    procedure SetBoolean(AName:string; AValue:boolean);

    property Lines : TValueList read FLines;
  end;

  TCriticalString = class(TCriticalValueBase)
  private
    FValue : string;
  public
    constructor Create; override;

    procedure SetValue(const AValue:string);

    property Value : string read FValue write FValue;
  end;

  TCriticalStringList = class(TCriticalValueBase)
  private
    FLines : TValueList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(AText:string);
    function Get:string;

    property Lines : TValueList read FLines;
  end;

implementation

{ TCriticalValueBase }

constructor TCriticalValueBase.Create;
begin
  inherited;

  FCS := TCriticalSection.Create;
end;

destructor TCriticalValueBase.Destroy;
begin
  FreeAndNil(FCS);

  inherited;
end;

procedure TCriticalValueBase.Lock;
begin
  FCS.Enter;
end;

procedure TCriticalValueBase.Unlock;
begin
  FCS.Leave;
end;

{ TCriticalValues }

constructor TCriticalValues.Create;
begin
  inherited;

  FLines := TValueList.Create;
end;

destructor TCriticalValues.Destroy;
begin
  FreeAndNil(FLines);

  inherited;
end;

procedure TCriticalValues.SetBoolean(AName: string; AValue: boolean);
begin
  Lock;
  try
    FLines.Booleans[AName] := AValue;
  finally
    Unlock;
  end;
end;

procedure TCriticalValues.SetInteger(AName: string; AValue: integer);
begin
  Lock;
  try
    FLines.Integers[AName] := AValue;
  finally
    Unlock;
  end;
end;

procedure TCriticalValues.SetString(AName, AValue: string);
begin
  Lock;
  try
    FLines.Values[AName] := AValue;
  finally
    Unlock;
  end;
end;

{ TCriticalString }

constructor TCriticalString.Create;
begin
  inherited;

  FValue := '';
end;

procedure TCriticalString.SetValue(const AValue: string);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

{ TCriticalStringList }

procedure TCriticalStringList.Add(AText: string);
begin
  Lock;
  try
    FLines.Add(AText);
  finally
    Unlock;
  end;
end;

constructor TCriticalStringList.Create;
begin
  inherited;

  FLines := TValueList.Create;
end;

destructor TCriticalStringList.Destroy;
begin
  FreeAndNil(FLines);

  inherited;
end;

function TCriticalStringList.Get: string;
begin
  Result := '';

  Lock;
  try
    if FLines.Count = 0 then Exit;

    Result := FLines[0];
    FLines.Delete(0);
  finally
    Unlock;
  end;
end;

end.
