unit HtmlElement;

interface

uses
  Strg,
  Generics.Collections,
  SysUtils, Classes;

type
  THtmlElement = class;

  THtmlElementBase = class abstract
  private
    FParent : THtmlElement;
    FTagName: string;
    function GetValue:string; virtual;
  public
    class function CreateElement(AParent:THtmlElement; AElement:TStringList):THtmlElementBase;

    procedure SetProperty(const AName,AValue:string); virtual;
  public
    property Parent : THtmlElement read FParent;
    property TagName : string read FTagName;
    property Value : string read GetValue;
  end;

  THtmlTerminal = class (THtmlElementBase)
  private
  public
  end;

  THtmlElement = class (THtmlElementBase)
  private
    FElements : TList<THtmlElementBase>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AElement:THtmlElementBase);

    function GetTestString:string;
  end;

  THtmlText = class (THtmlTerminal)
  private
    FText : string;
    function GetValue:string; override;
  public
    constructor Create(AParent:THtmlElement; const AText:string); reintroduce;
  end;

  THtmlBr = class (THtmlTerminal)
  private
  public
  end;

  THtmlB = class (THtmlElement)
  private
  public
  end;

  THtmlFont = class (THtmlElement)
  private
  public
    procedure SetProperty(const AName,AValue:string); override;
  end;

implementation

{ THtmlTagBase }

function GetFirstLine(AStringList:TStringList):string;
begin
  Result := '';
  if AStringList.Count = 0 then Exit;

  Result := AStringList[0];
  AStringList.Delete(0);
end;

class function THtmlElementBase.CreateElement(AParent:THtmlElement;
  AElement: TStringList): THtmlElementBase;
var
  TagName, PropertyName, Equal, PropertyValue : string;
begin
  TagName := LowerCase(GetFirstLine(AElement));
  if TagName = '' then
    raise Exception.Create('Tag name is required but is missing.');

       if TagName = 'font' then Result := THtmlFont.Create
  else if TagName = 'b'    then Result := THtmlB.Create
  else if TagName = 'br'   then Result := THtmlBr.Create
  else
    Result := nil;

  if Result = nil then
    raise Exception.Create(Format('Tag <%s> is not supported.', [TagName]));

  Result.FParent := AParent;
  Result.FTagName := TagName;

  while AElement.Count > 0 do begin
    PropertyName := GetFirstLine(AElement);
    if PropertyName = '' then
      raise Exception.Create(Format('Property name is required but is missing. (Tag name: %s)', [TagName]));

    Equal := GetFirstLine(AElement);
    if Equal <> '=' then
      raise Exception.Create(Format('"=" is required but is missing. (Tag name: %s, Property name: %s)', [TagName, PropertyName]));

    PropertyValue := GetFirstLine(AElement);

    Result.SetProperty(PropertyName, PropertyValue);
  end;
end;

function THtmlElementBase.GetValue: string;
begin
  Result := FTagName;
end;

procedure THtmlElementBase.SetProperty(const AName, AValue: string);
begin
//
end;

{ THtmlElement }

procedure THtmlElement.Add(AElement: THtmlElementBase);
begin
  FElements.Add(AElement);
end;

procedure THtmlElement.Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FElements.Count-1 do FElements[Loop].Free;
  FElements.Clear;
end;

constructor THtmlElement.Create;
begin
  inherited;

  FParent := nil;
  FElements := TList<THtmlElementBase>.Create;
end;

destructor THtmlElement.Destroy;
begin
  Clear;

  FreeAndNil(FElements);

  inherited;
end;

function _GetTestString(AElement:THtmlElement; ADepth:integer):string;
var
  Loop: Integer;
begin
  if AElement.Parent <> nil then Result := CharString(' ', ADepth * 8) + AElement.Value + #13#10
  else Result := '';

  for Loop := 0 to AElement.FElements.Count-1 do begin
    if AElement.FElements.Items[Loop] is THtmlElement then
      Result := Result + _GetTestString(AElement.FElements.Items[Loop] as THtmlElement, ADepth + 1)
    else
      Result := Result + CharString(' ', ADepth * 8 + 4) + AElement.FElements.Items[Loop].Value + #13#10;
  end;
end;

function THtmlElement.GetTestString: string;
begin
  Result := _GetTestString(Self, 0);
end;

{ THtmlText }

constructor THtmlText.Create(AParent:THtmlElement; const AText: string);
begin
  inherited Create;

  FParent := AParent;
  FText := AText;
end;

function THtmlText.GetValue: string;
begin
  Result := FText;
end;

{ THtmlFont }

procedure THtmlFont.SetProperty(const AName, AValue: string);
begin
  // TODO:
end;

end.
