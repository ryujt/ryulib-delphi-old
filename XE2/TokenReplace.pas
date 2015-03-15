unit TokenReplace;

interface

uses
  Classes, SysUtils;

type
  TTokenReplace = class (TComponent)
  private
    FTokenList : TStringList;
    FTargetList : TStringList;
    FState, FStateBase, FStateWhiteSpace, FStateToken : TObject;
    function get_Target(AToken:string):string;
  private
    FWhiteSpace: string;
    FOutput: string;
    FInput: string;
    FIgnoreCase: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Replace(AToken,ATarget:string);
    procedure Execute;
  published
    property IgnoreCase : boolean read FIgnoreCase write FIgnoreCase;
    property WhiteSpace : string read FWhiteSpace write FWhiteSpace;
    property Input : string read FInput write FInput;
    property Output : string read FOutput;
  end;

implementation

type
  TState = class (TComponent)
  private
  public
    function Parent:TTokenReplace;
    procedure Execute(AIndex:integer); virtual; abstract;
  end;

  TStateBase = class (TState)
  private
  public
    procedure Execute(AIndex:integer); override;
  end;

  TStateWhiteSpace = class (TState)
  private
  public
    procedure Execute(AIndex:integer); override;
  end;

  TStateToken = class (TState)
  private
    FToken : string;
  public
    procedure Execute(AIndex:integer); override;
  end;

{ TState }

function TState.Parent: TTokenReplace;
begin
  Result := Pointer(Owner);
end;

{ TStateBase }

procedure TStateBase.Execute(AIndex: integer);
begin
  if Pos(Parent.FInput[AIndex], Parent.FWhiteSpace) > 0 then
    Parent.FState := Parent.FStateWhiteSpace
  else begin
    TStateToken(Parent.FStateToken).FToken := '';
    Parent.FState := Parent.FStateToken;
  end;

  TState(Parent.FState).Execute(AIndex);
end;

{ TStateWhiteSpace }

procedure TStateWhiteSpace.Execute(AIndex: integer);
begin
  if Pos(Parent.FInput[AIndex], Parent.FWhiteSpace) > 0 then begin
    Parent.FOutput := Parent.FOutput + Parent.FInput[AIndex];
  end else begin
    TStateToken(Parent.FStateToken).FToken := '';
    Parent.FState := Parent.FStateToken;
    TState(Parent.FState).Execute(AIndex);
  end;
end;

{ TStateToken }

procedure TStateToken.Execute(AIndex: integer);
begin
  if Pos(Parent.FInput[AIndex], Parent.FWhiteSpace) > 0 then begin
    if FToken <> '' then Parent.FOutput := Parent.FOutput + Parent.get_Target(FToken);

    Parent.FState := Parent.FStateWhiteSpace;
    TState(Parent.FState).Execute(AIndex);
  end else begin
    FToken := FToken + Parent.FInput[AIndex];
  end;
end;

{ TTokenReplace }

procedure TTokenReplace.Clear;
begin
  FInput := '';
  FTokenList.Clear;
  FTargetList.Clear;
end;

constructor TTokenReplace.Create(AOwner: TComponent);
begin
  inherited;

  FIgnoreCase := true;
  FWhiteSpace := '~!@#$%^&*()+`-={}|[]\:";''<>?,./ '#13#10#0;

  FTokenList := TStringList.Create;
  FTargetList := TStringList.Create;

  FStateBase := TStateBase.Create(Self);
  FStateWhiteSpace := TStateWhiteSpace.Create(Self);
  FStateToken := TStateToken.Create(Self);
  FState := FStateBase;
end;

destructor TTokenReplace.Destroy;
begin
  FreeAndNil(FTokenList);
  FreeAndNil(FTargetList);
  FreeAndNil(FStateBase);
  FreeAndNil(FStateWhiteSpace);
  FreeAndNil(FStateToken);

  inherited;
end;

procedure TTokenReplace.Execute;
var
  Loop : Integer;
begin
  FOutput := '';
  TStateToken(FStateToken).FToken := '';

  FState := FStateBase;
  for Loop := 1 to Length(FInput) do TState(FState).Execute(Loop);

  if (FState = FStateToken)  and (TStateToken(FStateToken).FToken <> '') then begin
    FOutput := FOutput + get_Target(TStateToken(FStateToken).FToken);
  end;
end;

function TTokenReplace.get_Target(AToken: string): string;
var
  Loop: Integer;
  bCondition : boolean;
begin
  Result := AToken;
  for Loop := 0 to FTokenList.Count - 1 do  begin
    if IgnoreCase then bCondition := LowerCase(FTokenList[Loop]) = LowerCase(AToken)
    else bCondition := FTokenList[Loop] = AToken;

    if bCondition then begin
      Result := FTargetList[Loop];
      Break;
    end;
  end;
end;

procedure TTokenReplace.Replace(AToken, ATarget: string);
begin
  FTokenList.Add(AToken);
  FTargetList.Add(ATarget);
end;

end.
