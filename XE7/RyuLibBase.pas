unit RyuLibBase;

// Note :
// * 여기 저기서 사용하는 공용 루틴이나 타입을 정의 한다.

interface

uses
  Windows, Classes, SysUtils, Types;

type
  TPacket = class
  private
  public
    Data : pointer;
    Size : integer;
    Tag : pointer;
    constructor Create(AData:pointer; ASize:integer); reintroduce; overload;
    constructor Create(AData:pointer; ASize:integer; ATag:pointer); reintroduce; overload;
  end;

  TScreenSize = record
    Width, Height : integer;
    procedure SetValue(AWidth,AHeight:integer);
    function ToPoint:TPoint;
  end;

  ExceptionWithErrorCode = class (Exception)
  private
  protected
    FErrorCode : integer;
  public
    constructor Create(const AMsg:string; AErrorCode:integer);

    property ErrorCode : integer read FErrorCode;
  end;

  TProcedureReference<T> = reference to procedure(Context:T);

  TBooleanResultEvent = function (Sender:TObject):boolean of object;
  TPacketEvent = procedure (Sender:TObject; APacket:TPacket) of object;
  TDataEvent = procedure (Sender:TObject; AData:pointer; ASize:integer) of object;
  TDataAndTagEvent = procedure (Sender:TObject; AData:pointer; ASize:integer; ATag:pointer) of object;
  TIntegerEvent = procedure (Sender:TObject; AValue:Integer) of object;
  TStringEvent = procedure (Sender:TObject; const AValue:string) of object;
  TMsgAndCodeEvent = procedure (Sender:TObject; const AMsg:string; ACode:integer) of object;

  TMemory = class
  private
    FSize: integer;
    FData: pointer;
  public
    Tag: DWord;

    constructor Create(ASize:integer); reintroduce; overload;
    constructor Create(AData:pointer; ASize:integer); reintroduce; overload;
    constructor Create(AData:pointer; ASize:integer; ATag:DWord); reintroduce; overload;

    constructor Create(AText:string); reintroduce; overload;

    destructor Destroy; override;

    procedure Assign(AMemory:TMemory);

    function ToString:string; override;
  public
    property Data : pointer read FData;
    property Size : integer read FSize;
  end;

  TObjectClass = class of TObject;

  // 참조 카운트에 의해서 메모리 해제를 허용하지 않음
  TInterfaceBase = class (TObject, IInterface)
  private
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

procedure CreateObject(AInstanceClass: TObjectClass; var AReference);
function GetPacket(AData:pointer; ASize:integer): pointer;

implementation

uses
  Strg;

procedure CreateObject(AInstanceClass: TObjectClass; var AReference);
begin
  {$IF DEFINED(CLR)}
    TObject(AReference) := AInstanceClass.Create;
  {$ELSE}
    TObject(AReference) := AInstanceClass.NewInstance;
    TObject(AReference).Create;
  {$IFEND}
end;

function GetPacket(AData:pointer; ASize:integer): pointer;
var
  pBuffer : ^integer;
begin
  if ASize >= 0 then GetMem(Result, ASize + SizeOf(Integer))
  else GetMem(Result, SizeOf(Integer));

  pBuffer := Result;

  Move(ASize, pBuffer^, SizeOf(ASize));

  if ASize > 0 then begin
    Inc(pBuffer);
    Move(AData^, pBuffer^, ASize);
  end;
end;

{ TPacket }

constructor TPacket.Create(AData: pointer; ASize: integer);
begin
  inherited Create;

  Size := ASize;
  if Size <= 0 then begin
    Data := nil;
  end else begin
    GetMem(Data, Size);
    Move(AData^, Data^, Size);
  end;

  Tag := nil;
end;

constructor TPacket.Create(AData: pointer; ASize: integer; ATag: pointer);
begin
  inherited Create;

  Size := ASize;
  if Size <= 0 then begin
    Data := nil;
  end else begin
    GetMem(Data, Size);
    Move(AData^, Data^, Size);
  end;

  Tag := ATag;
end;

{ TScreenSize }

procedure TScreenSize.SetValue(AWidth, AHeight: integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

function TScreenSize.ToPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

{ ExceptionWithErrorCode }

constructor ExceptionWithErrorCode.Create(const AMsg: string;
  AErrorCode: integer);
begin
  Message := AMsg;
  FErrorCode := AErrorCode;
end;

{ TMemory }

procedure TMemory.Assign(AMemory: TMemory);
begin
  if FData <> nil then begin
    FreeMem(FData);
    FData := nil;
  end;

  FSize := AMemory.Size;
  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AMemory.Data^, FData^, FSize);
  end;

  Tag := AMemory.Tag;
end;

constructor TMemory.Create(AData: pointer; ASize: integer; ATag: DWord);
begin
  inherited Create;

  FSize := ASize;
  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  Tag := ATag;
end;

constructor TMemory.Create(AData: pointer; ASize: integer);
begin
  inherited Create;

  FSize := ASize;
  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
    Move(AData^, FData^, FSize);
  end;

  Tag := 0;
end;

constructor TMemory.Create(ASize: integer);
begin
  inherited Create;

  FSize := ASize;
  if FSize <= 0 then begin
    FData := nil;
  end else begin
    GetMem(FData, FSize);
  end;

  Tag := 0;
end;

destructor TMemory.Destroy;
begin
  if FData <> nil then begin
    FreeMem(FData);
    FData := nil;
  end;

  inherited;
end;

function TMemory.ToString: string;
begin
  Result := DataToText( FData, FSize );
end;

constructor TMemory.Create(AText: string);
begin
  inherited Create;

  TextToData( AText, FData, FSize );

  Tag := 0;
end;

{ TInterfaceBase }

function TInterfaceBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceBase._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceBase._Release: Integer;
begin
  Result := -1;
end;

end.
