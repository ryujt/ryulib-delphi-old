unit RWLock;

interface

uses
  SRWLock,
  SysUtils, Classes;

type
  IRWLock = interface
    ['{93E5B7E2-78E6-4BF1-865B-0E5B27693501}']

    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
  end;

  TRWLock = class
  private
    FLock : IRWLock;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
  end;

implementation

type
  TSRWLockBase = class (TSRWLock, IRWLock)
  private
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;

  TMultiLockBase = class (TMultiReadExclusiveWriteSynchronizer, IRWLock)
  private
    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
  public
  end;

{ TSRWLockBase }

function TSRWLockBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSRWLockBase._AddRef: Integer;
begin
  Result := -1;
end;

function TSRWLockBase._Release: Integer;
begin
  Result := -1;
end;

{ TMultiLockBase }

procedure TMultiLockBase.AcquireExclusive;
begin
  Self.BeginWrite;
end;

procedure TMultiLockBase.AcquireShared;
begin
  Self.BeginRead;
end;

procedure TMultiLockBase.ReleaseExclusive;
begin
  Self.EndWrite;
end;

procedure TMultiLockBase.ReleaseShared;
begin
  Self.EndRead;
end;

{ TRWLock }

procedure TRWLock.AcquireExclusive;
begin
  FLock.AcquireExclusive;
end;

procedure TRWLock.AcquireShared;
begin
  FLock.AcquireShared;
end;

constructor TRWLock.Create;
begin
  inherited;

  if TOSVersion.Major >= 6 then FLock := TSRWLockBase.Create
  else FLock := TMultiLockBase.Create;
end;

destructor TRWLock.Destroy;
begin
  TObject(FLock).Free;

  inherited;
end;

procedure TRWLock.ReleaseExclusive;
begin
  FLock.ReleaseExclusive;
end;

procedure TRWLock.ReleaseShared;
begin
  FLock.ReleaseShared;
end;

end.
