unit SRWLock;

interface

type
  TSRWLock = class
  private
    FLock: Pointer;
  public
    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
  end;

implementation

{$WARN SYMBOL_PLATFORM  OFF}

procedure AcquireSRWLockShared(var P: Pointer); stdcall; external 'kernel32.dll' name 'AcquireSRWLockShared' delayed;
procedure ReleaseSRWLockShared(var P: Pointer); stdcall; external 'kernel32.dll' name 'ReleaseSRWLockShared' delayed;

procedure AcquireSRWLockExclusive(var P: Pointer); stdcall; external 'kernel32.dll' name 'AcquireSRWLockExclusive' delayed;
procedure ReleaseSRWLockExclusive(var P: Pointer); stdcall; external 'kernel32.dll' name 'ReleaseSRWLockExclusive' delayed;

{$WARN SYMBOL_PLATFORM  ON}

{ TSRWLock }

procedure TSRWLock.AcquireShared;
begin
  AcquireSRWLockShared(FLock);
end;

procedure TSRWLock.ReleaseShared;
begin
  ReleaseSRWLockShared(FLock);
end;

procedure TSRWLock.AcquireExclusive;
begin
  AcquireSRWLockExclusive(FLock);
end;

procedure TSRWLock.ReleaseExclusive;
begin
  ReleaseSRWLockExclusive(FLock);
end;

end.
