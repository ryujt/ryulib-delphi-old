unit ThreadPool;

interface

uses
  Windows, Classes, SysUtils;

const
  WT_EXECUTEDEFAULT       = ULONG($00000000);
  WT_EXECUTEINIOTHREAD    = ULONG($00000001);
  WT_EXECUTEINUITHREAD    = ULONG($00000002);
  WT_EXECUTEINWAITTHREAD  = ULONG($00000004);
  WT_EXECUTEONLYONCE      = ULONG($00000008);
  WT_EXECUTEINTIMERTHREAD = ULONG($00000020);
  WT_EXECUTELONGFUNCTION  = ULONG($00000010);
  WT_EXECUTEINPERSISTENTIOTHREAD  = ULONG($00000040);
  WT_EXECUTEINPERSISTENTTHREAD = ULONG($00000080);
  WT_TRANSFER_IMPERSONATION = ULONG($00000100);

function QueueWorkItem(Func:TThreadStartRoutine; Context:Pointer; ALimit:integer=0):boolean;
function QueueIOWorkItem(Func:TThreadStartRoutine; Context:Pointer):boolean;
function QueueUIWorkItem(Func:TThreadStartRoutine; Context:Pointer):boolean;

implementation

function WT_SET_MAX_THREADPOOL_THREADS(AFlags,ALimit:DWORD):DWORD;
begin
  Result := AFlags or (ALimit shl 16);
end;

function QueueUserWorkItem (func: TThreadStartRoutine; Context: Pointer; Flags: ULONG): BOOL; stdcall; external kernel32 name 'QueueUserWorkItem';

function InternalThreadFunctionSample(lpThreadParameter: Pointer): Integer; stdcall;
begin
  Result := 0;
  try
    try
      // lpThreadParameter »ç¿ë
    finally
    end;
  except
  end;
end;

function AddQueueWorkItem(Func:TThreadStartRoutine; Context:Pointer; Flags:ULONG):boolean;
begin
  IsMultiThread := True;
  Result := QueueUserWorkItem(Func, Context, Flags);
  if not Result then RaiseLastOSError;
end;

function QueueWorkItem(Func:TThreadStartRoutine; Context:Pointer; ALimit:integer):boolean;
var
  Flags : DWORD;
begin
  Flags := WT_EXECUTEDEFAULT;
  if ALimit > 0 then
    Flags := WT_SET_MAX_THREADPOOL_THREADS( WT_EXECUTEDEFAULT, ALimit );

  Result := AddQueueWorkItem(Func, Context, Flags);
end;

function QueueIOWorkItem(Func:TThreadStartRoutine; Context:Pointer):boolean;
begin
  Result := AddQueueWorkItem(Func, Context, WT_EXECUTEINIOTHREAD);
end;

function QueueUIWorkItem(Func:TThreadStartRoutine; Context:Pointer):boolean;
begin
  Result := AddQueueWorkItem(Func, Context, WT_EXECUTEINUITHREAD);
end;

end.
