/// RyuLib에서 제공되는 스레드 클래스에 대한 지원
unit ThreadUtils;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Types;

procedure AddThreadObject(AHandle:THandle);
procedure RemoveThreadObject(AHandle:THandle);

function GetCurrentThreadPriority:integer;

procedure SetThreadPriorityWithValue(AValue:integer);

procedure SetThreadPriorityNormal;
procedure SetThreadPrioritySlow;
procedure SetThreadPriorityFast;
procedure SetThreadPriorityRealTime;
procedure SetThreadPriorityTurbo;

implementation

var
  CurrentThreadPriority : Integer = THREAD_PRIORITY_HIGHEST;
  ThreadList : TList;
  CS : TCriticalSection;

procedure AddThreadObject(AHandle:THandle);
begin
  CS.Acquire;
  try
    SetThreadPriority( AHandle, CurrentThreadPriority );
    ThreadList.Add( Pointer(AHandle) );
  finally
    CS.Release;
  end;
end;

procedure RemoveThreadObject(AHandle:THandle);
begin
  CS.Acquire;
  try
    ThreadList.Remove( Pointer(AHandle) );
  finally
    CS.Release;
  end;
end;

function GetCurrentThreadPriority:integer;
begin
  Result := CurrentThreadPriority;
end;

procedure SetThreadPriorityWithValue(AValue:integer);
var
  Loop: Integer;
begin
  CS.Acquire;
  try
    if AValue = CurrentThreadPriority then Exit;

    CurrentThreadPriority := AValue;

    for Loop := 0 to ThreadList.Count-1 do
      SetThreadPriority( THandle(ThreadList[Loop]), AValue );
  finally
    CS.Release;
  end;
end;

procedure SetThreadPriorityNormal;
begin
  SetThreadPriorityWithValue( THREAD_PRIORITY_NORMAL );
end;

procedure SetThreadPrioritySlow;
begin
  SetThreadPriorityWithValue( THREAD_PRIORITY_BELOW_NORMAL );
end;

procedure SetThreadPriorityFast;
begin
  SetThreadPriorityWithValue( THREAD_PRIORITY_HIGHEST );
end;

procedure SetThreadPriorityRealTime;
begin
  SetThreadPriorityWithValue( THREAD_PRIORITY_TIME_CRITICAL );
end;

procedure SetThreadPriorityTurbo;
begin
  SetThreadPriorityWithValue( 31 );
end;

procedure ClearThreadObject;
begin
  CS.Acquire;
  try
    ThreadList.Clear;
  finally
    CS.Release;
  end;
end;


initialization
  ThreadList := TList.Create;
  CS := TCriticalSection.Create;
finalization
  ClearThreadObject;

  FreeAndNil(ThreadList);
  FreeAndNil(CS);
end.
