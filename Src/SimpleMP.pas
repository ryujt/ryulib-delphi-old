unit SimpleMP;

interface

uses
  DebugTools, CountLock, ThreadPool,
  Windows, SysUtils, Classes;

type
  {*
    스레드 마다 한 번만 호출한다.
    @param Context Execute 메소드의 AContext 인자가 그대로 전달 됩니다.
    @param ThreadNo 현재 TSimpleProcedureSingle 함수를 실행하는 스레드의 순번이 전달 됩니다.
           이를 통해서 스레드를 구별 할 수 있습니다.
    @param ThreadCount 현재 사용 중인 전체 스레드의 숫자를 알려줍니다.
           Execute 함수를 호출 할 때 지정한 AThreadCount와 동일한 값을 가집니다.
  }
  TSimpleProcedureSingle = reference to procedure(Context:pointer; ThreadNo,ThreadCount:integer);

  {*
    스레드 내부에서 익명메소드를 필요한 횟수만큼 반복한다.  (전체 Task를 나눠서 해당 스레드가 꼭 필요한 횟수 만큼)
    @param Context Execute 메소드의 AContext 인자가 그대로 전달 됩니다.
    @param ThreadNo 현재 TSimpleProcedureSingle 함수를 실행하는 스레드의 순번이 전달 됩니다.
           이를 통해서 스레드를 구별 할 수 있습니다.
    @param Index Execute 메소드의 ATaskCount 횟 수만큼 반복해서 TSimpleProcedureRepeat를 실행할 때, 현재 반복하고 있는 순서입니다.
           병렬처리이기 때문에 Index는 순서대로 진행된다는 보장은 없습니다.
  }
  TSimpleProcedureRepeat = reference to procedure(Context:pointer; ThreadNo,Index:integer);

  TSimpleMP = class
  private
  public
    {*
       AThreadCount 숫자만큼 스레드를 만들어서, 각각의 스레드가 ASimpleProcedure를 병렬로 실행합니다.
       스레드와 ASimpleProcedure가 1:1로 매칭 됩니다.

       AContext는 ASimpleProcedure 내부에서 사용하고 싶은 객체(데이터)의 참조를 넘겨주고 싶을 때 사용합니다.
    }
    class procedure Execute(AContext:pointer; AThreadCount:integer; ASimpleProcedure:TSimpleProcedureSingle); overload;

    {*
      반복 횟수(ATaskCount)가 정해져 있는 경우 사용합니다.

      만약 "for i:= 1 to 10 do ..."를 병렬처리한다면, 아래처럼 사용하게 됩니다.
      AThreadCount는 사용하고 싶은 스레드 개수입니다.
        TSimpleMP.Execute( nil, 10, AThreadCount,
          procedure(Context:pointer; ThreadNo,Index:integer)
          begin
            ...
          end
        );
    }
    class procedure Execute(AContext:pointer; ATaskCount,AThreadCount:integer; ASimpleProcedure:TSimpleProcedureRepeat); overload;
  end;

implementation

type
  TThreadSingleInfo = class
  private
    FLock : TCountLock;
    FThreadNo : integer;
    FThreadCount : integer;
    FContext : pointer;
    FSimpleProcedure : TSimpleProcedureSingle;
  public
    constructor Create(
      AThreadNo,AThreadCount:integer;
      ALock:TCountLock;
      AContext:pointer;
      ASimpleProcedure:TSimpleProcedureSingle); reintroduce;
  end;

  TThreadRepeatInfo = class
  private
    FLock : TCountLock;
    FThreadNo : integer;
    FContext : pointer;
    FIndex : integer;
    FCount : integer;
    FSimpleProcedure : TSimpleProcedureRepeat;
  public
    constructor Create(
      AThreadNo:integer;
      ALock:TCountLock;
      AContext:pointer;
      AIndex,ACount:integer;
      ASimpleProcedure:TSimpleProcedureRepeat); reintroduce;
  end;

{ TThreadSingleInfo }

constructor TThreadSingleInfo.Create(AThreadNo,AThreadCount: integer;
  ALock: TCountLock;
  AContext: pointer;
  ASimpleProcedure: TSimpleProcedureSingle);
begin
  inherited Create;

  FThreadNo := AThreadNo;
  FThreadCount := AThreadCount;
  FLock := ALock;
  FContext := AContext;
  FSimpleProcedure := ASimpleProcedure;
end;

{ TThreadRepeatInfo }

constructor TThreadRepeatInfo.Create(
  AThreadNo:integer;
  ALock:TCountLock;
  AContext:pointer;
  AIndex,ACount:integer;
  ASimpleProcedure:TSimpleProcedureRepeat);
begin
  inherited Create;

  FThreadNo := AThreadNo;
  FLock := ALock;
  FContext := AContext;
  FIndex := AIndex;
  FCount := ACount;
  FSimpleProcedure := ASimpleProcedure;
end;

{ TSimpleMP }

function ThreadFunction_Single(lpThreadParameter:pointer):integer; stdcall;
var
  ThreadSingleInfo : TThreadSingleInfo ABSOLUTE lpThreadParameter;
begin
  Result := 0;

  try
    try
      ThreadSingleInfo.FSimpleProcedure(ThreadSingleInfo.FContext, ThreadSingleInfo.FThreadNo, ThreadSingleInfo.FThreadCount );
    finally
      ThreadSingleInfo.FLock.Dec;
    end;
  except
    on E : Exception do Trace( Format('TSimpleMP.ThreadFunction_Repeat - %s', [E.Message]) );
  end;

  ThreadSingleInfo.Free;
end;

class procedure TSimpleMP.Execute(AContext: pointer; AThreadCount: integer;
  ASimpleProcedure: TSimpleProcedureSingle);
var
  Lock : TCountLock;
  Loop: Integer;
begin
  Lock := TCountLock.Create;
  try
    for Loop := 0 to AThreadCount-1 do begin
      Lock.Inc;
      QueueWorkItem( ThreadFunction_Single, TThreadSingleInfo.Create(Loop, AThreadCount, Lock, AContext, ASimpleProcedure) );
    end;

    Lock.WaitFor;
  finally
    Lock.Free;
  end;
end;

function ThreadFunction_Repeat(lpThreadParameter:pointer):integer; stdcall;
var
  Loop: Integer;
  ThreadRepeatInfo : TThreadRepeatInfo ABSOLUTE lpThreadParameter;
begin
  Result := 0;

  try
    try
      for Loop := 0 to ThreadRepeatInfo.FCount-1 do begin
        ThreadRepeatInfo.FSimpleProcedure(ThreadRepeatInfo.FContext, ThreadRepeatInfo.FThreadNo, ThreadRepeatInfo.FIndex + Loop );
      end;
    finally
      ThreadRepeatInfo.FLock.Dec;
    end;
  except
    on E : Exception do Trace( Format('TSimpleMP.ThreadFunction_Repeat - %s', [E.Message]) );
  end;

  ThreadRepeatInfo.Free;
end;

class procedure TSimpleMP.Execute(AContext:pointer; ATaskCount,AThreadCount: integer;
  ASimpleProcedure: TSimpleProcedureRepeat);
var
  Lock : TCountLock;
  iIndex, iTaskCount : integer;
  Loop: Integer;
begin
  iTaskCount := ATaskCount div AThreadCount;
  if iTaskCount = 0 then iTaskCount := 1;

  iIndex := 0;

  Lock := TCountLock.Create;
  try
    for Loop := 0 to AThreadCount-1 do begin
      if ATaskCount < iTaskCount then iTaskCount := ATaskCount;

      if Loop = (AThreadCount-1) then iTaskCount := ATaskCount;

      Lock.Inc;

      QueueWorkItem( ThreadFunction_Repeat, TThreadRepeatInfo.Create(Loop, Lock, AContext, iIndex, iTaskCount, ASimpleProcedure) );

      iIndex := iIndex + iTaskCount;

      ATaskCount := ATaskCount - iTaskCount;
    end;

    Lock.WaitFor;
  finally
    Lock.Free;
  end;
end;

end.
