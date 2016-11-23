unit DebugTools;

interface

uses
  Windows, Classes, SysUtils;

procedure Trace(const AMsg:string); overload;
procedure Trace(AMsg:integer); overload;

function TraceCount:integer;

implementation

uses
  SuspensionQueue, SimpleThread;

var
  Queue : TSuspensionQueue<string>;

procedure Trace(const AMsg:string);
begin
  Queue.Push('[Ryu] ' + AMsg);
end;

procedure Trace(AMsg:integer);
begin
  Queue.Push('[Ryu] ' + IntToStr(AMsg));
end;

function TraceCount:integer;
begin
  Result := Queue.Count;
end;

initialization
  Queue := TSuspensionQueue<string>.Create;

  TSimpleThread.Create(
    'DebugTools',
    procedure (ASimpleThread:TSimpleThread)
    var
      Msg : string;
    begin
      while True do begin
        try
          Msg := Queue.Pop;
          OutputDebugString( PChar(Msg) );
        except
          //
        end;
      end;
    end
  );
end.
