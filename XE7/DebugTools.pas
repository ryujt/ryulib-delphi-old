unit DebugTools;

interface

uses
  Windows, Classes, SysUtils;

procedure Trace(const AMsg:string);
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

function TraceCount:integer;
begin
  Result := Queue.Count;
end;

initialization
  Queue := TSuspensionQueue<string>.Create;

  TSimpleThread.Create(
    'DebugTools', nil,
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
