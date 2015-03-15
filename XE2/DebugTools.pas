unit DebugTools;

interface

uses
  Windows, Classes, SysUtils;

procedure SetDebugStr(AMsg:string);
function GetDebugStr:string;

procedure Trace(const AMsg:string);

implementation

var
  DebugStr : string = '';  // TODO: 스레드 세이프하도록 변경

procedure SetDebugStr(AMsg:string);
begin
  DebugStr := AMsg;
end;

function GetDebugStr:string;
begin
  Result := DebugStr;
end;

procedure Trace(const AMsg:string);
begin
  // DebugView에서 필터링하기 위해서 테그를 붙여 둠
  OutputDebugString(PChar('[MW] ' + AMsg));
end;

end.
