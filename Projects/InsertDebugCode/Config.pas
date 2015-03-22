unit Config;

interface

uses
  SysUtils;

const
//  DebugCodeUses = #13#10 + 'DebugTools, ';
  DebugCodeUses = #13#10 + '  CodeSiteLogging, ';

function DebugCodeMethodBegin(AMethodName:string):string;
function DebugCodeMethodEnd(AMethodName:string):string;

implementation

function DebugCodeMethodBegin;
begin
//  Result := Format(#13#10+'  Trace(''%s - Begin'');'+#13#10, [AMethodName])
  Result := Format(#13#10+'  CodeSite.Send(''%s - Begin'', Self);'+#13#10, [AMethodName])
end;

function DebugCodeMethodEnd;
begin
  Result := '';
//  Result := Format(#13#10+'  CodeSite.Send(''%s - End'');'+#13#10, [AMethodName])
end;

end.
