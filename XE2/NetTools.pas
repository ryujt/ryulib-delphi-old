unit NetTools;

interface

uses
  Windows, Classes, SysUtils, IdStack;

function GetLocalIP:string;

implementation

function GetLocalIP:string;
var
  IdStack : TIdStack;
begin
  IdStack := TIdStack.CreateStack;
  try
    Result := IdStack.LocalAddress;
  finally
    IdStack.Free;
  end;
end;

end.
