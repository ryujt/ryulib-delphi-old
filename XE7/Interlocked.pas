unit Interlocked;

interface

uses
  Windows;

function InterlockedExchangeAdd64(var Addend: Int64; Value: Int64): Int64;

implementation

{$IFDEF CPUX86}
function InterlockedExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
begin
  repeat
    Result := Addend;
  until InterlockedCompareExchange64(Addend, Result+Value, Result) = Result;
end;
{$ENDIF}

{$IFDEF CPUX64}
function InterlockedExchangeAdd64(var Addend: Int64; Value: Int64): Int64; register;
asm
          .NOFRAME
          MOV     RAX, RDX
          LOCK    XADD [RCX].Int64, RAX
end;
{$ENDIF}

end.
