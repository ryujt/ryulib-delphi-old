unit ocxUtils;

interface

uses
  Windows, SysUtils, ComObj;

procedure RegOCX(const DLLName: string);
procedure UnRegOCX(const DLLName: string);

implementation

procedure RegOCX(const DLLName: string);
begin
  RegisterComServer(DLLName);
end;

procedure UnRegOCX(const DLLName: string);
type
  TRegProc = function: HResult; stdcall;
const
  RegProcName = 'DllUnregisterServer';
var
  Handle: THandle;
  RegProc: TRegProc;
begin
  Handle := SafeLoadLibrary(DLLName);
  if Handle <= HINSTANCE_ERROR then
    raise Exception.CreateFmt('%s: %s', [SysErrorMessage(GetLastError), DLLName]);
  try
    RegProc := GetProcAddress(Handle, RegProcName);
    if Assigned(RegProc) then OleCheck(RegProc) else RaiseLastOSError;
  finally
    FreeLibrary(Handle);
  end;
end;

end.
