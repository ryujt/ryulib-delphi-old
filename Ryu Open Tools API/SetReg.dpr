program SetReg;

{$R UAC.res}

uses
  Strg,
  Windows,
  Registry,
  System.SysUtils;

var
  Reg : TRegistry;
  sPath : string;
begin
  sPath := ExtractFilePath(ParamStr(0));
  SetLastChar(sPath, '\');

  sPath := sPath + 'Template\';

  Reg:= TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('SoftWare\RyuLib\Delphi\OpenToolsAPI\', true);
    Reg.WriteString('Path', sPath);
  finally
    Reg.Free;
  end;
end.
