/// 폴더 전체에 있는 소스 파일을 읽어서, 문자열 부분만 추려 옵니다.
program ExtractStrings;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Para,
  SearchDir,
  Disk,
  Strg,
  System.SysUtils,
  PrintString in 'PrintString.pas';

var
  sPath, sExt : string;
  isRecursive : boolean;

begin
  if GetParameterCount = 0 then sPath := GetExecPath
  else sPath := GetParameter(0);

  if (sPath = './') or (sPath = '.\') then sPath := GetExecPath;

  sPath := ExtractFilePath(sPath);

  sExt := GetParameter(1);

  if sExt = '' then sExt := '.pas';

  if Pos('.', sExt) > 0 then sExt := DeleteLeft(sExt, '.')
  else sExt := '.' + sExt;

  sExt := LowerCase(sExt);

  isRecursive := FindSwitchName('Recursive') or FindSwitchName('R');

  WriteLn( '* Path: ', sPath );
  WriteLn( '* Ext: ', sExt );
  WriteLn( '* Recursive: ', isRecursive );
  WriteLn;

  SearchFiles(

    sPath, isRecursive,

    procedure(Path:string; SearchRec:TSearchRec; var NeedStop:boolean)
    var
      iIndex : integer;
      sTargetExt, sFullName : string;
    begin
      sTargetExt := LowerCase(ExtractFileExt(SearchRec.Name));

      if sExt <> sTargetExt then Exit;

      sFullName := Path + SearchRec.Name;

      iIndex := Length(sFullName) - 60;

      if iIndex > 1 then begin
        WriteLn('....' + Copy(sFullName, iIndex, 61));
      end else begin
        WriteLn(Copy(sFullName, 1, 64));
      end;

      TPrintString.Obj.Execute( sFullName );
      WriteLn;
    end

  );
end.
