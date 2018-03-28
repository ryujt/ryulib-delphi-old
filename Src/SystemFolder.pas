unit SystemFolder;

// 린이가 만든 유닛

interface

uses
  Windows, Registry, SHFolder, ShlObj;

function PublicDocuments:string;
function MyDocuments:string;
function TempFolder:string;
function GetRandomTempFileName:string;
function WindowsFolder:String;

implementation

function PublicDocuments:string;
var
  Buffer : array [0..MAX_PATH - 1] of char;
begin
  SHGetFolderPath(0, CSIDL_COMMON_DOCUMENTS, 0, 0, Buffer);
  Result := Buffer;
end;

function WindowsFolder:String;
var
  Buffer : array [0..MAX_PATH - 1] of char;
begin
  GetEnvironmentVariable(PChar('SYSTEMROOT'), Buffer, MAX_PATH);
  Result := Buffer;
end;

function MyDocuments:string;
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  Result := '';
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\explorer\Shell Folders', false);
    Result := Reg.ReadString('Personal');
  finally
    Reg.Free;
  end;
end;

function TempFolder:string;
var
  Buffer : array [0..MAX_PATH - 1] of char;
begin
  GetEnvironmentVariable(PChar('TEMP'), Buffer, MAX_PATH);
  Result := Buffer;
end;

function GetRandomTempFileName:string;
var
  TempFolderName: string;
  CreatedFileName : array [0..MAX_PATH] of char;
begin
  TempFolderName := TempFolder;
  Windows.GetTempFileName(PChar(TempFolderName), '', Random(MaxInt), @CreatedFileName[0]);
  Result := CreatedFileName;
end;

begin
  Randomize;
end.
