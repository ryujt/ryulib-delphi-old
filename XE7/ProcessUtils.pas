unit ProcessUtils;

interface

uses
  DebugTools,
  Windows, Classes, SysUtils, Variants, ShellAPI, TlHelp32, PsAPI;

type
  TProcessList = class
  private
    FList : TStringList;
    function GetCount: integer;
    function GetItems(Index: integer): TProcessEntry32;
    function GetNames(Index: integer): string;
    function GetCaptions(Index: integer): string;
    function GetFullNames(Index: integer): string;
    function GetWindowClassName(Index: integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Update;
    procedure KillByIndex(Index:integer);
    procedure KillByFullName(AFullName:string);

    property Items [Index:integer] : TProcessEntry32 read GetItems;
    property Names [Index:integer] : string read GetNames;
    property FullNames [Index:integer] : string read GetFullNames;
    property Captions [Index:integer] : string read GetCaptions;
    property WindowClassName [Index:integer] : string read GetWindowClassName;
    property Count : integer read GetCount;
  end;

function ProcessFileName(PID: DWORD): string;
procedure Process32List(List:TStringList);
procedure KillProcess(List:TStringList; Index:integer);
function IndexOfProcess(List:TStringList; ProcessName:string):integer;

implementation

function ProcessFileName(PID: DWORD): string;
var
  Handle: THandle;
begin
  Result := '';
  Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if Handle <> 0 then
    try
      SetLength(Result, MAX_PATH);
      if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
    finally
      CloseHandle(Handle);
    end;
end;

// kernel32.dll을 사용하여 현재 떠있는 process를 읽어온다
procedure Process32List(List:TStringList);
var
  Next : BOOL;
  SHandle : THandle;
  Process32 : TProcessEntry32;
begin
  Process32.dwSize := SizeOf(TProcessEntry32);
  SHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if Process32First(SHandle, Process32) then begin
    // 실행화일명과 process object 저장
    List.AddObject(Process32.szExeFile, TObject(Process32.th32ProcessID));
    repeat
      Next := Process32Next(SHandle, Process32);
      if Next then List.AddObject(Process32.szExeFile, TObject(Process32.th32ProcessID));
    until not Next;
  end;

  // closes an open object handle
  CloseHandle(SHandle);
end;

procedure KillProcess(List:TStringList; Index:integer);
var
  hProcess : THandle;
  ProcId :    DWORD;
  TermSucc : BOOL;
begin
  ProcId := DWORD(List.Objects[Index]);
  
  // 존재하는 process object의 handle을 return한다
  hProcess := OpenProcess(PROCESS_TERMINATE, TRUE, ProcId);
  if hProcess = NULL then raise Exception.Create('KillProcess: OpenProcess error !');

  // 명시한 process를 강제 종료시킨다
  TermSucc := TerminateProcess(hProcess, 0);
  if TermSucc = false then raise Exception.Create('KillProcess: TerminateProcess error !');
end;

function IndexOfProcess(List:TStringList; ProcessName:string):integer;
begin
  Result := List.IndexOf(LowerCase(ProcessName));
end;

{ TProcessList }

procedure TProcessList.Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do Dispose(Pointer(FList.Objects[Loop]));
  FList.Clear;
end;

constructor TProcessList.Create;
begin
  inherited;

  FList := TStringList.Create;
end;

destructor TProcessList.Destroy;
begin
  Clear;

  FreeAndNil(FList);

  inherited;
end;

function ProcIDFromWnd(AHandle:THandle):DWord;
begin
  GetWindowThreadProcessId( AHandle, &Result );
end;

function GetWinHandle(APID:DWord):THandle;
var
  iHandle : THandle;
begin
  Result := 0;

  iHandle := FindWindow( nil, nil);

  while (iHandle <> 0) do begin
    if (GetParent(iHandle) = 0) and (APID = ProcIDFromWnd(iHandle)) then begin
      Result := iHandle;
      Exit;
    end;

    iHandle := GetWindow(iHandle, GW_HWNDNEXT);
  end;
end;

function TProcessList.GetCaptions(Index: integer): string;
var
  iHandle : THandle;
  Buffer: array [0..255] of Char;
begin
  Result := '';

  iHandle := GetWinHandle( Items[Index].th32ProcessID );
  if iHandle = 0 then Exit;

  if GetWindowText(iHandle, Buffer, 256) > 0 then Result := StrPas( Buffer );
end;

function TProcessList.GetWindowClassName(Index: integer): string;
var
  iHandle : THandle;
  Buffer : array [0..255] of Char;
begin
  Result := '';

  iHandle := GetWinHandle( Items[Index].th32ProcessID );
  if iHandle = 0 then Exit;

  if GetClassName(iHandle, Buffer, 256) > 0 then Result := StrPas( Buffer );
end;

function TProcessList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TProcessList.GetFullNames(Index: integer): string;
begin
  Result := ProcessFileName(Items[Index].th32ProcessID);
end;

function TProcessList.GetItems(Index: integer): TProcessEntry32;
var
  Item : ^TProcessEntry32;
begin
  Item := Pointer(FList.Objects[Index]);
  Result := Item^;
end;

function TProcessList.GetNames(Index: integer): string;
begin
  Result := FList[Index];
end;

procedure TProcessList.KillByFullName(AFullName: string);
var
  hProcess : THandle;
  Loop: Integer;
begin
  AFullName := LowerCase(AFullName);

  for Loop := 0 to FList.Count-1 do begin
    if AFullName = LowerCase(FullNames[Loop]) then begin
      hProcess := OpenProcess(PROCESS_TERMINATE, TRUE, Items[Loop].th32ProcessID);
      if hProcess <> NULL then begin
        TerminateProcess(hProcess, 0);
        CloseHandle(hProcess);
      end;
    end;
  end;
end;

procedure TProcessList.KillByIndex(Index: integer);
var
  hProcess : THandle;
  ProcId :    DWORD;
  TermSucc : BOOL;
begin
  ProcId := DWORD(Items[Index].th32ProcessID);

  // 존재하는 process object의 handle을 return한다
  hProcess := OpenProcess(PROCESS_TERMINATE, TRUE, ProcId);
  if hProcess = NULL then raise Exception.Create('KillProcess: OpenProcess error !');

  // 명시한 process를 강제 종료시킨다
  TermSucc := TerminateProcess(hProcess, 0);
  if TermSucc = false then raise Exception.Create('KillProcess: TerminateProcess error !');
end;

procedure TProcessList.Update;
var
  Next : BOOL;
  SHandle : THandle;
  Item : ^TProcessEntry32;
  Process32 : TProcessEntry32;
begin
  Clear;

  Process32.dwSize := SizeOf(TProcessEntry32);
  SHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if Process32First(SHandle, Process32) then begin
    New(Item);
    Move(Process32, Item^, SizeOf(TProcessEntry32));
    FList.AddObject(Process32.szExeFile, TObject(Item));

    repeat
      Next := Process32Next(SHandle, Process32);
      if Next then begin
        New(Item);
        Move(Process32, Item^, SizeOf(TProcessEntry32));
        FList.AddObject(Process32.szExeFile, TObject(Item));
      end;
    until not Next;
  end;

  // closes an open object handle
  CloseHandle(SHandle);
end;

end.
