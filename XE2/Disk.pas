Unit Disk;

Interface

Uses
  Strg, ShlObj, WinTypes, ShellAPI, iniFiles, Registry, PsAPI, TlHelp32,
  Windows, Classes, SysUtils, VCL.Forms;

function GetProcessNameFromWnd(Wnd:HWND):string;

function GetSpecialFolder(const CSIDL:integer):string;
function GetCommonAppDataPath:string;
function GetMyDocumentsPath:string;
function GetWindowsPath:string;
function GetSystemPath:string;
function GetTempDirectory:string;
function GetProgramsFolder:string;

function GetDiskSerialNumber(Drive:Char):string;

function GetFileLastAccess(FileName:string):TDateTime;

function GetExecPath:string;
function GetExecFileNameWithoutExtention:string;

function File_Size(FileName:string):int64;

function  EraseFile(FileName:string):Boolean;
procedure EraseFiles(FileName:string);

function TrashFile(AFilename:string):boolean;
procedure TrashFiles(AFilename:string);

procedure CopyFiles(SPath,TPath,Source:string);

procedure MoveFiles(SPath,TPath,Source:string);

procedure DeleteFolder(AFolder:string);

function ShellExecuteFile(FileName,Parameters,Directory:string):integer;
procedure ShellExecuteAndWait(AFileName,AParams,APath: string);
function RunFile(AFileName,AWorkDir:string; AVisibility:boolean=true):Cardinal;

function  IniString(FileName,Section,Ident,DefaultStr:string):string;
function  IniInteger(FileName,Section,Ident:string; DefaultInt:Integer):Integer;
procedure IniSection(FileName,Section:string; Strings:TStrings);
procedure WriteIniStr(FileName,Section,Ident,stData:string);
procedure WriteIniInt(FileName,Section,Ident:string; iData:Integer);
procedure DelIniSection(FileName,Section:string);
procedure DelIniKey(FileName,Section,Ident:string);

procedure SaveTextToFile(AFileName,AText:string);
function  LoadFileAsText(FileName:string):string;
procedure SaveLogMessage(FileName,Msg:string);
procedure AddDataToFile(FileName:string; var Data; Size:Integer);
procedure ShellExecuteWait(FileName,Params,Directory:string);
function GetStorageFreeSpace(const ADrive: string): Int64;
function GetStorageTotalSpace(const ADrive: string): Int64;
function GetCurrentDrive: string;

function AddAccountNameToAcl(ptr_acl:PACL; AccountName:string):boolean;
function SetEveryoneAllowedToUseFile(AFileName:string):boolean;

function GetVersionNumber(Filename:string):string;

function RunCaptured(const APath, AExecutive, AParameter: string): string;

function GetAssociation(const DocFileName: string): string;
procedure SetAssociation(ExtName:String; AppName:String);

function FindFile(AFileName:string):boolean;

Implementation

const
  RsSystemIdleProcess = 'System Idle Process';
  RsSystemProcess = 'System Process';

function IsWinXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion = 5)
    and (Win32MinorVersion = 1);
end;

function IsWin2k: Boolean;
begin
  Result := (Win32MajorVersion >= 5) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

function IsWinNT4: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 4);
end;

function IsWin3X: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 3) and
    ((Win32MinorVersion = 1) or (Win32MinorVersion = 5) or
    (Win32MinorVersion = 51));
end;

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;

  function ProcessFileName(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
      False, PID);
    if Handle <> 0 then
      try
        SetLength(Result, MAX_PATH);
        if FullPath then
        begin
          if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
        end
        else
        begin
          if GetModuleBaseNameA(Handle, 0, PAnsiChar(AnsiString(Result)), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
        end;
      finally
        CloseHandle(Handle);
      end;
  end;

  function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
    FileName: string;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then
      try
        ProcEntry.dwSize := SizeOf(ProcEntry);
        NextProc := Process32First(SnapProcHandle, ProcEntry);
        while NextProc do
        begin
          if ProcEntry.th32ProcessID = 0 then
          begin
            FileName := RsSystemIdleProcess;
          end
          else
          begin
            if IsWin2k or IsWinXP then
            begin
              FileName := ProcessFileName(ProcEntry.th32ProcessID);
              if FileName = '' then
                FileName := ProcEntry.szExeFile;
            end
            else
            begin
              FileName := ProcEntry.szExeFile;
              if not FullPath then
                FileName := ExtractFileName(FileName);
            end;
          end;
          List.AddObject(FileName, Pointer(ProcEntry.th32ProcessID));
          NextProc := Process32Next(SnapProcHandle, ProcEntry);
        end;
      finally
        CloseHandle(SnapProcHandle);
      end;
  end;

  function BuildListPS: Boolean;
  var
    PIDs: array [0 .. 1024] of DWORD;
    Needed: DWORD;
    I: Integer;
    FileName: string;
  begin
    Result := EnumProcesses(@PIDs, SizeOf(PIDs), Needed);
    if Result then
    begin
      for I := 0 to (Needed div SizeOf(DWORD)) - 1 do
      begin
        case PIDs[I] of
          0:
            FileName := RsSystemIdleProcess;
          2:
            if IsWinNT4 then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
          8:
            if IsWin2k or IsWinXP then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
        else
          FileName := ProcessFileName(PIDs[I]);
        end;
        if FileName <> '' then
          List.AddObject(FileName, Pointer(PIDs[I]));
      end;
    end;
  end;

begin
  if IsWin3X or IsWinNT4 then
    Result := BuildListPS
  else
    Result := BuildListTH;
end;

function GetProcessNameFromWnd(Wnd: HWND): string;
var
  List: TStringList;
  PID: DWORD;
  I: Integer;
begin
  Result := '';
  if IsWindow(Wnd) then
  begin
    PID := INVALID_HANDLE_VALUE;
    GetWindowThreadProcessId(Wnd, @PID);
    List := TStringList.Create;
    try
      if RunningProcessesList(List, True) then
      begin
        I := List.IndexOfObject(Pointer(PID));
        if I > -1 then
          Result := List[I];
      end;
    finally
      List.Free;
    end;
  end;
end;

function GetSpecialFolder(const CSIDL:integer):string;
var
  RecPath : PWideChar;
begin
  RecPath := StrAlloc(MAX_PATH);
  try
    FillChar(RecPath^, MAX_PATH, 0);
    SHGetSpecialFolderPath(0, RecPath, CSIDL, true);
    Result := RecPath;
  finally
    StrDispose(RecPath);
  end;
end;

function GetCommonAppDataPath:string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_APPDATA);
  SetLastChar(Result, '\');
end;

function GetMyDocumentsPath:string;
begin
  Result := GetSpecialFolder(CSIDL_PERSONAL);
  SetLastChar(Result, '\');
end;

function GetWindowsPath:string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_APPDATA);
  SetLastChar(Result, '\');
end;

function GetSystemPath:string;
begin
  Result := GetSpecialFolder(CSIDL_SYSTEM);
  SetLastChar(Result, '\');
end;

function GetProgramsFolder:string;
begin
  Result := GetSpecialFolder(CSIDL_PROGRAM_FILES);
  SetLastChar(Result, '\');
end;

function GetTempDirectory:string;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  Result := StrPas(tempFolder);
  SetLastChar(Result, '\');
end;

Function  GetDiskSerialNumber(Drive:Char):String;
Var
   FileSystemFlags : DWORD;
   VolumeSerialNumber : DWORD;
   MaximumComponentLength : DWORD;
Begin
  GetVolumeInformation(PChar(Drive+':\'), Nil, 0,
                       @VolumeSerialNumber,
                       MaximumComponentLength,
                       FileSystemFlags, Nil, 0);
  Result:= IntToHex(HiWord(VolumeSerialNumber), 4)+
           IntToHex(LoWord(VolumeSerialNumber), 4);
End;

function GetFileLastAccess(FileName:string):TDateTime;
var
  DT : TFileTime;
  ST : TSystemTime;
  SearchRec : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then begin
    FileTimeToLocalFileTime(SearchRec.FindData.ftLastAccessTime, DT);
    FileTimeToSystemTime(DT, ST);
    Result:= SystemTimeToDateTime(ST);
  end else begin
    FillChar(Result, SizeOf(Result), $00);
  end;

  SysUtils.FindClose(SearchRec);
end;

function GetExecPath:String;
begin
  Result := ExtractFilePath(ParamStr(0));
  SetLastChar(Result, '\');
end;

function GetExecFileNameWithoutExtention:string;
begin
  Result := DeleteRightPlus( ParamStr(0), '.' );
end;

function File_Size(FileName:String):int64;
var
   SInformation : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SInformation) = 0 then
     Result:= SInformation.Size
  else Result:= 0;
end;

function EraseFile(FileName:string):boolean;
var
   EFile : File;
begin
  AssignFile(EFile, FileName);
  try
    Erase(EFile);
    Result:= True;
  except
    Result:= False;
  end;
end;

procedure EraseFiles(FileName:String);
var
   EFile  : file;
   sDir : string;
   iResult : integer;
   SearchRec : TSearchRec;
begin
  if FileName = '' then Exit;

  sDir:= FileName;
  sDir:= ExtractFileDir(sDir);

  SetLastChar(sDir, '\');

  iResult:= FindFirst(FileName, faAnyFile, SearchRec);
  while iResult = 0 do begin
    if (SearchRec.Name = '.') or (SearchRec.Name = '..') then begin
      iResult:= FindNext(SearchRec);
      Continue;
    end;

    try
      AssignFile(EFile, sDir+SearchRec.Name);
      Erase(EFile);
    except
      // TODO:
    end;

    iResult:= FindNext(SearchRec);
  end;

  SysUtils.FindClose(SearchRec);
end;

function TrashFile(AFilename:string):boolean;
var
  fos : TSHFileOpStruct;
begin
  FillChar( fos, SizeOf(fos), 0 );

  fos.wFunc  := FO_DELETE;
  fos.pFrom  := PChar(AFilename);
  fos.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_NOERRORUI;

  Result := ShFileOperation(fos) <> 0;
end;

procedure TrashFiles(AFilename:string);
var
   sDir : string;
   iResult : integer;
   SearchRec : TSearchRec;
begin
  if AFilename = '' then Exit;

  sDir:= AFilename;
  sDir:= ExtractFileDir(sDir);

  SetLastChar(sDir, '\');

  iResult:= FindFirst(AFilename, faAnyFile, SearchRec);
  while iResult = 0 do begin
    if (SearchRec.Name = '.') or (SearchRec.Name = '..') then begin
      iResult:= FindNext(SearchRec);
      Continue;
    end;

    TrashFile(sDir+SearchRec.Name);

    iResult:= FindNext(SearchRec);
  end;

  SysUtils.FindClose(SearchRec);
end;

procedure CopyFiles(SPath,TPath,Source:string);
var
  FResult : Integer;
  SearchRec : TSearchRec;
begin
  SetLastChar(SPath, '\');
  SetLastChar(TPath, '\');
  FResult:= FindFirst(SPath+Source, faAnyFile, SearchRec);
  while FResult = 0 do begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
       CopyFile(PChar(SPath+SearchRec.Name), PChar(TPath+SearchRec.Name), false);
    FResult:= FindNext(SearchRec);
  end;
end;

procedure MoveFiles(SPath,TPath,Source:string);
var
  FResult : Integer;
  SearchRec : TSearchRec;
begin
  SetLastChar(SPath, '\');
  SetLastChar(TPath, '\');
  FResult:= FindFirst(SPath+Source, faAnyFile, SearchRec);
  while FResult = 0 do begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
       MoveFileEx(PChar(SPath+SearchRec.Name), PChar(TPath+SearchRec.Name), MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING);
    FResult:= FindNext(SearchRec);
  end;
end;

procedure DeleteFolder(AFolder:string);
var
  Shfi: TSHFileOpStruct;
begin
  Shfi.Wnd := 0;
  Shfi.wFunc := FO_DELETE;
  Shfi.pFrom := PChar(AFolder + #0#0);
  Shfi.pTo := PChar(#0#0);
  Shfi.fFlags := FOF_NO_UI;
  ShFileOperation(Shfi);
end;

function ShellExecuteFile(FileName,Parameters,Directory:String):integer;
begin
  Result:= ShellExecute(GetForegroundWindow, 'open', PChar(FileName), PChar(Parameters), PChar(Directory), SW_SHOWNORMAL);
end;

procedure ShellExecuteAndWait(AFileName,AParams,APath: string);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TShellExecuteInfo);
  with SEInfo do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(AFileName);
    lpParameters := PChar(AParams);
    lpDirectory := PChar(APath);
    nShow := SW_SHOWNORMAL;
  end;

  if ShellExecuteEx(@SEInfo) then begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(SEInfo.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
  end;
end;

function RunFile(AFileName,AWorkDir:string; AVisibility:boolean=true):Cardinal;
var
  isCondition : boolean;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if AWorkDir = '' then GetDir(0, AWorkDir);

  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Integer(AVisibility);

  isCondition := CreateProcess(
    nil, PChar(AFileName), nil, nil, false,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
    nil, StartupInfo, ProcessInfo
  );

  if isCondition then Result := ProcessInfo.hProcess
  else Result := 0;
end;

Function  IniString(FileName,Section,Ident,DefaultStr:String):String;
var
  iniFile : TiniFile;
begin
  iniFile:= TiniFile.Create(FileName);
  try
    try
      Result:= iniFile.ReadString(Section, Ident, DefaultStr);
    except
      Result := DefaultStr;
    end;
  finally
    iniFile.Free;
  end;
end;

Function  IniInteger(FileName,Section,Ident:String; DefaultInt:Integer):Integer;
var
  iniFile: TiniFile;
begin
  iniFile := TiniFile.Create(FileName);
  Try
    try
      Result := iniFile.ReadInteger(Section, Ident, DefaultInt);
    except
      Result := DefaultInt;
    end;
  Finally
    iniFile.Free;
  End;
end;

Procedure IniSection(FileName,Section:String; Strings:TStrings);
Var
   iniFile : TiniFile;
Begin
  iniFile:= TiniFile.Create(FileName);
  Try
    try
      iniFile.ReadSectionValues(Section, Strings);
    except
      //
    end;
  Finally
    iniFile.Free;
  End;
End;

Procedure WriteIniStr(FileName,Section,Ident,stData:String);
Var
   iniFile : TiniFile;
Begin
  iniFile:= TiniFile.Create(FileName);
  Try
    try
      iniFile.WriteString(Section, Ident, stData);
    except
      //
    end;
  Finally
    iniFile.Free;
  End;
End;

Procedure WriteIniInt(FileName,Section,Ident:String; iData:Integer);
Var
   iniFile : TiniFile;
Begin
  iniFile:= TiniFile.Create(FileName);
  Try
    try
      iniFile.WriteInteger(Section, Ident, iData);
    except
      //
    end;
  Finally
    iniFile.Free;
  End;
End;

Procedure DelIniSection(FileName,Section :String);
Var
  iniFile : TiniFile;
Begin
  iniFile:= TiniFile.Create(FileName);
  Try
    iniFile.EraseSection(Section);
  Finally
    iniFile.Free;
  End;
End;

Procedure DelIniKey(FileName,Section,Ident:String);
Var
    iniFile : TiniFile;
Begin
    iniFile:= TiniFile.Create(FileName);
    iniFile.DeleteKey(Section, Ident);
    iniFile.Free;
End;

procedure SaveTextToFile(AFileName,AText:string);
var
   fs : TFileStream;
   ss : TStringStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  ss := TStringStream.Create(AText);
  try
    ss.Position := 0;
    fs.CopyFrom(ss, ss.Size);
  finally
    fs.Free;
    ss.Free;
  end;
end;

Function  LoadFileAsText(FileName:String):String;
Var
   fs : TFileStream;
   ss : TStringStream;
Begin
  fs:= TFileStream.Create(FileName, fmOpenRead);
  ss:= TStringStream.Create('');
  Try
    ss.CopyFrom(fs, fs.Size);
    ss.Position:= 0;
    Result:= ss.DataString;
  Finally
    ss.Free;
    fs.Free;
  End;
End;

procedure SaveLogMessage(FileName,Msg:String);
var
  LogFile : TextFile;
begin
  Assign(LogFile, FileName);
  try
    if FileExists(FileName) then Append(LogFile)
    else Rewrite(LogFile);
    WriteLn(LogFile, Msg);
    Flush(LogFile);
  finally
    Close(LogFile);
  end;
end;

procedure AddDataToFile(FileName:string; var Data; Size:integer);
var
  fsData : TFileStream;
begin
  if FileExists(FileName) then fsData:= TFileStream.Create(FileName, fmOpenWrite)
  else fsData:= TFileStream.Create(FileName, fmCreate);

  try
    fsData.Position:= fsData.Size;
    fsData.Write(Data, Size);
  finally
    fsData.Free;
  end;
end;

procedure ShellExecuteWait(FileName,Params,Directory:String);
var
  Ph : DWORD;
  exInfo : TShellExecuteInfo;
begin
 FillChar( exInfo, Sizeof(exInfo), 0 );

 with exInfo do begin
   cbSize:= Sizeof( exInfo );
   fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
   Wnd := GetActiveWindow();
   ExInfo.lpVerb := 'open';
   ExInfo.lpParameters := PChar(Params);
   ExInfo.lpDirectory := PChar(Directory);
   lpFile:= PChar(FileName);
   nShow := SW_SHOWNORMAL;
 end;

 if ShellExecuteEx(@exInfo) then begin
   Ph := exInfo.HProcess;
 end else begin
   raise Exception.Create(SysErrorMessage(GetLastError));
   exit;
 end;

 while WaitForSingleObject(ExInfo.hProcess, 50) <> WAIT_OBJECT_0 do
   Sleep(1);
//   Application.ProcessMessages;

 CloseHandle(Ph);
end;

procedure _GetDiskSpaceEx(const ADrive: string;
  var ATotalNumberOfBytes, ATotalNumberOfFreeBytes: Int64);
var
  FreeBytesAvailableToCaller: Int64;
begin
  FreeBytesAvailableToCaller := 0;
  ATotalNumberOfBytes := 0;
  ATotalNumberOfFreeBytes := 0;

  GetDiskFreeSpaceEx(
    PChar(ADrive),
    FreeBytesAvailableToCaller,
    ATotalNumberOfBytes,
    @ATotalNumberOfFreeBytes
  );
end;

function GetStorageFreeSpace(const ADrive: string): Int64;
var
  TotalNumberOfBytes,
  TotalNumberOfFreeBytes : Int64;
begin
  _GetDiskSpaceEx(ADrive, TotalNumberOfBytes, TotalNumberOfFreeBytes);
  Result := TotalNumberOfFreeBytes;
end;

function GetStorageTotalSpace(const ADrive: string): Int64;
var
  TotalNumberOfBytes,
  TotalNumberOfFreeBytes : Int64;
begin
  _GetDiskSpaceEx(ADrive, TotalNumberOfBytes, TotalNumberOfFreeBytes);
  Result := TotalNumberOfBytes;
end;

function GetCurrentDrive: string;
begin
  Result := ExtractFileDrive(ParamStr(0));
end;

const
  ACL_REVISION = 2;

function AddAccountNameToAcl(ptr_acl:PACL; AccountName:string):boolean;
var
  SidBuffer: array [1..100] of byte;
  ptr_sid: PSID;
  SidBufSz: dword;
  DomainNameBuf: String;
  DomainNameBufSz: dword;
  SNU: Sid_Name_Use;
begin
  ptr_sid := @SidBuffer;
  SidBufSz := SizeOf(SidBuffer);
  SetLength(DomainNameBuf, 256);
  DomainNameBufSz := 256;

  Result := LookupAccountName(nil, PChar(AccountName), ptr_sid, SidBufSz, PChar(DomainNameBuf), DomainNameBufSz, SNU);
  if not Result then Exit;

  SetLength(DomainNameBuf, DomainNameBufSz);

  Win32Check(AddAccessAllowedAce(ptr_acl^, ACL_REVISION,
    GENERIC_ALL + GENERIC_READ + GENERIC_WRITE + GENERIC_EXECUTE, ptr_sid));
end;

function SetEveryoneAllowedToUseFile(AFileName:string):boolean;
var
  SD: TSecurityDescriptor;
  SA: TSecurityAttributes;
  SI: SECURITY_INFORMATION;
  AclBuf: array [1 .. 1024] of byte;
  ptr_acl: PACL;
begin
  Win32Check(InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION));

  ptr_acl := @AclBuf;
  Win32Check(InitializeACL(ptr_acl^, SizeOf(AclBuf), ACL_REVISION));

  AddAccountNameToAcl(ptr_acl, 'Everyone');
  Win32Check(SetSecurityDescriptorDacl(@SD, true, ptr_acl, false));

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := false;
  SA.lpSecurityDescriptor := @SD;
  SI := DACL_SECURITY_INFORMATION;

  Result := SetFileSecurity(PChar(AFileName), SI, @SD);
end;

function GetVersionNumber(Filename:string):string;
var
  Major, Minor: Word;
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VerInfo);
  if not VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
    Result := ''
  else
    with VerValue^ do
    begin
      Major := dwFileVersionMS shr 16;
      Minor := dwFileVersionMS and $FFFF;
      Result := IntToStr(Major) + '.' + IntToStr(Minor);

      Major := dwFileVersionLS shr 16;
      Minor := dwFileVersionLS and $FFFF;
      Result := Result + '.' + IntToStr(Major) + '.' + IntToStr(Minor);
    end;
  FreeMem(VerInfo, VerInfoSize);
end;

function RunCaptured(const APath, AExecutive, AParameter: string): string;
var
  TempFile: THandle;
  TempFileName : string;
  SecurityAttributes: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  procInfo: TProcessInformation;
  ExitCode: Cardinal;
begin
  FillChar(SecurityAttributes, SizeOf(SecurityAttributes), #0);
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;

  TempFileName := GetTempDirectory + RandomStr;
  TempFile := CreateFile(PChar(TempFileName), Generic_Write, File_Share_Write, @SecurityAttributes, Create_Always, File_Attribute_Normal, 0);
  try
    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.hStdOutput := TempFile;
    StartupInfo.dwFlags := StartF_UseStdHandles or StartF_UseShowWindow;
    StartupInfo.wShowWindow := SW_Minimize;

    if not CreateProcess(nil, PChar(AExecutive + ' ' + AParameter), nil, nil, True, 0, nil, PChar(APath), StartupInfo, procInfo) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    try
      SetPriorityClass(procInfo.hProcess, Idle_Priority_Class);
      WaitForSingleObject(procInfo.hProcess, Infinite);
      GetExitCodeProcess(procInfo.hProcess, ExitCode);
    finally
      CloseHandle(procInfo.hThread);
      CloseHandle(procInfo.hProcess);
    end;
  finally
    CloseHandle(TempFile);
  end;

  Result := LoadFileAsText(TempFileName);
end;

function GetAssociation(const DocFileName: string): string;
var
  FileClass: string;
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_EXECUTE);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  FileClass := '';
  if Reg.OpenKeyReadOnly(ExtractFileExt(DocFileName)) then
  begin
    FileClass := Reg.ReadString('');
    Reg.CloseKey;
  end;
  if FileClass <> '' then begin
    if Reg.OpenKeyReadOnly(FileClass + '\Shell\Open\Command') then
    begin
      Result := Reg.ReadString('');
      Reg.CloseKey;
    end;
  end;
  Reg.Free;
end;

procedure SetAssociation(ExtName:String; AppName:String);
var
   reg:TRegistry;
begin
  reg := TRegistry.Create;
  try
   reg.RootKey:=HKEY_CLASSES_ROOT;
   reg.OpenKey('.' + ExtName, True) ;
   reg.WriteString('', ExtName + 'file') ;
   reg.CloseKey;

   reg.CreateKey(ExtName + 'file') ;
   reg.OpenKey(ExtName + 'file\DefaultIcon', True) ;
   reg.WriteString('', AppName + ',0') ;
   reg.CloseKey;

   reg.OpenKey(ExtName + 'file\shell\open\command', True) ;
   reg.WriteString('', AppName + ' "%1"') ;
   reg.CloseKey;
  finally
   reg.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function FindFile(AFileName:string):boolean;
var
  iResult : integer;
  SearchRec : TSearchRec;
  isCondition : boolean;
begin
  Result := false;

  iResult:= FindFirst(AFileName, faAnyFile, SearchRec);
  while iResult = 0 do begin
    isCondition :=
      ((SearchRec.Attr and faDirectory) = faDirectory) or
      (SearchRec.Name = '.') or
      (SearchRec.Name = '..');

    if isCondition then begin
      iResult:= FindNext(SearchRec);
      Continue;
    end;

    // AFileName 조건에 맞는 파일을 찾았음
    Result := true;
    Break;

    iResult:= FindNext(SearchRec);
  end;
end;

end.
