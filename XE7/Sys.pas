unit Sys;

interface

uses
  Windows, SysUtils, Classes, Messages, Registry, WinSock, Tlhelp32;

const
  BELOW_NORMAL_PRIORITY_CLASS = $4000;
  NORMAL_PRIORITY_CLASS = $20;
  ABOVE_NORMAL_PRIORITY_CLASS = $8000;
  HIGH_PRIORITY_CLASS = $80;
  REALTIME_PRIORITY_CLASS = $100;

Function  DesktopSize:TSize;

Function  RegReadString(Root:HKey; Path,Ident:String):String;
Procedure RegWriteString(Root:HKey; Path,Ident,Value:String);

function GetComputerNetName: string;
function GetUserFromWindows: string;

procedure RegisterProtocol(const Name, Describtion, ExecuteStr: string);
procedure UnregisterProtocol(const Name: string);

procedure AddProgramOnStart(ProgramName,FileName:string);
procedure RemoveProgramOnStart(ProgramName:string);

procedure AutoRun(Ext,Name,Parameter:string);

Function  GetOSLanguage:String;
procedure EnableWindowKey;
procedure DisableWindowKey;
Procedure SetRebootKey(Reboot:Boolean);
Procedure ShowStartButton(Show:Boolean);
Procedure EnableStartButton(Enable:Boolean);
Procedure ShowTakBar(Show:Boolean);
Function  TurnScreenSaverOn:Boolean;
Procedure DeleteRecentDocs;
Procedure AddRecentDocs(FileName:String);
Procedure RestartWindows;
Procedure RebootSystem;
Procedure ExitAndExecApp;
Procedure ShowDesktopIcons(Show:Boolean);
Procedure EnableTaskSwitchs(Enable:Boolean);

{$IFDEF CPUX86}
function CPUSpeed:Double;
{$ENDIF}

Function  ClassByName(AOwner:TComponent; ClassName:String):TComponent;
Function  BringWindowFront(ClassName,WindowName:PChar):Boolean;
Procedure CloseWindows(ClassName,WindowName:PChar);
Procedure SendCopyData(hTo,hFrom:HWND; Text:String);

{$IFDEF CPUX86}
function GetCPUBrandString:string;
{$ENDIF}

procedure ChangeWallPaper(FileName:String; Tile:Boolean);
procedure HardwareAcceleration(level: integer);  // 0: 최대, 5: 없음

procedure AddContextMenu(AMenuName,ATitle,ACommand:string);

function LocalIP: string;
function LocalIPs: string;

function TotalPhysicalSize:int64;
function FreePhysicalSize:NativeUInt;

function MyExitWindows(RebootParam: Longword): Boolean;

function ForceForegroundWindow(hwnd: THandle): boolean;

var
  SystemInfo : TSystemInfo;

implementation

uses
  ComObj, ActiveX, ShlObj;

Function  DesktopSize:TSize;
begin
  Result.cx:= GetSystemMetrics(SM_CXSCREEN);
  Result.cy:= GetSystemMetrics(SM_CYSCREEN);
end;

Function  RegReadString(Root:HKey; Path,Ident:String):String;
Var
  Reg : TRegistry;
Begin
 Reg:= TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
 Try
   Reg.RootKey:= Root;
   Reg.OpenKey(Path, False);
   Result:= Reg.ReadString(Ident);
 Finally
   Reg.Free;
 End;
End;

procedure RegWriteString(Root:HKey; Path,Ident,Value:string);
var
  Reg : TRegistry;
begin
  Reg:= TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
  try
    Reg.RootKey := Root;
    Reg.OpenKey(Path, True);
    Reg.WriteString(Ident, Value);
  finally
    Reg.Free;
  end;
end;

function GetComputerNetName: string;
var
  buffer: array [0 .. 255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

function GetUserFromWindows: string;
var
  UserName: string;
  UserNameLen: dword;
begin
  UserNameLen := 255;
  SetLength(UserName, UserNameLen);
  if GetUserName(PChar(UserName), UserNameLen) then
    Result := Copy(UserName, 1, UserNameLen - 1)
  else
    Result := 'Unknown';
end;

procedure RegisterProtocol(const Name, Describtion, ExecuteStr: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.OpenKey(Name, True);
    try
      reg.Writestring('', 'URL:' + Name +' (' + Describtion + ')');
      reg.WriteInteger('EditFlags', 2);
      reg.WriteString('Source Filter', '');
      reg.WriteString('URL Protocol', '');
      reg.OpenKey('shell', True);
      reg.OpenKey('open', True);
      reg.OpenKey('command', True);
      reg.Writestring('', ExecuteStr);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure UnregisterProtocol(const Name: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.DeleteKey(Name);
  finally
    reg.Free;
  end;
end;

procedure AddProgramOnStart(ProgramName,FileName:string);
begin
  RegWriteString(HKEY_LOCAL_MACHINE, '\SoftWare\Microsoft\Windows\CurrentVersion\Run', ProgramName, FileName);
end;

procedure RemoveProgramOnStart(ProgramName:string);
var
  Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\SoftWare\Microsoft\Windows\CurrentVersion\Run', false);
    Reg.DeleteKey(ProgramName);
  finally
    Reg.Free;
  end;
end;

Function  ReadRegistry(Root:HKey; Path:String; CanCreate:Boolean):TRegistry;
Var
   stPath : String;
Begin
  Result:= TRegistry.Create;
  Result.RootKey:= Root;
  While Length(Path) > 0 do
    Begin
      stPath:= Copy(Path, 1, Pos('\', Path)-1);
      If Length(stPath) = 0 then stPath:= Path;
      Delete(Path, 1, Length(stPath)+1);
      Result.OpenKey(stPath, CanCreate);
    End;
End;

Procedure AutoRun(Ext,Name,Parameter:String);
Begin
  With ReadRegistry(HKEY_CLASSES_ROOT, '.'+Ext, True) do
    Begin
      WriteString('', Name);
      Free;
    End;
  With ReadRegistry(HKEY_CLASSES_ROOT, Name+'\shell\open\command', True) do
    Begin
      WriteString('', Parameter);
      Free;
    End;
End;

Function  GetOSLanguage:String;
Begin
  SetLength(Result, Max_Path);
  VerLanguageName(GetSystemDefaultLangID, PChar(Result), Max_Path);
  SetLength(Result, StrLen(PChar(Result)));
End;

procedure EnableWindowKey;
var
  Reg : TRegistry;
begin
//[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]
//"Scancode Map"=hex:

  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_CURRENT_USER;
   Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer', true);
   Reg.WriteInteger('NoWinKeys', 1);

   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer', true);
   Reg.WriteInteger('NoWinKeys', 1);
  finally
   Reg.Free;
  end;
end;

procedure DisableWindowKey;
var
  Reg : TRegistry;
//  Buffer : packed array [1..24] of byte;
begin
//[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]
//"Scancode Map"=hex:00,00,00,00,00,00,00,00,03,00,00,00,00,00,5B,E0,00,00,5C,E0,00,00,00,00
//                                           9                 15          19
//  FillChar(Buffer, SizeOf(Buffer), $00);
//  Buffer[09] := $03;
//  Buffer[15] := $5B;
//  Buffer[16] := $E0;
//  Buffer[19] := $5C;
//  Buffer[20] := $E0;

  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_CURRENT_USER;
   Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer', true);
   Reg.WriteInteger('NoWinKeys', 0);

   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer', true);
   Reg.WriteInteger('NoWinKeys', 0);
  finally
   Reg.Free;
  end;
end;

Procedure SetRebootKey(Reboot:Boolean);
Var
   OldValue : LongBool;
Begin
  If Reboot then SystemParametersInfo(97, Word(False), @OldValue, 0)
  Else SystemParametersInfo(97, Word(True), @OldValue, 0);
End;

Procedure ShowStartButton(Show:Boolean);
Var
   Rgn : hRgn;
Begin
  Rgn:= CreateRectRgn(0, 0, 0, 0);
  If Show then
     SetWindowRgn(FindWindowEx(FindWindow('Shell_TrayWnd', Nil), 0, 'Button', Nil), 0, True)
  Else
    SetWindowRgn(FindWindowEx(FindWindow('Shell_TrayWnd', Nil), 0, 'Button', Nil), Rgn, True);
End;

Procedure EnableStartButton(Enable:Boolean);
Begin
  EnableWindow(FindWindowEx(FindWindow('Shell_TrayWnd', Nil),
                                       0,
                                       'Button',
                                       Nil),
                                       Enable);
End;

Procedure ShowTakBar(Show:Boolean);
Var
   hTaskBar : THandle;
Begin
  hTaskbar:= FindWindow('Shell_TrayWnd', Nil);
  If Show then ShowWindow(hTaskBar, SW_SHOWNORMAL)
  Else ShowWindow(hTaskBar, SW_HIDE);
End;

Function  TurnScreenSaverOn:Boolean;
Var
   b : bool;
Begin
  Result:= False;
  If SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @b, 0) <> True then Exit;
  If not b then Exit;
  PostMessage(GetDesktopWindow, WM_SYSCOMMAND, SC_SCREENSAVE, 0);
  Result:= True;
End;

Procedure DeleteRecentDocs;
Begin
  SHAddToRecentDocs(SHARD_PATH, Nil);
End;

Procedure AddRecentDocs(FileName:String);
Begin
  SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
End;

Procedure RestartWindows;
Begin
  ExitWindows(EW_RESTARTWINDOWS, 0);
End;

Procedure RebootSystem;
Begin
  ExitWindows(EW_REBOOTSYSTEM, 0);
End;

Procedure ExitAndExecApp;
Begin
  ExitWindows(EW_EXITANDEXECAPP, 0);
End;

Procedure ShowDesktopIcons(Show:Boolean);
Begin
  SendMessage(FindWindow('Progman', 'Program Manager'),
              WM_SHOWWINDOW, Word(Show), SW_OTHERUNZOOM);
End;

Procedure EnableTaskSwitchs(Enable:Boolean);
Var
   iChild : Integer;
Begin
  iChild:= FindWindowEx(FindWindow('Shell_TrayWnd', Nil),
                        0, 'MSTaskSwWClass', Nil);
  EnableWindow(iChild, Enable);
End;

{$IFDEF CPUX86}
function  CPUSpeed:Double;
const
  DelayTime = 500;
var
  TimerHi, TimerLo : DWord;
  PriorityClass, Priority : Integer;
begin
  PriorityClass:= GetPriorityClass(GetCurrentProcess);
  Priority:= GetThreadPriority(GetCurrentThread);

  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);

  Sleep(10);
  Asm
    DW    310Fh // rdtsc
    MOV   TimerLo, EAX
    MOV   TimerHi, EDX
  End;
  Sleep(DelayTime);
  Asm
    DW   310Fh // rdtsc
    SUB  EAX, TimerLo
    SBB  EDX, TimerHi
    MOV  TimerLo, EAX
    MOV  TimerHi, EDX
  End;

  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);

  Result:= TimerLo / (1000.0 * DelayTime);
end;
{$ENDIF}

Function  ClassByName(AOwner:TComponent; ClassName:String):TComponent;
Var
   Loop : Integer;
Begin
  Result:= Nil;
  For Loop:= 0 to AOwner.ComponentCount-1 do
      If LowerCase(AOwner.Components[Loop].ClassName) = LowerCase(ClassName) then
         Begin
           Result:= AOwner.Components[Loop];
           Exit;
         End;
End;

Function  BringWindowFront(ClassName,WindowName:PChar):Boolean;
Var
   iHandle : Integer;
Begin
  iHandle:= FindWindow(ClassName, WindowName);
  Result:= iHandle <> 0;
  If iHandle > 0 then
     Begin
       ShowWindow(iHandle, SW_MINIMIZE);
       ShowWindow(iHandle, SW_RESTORE);
     End;
End;

Procedure CloseWindows(ClassName,WindowName:PChar);
Begin
  While FindWindow(ClassName, WindowName) > 0 do
    SendMessage(FindWindow(ClassName, WindowName), WM_Close, 0, 0);
End;

Procedure SendCopyData(hTo,hFrom:HWND; Text:String);
Var
  CopyData : TCopyDataStruct;
Begin
  Text:= Text + #0;

  With CopyData do Begin
    cbData:= Length(Text)+1;
    lpData:= pChar(Text);
  End;

  SendMessage(hTo, WM_COPYDATA, hFrom, LongInt(@CopyData));
End;

{$IFDEF CPUX86}
function CPUIDAvail : boolean; assembler;
{Thests wheter the CPUID instruction is available}
asm
  pushfd                // get flags into ax
  pop   eax             // save a copy on the stack
  mov   edx,eax
  xor   eax, 0200000h   // flip bit 21
  push  eax             // load new value into flags
  popfd                 // get them back
  pushfd
  pop   eax
  xor   eax,edx
  and   eax, 0200000h   // clear all but bit 21
  shr   eax, 21
end;
{$ENDIF}

{$IFDEF CPUX86}
function GetCPUBrandString : string;
const
  CPUID=$a20f;
var
  s:array[0..48] of AnsiChar;
begin
  fillchar(s, sizeof(s), 0);
  if CPUIDAvail then begin
    asm
      //save regs
      push ebx
      push ecx
      push edx
      //check if necessary extended CPUID calls are
      //supported, if not return null string
      mov  eax,080000000h
      {dw}   CPUID
      cmp  eax,080000004h
      jb   @@endbrandstr
      //get first name part
      mov  eax,080000002h
      {dw}   CPUID
      mov  longword(s[0]),eax
      mov  longword(s[4]),ebx
      mov  longword(s[8]),ecx
      mov  longword(s[12]),edx
      //get second name part
      mov  eax,080000003h
      {dw}   CPUID
      mov  longword(s[16]),eax
      mov  longword(s[20]),ebx
      mov  longword(s[24]),ecx
      mov  longword(s[28]),edx
      //get third name part
      mov  eax,080000004h
      {dw}   CPUID
      mov  longword(s[32]),eax
      mov  longword(s[36]),ebx
      mov  longword(s[40]),ecx
      mov  longword(s[44]),edx
    @@endbrandstr:
      //restore regs
      pop  edx
      pop  ecx
      pop  ebx
     end;
   end;

   Result := String(StrPas(s));
end;
{$ENDIF}

Procedure ChangeWallPaper(FileName:String; Tile:Boolean);
Var
   Reg : TRegIniFile;
Begin
  Reg:= TRegIniFile.Create('Control Panel');
  Reg.WriteString('Desktop', 'Wallpaper', PChar(FileName));
  If Tile then Reg.WriteString('Desktop', 'TileWallpaper', '1')
  Else Reg.WriteString('Desktop', 'TileWallpaper', '0');
  Reg.Free;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, Nil, SPIF_SENDWININICHANGE);
End;

procedure HardwareAcceleration(level: integer);
var
 Reg : TRegistry;
 reg_key : string;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    reg_key := '\HARDWARE\DEVICEMAP\VIDEO\';
    if reg.OpenKey(reg_key, false) then begin
      reg_key := reg.ReadString('\Device\Video0');
      reg_key := StringReplace(reg_key, '\Registry\Machine', '', [rfReplaceAll]);
      if reg.OpenKey(reg_key, false) then begin
        if level = 0 then reg.DeleteValue('Acceleration.Level')
        else reg.WriteInteger('Acceleration.Level', level);
      end;
    end;
  finally
    reg.CloseKey;
    reg.Destroy;
  end;
  ChangeDisplaySettings(TDevMode(Nil^), CDS_UPDATEREGISTRY);
end;

procedure AddContextMenu(AMenuName,ATitle,ACommand:string);
begin
  RegWriteString(
    HKEY_LOCAL_MACHINE,
    '\SOFTWARE\Classes\Folder\shell\' + AMenuName,
    '',
    ATitle
  );

  RegWriteString(
    HKEY_LOCAL_MACHINE,
    '\SOFTWARE\Classes\Folder\shell\' + AMenuName + '\command',
    '',
    ACommand
  );
end;

function LocalIP: string;
type
  TaPInAddr = array [0 .. 10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: Array [0 .. 63] of AnsiChar;
  I: Integer;
  GInitData: TWSAData;
begin
  Result := '';

  WSAStartup($101, GInitData);

  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then Exit;

  pptr := PaPInAddr(phe^.h_addr_list);
  I := 0;
  while pptr^[I] <> nil do begin
    Result := String(inet_ntoa(pptr^[I]^));
    if (Result <> '') and (Result <> '127.0.0.1') then Break;
    Inc(I);
  end;

  WSACleanup;
end;

function LocalIPs: string;
type
  TaPInAddr = array [0 .. 10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: Array [0 .. 63] of AnsiChar;
  I: Integer;
  GInitData: TWSAData;
begin
  Result := '';

  WSAStartup($101, GInitData);

  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then Exit;

  pptr := PaPInAddr(phe^.h_addr_list);
  I := 0;
  while pptr^[I] <> nil do begin
    Result := Result + String(inet_ntoa(pptr^[I]^)) + ';';
    Inc(I);
  end;

  WSACleanup;
end;

function TotalPhysicalSize:int64;
var
  MemoryStatus: TMemoryStatusEx;
begin
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatusEx(MemoryStatus);

  Result := MemoryStatus.ullTotalPhys;
end;

function FreePhysicalSize:NativeUInt;
var
  MemoryStatus : TMemoryStatus;
begin
  MemoryStatus.dwLength := SizeOf(MemoryStatus) ;
  GlobalMemoryStatus(MemoryStatus) ;
  Result := MemoryStatus.dwAvailPhys;
end;

function MyExitWindows(RebootParam: Longword): Boolean;
var
  TTokenHd: THandle;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    tpResult := OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TTokenHd);

    if tpResult then begin
      tpResult := LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, TTokenPvg.Privileges[0].Luid);
      TTokenPvg.PrivilegeCount := 1;
      TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      cbtpPrevious := SizeOf(rTTokenPvg);
      pcbtpPreviousRequired := 0;
      if tpResult then
        AdjustTokenPrivileges(TTokenHd, False, TTokenPvg, cbtpPrevious, rTTokenPvg, pcbtpPreviousRequired);
    end;
  end;

  Result := ExitWindowsEx(RebootParam, 0);
end;

function ForceForegroundWindow(hwnd: THandle): boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;
end;

initialization
  GetSystemInfo(SystemInfo);
end.
