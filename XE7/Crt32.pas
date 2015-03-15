unit Crt32;

{$Define NEW_STYLES}

{..$Define HARD_CRT}      {Redirect STD_...}
{..$Define CRT_EVENT}     {CTRL-C,...}
{$Define MOUSE_IS_USED}   {Handle mouse or not}
{..$Define OneByOne}      {Block or byte style write}

Interface
  {$IfDef Win32}
  Const
    { CRT modes of original CRT unit }
    BW40 = 0;     { 40x25 B/W on Color Adapter }
    CO40 = 1;     { 40x25 Color on Color Adapter }
    BW80 = 2;     { 80x25 B/W on Color Adapter }
    CO80 = 3;     { 80x25 Color on Color Adapter }
    Mono = 7;     { 80x25 on Monochrome Adapter }
    Font8x8 = 256;{ Add-in for ROM font }
    { Mode constants for 3.0 compatibility of original CRT unit }
    C40 = CO40;
    C80 = CO80;
    { Foreground and background color constants of original CRT unit }
    Black = 0;
    Blue = 1;
    Green = 2;
    Cyan = 3;
    Red = 4;
    Magenta = 5;
    Brown  = 6;
    LightGray = 7;
    { Foreground color constants of original CRT unit }
    DarkGray = 8;
    LightBlue = 9;
    LightGreen = 10;
    LightCyan = 11;
    LightRed = 12;
    LightMagenta = 13;
    Yellow = 14;
    White = 15;
    { Add-in for blinking of original CRT unit }
    Blink = 128;
    {  }
    {  New constans there are not in original CRT unit }
    {  }
    MouseLeftButton = 1;
    MouseRightButton = 2;
    MouseCenterButton = 4;

var
  { Interface variables of original CRT unit }
  CheckBreak: Boolean;    { Enable Ctrl-Break }
  CheckEOF: Boolean;      { Enable Ctrl-Z }
  DirectVideo: Boolean;   { Enable direct video addressing }
  CheckSnow: Boolean;     { Enable snow filtering }
  LastMode: Word;         { Current text mode }
  TextAttr: Byte;         { Current text attribute }
  WindMin: Word;          { Window upper left coordinates }
  WindMax: Word;          { Window lower right coordinates }
  {  }
  {  New variables there are not in original CRT unit }
  {  }
  MouseInstalled: boolean;
  MousePressedButtons: word;

{ Interface functions & procedures of original CRT unit }
procedure AssignCrt(var F: Text);
function KeyPressed: Boolean;
function ReadKey: AnsiChar;
procedure TextMode(Mode: Integer);
procedure Window(X1, Y1, X2, Y2: Byte);
procedure GotoXY(X, Y: Byte);
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure ClrEol;
procedure InsLine;
procedure DelLine;
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure LowVideo;
procedure HighVideo;
procedure NormVideo;
procedure Delay(MS: Word);
procedure Sound(Hz: Word);
procedure NoSound;
{ New functions & procedures there are not in original CRT unit }
procedure FillerScreen(FillChar: AnsiChar);
procedure FlushInputBuffer;
function GetCursor: Word;
procedure SetCursor(NewCursor: Word);
function MouseKeyPressed: Boolean;
procedure MouseGotoXY(X, Y: Integer);
function MouseWhereY: Integer;
function MouseWhereX: Integer;
procedure MouseShowCursor;
procedure MouseHideCursor;
{ These functions & procedures are for inside use only }
function MouseReset: Boolean;
procedure WriteChrXY(X, Y: Byte; Chr: AnsiChar);
procedure WriteStrXY(X, Y: Byte; Str: PAnsiChar; dwSize: Integer);
procedure OverwriteChrXY(X, Y: Byte; Chr: AnsiChar);
{$EndIf Win32}

implementation
{$IfDef Win32}

uses Windows, SysUtils;

type
  POpenText = ^TOpenText;
  TOpenText = function(var F: Text; Mode: Word): Integer; far;

var
  IsWinNT: boolean;
  PtrOpenText: POpenText;
  hConsoleInput: THandle;
  hConsoleOutput: THandle;
  ConsoleScreenRect: TSmallRect;
  StartAttr: word;
  LastX, LastY: byte;
  SoundDuration: integer;
  SoundFrequency: integer;
  OldCP: integer;
  MouseRowWidth, MouseColWidth: word;
  MousePosX, MousePosY: smallInt;
  MouseButtonPressed: boolean;
  MouseEventTime: TDateTime;
{  }
{  This function handles the Write and WriteLn commands }
{  }

function TextOut(var F: Text): Integer; far;
  {$IfDef OneByOne}
var
  dwSize: DWORD;
  {$EndIf}
begin
  with TTExtRec(F) do
  begin
    if BufPos > 0 then
    begin
      LastX := WhereX;
      LastY := WhereY;
      {$IfDef OneByOne}
      dwSize := 0;
      while (dwSize < BufPos) do
      begin
        WriteChrXY(LastX, LastY, BufPtr[dwSize]);
        Inc(dwSize);
      end;
      {$Else}
      WriteStrXY(LastX, LastY, BufPtr, BufPos);
      FillChar(BufPtr^, BufPos + 1, #0);
      {$EndIf}
      BufPos := 0;
    end;
  end;
  Result := 0;
end;
{  }
{  This function handles the exchanging of Input or Output }
{  }

function OpenText(var F: Text; Mode: Word): Integer; far;
var
  OpenResult: integer;
begin
  OpenResult := 102; { Text not assigned }
  if Assigned(PtrOpenText) then
  begin
    TTextRec(F).OpenFunc := PtrOpenText;
    OpenResult := PtrOpenText^(F, Mode);
    if OpenResult = 0 then
    begin
      if Mode = fmInput then
        hConsoleInput := TTextRec(F).Handle
      else
      begin
        hConsoleOutput := TTextRec(F).Handle;
        TTextRec(Output).InOutFunc := @TextOut;
        TTextRec(Output).FlushFunc := @TextOut;
      end;
    end;
  end;
  Result := OpenResult;
end;
{  }
{  Fills the current window with special character }
{  }

procedure FillerScreen(FillChar: AnsiChar);
var
  Coord: TCoord;
  dwSize, dwCount: DWORD;
  Y: integer;
begin
  Coord.X := ConsoleScreenRect.Left;
  dwSize := ConsoleScreenRect.Right - ConsoleScreenRect.Left + 1;
  for Y := ConsoleScreenRect.Top to ConsoleScreenRect.Bottom do
  begin
    Coord.Y := Y;
    FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
    FillConsoleOutputCharacter(hConsoleOutput, Char(FillChar), dwSize, Coord, dwCount);
  end;
  GotoXY(1,1);
end;
{  }
{  Write one character at the X,Y position }
{  }

procedure WriteChrXY(X, Y: Byte; Chr: AnsiChar);
var
  Coord: TCoord;
  dwSize, dwCount: DWORD;
begin
  LastX := X;
  LastY := Y;
  case Chr of
    #13: LastX := 1;
    #10:
      begin
        LastX := 1;
        Inc(LastY);
      end;
    else
      begin
        Coord.X := LastX - 1 + ConsoleScreenRect.Left;
        Coord.Y := LastY - 1 + ConsoleScreenRect.Top;
        dwSize := 1;
        FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
        FillConsoleOutputCharacterA(hConsoleOutput, Chr, dwSize, Coord, dwCount);
        Inc(LastX);
      end;
  end;
  if (LastX + ConsoleScreenRect.Left) > (ConsoleScreenRect.Right + 1) then
  begin
    LastX := 1;
    Inc(LastY);
  end;
  if (LastY + ConsoleScreenRect.Top) > (ConsoleScreenRect.Bottom + 1) then
  begin
    Dec(LastY);
    GotoXY(1,1);
    DelLine;
  end;
  GotoXY(LastX, LastY);
end;
{  }
{  Write string into the X,Y position }
{  }
(* !!! The WriteConsoleOutput does not write into the last line !!!
  Procedure WriteStrXY(X,Y: byte; Str: PAnsiChar; dwSize: integer );
  {$IfDef OneByOne}
    Var
      dwCount: integer;
  {$Else}
    Type
      PBuffer= ^TBuffer;
      TBUffer= packed array [0..16384] of TCharInfo;
    Var
      I: integer;
      dwCount: DWORD;
      WidthHeight,Coord: TCoord;
      hTempConsoleOutput: THandle;
      SecurityAttributes: TSecurityAttributes;
      Buffer: PBuffer;
      DestinationScreenRect,SourceScreenRect: TSmallRect;
  {$EndIf}
  Begin
    If dwSize>0 Then Begin
      {$IfDef OneByOne}
        LastX:=X;
        LastY:=Y;
        dwCount:=0;
        While dwCount < dwSize Do Begin
          WriteChrXY(LastX,LastY,Str[dwCount]);
          Inc(dwCount);
        End;
      {$Else}
        SecurityAttributes.nLength:=SizeOf(SecurityAttributes)-SizeOf(DWORD);
        SecurityAttributes.lpSecurityDescriptor:=NIL;
        SecurityAttributes.bInheritHandle:=TRUE;
        hTempConsoleOutput:=CreateConsoleScreenBuffer(
         GENERIC_READ OR GENERIC_WRITE,
         FILE_SHARE_READ OR FILE_SHARE_WRITE,
         @SecurityAttributes,
         CONSOLE_TEXTMODE_BUFFER,
         NIL
        );
        If dwSize<=(ConsoleScreenRect.Right-ConsoleScreenRect.Left+1) Then Begin
          WidthHeight.X:=dwSize;
          WidthHeight.Y:=1;
        End Else Begin
          WidthHeight.X:=ConsoleScreenRect.Right-ConsoleScreenRect.Left+1;
          WidthHeight.Y:=dwSize DIV WidthHeight.X;
          If (dwSize MOD WidthHeight.X) > 0 Then Inc(WidthHeight.Y);
        End;
        SetConsoleScreenBufferSize(hTempConsoleOutput,WidthHeight);
        DestinationScreenRect.Left:=0;
        DestinationScreenRect.Top:=0;
        DestinationScreenRect.Right:=WidthHeight.X-1;
        DestinationScreenRect.Bottom:=WidthHeight.Y-1;
        SetConsoleWindowInfo(hTempConsoleOutput,FALSE,DestinationScreenRect);
        Coord.X:=0;
        For I:=1 To WidthHeight.Y Do Begin
          Coord.Y:=I-0;
          FillConsoleOutputAttribute(hTempConsoleOutput,TextAttr,WidthHeight.X,Coord,dwCount);
          FillConsoleOutputCharacter(hTempConsoleOutput,' '     ,WidthHeight.X,Coord,dwCount);
        End;
        WriteConsoleA(hTempConsoleOutput,Str,dwSize,dwCount,NIL);
        {  }
        New(Buffer);
        Coord.X:= 0;
        Coord.Y:= 0;
        SourceScreenRect.Left:=0;
        SourceScreenRect.Top:=0;
        SourceScreenRect.Right:=WidthHeight.X-1;
        SourceScreenRect.Bottom:=WidthHeight.Y-1;
        ReadConsoleOutputA(hTempConsoleOutput,Buffer,WidthHeight,Coord,SourceScreenRect);
        Coord.X:=X-1;
        Coord.Y:=Y-1;
        DestinationScreenRect:=ConsoleScreenRect;
        WriteConsoleOutputA(hConsoleOutput,Buffer,WidthHeight,Coord,DestinationScreenRect);
        GotoXY((dwSize MOD WidthHeight.X)-1,WidthHeight.Y+1);
        Dispose(Buffer);
        {  }
        CloseHandle(hTempConsoleOutput);
      {$EndIf}
    End;
  End;
*)

procedure WriteStrXY(X, Y: Byte; Str: PAnsiChar; dwSize: Integer);
  {$IfDef OneByOne}
var
  dwCount: integer;
  {$Else}
var
  I: integer;
  LineSize, dwCharCount, dwCount, dwWait: DWORD;
  WidthHeight: TCoord;
  OneLine: packed array [0..131] of AnsiChar;
  Line, TempStr: PAnsiChar;

  procedure NewLine;
  begin
    LastX := 1;
    Inc(LastY);
    if (LastY + ConsoleScreenRect.Top) > (ConsoleScreenRect.Bottom + 1) then
    begin
      Dec(LastY);
      GotoXY(1,1);
      DelLine;
    end;
    GotoXY(LastX, LastY);
  end;

  {$EndIf}
begin
  if dwSize > 0 then
  begin
    {$IfDef OneByOne}
    LastX := X;
    LastY := Y;
    dwCount := 0;
    while dwCount < dwSize do
    begin
      WriteChrXY(LastX, LastY, Str[dwCount]);
      Inc(dwCount);
    end;
    {$Else}
    LastX := X;
    LastY := Y;
    GotoXY(LastX, LastY);
    dwWait  := dwSize;
    TempStr := Str;
    while (dwWait > 0) and (Pos(#13#10, StrPas(TempStr)) = 1) do
    begin
      Dec(dwWait, 2);
      Inc(TempStr, 2);
      NewLine;
    end;
    while (dwWait > 0) and (Pos(#10, StrPas(TempStr)) = 1) do
    begin
      Dec(dwWait);
      Inc(TempStr);
      NewLine;
    end;
    if dwWait > 0 then
    begin
      if dwSize <= (ConsoleScreenRect.Right - ConsoleScreenRect.Left - LastX + 1) then
      begin
        WidthHeight.X := dwSize + LastX - 1;
        WidthHeight.Y := 1;
      end
      else
      begin
        WidthHeight.X := ConsoleScreenRect.Right - ConsoleScreenRect.Left + 1;
        WidthHeight.Y := dwSize div WidthHeight.X;
        if (dwSize mod WidthHeight.X) > 0 then Inc(WidthHeight.Y);
      end;
      for I := 1 to WidthHeight.Y do
      begin
        FillChar(OneLine, SizeOf(OneLine), #0);
        Line := @OneLine;
        LineSize := WidthHeight.X - LastX + 1;
        if LineSize > dwWait then LineSize := dwWait;
        Dec(dwWait, LineSize);
        StrLCopy(Line, TempStr, LineSize);
        Inc(TempStr, LineSize);
        dwCharCount := Pos(#13#10, StrPas(Line));
        if dwCharCount > 0 then
        begin
          OneLine[dwCharCount - 1] := #0;
          OneLine[dwCharCount]     := #0;
          WriteConsoleA(hConsoleOutput, Line, dwCharCount - 1,dwCount, nil);
          Inc(Line, dwCharCount + 1);
          NewLine;
          LineSize := LineSize - (dwCharCount + 1);
        end
        else
        begin
          dwCharCount := Pos(#10, StrPas(Line));
          if dwCharCount > 0 then
          begin
            OneLine[dwCharCount - 1] := #0;
            WriteConsoleA(hConsoleOutput, Line, dwCharCount - 1,dwCount, nil);
            Inc(Line, dwCharCount);
            NewLine;
            LineSize := LineSize - dwCharCount;
          end;
        end;
        if LineSize <> 0 then
        begin
          WriteConsoleA(hConsoleOutput, Line, LineSize, dwCount, nil);
        end;
        if dwWait > 0 then
        begin
          NewLine;
        end;
      end;
    end;
    {$EndIf}
  end;
end;
{  }
{  Empty the buffer }
{  }

procedure FlushInputBuffer;
begin
  FlushConsoleInputBuffer(hConsoleInput);
end;
{  }
{  Get size of current cursor }
{  }

function GetCursor: Word;
var
  CCI: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(hConsoleOutput, CCI);
  GetCursor := CCI.dwSize;
end;
{  }
{  Set size of current cursor }
{  }

procedure SetCursor(NewCursor: Word);
var
  CCI: TConsoleCursorInfo;
begin
  if NewCursor = $0000 then
  begin
    CCI.dwSize := GetCursor;
    CCI.bVisible := False;
  end
  else
  begin
    CCI.dwSize := NewCursor;
    CCI.bVisible := True;
  end;
  SetConsoleCursorInfo(hConsoleOutput, CCI);
end;
{  }
{ --- Begin of Interface functions & procedures of original CRT unit --- }

procedure AssignCrt(var F: Text);
begin
  Assign(F, '');
  TTextRec(F).OpenFunc := @OpenText;
end;

function KeyPressed: Boolean;
var
  NumberOfEvents: DWORD;
  NumRead: DWORD;
  InputRec: TInputRecord;
  Pressed: boolean;
begin
  Pressed := False;
  GetNumberOfConsoleInputEvents(hConsoleInput, NumberOfEvents);
  if NumberOfEvents > 0 then
  begin
    if PeekConsoleInput(hConsoleInput, InputRec, 1,NumRead) then
    begin
      if (InputRec.EventType = KEY_EVENT) and
        (InputRec{$IfDef NEW_STYLES}.Event{$EndIf}.KeyEvent.bKeyDown) then
      begin
        Pressed := True;
        {$IfDef MOUSE_IS_USED}
        MouseButtonPressed := False;
        {$EndIf}
      end
      else
      begin
        {$IfDef MOUSE_IS_USED}
        if (InputRec.EventType = _MOUSE_EVENT) then
        begin
          with InputRec{$IfDef NEW_STYLES}.Event{$EndIf}.MouseEvent do
          begin
            MousePosX := dwMousePosition.X;
            MousePosY := dwMousePosition.Y;
            if dwButtonState = FROM_LEFT_1ST_BUTTON_PRESSED then
            begin
              MouseEventTime := Now;
              MouseButtonPressed := True;
              {If (dwEventFlags AND DOUBLE_CLICK)<>0 Then Begin}
              {End;}
            end;
          end;
        end;
        ReadConsoleInput(hConsoleInput, InputRec, 1,NumRead);
        {$Else}
        ReadConsoleInput(hConsoleInput, InputRec, 1,NumRead);
        {$EndIf}
      end;
    end;
  end;
  Result := Pressed;
end;

function ReadKey: AnsiChar;
var
  NumRead: DWORD;
  InputRec: TInputRecord;
begin
  repeat
    repeat
    until KeyPressed;
    ReadConsoleInput(hConsoleInput, InputRec, 1,NumRead);
  until InputRec{$IfDef NEW_STYLES}.Event{$EndIf}.KeyEvent.AsciiChar > #0;
  Result := InputRec{$IfDef NEW_STYLES}.Event{$EndIf}.KeyEvent.AsciiChar;
end;

procedure TextMode(Mode: Integer);
begin
end;

procedure Window(X1, Y1, X2, Y2: Byte);
begin
  ConsoleScreenRect.Left := X1 - 1;
  ConsoleScreenRect.Top := Y1 - 1;
  ConsoleScreenRect.Right := X2 - 1;
  ConsoleScreenRect.Bottom := Y2 - 1;
  WindMin := (ConsoleScreenRect.Top shl 8) or ConsoleScreenRect.Left;
  WindMax := (ConsoleScreenRect.Bottom shl 8) or ConsoleScreenRect.Right;
  {$IfDef WindowFrameToo}
  SetConsoleWindowInfo(hConsoleOutput, True, ConsoleScreenRect);
  {$EndIf}
  GotoXY(1,1);
end;

procedure GotoXY(X, Y: Byte);
var
  Coord: TCoord;
begin
  Coord.X := X - 1 + ConsoleScreenRect.Left;
  Coord.Y := Y - 1 + ConsoleScreenRect.Top;
  if not SetConsoleCursorPosition(hConsoleOutput, Coord) then
  begin
    GotoXY(1, 1);
    DelLine;
  end;
end;

function WhereX: Byte;
var
  CBI: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(hConsoleOutput, CBI);
  Result := TCoord(CBI.dwCursorPosition).X + 1 - ConsoleScreenRect.Left;
end;

function WhereY: Byte;
var
  CBI: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(hConsoleOutput, CBI);
  Result := TCoord(CBI.dwCursorPosition).Y + 1 - ConsoleScreenRect.Top;
end;

procedure ClrScr;
begin
  FillerScreen(' ');
end;

procedure ClrEol;
var
  Coord: TCoord;
  dwSize, dwCount: DWORD;
begin
  Coord.X := WhereX - 1 + ConsoleScreenRect.Left;
  Coord.Y := WhereY - 1 + ConsoleScreenRect.Top;
  dwSize  := ConsoleScreenRect.Right - Coord.X + 1;
  FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
  FillConsoleOutputCharacter(hConsoleOutput, ' ', dwSize, Coord, dwCount);
end;

procedure InsLine;
var
  SourceScreenRect: TSmallRect;
  Coord: TCoord;
  CI: TCharInfo;
  dwSize, dwCount: DWORD;
begin
  SourceScreenRect := ConsoleScreenRect;
  SourceScreenRect.Top := WhereY - 1 + ConsoleScreenRect.Top;
  SourceScreenRect.Bottom := ConsoleScreenRect.Bottom - 1;
  CI.AsciiChar := ' ';
  CI.Attributes := TextAttr;
  Coord.X := SourceScreenRect.Left;
  Coord.Y := SourceScreenRect.Top + 1;
  dwSize := SourceScreenRect.Right - SourceScreenRect.Left + 1;
  ScrollConsoleScreenBuffer(hConsoleOutput, SourceScreenRect, nil, Coord, CI);
  Dec(Coord.Y);
  FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
end;

procedure DelLine;
var
  SourceScreenRect: TSmallRect;
  Coord: TCoord;
  CI: TCharinfo;
  dwSize, dwCount: DWORD;
begin
  SourceScreenRect := ConsoleScreenRect;
  SourceScreenRect.Top := WhereY + ConsoleScreenRect.Top;
  CI.AsciiChar := ' ';
  CI.Attributes := TextAttr;
  Coord.X := SourceScreenRect.Left;
  Coord.Y := SourceScreenRect.Top - 1;
  dwSize := SourceScreenRect.Right - SourceScreenRect.Left + 1;
  ScrollConsoleScreenBuffer(hConsoleOutput, SourceScreenRect, nil, Coord, CI);
  FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
end;

procedure TextColor(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (Color and $0F) or (TextAttr and $F0);
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
end;

procedure TextBackground(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (Color shl 4) or (TextAttr and $0F);
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
end;

procedure LowVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr and $F7;
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
end;

procedure HighVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr or $08;
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
end;

procedure NormVideo;
begin
  LastMode := TextAttr;
  TextAttr := StartAttr;
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
end;

procedure Delay(MS: Word);
  {
  Const
    Magic= $80000000;
  var
   StartMS,CurMS,DeltaMS: DWORD;
   }
begin
  Windows.SleepEx(MS, False);  // Windows.Sleep(MS);
    {
    StartMS:= GetTickCount;
    Repeat
      CurMS:= GetTickCount;
      If CurMS >= StartMS Then
         DeltaMS:= CurMS - StartMS
      Else DeltaMS := (CurMS + Magic) - (StartMS - Magic);
    Until MS<deltams ;
    }
end;

procedure Sound(Hz: Word);
begin
  {SetSoundIOPermissionMap(LocalIOPermission_ON);}
  SoundFrequency := Hz;
  if IsWinNT then
  begin
    Windows.Beep(SoundFrequency, SoundDuration)
  end
  else
  begin
    asm
        mov  BX,Hz
        cmp  BX,0
        jz   @2
        mov  AX,$34DD
        mov  DX,$0012
        cmp  DX,BX
        jnb  @2
        div  BX
        mov  BX,AX
        { Sound is On ? }
        in   Al,$61
        test Al,$03
        jnz  @1
        { Set Sound On }
        or   Al,03
        out  $61,Al
        { Timer Command }
        mov  Al,$B6
        out  $43,Al
        { Set Frequency }
    @1: mov  Al,Bl
        out  $42,Al
        mov  Al,Bh
        out  $42,Al
    @2:
    end;
  end;
end;

procedure NoSound;
begin
  if IsWinNT then
  begin
    Windows.Beep(SoundFrequency, 0);
  end
  else
  begin
      asm
        { Set Sound On }
        in   Al,$61
        and  Al,$FC
        out  $61,Al
      end;
  end;
  {SetSoundIOPermissionMap(LocalIOPermission_OFF);}
end;
{ --- End of Interface functions & procedures of original CRT unit --- }
{  }

procedure OverwriteChrXY(X, Y: Byte; Chr: AnsiChar);
var
  Coord: TCoord;
  dwSize, dwCount: DWORD;
begin
  LastX := X;
  LastY := Y;
  Coord.X := LastX - 1 + ConsoleScreenRect.Left;
  Coord.Y := LastY - 1 + ConsoleScreenRect.Top;
  dwSize := 1;
  FillConsoleOutputAttribute(hConsoleOutput, TextAttr, dwSize, Coord, dwCount);
  FillConsoleOutputCharacterA(hConsoleOutput, Chr, dwSize, Coord, dwCount);
  GotoXY(LastX, LastY);
end;

{  --------------------------------------------------  }
{  Console Event Handler }
{  }
{$IfDef CRT_EVENT}
function ConsoleEventProc(CtrlType: DWORD): Bool; stdcall; far;
var
  S: ShortString;
  Message: PAnsiChar;
begin
  case CtrlType of
    CTRL_C_EVENT: S        := 'CTRL_C_EVENT';
    CTRL_BREAK_EVENT: S    := 'CTRL_BREAK_EVENT';
    CTRL_CLOSE_EVENT: S    := 'CTRL_CLOSE_EVENT';
    CTRL_LOGOFF_EVENT: S   := 'CTRL_LOGOFF_EVENT';
    CTRL_SHUTDOWN_EVENT: S := 'CTRL_SHUTDOWN_EVENT';
    else
      S := 'UNKNOWN_EVENT';
  end;
  S := S + ' detected, but not handled.';
  Message := @S;
  Inc(Message);
  MessageBox(0, Message, 'Win32 Console', MB_OK);
  Result := True;
end;
  {$EndIf}

function MouseReset: Boolean;
begin
  MouseColWidth := 1;
  MouseRowWidth := 1;
  Result := True;
end;

procedure MouseShowCursor;
const
  ShowMouseConsoleMode = ENABLE_MOUSE_INPUT;
var
  cMode: DWORD;
begin
  GetConsoleMode(hConsoleInput, cMode);
  if (cMode and ShowMouseConsoleMode) <> ShowMouseConsoleMode then
  begin
    cMode := cMode or ShowMouseConsoleMode;
    SetConsoleMode(hConsoleInput, cMode);
  end;
end;

procedure MouseHideCursor;
const
  ShowMouseConsoleMode = ENABLE_MOUSE_INPUT;
var
  cMode: DWORD;
begin
  GetConsoleMode(hConsoleInput, cMode);
  if (cMode and ShowMouseConsoleMode) = ShowMouseConsoleMode then
  begin
    cMode := cMode and ($FFFFFFFF xor ShowMouseConsoleMode);
    SetConsoleMode(hConsoleInput, cMode);
  end;
end;

function MouseKeyPressed: Boolean;
  {$IfDef MOUSE_IS_USED}
const
  MouseDeltaTime = 200;
var
  ActualTime: TDateTime;
  HourA, HourM, MinA, MinM, SecA, SecM, MSecA, MSecM: word;
  MSecTimeA, MSecTimeM: longInt;
  MSecDelta: longInt;
  {$EndIf}
begin
  MousePressedButtons := 0;
  {$IfDef MOUSE_IS_USED}
  Result := False;
  if MouseButtonPressed then
  begin
    ActualTime := NOW;
    DecodeTime(ActualTime, HourA, MinA, SecA, MSecA);
    DecodeTime(MouseEventTime, HourM, MinM, SecM, MSecM);
    MSecTimeA := (3600 * HourA + 60 * MinA + SecA) * 100 + MSecA;
    MSecTimeM := (3600 * HourM + 60 * MinM + SecM) * 100 + MSecM;
    MSecDelta := Abs(MSecTimeM - MSecTimeA);
    if (MSecDelta < MouseDeltaTime) or (MSecDelta > (8784000 - MouseDeltaTime)) then
    begin
      MousePressedButtons := MouseLeftButton;
      MouseButtonPressed := False;
      Result := True;
    end;
  end;
  {$Else}
  Result := False;
  {$EndIf}
end;

procedure MouseGotoXY(X, Y: Integer);
begin
  {$IfDef MOUSE_IS_USED}
  mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE,
    X - 1,Y - 1,WHEEL_DELTA, GetMessageExtraInfo());
  MousePosY := (Y - 1) * MouseRowWidth;
  MousePosX := (X - 1) * MouseColWidth;
  {$EndIf}
end;

function MouseWhereY: Integer;
  {$IfDef MOUSE_IS_USED}
    {Var
      lppt, lpptBuf: TMouseMovePoint;}
  {$EndIf}
begin
  {$IfDef MOUSE_IS_USED}
      {GetMouseMovePoints(
        SizeOf(TMouseMovePoint), lppt, lpptBuf,
        7,GMMP_USE_DRIVER_POINTS
      );
      Result:=lpptBuf.Y DIV MouseRowWidth;}
  Result := (MousePosY div MouseRowWidth) + 1;
  {$Else}
  Result := -1;
  {$EndIf}
end;

function MouseWhereX: Integer;
  {$IfDef MOUSE_IS_USED}
    {Var
      lppt, lpptBuf: TMouseMovePoint;}
  {$EndIf}
begin
  {$IfDef MOUSE_IS_USED}
      {GetMouseMovePoints(
        SizeOf(TMouseMovePoint), lppt, lpptBuf,
        7,GMMP_USE_DRIVER_POINTS
      );
      Result:=lpptBuf.X DIV MouseColWidth;}
  Result := (MousePosX div MouseColWidth) + 1;
  {$Else}
  Result := -1;
  {$EndIf}
end;
  {  }

procedure Init;
const
  ExtInpConsoleMode = ENABLE_WINDOW_INPUT or ENABLE_PROCESSED_INPUT or ENABLE_MOUSE_INPUT;
  ExtOutConsoleMode = ENABLE_PROCESSED_OUTPUT or ENABLE_WRAP_AT_EOL_OUTPUT;
var
  cMode: DWORD;
  Coord: TCoord;
  OSVersion: TOSVersionInfo;
  CBI: TConsoleScreenBufferInfo;
begin
  OSVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(OSVersion);
  if OSVersion.dwPlatformId = VER_PLATFORM_WIN32_NT then
    IsWinNT := True
  else
    IsWinNT := False;
  PtrOpenText := TTextRec(Output).OpenFunc;
  {$IfDef HARD_CRT}
  AllocConsole;
  Reset(Input);
  hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
  TTextRec(Input).Handle := hConsoleInput;
  ReWrite(Output);
  hConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  TTextRec(Output).Handle := hConsoleOutput;
  {$Else}
  Reset(Input);
  hConsoleInput := TTextRec(Input).Handle;
  ReWrite(Output);
  hConsoleOutput := TTextRec(Output).Handle;
  {$EndIf}
  GetConsoleMode(hConsoleInput, cMode);
  if (cMode and ExtInpConsoleMode) <> ExtInpConsoleMode then
  begin
    cMode := cMode or ExtInpConsoleMode;
    SetConsoleMode(hConsoleInput, cMode);
  end;

  TTextRec(Output).InOutFunc := @TextOut;
  TTextRec(Output).FlushFunc := @TextOut;
  GetConsoleScreenBufferInfo(hConsoleOutput, CBI);
  GetConsoleMode(hConsoleOutput, cMode);
  if (cMode and ExtOutConsoleMode) <> ExtOutConsoleMode then
  begin
    cMode := cMode or ExtOutConsoleMode;
    SetConsoleMode(hConsoleOutput, cMode);
  end;
  TextAttr  := CBI.wAttributes;
  StartAttr := CBI.wAttributes;
  LastMode  := CBI.wAttributes;

  Coord.X := CBI.srWindow.Left;
  Coord.Y := CBI.srWindow.Top;
  WindMin := (Coord.Y shl 8) or Coord.X;
  Coord.X := CBI.srWindow.Right;
  Coord.Y := CBI.srWindow.Bottom;
  WindMax := (Coord.Y shl 8) or Coord.X;
  ConsoleScreenRect := CBI.srWindow;

  SoundDuration := -1;
  OldCp := GetConsoleOutputCP;
  SetConsoleOutputCP(1250);
  {$IfDef CRT_EVENT}
  SetConsoleCtrlHandler(@ConsoleEventProc, True);
  {$EndIf}
  {$IfDef MOUSE_IS_USED}
  SetCapture(hConsoleInput);
  KeyPressed;
  {$EndIf}
  MouseInstalled := MouseReset;
  Window(1,1,80,25);
  ClrScr;
end;

{  }

procedure Done;
begin
  {$IfDef CRT_EVENT}
  SetConsoleCtrlHandler(@ConsoleEventProc, False);
  {$EndIf}
  SetConsoleOutputCP(OldCP);
  TextAttr := StartAttr;
  SetConsoleTextAttribute(hConsoleOutput, TextAttr);
  ClrScr;
  FlushInputBuffer;
  {$IfDef HARD_CRT}
  TTextRec(Input).Mode := fmClosed;
  TTextRec(Output).Mode := fmClosed;
  FreeConsole;
  {$Else}
  Close(Input);
  Close(Output);
  {$EndIf}
end;

initialization
  Init;

finalization
  Done;
  {$Endif win32}
end.

