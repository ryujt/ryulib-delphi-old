{ *********************************************************************** }
{                                                                         }
{ Delphi Runtime Library                                                  }
{ Crt Interface Unit                                                      }
{                                                                         }
{ Copyright (c) 1988-2003 Borland Software Corporation                    }
{                                                                         }
{ *********************************************************************** }

unit CRT;

{$I CRT.inc}

{$IFDEF VER140}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

interface

uses
  SysUtils, Windows;

const

{ CRT modes }

  BW40          = 0;                        { 40x25 B/W on Color Adapter }
  CO40          = 1;                        { 40x25 Color on Color Adapter }
  BW80          = 2;                        { 80x25 B/W on Color Adapter }
  CO80          = 3;                        { 80x25 Color on Color Adapter }
  Mono          = 7;                        { 80x25 on Monochrome Adapter }
  C4350         = 64;
  Font8x8       = 256 {$IFDEF VER150} deprecated {$ENDIF};
                                            { Add-in for ROM font }

  C40X14        = 8;
  C40X21        = 9;
  C40X28        = 10;
  C40X43        = 11;
  C40X50        = 12;
  C40X60        = 13;

  C80X14        = 14;
  C80X21        = 15;
  C80X28        = 16;
  C80X43        = 17;
  C80X50        = 18;
  C80X60        = 19;

{ New Black & White modes }

  BW40X14       = 20;
  BW40X21       = 21;
  BW40X28       = 22;
  BW40X43       = 23;
  BW40X50       = 24;
  BW40X60       = 25;

  BW80X14       = 26;
  BW80X21       = 27;
  BW80X28       = 28;
  BW80X43       = 29;
  BW80X50       = 30;
  BW80X60       = 31;

{ New Monochrome modes }

  MONO14        = 32;                       { Invalid VGA mode }
  MONO21        = 33;
  MONO28        = 34;
  MONO43        = 35;
  MONO50        = 36;
  MONO60        = 37;

  _ORIGMODE     = 65;                       { Original mode at program startup }

{ Mode constants for 3.0 compatibility }

  C40           = CO40;
  C80           = CO80;

{ Foreground and background color constants }

  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }

  Blink         = 128 {$IFDEF VER150} deprecated {$ENDIF};
                                            { Does not blink in Win32, instead
                                              it turns on high intensity back-
                                              ground colors. }
  HighIntensity = 128;                      { Use this instead of Blink if you
                                              intentionally want high intensity
                                              background colors. }

{ SetCursorMode constants }

  _CUSTOM       = -1;
  _NOCURSOR     = 0;
  _SOLIDCURSOR  = 1;
  _NORMALCURSOR = 2;
  _SMALLCURSOR  = 3;

{ Legacy Crt variables (now read-only) }

  CheckEOF: boolean = False {$IFDEF VER150} deprecated {$ENDIF};
                                            { Enable Ctrl-Z }

type
{$IFNDEF VER120}
  longword = longint;
{$ENDIF}
  { TCrtEventProc -

      Used to intercept events and potentially override the default behavior
      used by the Crt unit.  The events handled are those defined with
      CTRL_xxx_EVENT as their name.  If you install an event procedure you
      should return True if the event was handled by your code, and False if it
      was not.  The event procedure is installed by creating a function of the
      type defined below (with any name), then assigning that implementation to
      CrtEventProc. }
  TCrtEventProc = function(CtrlEvent: longword): boolean;

var

{ Interface variables }

{$IFNDEF NOCRTEXTENSIONS}
  BinaryWrite: boolean = False;             { Enable raw writes }
  CrtEventProc: TCrtEventProc = nil;        { Allows customized handling of
                                              console events }
{$ENDIF}
  CheckBreak: boolean = True;               { Enable Ctrl-Break }
  CheckSnow: boolean {$IFDEF VER150} deprecated {$ENDIF};
                                            { Enable snow filtering }
  DirectVideo: boolean {$IFDEF VER150} deprecated {$ENDIF};
                                            { Enable direct video addressing }
  LastMode: word;                           { Current text mode }
  TextAttr: byte;                           { Current text attribute }
  WindMin: longword;                        { Window upper left coordinates }
  WindMax: longword;                        { Window lower right coordinates }

{ Standard Crt Functions }

procedure AssignCrt(var f: Text);
procedure ClrEol;
procedure ClrScr;
procedure Delay(MS: Cardinal);
procedure DelLine;
procedure GotoXY(X, Y: integer);
function  GetExtendedKey:AnsiChar;
procedure HighVideo;
procedure InsLine;
function  KeyPressed: boolean;
procedure LowVideo;
procedure NormVideo;
procedure NoSound;
function  ReadKey: AnsiChar;
procedure Sound(Hz:cardinal; Duration:DWord);
procedure TextBackground(Color: integer);
procedure TextColor(Color: integer);
procedure TextMode(Mode: integer);
function  WhereX: integer;
function  WhereY: integer;
procedure Window(X1, Y1, X2, Y2: integer);

{ Extended Crt Functions }

{$IFNDEF NOCRTEXTENSIONS}

const
  { For use with GetKeyStatus }

  RIGHT_ALT_PRESSED     = word($0001);
  LEFT_ALT_PRESSED      = word($0002);
  ALT_PRESSED           = RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED;
  RIGHT_CTRL_PRESSED    = word($0004);
  LEFT_CTRL_PRESSED     = word($0008);
  CTRL_PRESSED          = RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED;
  SHIFT_PRESSED         = word($0010);
  NUMLOCK_ON            = word($0020);
  SCROLLLOCK_ON         = word($0040);
  CAPSLOCK_ON           = word($0080);
  ENHANCED_KEY          = word($0100);

  { For use with CrtEventProc implementations (CTRL_xxxx) }

  CTRL_C_EVENT          = 0;
  CTRL_BREAK_EVENT      = 1;
  CTRL_CLOSE_EVENT      = 2;
  CTRL_LOGOFF_EVENT     = 5;
  CTRL_SHUTDOWN_EVENT   = 6;

type
  { TCrtSaveStates -

    Allows for selective state saving with SaveCrtState.  For example,
    [csCursor, csTextAttr] only saves the current position of the cursor and
    the current value of TextAttr, while [csScreen, csCursor, csTexAttr] saves
    all that, plus the current screen contents. In all cases, the same amount
    of memory is used, except with csScreen. csScreen allocates additional
    memory to store the current screen contents (text and color). }

  TCrtSaveStates = set of (csScreen, csCursorPos, csCursor, csTextAttr, csMode, csWindow, csVars);

function  FreeCrtState(var SaveState: pointer): boolean;
function  GetControlKeyState: longword;
function  IsAltPressed: boolean;
function  IsCtrlPressed: boolean;
function  IsShiftPressed: boolean;
function  RestoreCrtState(var SaveState: pointer): boolean;
function  SaveCrtState(StatesToSave: TCrtSaveStates): pointer;
function  SetScreenSize(Width, Height: integer): boolean;
procedure SetCursorType(Cursor: integer; Custom: longword{$IFDEF VER120} = 15{$ENDIF}; Visible: boolean{$IFDEF VER120} = True{$ENDIF});
function  ScreenX: integer;
function  ScreenY: integer;
function  ScreenHeight: integer;
function  ScreenWidth: integer;

{$ENDIF}

type
  { TWindCoord -

    Used with WindMin and WindMax to grab the X and Y coordinates stored inside
    the 32-bit quantity.  e.g.: TWindCoord(WindMin).X is the left column, while
    TWindCoord(WindMax).X is the right column. }

  TWindCoord = packed record
    case integer of
      0: (Value: longword);
      1: (X, Y: word);
  end;

implementation

{$IFNDEF VER120}
  { Types that are either incorrectly defined in Windows.pas in pre-Delphi 4,
    or that aren't defined at all. }

type
{$IFNDEF NOCRTEXTENSIONS}
  OSVERSIONINFO = TOSVersionInfo;
{$ENDIF}
  CONSOLE_CURSOR_INFO = TConsoleCursorInfo;
  CONSOLE_SCREEN_BUFFER_INFO = TConsoleScreenBufferInfo;
  COORD = TCoord;
  SMALL_RECT = TSmallRect;

  CHAR_INFO = record
    case Integer of
      0: (
        UnicodeChar: WCHAR;
        Attributes: Word);
      1: (
        AsciiChar: Char);
  end;

  KEY_EVENT_RECORD = packed record
    bKeyDown: BOOL;
    wRepeatCount: Word;
    wVirtualKeyCode: Word;
    wVirtualScanCode: Word;
    case Integer of
      0: (
        UnicodeChar: WCHAR;
        dwControlKeyState: DWORD);
      1: (
        AsciiChar: Char);
  end;
  TKeyEventRecord = KEY_EVENT_RECORD;

  INPUT_RECORD = record
    EventType: Word;
    Reserved: Word;
    Event: Record case Integer of
      0: (KeyEvent: TKeyEventRecord);
      1: (MouseEvent: TMouseEventRecord);
      2: (WindowBufferSizeEvent: TWindowBufferSizeRecord);
      3: (MenuEvent: TMenuEventRecord);
      4: (FocusEvent: TFocusEventRecord);
    end;
  end;

function  PeekConsoleInput(hConsoleInput: THandle; var lpBuffer: INPUT_RECORD; nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'PeekConsoleInputA';
function  ReadConsoleInput(hConsoleInput: THandle; var lpBuffer: INPUT_RECORD; nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'ReadConsoleInputA';
function  ScrollConsoleScreenBuffer(hConsoleOutput: THandle; const lpScrollRectangle: TSmallRect; lpClipRectangle: PSmallRect; dwDestinationOrigin: TCoord; var lpFill: CHAR_INFO): BOOL; stdcall;
  external 'kernel32.dll' name 'ScrollConsoleScreenBufferA';

{$ENDIF}  // IFNDEF VER120

const
  VK_BACK               = $08;

type
{$IFNDEF NOCRTEXTENSIONS}
  PCrtSaveState = ^TCrtSaveState;
  TCrtSaveState = record
    ValidFields: TCrtSaveStates;
    Screen: pointer;      // To a 2-dimensional array of CHAR_INFO
    ScreenWidth, ScreenHeight: integer;
    WhereX, WhereY: integer;
    Cursor: CONSOLE_CURSOR_INFO;
    TextAttr: byte;
    LastMode: word;
    CurrMode: byte;
    OriginalCoord: COORD;
    WindMin: longword;
    WindMax: longword;
    BinaryWrite,
    CheckBreak,
    CheckSnow,
    DirectVideo: boolean;
  end;
{$ENDIF}

  TStringInfo = record
    X, Y: integer;
    SStart,
    SEnd: PAnsiChar;
  end;

  PKBDCode = ^TKBDCode;
  TKBDCode = record
    Code,
    Normal,
    Shift,
    Ctrl,
    Alt: word;
  end;

var
  { Internal state variables }

  _ScreenWidth: integer = 0;
  _ScreenHeight: integer = 0;
  _CurrMode: byte = 0;
  _NormAttr: byte = $07;
  _C4350Mode: integer;                      { The mode that C4350 maps to }
  _Orig_C: COORD;

  _ExtendedChar: smallint = -1;
{$IFNDEF NOCRTEXTENSIONS}
  _LastInput: INPUT_RECORD;
  _IsWinNT: boolean;
{$ENDIF}

  _InputHandle: THandle = INVALID_HANDLE_VALUE;
  _OutputHandle: THandle = INVALID_HANDLE_VALUE;

const
  { Key translation table }
  KDBCodeTable: array[0..91] of TKBDCode = (
    (Code: VK_BACK;   Normal: $08;        Shift: $08;        Ctrl: $7F;     Alt: $100 + 14),
    (Code: VK_TAB;    Normal: $09;        Shift: $10F;       Ctrl: $194;    Alt: $100 + 165),
    (Code: VK_RETURN; Normal: $0D;        Shift: $0D;        Ctrl: $0A;     Alt: $100 + 166),
    (Code: VK_ESCAPE; Normal: $1B;        Shift: $1B;        Ctrl: $1B;     Alt: $100 + 1),
    (Code: VK_SPACE;  Normal: $20;        Shift: $20;        Ctrl: $103;    Alt: $20),
    (Code: Ord('0');  Normal: Ord('0');   Shift: Ord(')');   Ctrl: $FFFF;   Alt: $100 + 129),
    (Code: Ord('1');  Normal: Ord('1');   Shift: Ord('!');   Ctrl: $FFFF;   Alt: $100 + 120),
    (Code: Ord('2');  Normal: Ord('2');   Shift: Ord('@');   Ctrl: $103;    Alt: $100 + 121),
    (Code: Ord('3');  Normal: Ord('3');   Shift: Ord('#');   Ctrl: $FFFF;   Alt: $100 + 122),
    (Code: Ord('4');  Normal: Ord('4');   Shift: Ord('$');   Ctrl: $FFFF;   Alt: $100 + 123),
    (Code: Ord('5');  Normal: Ord('5');   Shift: Ord('%');   Ctrl: $FFFF;   Alt: $100 + 124),
    (Code: Ord('6');  Normal: Ord('6');   Shift: Ord('^');   Ctrl: $1E;     Alt: $100 + 125),
    (Code: Ord('7');  Normal: Ord('7');   Shift: Ord('&');   Ctrl: $FFFF;   Alt: $100 + 126),
    (Code: Ord('8');  Normal: Ord('8');   Shift: Ord('*');   Ctrl: $FFFF;   Alt: $100 + 127),
    (Code: Ord('9');  Normal: Ord('9');   Shift: Ord('(');   Ctrl: $FFFF;   Alt: $100 + 128),
    (Code: Ord('A');  Normal: Ord('a');   Shift: Ord('A');   Ctrl: $01;     Alt: $100 + 30),
    (Code: Ord('B');  Normal: Ord('b');   Shift: Ord('B');   Ctrl: $02;     Alt: $100 + 48),
    (Code: Ord('C');  Normal: Ord('c');   Shift: Ord('C');   Ctrl: $03;     Alt: $100 + 46),
    (Code: Ord('D');  Normal: Ord('d');   Shift: Ord('D');   Ctrl: $04;     Alt: $100 + 32),
    (Code: Ord('E');  Normal: Ord('e');   Shift: Ord('E');   Ctrl: $05;     Alt: $100 + 18),
    (Code: Ord('F');  Normal: Ord('f');   Shift: Ord('F');   Ctrl: $06;     Alt: $100 + 33),
    (Code: Ord('G');  Normal: Ord('g');   Shift: Ord('G');   Ctrl: $07;     Alt: $100 + 34),
    (Code: Ord('H');  Normal: Ord('h');   Shift: Ord('H');   Ctrl: $08;     Alt: $100 + 35),
    (Code: Ord('I');  Normal: Ord('i');   Shift: Ord('I');   Ctrl: $09;     Alt: $100 + 23),
    (Code: Ord('J');  Normal: Ord('j');   Shift: Ord('J');   Ctrl: $0A;     Alt: $100 + 36),
    (Code: Ord('K');  Normal: Ord('k');   Shift: Ord('K');   Ctrl: $0B;     Alt: $100 + 37),
    (Code: Ord('L');  Normal: Ord('l');   Shift: Ord('L');   Ctrl: $0C;     Alt: $100 + 38),
    (Code: Ord('M');  Normal: Ord('m');   Shift: Ord('M');   Ctrl: $0D;     Alt: $100 + 50),
    (Code: Ord('N');  Normal: Ord('n');   Shift: Ord('N');   Ctrl: $0E;     Alt: $100 + 49),
    (Code: Ord('O');  Normal: Ord('o');   Shift: Ord('O');   Ctrl: $0F;     Alt: $100 + 24),
    (Code: Ord('P');  Normal: Ord('p');   Shift: Ord('P');   Ctrl: $10;     Alt: $100 + 25),
    (Code: Ord('Q');  Normal: Ord('q');   Shift: Ord('Q');   Ctrl: $11;     Alt: $100 + 16),
    (Code: Ord('R');  Normal: Ord('r');   Shift: Ord('R');   Ctrl: $12;     Alt: $100 + 19),
    (Code: Ord('S');  Normal: Ord('s');   Shift: Ord('S');   Ctrl: $13;     Alt: $100 + 31),
    (Code: Ord('T');  Normal: Ord('t');   Shift: Ord('T');   Ctrl: $14;     Alt: $100 + 20),
    (Code: Ord('U');  Normal: Ord('u');   Shift: Ord('U');   Ctrl: $15;     Alt: $100 + 22),
    (Code: Ord('V');  Normal: Ord('v');   Shift: Ord('V');   Ctrl: $16;     Alt: $100 + 47),
    (Code: Ord('W');  Normal: Ord('w');   Shift: Ord('W');   Ctrl: $17;     Alt: $100 + 17),
    (Code: Ord('X');  Normal: Ord('x');   Shift: Ord('X');   Ctrl: $18;     Alt: $100 + 45),
    (Code: Ord('Y');  Normal: Ord('y');   Shift: Ord('Y');   Ctrl: $19;     Alt: $100 + 21),
    (Code: Ord('Z');  Normal: Ord('z');   Shift: Ord('Z');   Ctrl: $1A;     Alt: $100 + 44),
    (Code: VK_PRIOR;  Normal: $149;       Shift: $149;       Ctrl: $18F;    Alt: $100 + 153),
    (Code: VK_NEXT;   Normal: $151;       Shift: $151;       Ctrl: $176;    Alt: $100 + 161),
    (Code: VK_END;    Normal: $14F;       Shift: $14F;       Ctrl: $175;    Alt: $100 + 159),
    (Code: VK_HOME;   Normal: $147;       Shift: $147;       Ctrl: $177;    Alt: $100 + 151),
    (Code: VK_LEFT;   Normal: $14B;       Shift: $14B;       Ctrl: $173;    Alt: $100 + 155),
    (Code: VK_UP;     Normal: $148;       Shift: $148;       Ctrl: $18D;    Alt: $100 + 152),
    (Code: VK_RIGHT;  Normal: $14D;       Shift: $14D;       Ctrl: $174;    Alt: $100 + 157),
    (Code: VK_DOWN;   Normal: $150;       Shift: $150;       Ctrl: $191;    Alt: $100 + 160),
    (Code: VK_INSERT; Normal: $152;       Shift: $152;       Ctrl: $192;    Alt: $100 + 162),
    (Code: VK_DELETE; Normal: $153;       Shift: $153;       Ctrl: $193;    Alt: $100 + 163),
    (Code: VK_NUMPAD0; Normal: Ord('0');  Shift: $152;       Ctrl: $100 + 146; Alt: $FFFF),
    (Code: VK_NUMPAD1; Normal: Ord('1');  Shift: $14F;       Ctrl: $100 + 117; Alt: $FFFF),
    (Code: VK_NUMPAD2; Normal: Ord('2');  Shift: $150;       Ctrl: $100 + 145; Alt: $FFFF),
    (Code: VK_NUMPAD3; Normal: Ord('3');  Shift: $151;       Ctrl: $100 + 118; Alt: $FFFF),
    (Code: VK_NUMPAD4; Normal: Ord('4');  Shift: $14B;       Ctrl: $100 + 115; Alt: $FFFF),
    (Code: VK_NUMPAD5; Normal: Ord('5');  Shift: $14C;       Ctrl: $100 + 143; Alt: $FFFF),
    (Code: VK_NUMPAD6; Normal: Ord('6');  Shift: $14D;       Ctrl: $100 + 116; Alt: $FFFF),
    (Code: VK_NUMPAD7; Normal: Ord('7');  Shift: $147;       Ctrl: $100 + 119; Alt: $FFFF),
    (Code: VK_NUMPAD8; Normal: Ord('8');  Shift: $148;       Ctrl: $100 + 141; Alt: $FFFF),
    (Code: VK_NUMPAD9; Normal: Ord('9');  Shift: $149;       Ctrl: $100 + 132; Alt: $FFFF),
    (Code: VK_MULTIPLY; Normal: Ord('*'); Shift: Ord('*');   Ctrl: $100 + 150; Alt: $100 + 55),
    (Code: VK_ADD;    Normal: Ord('+');   Shift: Ord('+');   Ctrl: $100 + 144; Alt: $100 + 78),
    (Code: VK_SUBTRACT; Normal: Ord('-'); Shift: Ord('-');   Ctrl: $100 + 142; Alt: $100 + 74),
    (Code: VK_DECIMAL; Normal: Ord('.');  Shift: Ord('.');   Ctrl: $100 + 83; Alt: $100 + 147),
    (Code: VK_DIVIDE; Normal: Ord('/');   Shift: Ord('/');   Ctrl: $100 + 149; Alt: $100 + 164),
    (Code: VK_F1;     Normal: $100 + 59;  Shift: $100 + 84;  Ctrl: $100 + 94; Alt: $100 + 104),
    (Code: VK_F2;     Normal: $100 + 60;  Shift: $100 + 85;  Ctrl: $100 + 95; Alt: $100 + 105),
    (Code: VK_F3;     Normal: $100 + 61;  Shift: $100 + 86;  Ctrl: $100 + 96; Alt: $100 + 106),
    (Code: VK_F4;     Normal: $100 + 62;  Shift: $100 + 87;  Ctrl: $100 + 97; Alt: $100 + 107),
    (Code: VK_F5;     Normal: $100 + 63;  Shift: $100 + 88;  Ctrl: $100 + 98; Alt: $100 + 108),
    (Code: VK_F6;     Normal: $100 + 64;  Shift: $100 + 89;  Ctrl: $100 + 99; Alt: $100 + 109),
    (Code: VK_F7;     Normal: $100 + 65;  Shift: $100 + 90;  Ctrl: $100 + 100; Alt: $100 + 110),
    (Code: VK_F8;     Normal: $100 + 66;  Shift: $100 + 91;  Ctrl: $100 + 101; Alt: $100 + 111),
    (Code: VK_F9;     Normal: $100 + 67;  Shift: $100 + 92;  Ctrl: $100 + 102; Alt: $100 + 112),
    (Code: VK_F10;    Normal: $100 + 68;  Shift: $100 + 93;  Ctrl: $100 + 103; Alt: $100 + 113),
    (Code: VK_F11;    Normal: $100 + 133; Shift: $100 + 135; Ctrl: $100 + 137; Alt: $100 + 139),
    (Code: VK_F12;    Normal: $100 + 134; Shift: $100 + 136; Ctrl: $100 + 138; Alt: $100 + 140),
    (Code: $DC;       Normal: Ord('\');   Shift: Ord('|');   Ctrl: $1C;     Alt: $100 + 43),
    (Code: $BF;       Normal: Ord('/');   Shift: Ord('?');   Ctrl: $FFFF;   Alt: $100 + 53),
    (Code: $BD;       Normal: Ord('-');   Shift: Ord('_');   Ctrl: $1F;     Alt: $100 + 130),
    (Code: $BB;       Normal: Ord('=');   Shift: Ord('+');   Ctrl: $FFFF;   Alt: $100 + 131),
    (Code: $DB;       Normal: Ord('[');   Shift: Ord('{');   Ctrl: $1B;     Alt: $100 + 26),
    (Code: $DD;       Normal: Ord(']');   Shift: Ord('}');   Ctrl: $1D;     Alt: $100 + 27),
    (Code: $BA;       Normal: Ord(';');   Shift: Ord(':');   Ctrl: $FFFF;   Alt: $100 + 39),
    (Code: $DE;       Normal: Ord('''');  Shift: Ord('"');   Ctrl: $FFFF;   Alt: $100 + 40),
    (Code: $BC;       Normal: Ord(',');   Shift: Ord('<');   Ctrl: $FFFF;   Alt: $100 + 51),
    (Code: $BE;       Normal: Ord('.');   Shift: Ord('>');   Ctrl: $FFFF;   Alt: $100 + 52),
    (Code: $C0;       Normal: Ord('`');   Shift: Ord('~');   Ctrl: $FFFF;   Alt: $100 + 41),
    // TODO 4 -c Crt: Determine what the Win9x keys return
    (Code: VK_LWIN;   Normal: $FFFF;      Shift: $FFFF;      Ctrl: $FFFF;   Alt: $FFFF),
    (Code: VK_RWIN;   Normal: $FFFF;      Shift: $FFFF;      Ctrl: $FFFF;   Alt: $FFFF),
    (Code: VK_APPS;   Normal: $FFFF;      Shift: $FFFF;      Ctrl: $FFFF;   Alt: $FFFF)
  );

  iTYPE        = 3;    { Color, B&W, Monochrome }
  iROWS        = 7;    { 14, 21, 25, 28, 43, 50, 60 }
  iCOLS        = 2;    { 40, 80 }

{ +------------------------------------------------------------------------+
  | This mode table will be used in determining what a mode is, and in     |
  | constructing new mode information when a new mode is to be set.        |
  +------------------------------------------------------------------------+
  |                                                                        |
  |             Color           Black & White          Monochrome          |
  |        +----------------+------------------+----------------------+    |
  |        |  40      80    |  40      80      |    40          80    |    |
  |     ---+----------------+------------------+----------------------+    |
  |     14 |  C40X14 C80X14 |  BW40X14 BW80X14 |(*) MONO14 (*) MONO14 |    |
  |     21 |  C40X21 C80X21 |  BW40X21 BW80X21 |(*) MONO21     MONO21 |    |
  |     25 |  C40    C80    |  BW40    BW80    |(*) MONO       MONO   |    |
  |     28 |  C40X28 C80X28 |  BW40X28 BW80X28 |(*) MONO28     MONO28 |    |
  |     43 |  C40X43 C80X43 |  BW40X43 BW80X43 |(*) MONO43     MONO43 |    |
  |     50 |  C40X50 C80X50 |  BW40X50 BW80X50 |(*) MONO50     MONO50 |    |
  |     60 |  C40X60 C80X60 |  BW40X60 BW80X60 |(*) MONO60     MONO60 |    |
  |     ---+----------------+------------------+----------------------+    |
  |                                                                        |
  |     (*) This isn't a valid mode on a VGA.  We include it here for      |
  |         completeness.  Something like a Herc Ramfont could probably    |
  |         do modes like these.                                           |
  +------------------------------------------------------------------------+ }

{   BEWARE:  Code depends on the initialization orderings in this table! }

  _ModeTable: array[0..iTYPE - 1, 0..iROWS - 1, 0..iCOLS - 1] of cardinal =
    (
      ((C40X14,  C80X14),
       (C40X21,  C80X21),
       (C40,     C80),
       (C40X28,  C80X28),
       (C40X43,  C80X43),
       (C40X50,  C80X50),
       (C40X60,  C80X60)),

      ((BW40X14, BW80X14),
       (BW40X21, BW80X21),
       (BW40,    BW80),
       (BW40X28, BW80X28),
       (BW40X43, BW80X43),
       (BW40X50, BW80X50),
       (BW40X60, BW80X60)),

      ((MONO14, MONO14),
       (MONO21, MONO21),
       (MONO,   MONO),
       (MONO28, MONO28),
       (MONO43, MONO43),
       (MONO50, MONO50),
       (MONO50, MONO60))
    );

  RowLookUp: array[0..iROWS - 1] of cardinal = (14, 21, 25, 28, 43, 50, 60);
  ColLookUp: array[0..iCOLS - 1] of cardinal = (40, 80);

  INTENSE          = $08;

{ **** }

{ Internal Misc. Functions }

{ **** }

{$IFNDEF NOCRTEXTENSIONS}

procedure __InitExtended;
var
  Info: OSVERSIONINFO;
begin
  Info.dwOSVersionInfoSize := SizeOf(Info);
  GetVersionEx(Info);
  _IsWinNT := Info.dwPlatformId = VER_PLATFORM_WIN32_NT;
end;

function  __GetLibraryFunction(const Name, DLL: string; var Module: THandle): pointer;
begin
  Result := nil;

  if (Module = 0) then begin
    Module := LoadLibrary(PChar(DLL));
    if (Module <> 0) then begin
      Result := GetProcAddress(Module, PChar(Name));
      if (Result = nil) then begin
        if (FreeLibrary(Module)) then
          Module := 0;
      end;
    end;
  end else begin
    { Caller already allocated a Module handle, try finding function in that
      DLL. }
    Result := GetProcAddress(Module, PChar(Name));
  end;
end;

{$ENDIF}

function  __WhichMode(const ModeType: integer; const Rows, Cols: cardinal): integer; register;
var
  R, C: integer;
begin
  Result := -1;
  for R := 0 to High(RowLookUp) do begin
    if (RowLookUp[R] = Rows) then
    Break;
  end;
  if (R = High(RowLookUp) + 1) then
    Exit;

  if (Cols = 40) then
    C := 0
  else if (Cols = 80) then
    C := 1
  else
    Exit;

  Result := _ModeTable[ModeType, R, C];
end;

procedure __CrtInit(NewMode: integer); register;
var
  I, J, K,
  Actual_Mode: cardinal;
  C: COORD;
  R: SMALL_RECT;
begin
  if (NewMode = LASTMODE) then
    NewMode := LastMode;

  if (NewMode = C4350) then
    Actual_Mode := _C4350Mode
  else
    Actual_Mode := NewMode;

  for I := 0 to iTYPE - 1 do
    for J := 0 to iROWS - 1 do
      for K := 0 to iCOLS - 1 do begin
        if ((_ModeTable[I, J, K] = Actual_Mode) or (NewMode = _ORIGMODE)) then begin
          if (NewMode = _ORIGMODE) then
            C := _Orig_C
          else begin
            C.X := ColLookUp[K];
            C.Y := RowLookUp[J];
          end;
          SetConsoleScreenBufferSize(_OutputHandle, C);

          R.Left := 0;
          R.Top := 0;
          R.Right := C.X - 1;
          R.Bottom := C.Y - 1;
          if (not SetConsoleWindowInfo(_OutputHandle, True, R)) then
            Exit;
          if (SetConsoleScreenBufferSize(_OutputHandle, C)) then begin
            _CurrMode := NewMode;
            _ScreenWidth := C.X;
            _ScreenHeight := C.Y;

            TWindCoord(WindMin).X := 0;
            TWindCoord(WindMin).Y := 0;
            TWindCoord(WindMax).X := _ScreenWidth - 1;
            TWindCoord(WindMax).Y := _ScreenHeight - 1;

            LastMode := NewMode;
          end;
          Exit;
        end;
      end;
end;

function  __LookupKey(Code: word): PKBDCode; register;
var
  I: integer;
begin
  Result := nil;
  for I := Low(KDBCodeTable) to High(KDBCodeTable) do begin
    if (KDBCodeTable[I].Code = Code) then begin
      Result := @KDBCodeTable[I];
      Exit;
    end;
  end;
end;

function  __TranslateKey(Input: INPUT_RECORD; K: PKBDCode): word; register;
begin
  if ((Input.Event.KeyEvent.dwControlKeyState and (RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED)) <> 0) then
    Result := K.Alt
  else if ((Input.Event.KeyEvent.dwControlKeyState and (RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED)) <> 0) then
    Result := K.Ctrl
  else if ((Input.Event.KeyEvent.dwControlKeyState and SHIFT_PRESSED) <> 0) then
    Result := K.Shift
  else begin
    if (Chr(Input.Event.KeyEvent.wVirtualKeyCode) >= 'A') and (Chr(Input.Event.KeyEvent.wVirtualKeyCode) <= 'Z') then
      Result := Ord(Input.Event.KeyEvent.AsciiChar)
    else
      Result := K.Normal;
  end;
end;

function  __HandlerRoutine(dwCtrlType: DWORD): BOOL; stdcall;
{$IFNDEF NOCRTEXTENSIONS}
var
  Temp: TCrtEventProc;
{$ENDIF}
begin
  Result := False;
{$IFNDEF NOCRTEXTENSIONS}
  { TODO 2 -o Will -c Crt Extensions: Determine if there's a possible threading
issue with reading CrtEventProc for nil, then using it. }
  { Specifically, __HandlerRoutine is called from within the context of another
    thread (Win32 spawns a new thread to process events), so what we can have
    happen (in theory) is for thread 1 (the main thread) to change the value
    of CrtEventProc between the '<> nil' test and the actual call on the next
    line.  If this is an issue, the solution would be to remove direct access
    to CrtEventProc and make it an internal variable (one of the __ or _
    var's), then make some access functions (SetCrtEventProc, GetCrtEventProc)
    which are guarded by a CriticalSection. }
  Temp := CrtEventProc;
  if (@Temp <> nil) then begin
    Result := Temp(dwCtrlType);
    if (Result) then
      Exit;
  end;
{$ENDIF}
  case dwCtrlType of
    CTRL_CLOSE_EVENT,
    CTRL_BREAK_EVENT,
    CTRL_C_EVENT:
      begin
        Result := True;
        if (IsConsole) and (CheckBreak) then
          Halt;
      end;
  end;
end;

const
  __MAX_CELLS      = 64;      // C++ RTL defined this as 32

procedure __Flush(var S: TStringInfo); register;
var
  I, Len: integer;
  Size, C: COORD;
  Region: SMALL_RECT;
  Cells: array[0..__MAX_CELLS - 1] of CHAR_INFO;
begin
  Len := S.SEnd - S.SStart;
  if (Len = 0) then
    Exit;
  for I := 0 to Len - 1 do begin
    Cells[I].AsciiChar := S.SStart[I];
    Cells[I].Attributes := TextAttr;
  end;

  Size.X := Len;
  Size.Y := 1;
  C.X := 0;
  C.Y := 0;
  Region.Left := S.X - Len;
  Region.Right := S.X - 1;
  Region.Top := S.Y;
  Region.Bottom := Region.Top;
  WriteConsoleOutput(_OutputHandle, @Cells[0], Size, C, Region);
  S.SStart := S.SEnd;
end;

procedure __InitVideo; register;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
  C: COORD;
begin
  if (GetConsoleScreenBufferInfo(_OutputHandle, Info)) then begin
    _Orig_C := info.dwSize;        { save original screen size }

    _NormAttr := Info.wAttributes;
    TextAttr := Info.wAttributes;
    _ScreenWidth := Info.dwSize.X;
    _ScreenHeight := Info.dwSize.Y;
  end;

  C := GetLargestConsoleWindowSize(_OutputHandle);
  if (C.Y <= 43) then
    _C4350Mode := C80X43          { EGA can handle only 43 lines }
  else
    _C4350Mode := C80X50;         { VGA can handle 50 lines }

  LastMode := __WhichMode(0, _ScreenHeight, _ScreenWidth);

  if (LastMode = _C4350Mode) then
    LastMode := C4350
  else if (LastMode = $FFFF) then    { unknown mode? }
    LastMode := _ORIGMODE;

  _CurrMode := LastMode;
  CheckSnow := False;
  DirectVideo := True;

  TWindCoord(WindMin).X := 0;
  TWindCoord(WindMin).Y := 0;
  TWindCoord(WindMax).X := _ScreenWidth - 1;
  TWindCoord(WindMax).Y := _ScreenHeight - 1;
end;

procedure __MoveCursor(X, Y: word); register;
var
  C: COORD;
begin
  C.X := X;
  C.Y := Y;
  SetConsoleCursorPosition(_OutputHandle, C);
end;

const
  UP      = 0;
  DOWN    = 1;

procedure __Scroll(Dir: cardinal; X1, Y1, X2, Y2, Lines: word); register;
var
  Fill: CHAR_INFO;
  R: SMALL_RECT;
  C: COORD;
begin
  Fill.AsciiChar := ' ';
  Fill.Attributes := TextAttr;

  if (Lines = 0) then
    Lines := Y2 - Y1 + 1;

  R.Left := X1;
  R.Top := Y1;
  R.Right := X2;
  R.Bottom := Y2;

  C.X := X1;
  if (Dir = UP) then
    C.Y := Y1 - Lines
  else
    C.Y := Y1 + Lines;

  ScrollConsoleScreenBuffer(_OutputHandle, R, @R, C, Fill);
end;

function  _WhereX: integer; register;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if (GetConsoleScreenBufferInfo(_OutputHandle, Info)) then
    Result := Info.dwCursorPosition.X
  else
    Result := 0;
end;

function  _WhereY: integer; register;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if (GetConsoleScreenBufferInfo(_OutputHandle, Info)) then
    Result := Info.dwCursorPosition.Y
  else
    Result := 0;
end;

{ Internal Input Functions }

function  _CrtIn(var f: TTextRec): integer;
begin
  f.BufPos := 0;
  f.BufEnd := 0;
  while (f.BufEnd < f.BufSize) do begin
    f.BufPtr[f.BufEnd] := ReadKey;
    case f.BufPtr[f.BufEnd] of
      #0:
        if (_ExtendedChar <> -1) then
          ReadKey;   // Discard extended keys
  #1..#7,
      #9,
#11, #12,
#14..#31: ;          // Discard unsupported control keys
      #8:            // Backspace
        if (f.BufEnd > 0) then begin
          Write(#8' '#8);
          Dec(f.BufEnd);
        end;
     #13:           // Carriage Return
        begin
          Inc(f.BufEnd);
          if (f.BufEnd + 1 < f.BufSize) then begin
            f.BufPtr[f.BufEnd] := #10;
            Inc(f.BufEnd);
          end;
          Write(#13#10);
          Break;
        end;
      else begin
        Write(f.BufPtr[f.BufEnd]);
        Inc(f.BufEnd);
      end;
    end;
  end;
  Result := 0;
end;

function  _CrtInFlush(var f: TTextRec): integer;
begin
  Result := 0;
end;

function  _CrtInClose(var f: TTextRec): integer;
begin
  CloseHandle(f.Handle);
  FillChar(f, SizeOf(f), 0);
  f.Handle := integer(INVALID_HANDLE_VALUE);
  f.Mode := fmClosed;
  Result := 0;
end;

{ Internal Output Functions }

function  _CrtOut(var f: TTextRec): integer;
var
  S: TStringInfo;
  Special: boolean;
begin
  S.X := _WhereX;
  S.Y := _WhereY;
  S.SStart := f.BufPtr;
  S.SEnd := f.BufPtr;

{$IFNDEF NOCRTEXTENSIONS}
  if (BinaryWrite) then while (f.BufPos > 0) do begin
    Dec(f.BufPos);
    Inc(S.X);
    Inc(S.SEnd);
    if (S.SEnd - S.SStart >= __MAX_CELLS) then
      __Flush(S);

    if (S.X > TWindCoord(WindMax).X) then begin
      __Flush(S);
      S.X := TWindCoord(WindMin).X;
      Inc(S.Y);
    end;

    if (S.Y > TWindCoord(WindMax).Y) then begin
      __Flush(S);
      __Scroll(UP, TWindCoord(WindMin).X, TWindCoord(WindMin).Y, TWindCoord(WindMax).X, TWindCoord(WindMax).Y, 1);
      Dec(S.Y);
    end;
  end else {$ENDIF} while (f.BufPos > 0) do begin
    Dec(f.BufPos);
    Special := True;
    case S.SEnd[0] of
      #7:    // Bell
        begin
          __Flush(S);
          Beep(3000, 100);
        end;
      #8:    // Backspace
        begin
          __Flush(S);
          if (S.X > TWindCoord(WindMin).X) then
            Dec(S.X);
        end;
      #10:  // Linefeed
        begin
          __Flush(S);
          Inc(S.Y);
        end;
      #13:  // Carriage Return
        begin
          __Flush(S);
          S.X := TWindCoord(WindMin).X;
        end;
      else
        begin
          Special := False;
          Inc(S.X);
        end;
    end;
    Inc(S.SEnd);
    if (Special) then
      S.SStart := S.SEnd;
    if (S.SEnd - S.SStart >= __MAX_CELLS) then
      __Flush(S);

    if (S.X > TWindCoord(WindMax).X) then begin
      __Flush(S);
      S.X := TWindCoord(WindMin).X;
      Inc(S.Y);
    end;

    if (S.Y > TWindCoord(WindMax).Y) then begin
      __Flush(S);
      __Scroll(UP, TWindCoord(WindMin).X, TWindCoord(WindMin).Y, TWindCoord(WindMax).X, TWindCoord(WindMax).Y, 1);
      Dec(S.Y);
    end;
  end;

  __Flush(S);
  __MoveCursor(S.X, S.Y);
  Result := 0;
end;

function  _CrtOutClose(var f: TTextRec): integer;
begin
  CloseHandle(f.Handle);
  FillChar(f, SizeOf(f), 0);
  f.Handle := integer(INVALID_HANDLE_VALUE);
  f.Mode := fmClosed;
  Result := 0;
end;

function  _CrtOpen(var f: TTextRec): integer;
var
  _Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result := 0;
  case f.Mode of
    fmInput:
      begin
        _InputHandle := CreateFile('CONIN$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
        f.Handle := _InputHandle;
        f.Mode := fmInput;
        if (f.BufPtr = nil) then begin
          f.BufPtr := @f.Buffer;
          f.BufSize := SizeOf(f.Buffer)
        end;
        SetConsoleMode(f.Handle, 0);
        SetConsoleCtrlHandler(@__HandlerRoutine, True);
        f.InOutFunc := @_CrtIn;
        f.FlushFunc := @_CrtInFlush;
        f.CloseFunc := @_CrtInClose;
      end;
    fmOutput:
      begin
        _OutputHandle := CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
{$IFDEF VER140}
        // Delphi 6 and above default to LF-only line breaks
        f.Mode := fmClosed;
        SetLineBreakStyle(Text(f), tlbsCRLF);
{$ENDIF}
        f.Handle := _OutputHandle;
        f.Mode := fmOutput;
        if (f.BufPtr = nil) then begin
          f.BufPtr := @f.Buffer;
          f.BufSize := SizeOf(f.Buffer)
        end;
        __InitVideo;
        if (GetConsoleScreenBufferInfo(f.Handle, _Info)) then begin
          SetConsoleMode(f.Handle, 0);
          f.InOutFunc := @_CrtOut;
          f.FlushFunc := @_CrtOut;        // Doing double duty here
          f.CloseFunc := @_CrtOutClose;
        end else begin
          Result := GetLastError;
        end;
      end;
  end;
end;

{ **** }

{ Original CRT Functions and Procedures }

{ **** }

procedure AssignCrt(var f: Text);
begin
  with TTextRec(f) do begin
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @_CrtOpen;
    InOutFunc := nil;
    FlushFunc := nil;
    CloseFunc := nil;
    FillChar(Name, SizeOf(Name), 0);
  end;
end;

procedure ClrEol;
var
  C: COORD;
  Len, NumWritten: DWORD;
begin
  C.X := _WhereX;
  C.Y := _WhereY;

  Len := TWindCoord(WindMax).X - C.X + 1;

  FillConsoleOutputCharacter(_OutputHandle, ' ', Len, C, NumWritten);
  FillConsoleOutputAttribute(_OutputHandle, TextAttr, Len, C, NumWritten);
end;

procedure ClrScr;
var
  C: COORD;
  I, Len, NumWritten: DWORD;
begin
  if ((TWindCoord(WindMin).X = 0) and (TWindCoord(WindMin).Y = 0) and (TWindCoord(WindMax).X = _ScreenWidth - 1) and (TWindCoord(WindMax).Y = _ScreenHeight - 1)) then begin
    Len := _ScreenWidth * _ScreenHeight;
    C.X := 0;
    C.Y := 0;
    FillConsoleOutputCharacter(_OutputHandle, ' ', Len, C, NumWritten);
    FillConsoleOutputAttribute(_OutputHandle, TextAttr, Len, C, NumWritten);
  end else begin
    Len := TWindCoord(WindMax).X - TWindCoord(WindMin).X + 1;
    C.X := TWindCoord(WindMin).X;
    for I := TWindCoord(WindMin).Y to TWindCoord(WindMax).Y do begin
      C.Y := I;
      FillConsoleOutputCharacter(_OutputHandle, ' ', Len, C, NumWritten);
      FillConsoleOutputAttribute(_OutputHandle, TextAttr, Len, C, NumWritten);
    end;
  end;
  GotoXy(1, 1);
end;

procedure Delay(MS: Cardinal);
begin
  Sleep(MS);
end;

procedure DelLine;
begin
  __Scroll(UP, TWindCoord(WindMin).X, _WhereY, TWindCoord(WindMax).X, TWindCoord(WindMax).Y, 1);
end;

{ Note:
  GotoXY : 2007.10.11, 류종택, 수정
  - X, Y : 콘솔 상의 커서가 이동해야할 위치
}

procedure GotoXY(X,Y:integer);
var
  XY : TCoord;
begin
  XY.X:= X;
  XY.Y:= Y;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), xy)
end;

//procedure GotoXY(X, Y: integer);
//var
//  R, C: integer;
//begin
//  R := Y - 1 + TWindCoord(WindMin).Y;
//  C := X - 1 + TWindCoord(WindMin).X;
//
//  if ((R < TWindCoord(WindMin).Y) or (R > TWindCoord(WindMax).Y) or (C < TWindCoord(WindMin).X) or (C > TWindCoord(WindMax).X)) then
//    Exit;
//
//  __MoveCursor(C, R);
//end;

{ Note:
  GetExtendedKey : 2007.10.11, 류종택, 확장키를 읽기 위해서 추가함
  - Result : 읽혀진 키값
    . #75 : 왼쪽 커서 키
    . #77 : 오른쪽 커서 키
    . #72 : 윗쪽 커서 키
    . #80 : 아래쪽 커서 키
}

function GetExtendedKey:AnsiChar;
begin
  Result:= Crt.ReadKey;
  if Result = #0 then Result:= Crt.ReadKey;
end;

procedure HighVideo;
begin
  TextAttr := TextAttr or INTENSE;
end;

procedure InsLine;
begin
  __Scroll(DOWN, TWindCoord(WindMin).X, _WhereY, TWindCoord(WindMax).X, TWindCoord(WindMax).Y, 1);
end;

function  KeyPressed: boolean;
var
{$IFDEF VER120}
  Input: array of INPUT_RECORD;
{$ELSE}
  P, Input: ^INPUT_RECORD;
{$ENDIF}
  NumRead, NumEvents, J: DWORD;
  K: PKBDCode;
begin
  Result := False;
  if (_ExtendedChar <> -1) then begin
    Result := True;
    Exit;
  end;

  GetNumberOfConsoleInputEvents(_InputHandle, NumEvents);

  if (NumEvents = 0) then
    Exit;

{$IFDEF VER120}
  SetLength(Input, NumEvents);
  try
    PeekConsoleInput(_InputHandle, Input[0], NumEvents, NumRead);
    for J := 0 to NumRead - 1 do begin
      if ((Input[J].EventType and KEY_EVENT) <> 0) and (Input[J].Event.KeyEvent.bKeyDown) then begin
        if ((Input[J].Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and (Input[J].Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) and (Input[J].Event.KeyEvent.wVirtualKeyCode <> VK_MENU)) then begin
          K := __LookupKey(Input[J].Event.KeyEvent.wVirtualKeyCode);
          if (K <> nil) then begin
            if (smallint(__TranslateKey(Input[J], K)) <> -1) then begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    end;
    { Flush the events so WaitForSingleObject(_InputHandle, INFINITE) won't
      keep firing if the only events in the keyboard input queue are non-
      keyboard events.  (In other words, flush the buffer of the exact number
      of events we read and checked for keyboard activity from.) }
    ReadConsoleInput(_InputHandle, Input[0], NumRead, NumRead);
  finally
    SetLength(Input, 0);
  end;
{$ELSE}
  GetMem(Input, NumEvents * SizeOf(INPUT_RECORD));
  P := Input;
  try
    PeekConsoleInput(_InputHandle, Input^, NumEvents, NumRead);
    for J := 0 to NumRead - 1 do begin
      if ((P^.EventType and KEY_EVENT) <> 0) and (P^.Event.KeyEvent.bKeyDown) then begin
        if ((P^.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and (P^.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) and (P^.Event.KeyEvent.wVirtualKeyCode <> VK_MENU)) then begin
          K := __LookupKey(P^.Event.KeyEvent.wVirtualKeyCode);
          if (K <> nil) then begin
            if (smallint(__TranslateKey(P^, K)) <> -1) then begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
      Inc(P, SizeOf(INPUT_RECORD))
    end;
    { Flush the events so WaitForSingleObject(_InputHandle, INFINITE) won't
      keep firing if the only events in the keyboard input queue are non-
      keyboard events.  (In other words, flush the buffer of the exact number
      of events we read and checked for keyboard activity from.) }
    ReadConsoleInput(_InputHandle, Input^, NumRead, NumRead);
  finally
    FreeMem(Input, NumEvents * SizeOf(INPUT_RECORD));
  end;
{$ENDIF}
end;

procedure LowVideo;
begin
  TextAttr := TextAttr and (not INTENSE);
end;

procedure NormVideo;
begin
  TextAttr := _NormAttr;
end;

procedure NoSound;
begin
  Beep(0, 0);
end;

function  ReadKey: AnsiChar;
var
  Input: INPUT_RECORD;
  NumRead: DWORD;
  K: PKBDCode;
  C: smallint;
begin
  if (_ExtendedChar <> -1) then begin
    Result := AnsiChar(_ExtendedChar);
    _ExtendedChar := -1;
    Exit;
  end;
  while (True) do begin
    if (ReadConsoleInput(_InputHandle, Input, 1, NumRead)) then begin
      if ((Input.EventType = KEY_EVENT) and (Input.Event.KeyEvent.bKeyDown)) then begin
        K := __LookupKey(Input.Event.KeyEvent.wVirtualKeyCode);
        if (K <> nil) then begin
          C := __TranslateKey(Input, K);
          if (C <> -1) then begin
            if (C and $0100 <> 0) then begin
{$IFNDEF NOCRTEXTENSIONS}
              _LastInput := Input;
{$ENDIF}
              _ExtendedChar := C and $00FF;
               Result := #0;
              Exit;
            end;
            if (C = 0) and (Input.Event.KeyEvent.AsciiChar <> #0) then
              Result := Input.Event.KeyEvent.AsciiChar
            else
              Result := AnsiChar(C);
{$IFNDEF NOCRTEXTENSIONS}
            _LastInput := Input;
{$ENDIF}
            Exit;
          end;
        end;
      end;
    end else begin
      Result := AnsiChar(-1);
      Exit;
    end;
  end;
end;

procedure Sound(Hz: Cardinal; Duration: DWord);
begin
  Beep(Hz, Duration);
end;

procedure TextBackground(Color: integer); register;
begin
  TextAttr := (TextAttr and $8F) or ((Color shl 4) and $7F);
end;

procedure TextColor(Color: integer); register;
begin
  TextAttr := (TextAttr and $70) or (Color and $8F);
end;

procedure TextMode(Mode: integer);
begin
  if (Mode = LASTMODE) then
    __CrtInit(_CurrMode)
  else
    __CrtInit(Mode);

  TextAttr := _NormAttr;
end;

function  WhereX: integer;
begin
  Result := (_WhereX - TWindCoord(WindMin).X) + 1;
end;

function  WhereY: integer;
begin
  Result := (_WhereY - TWindCoord(WindMin).Y) + 1;
end;

procedure Window(X1, Y1, X2, Y2: integer);
begin
  Dec(X1);
  Dec(X2);
  Dec(Y1);
  Dec(Y2);

  if (X1 < 0) or (X2 >= _ScreenWidth) or (Y1 < 0) or (Y2 >= _ScreenHeight) or (X2 - X1 < 0) or (Y2 - Y1 < 0) then
    Exit;

  TWindCoord(WindMin).X := X1;
  TWindCoord(WindMax).X := X2;
  TWindCoord(WindMin).Y := Y1;
  TWindCoord(WindMax).Y := Y2;

  __MoveCursor(X1, Y1);
end;

{ **** }

{ New CRT Functions and Procedures }

{ **** }

{$IFNDEF NOCRTEXTENSIONS}

function  FreeCrtState(var SaveState: pointer): boolean;
begin
  Result := False;
  if (Assigned(SaveState)) then begin
    if (csScreen in PCrtSaveState(SaveState)^.ValidFields) then begin
      FreeMem(PCrtSaveState(SaveState)^.Screen);
      PCrtSaveState(SaveState)^.Screen := nil;
      Exclude(PCrtSaveState(SaveState)^.ValidFields, csScreen);
    end;
    FreeMem(SaveState);
    SaveState := nil;
    Result := True;
  end;
end;

function  GetControlKeyState: longword;
begin
  Result := _LastInput.Event.KeyEvent.dwControlKeyState;
end;

function  IsAltPressed: boolean;
begin
  Result := ((_LastInput.Event.KeyEvent.dwControlKeyState and (RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED)) <> 0);
end;

function  IsCtrlPressed: boolean;
begin
  Result := ((_LastInput.Event.KeyEvent.dwControlKeyState and (RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED)) <> 0);
end;

function  IsShiftPressed: boolean;
begin
  Result := ((_LastInput.Event.KeyEvent.dwControlKeyState and SHIFT_PRESSED) <> 0);
end;

function  RestoreCrtState(var SaveState: pointer): boolean;
var
  C, Size: COORD;
  R: SMALL_RECT;
begin
  Result := False;
  if (Assigned(SaveState)) then begin
    if (csVars in PCrtSaveState(SaveState)^.ValidFields) then begin
      BinaryWrite := PCrtSaveState(SaveState)^.BinaryWrite;
      CheckBreak := PCrtSaveState(SaveState)^.CheckBreak;
      CheckSnow := PCrtSaveState(SaveState)^.CheckSnow;
      DirectVideo := PCrtSaveState(SaveState)^.DirectVideo;
      Exclude(PCrtSaveState(SaveState)^.ValidFields, csVars);
    end;

    if (csMode in PCrtSaveState(SaveState)^.ValidFields) then begin
      LastMode := PCrtSaveState(SaveState)^.LastMode;
      _CurrMode := PCrtSaveState(SaveState)^.CurrMode;
      _Orig_C := PCrtSaveState(SaveState)^.OriginalCoord;
      TextMode(PCrtSaveState(SaveState)^.CurrMode);
      LastMode := PCrtSaveState(SaveState)^.LastMode;
      _CurrMode := PCrtSaveState(SaveState)^.CurrMode;

      Exclude(PCrtSaveState(SaveState)^.ValidFields, csMode);
    end;

    if (csWindow in PCrtSaveState(SaveState)^.ValidFields) then begin
      WindMin := PCrtSaveState(SaveState)^.WindMin;
      WindMax := PCrtSaveState(SaveState)^.WindMax;
      Exclude(PCrtSaveState(SaveState)^.ValidFields, csWindow);
    end;

    if (csTextAttr in PCrtSaveState(SaveState)^.ValidFields) then begin
      TextAttr := PCrtSaveState(SaveState)^.TextAttr;
      Exclude(PCrtSaveState(SaveState)^.ValidFields, csTextAttr);
    end;

    if (csCursor in PCrtSaveState(SaveState)^.ValidFields) then begin
      if (SetConsoleCursorInfo(_OutputHandle, PCrtSaveState(SaveState)^.Cursor)) then
        Exclude(PCrtSaveState(SaveState)^.ValidFields, csCursor);
    end;

    if (csCursorPos in PCrtSaveState(SaveState)^.ValidFields) then begin
      GotoXy(PCrtSaveState(SaveState)^.WhereX, PCrtSaveState(SaveState)^.WhereY);
      Exclude(PCrtSaveState(SaveState)^.ValidFields, csCursorPos);
    end;

    if (csScreen in PCrtSaveState(SaveState)^.ValidFields) then begin
      Size.X := PCrtSaveState(SaveState)^.ScreenWidth;
      Size.Y := PCrtSaveState(SaveState)^.ScreenHeight;
      C.X := 0;
      C.Y := 0;
      R.Left := 0;
      R.Top := 0;
      R.Right := PCrtSaveState(SaveState)^.ScreenWidth - 1;
      R.Bottom := PCrtSaveState(SaveState)^.ScreenHeight - 1;

      if (WriteConsoleOutput(_OutputHandle, PCrtSaveState(SaveState)^.Screen, Size, C, R)) then begin
        FreeMem(PCrtSaveState(SaveState)^.Screen);
        PCrtSaveState(SaveState)^.Screen := nil;

        PCrtSaveState(SaveState)^.ScreenWidth := 0;
        PCrtSaveState(SaveState)^.ScreenHeight := 0;
        Exclude(PCrtSaveState(SaveState)^.ValidFields, csScreen);
      end;
    end;
    FreeMem(SaveState);
    SaveState := nil;
    Result := True;
  end;
end;

function  SaveCrtState(StatesToSave: TCrtSaveStates): pointer;
var
  C, Size: COORD;
  R: SMALL_RECT;
  Temp: PCrtSaveState;
begin
  GetMem(Temp, SizeOf(TCrtSaveState));
  FillChar(Temp^, SizeOf(TCrtSaveState), 0);    // Set all fields to 0

  if (csScreen in StatesToSave) then begin
    Temp.ScreenWidth := _ScreenWidth;
    Temp.ScreenHeight := _ScreenHeight;
    GetMem(Temp.Screen, _ScreenWidth * _ScreenHeight * SizeOf(CHAR_INFO));

    Size.X := _ScreenWidth;
    Size.Y := _ScreenHeight;
    C.X := 0;
    C.Y := 0;
    R.Left := 0;
    R.Top := 0;
    R.Right := _ScreenWidth - 1;
    R.Bottom := _ScreenHeight - 1;
    if (ReadConsoleOutput(_OutputHandle, Temp.Screen, Size, C, R)) then
      Include(Temp.ValidFields, csScreen);
  end;

  if (csCursorPos in StatesToSave) then begin
    Temp.WhereX := WhereX;
    Temp.WhereY := WhereY;
    Include(Temp.ValidFields, csCursorPos);
  end;

  if (csCursor in StatesToSave) then begin
    if (GetConsoleCursorInfo(_OutputHandle, Temp.Cursor)) then
      Include(Temp.ValidFields, csCursor);
  end;

  if (csTextAttr in StatesToSave) then begin
    Temp.TextAttr := TextAttr;
    Include(Temp.ValidFields, csTextAttr);
  end;

  if (csWindow in StatesToSave) then begin
    Temp.WindMin := WindMin;
    Temp.WindMax := WindMax;
    Include(Temp.ValidFields, csWindow);
  end;

  if (csMode in StatesToSave) then begin
    Temp.LastMode := LastMode;
    Temp.CurrMode := _CurrMode;
    Temp.OriginalCoord := _Orig_C;
    Include(Temp.ValidFields, csMode);
  end;

  if (csVars in StatesToSave) then begin
    Temp.BinaryWrite := BinaryWrite;
    Temp.CheckBreak := CheckBreak;
    Temp.CheckSnow := CheckSnow;
    Temp.DirectVideo := DirectVideo;
    Include(Temp.ValidFields, csVars);
  end;
  Result := Temp;
end;

function  ScreenX: integer;
begin
  Result := _WhereX + 1;
end;

function  ScreenY: integer;
begin
  Result := _WhereY + 1;
end;

function  ScreenHeight: integer;
begin
  Result := _ScreenHeight;
end;

function  ScreenWidth: integer;
begin
  Result := _ScreenWidth;
end;

function  SetScreenSize(Width, Height: integer): boolean;
var
  C: COORD;
  R: SMALL_RECT;
begin
  Result := False;

  C.X := Width;
  C.Y := Height;

  R.Left := 0;
  R.Top := 0;
  R.Right := Width - 1;
  R.Bottom := Height - 1;

  SetConsoleScreenBufferSize(_OutputHandle, C);
  if ((not SetConsoleWindowInfo(_OutputHandle, True, R)) or (not SetConsoleScreenBufferSize(_OutputHandle, C))) then
    Exit;

  _ScreenHeight := Height;
  _ScreenWidth := Width;

  TWindCoord(WindMin).X := 0;
  TWindCoord(WindMin).Y := 0;
  TWindCoord(WindMax).X := _ScreenWidth - 1;
  TWindCoord(WindMax).Y := _ScreenHeight - 1;

  Result := True;
end;

procedure SetCursorType(Cursor: integer; Custom: longword{$IFDEF VER120} = 15{$ENDIF}; Visible: boolean{$IFDEF VER120} = True{$ENDIF});
var
  Info: CONSOLE_CURSOR_INFO;
begin
  case Cursor of
    _CUSTOM:
      begin
        info.dwSize := Custom;
        Info.bVisible := Visible;
      end;
    _NOCURSOR:
      begin
        { Originally set Info.dwSize to 1, which worked fine under Windows 2000
          if you did it in a non-full screen mode, but once you switched to
          full-screen the cursor re-appeared (and trying to turn it off again
          would not work). Testing has shown that, at least under NT/2000,
          setting dwSize to 25 works in both full-screen and windowed code.
          NOTE: This still needs testing under '9x. }
        // TODO 2 -c Crt Extensions: Test _NOCURSOR SetCursorType option under Windows '9x
        Info.dwSize := 25;
        Info.bVisible := False;
      end;
    _SOLIDCURSOR:
      begin
        { Win9x seems to have trouble setting the cursor size to 100%, even
          though NT does it correctly.  So we'll only go up to 99% which
          works everywhere. }
        Info.dwSize := 99;
        Info.bVisible := True;
      end;
    _NORMALCURSOR:
      begin
        Info.dwSize := 25;
        Info.bVisible := True;
      end;
    _SMALLCURSOR:
      begin
        Info.dwSize := 15;
        Info.bVisible := True;
      end;
  end;
  SetConsoleCursorInfo(_OutputHandle, Info);
end;

{$ENDIF}

initialization
{$IFNDEF NOCRTEXTENSIONS}
  __InitExtended;
{$ENDIF}
  { This initialization code does not work as expected in Delphi 2.  In D2,
    IsConsole appears to be always False even if $APPTYPE is set to CONSOLE in
    the main project file. }
  if (IsConsole) then begin
    AssignCrt(Input);
    Reset(Input);
    AssignCrt(Output);
    Rewrite(Output);
  end else
    CheckBreak := False;
end.
