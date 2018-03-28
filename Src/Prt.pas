{$I-}
Unit Prt;

Interface

Type
    CodeStr = String[20];

Const
     KSSM : Packed Array [1..48] of CodeStr
              = (
                 'KSSM', '46',
                 '#폼피드',   #12,
                 '#초기화',   #27+'@',
                 '#1/180',    #27+'J'+#1,
                 '#2/180',    #27+'J'+#2,
                 '#5/180',    #27+'J'+#5,
                 '#10/180',   #27+'J'+#10,
                 '#18/180',   #27+'J'+#18,
                 '#완성형',   #27+'@'+#28+'&'+#28+'t0',
                 '#조합형',   #27+'@'+#28+'&'+#28+'t1',
                 '#이탤릭체지정', #27+'4',
                 '#이탤릭체취소', #27+'5',
                 '#강조체지정',   #27+'E',
                 '#강조체취소',   #27+'F',
                 '#일반문자',     #27+'q'+#0,
                 '#음각문자',     #27+'q'+#1,
                 '#영상문자',     #27+'q'+#2,
                 '#음각영상',     #27+'q'+#3,
                 '#두배확대',     #28+'W1',
                 '#두배취소',     #28+'W0',
                 '#가로두배지정', #27+'W'+#1,
                 '#가로두배취소', #27+'W'+#0,
                 '#세로두배지정', #27+'y'+#1,
                 '#세로두배취소', #27+'y'+#0
              );
     KS : Packed Array [1..48] of CodeStr
              = (
                 'KS', '46',
                 '#폼피드',   #12,
                 '#초기화',   #27+'@',
                 '#1/180',    #27+'J'+#1,
                 '#2/180',    #27+'J'+#2,
                 '#5/180',    #27+'J'+#5,
                 '#10/180',   #27+'J'+#10,
                 '#18/180',   #27+'J'+#18,
                 '#고딕체',       #27+'m'+#1,
                 '#명조체',       #27+'m'+#0,
                 '#이탤릭체지정', #27+'4',
                 '#이탤릭체취소', #27+'5',
                 '#강조체지정',   #27+'E',
                 '#강조체취소',   #27+'F',
                 '#역상지정',     #27+'r'+#1,
                 '#역상취소',     #27+'r'+#0,
                 '#음영지정',     #27+'z'+#1,
                 '#음영취소',     #27+'z'+#0,
                 '#1.5배지정',    #27+'s'+#1,
                 '#1.5배취소',    #27+'s'+#0,
                 '#가로두배지정', #27+'W'+#1,
                 '#가로두배취소', #27+'W'+#0,
                 '#세로두배지정', #27+'y'+#1,
                 '#세로두배취소', #27+'y'+#0
              );

Var
   PrtFile                   : TextFile;
   TimeOut, PrtError         : Byte;
   PrintPage                 : Boolean;
   PrintFileName             : String;

procedure SetDefaultPrinter;
Procedure OpenPrinter;
Procedure PrintStr(Strg:String);
Procedure PrintLnStr(Strg:String);
Procedure CRLF(Lines:Word);
Procedure FormFeed;
Procedure ClosePrinter;

Implementation

Uses
  Windows, SysUtils, Forms, Dialogs, Printers, Strg;

// 경문이가 고친것  
procedure SetDefaultPrinter;
var
  aDevice, aDriver, aPort: PChar;
  aHandle: THandle;
begin
  GetMem(aDevice, 50); // 50은 충분히 큰 값
  GetMem(aDriver, 50);
  GetMem(aPort, 50);
  Printer.GetPrinter(aDevice, aDriver, aPort, aHandle);
  PrintFileName:= StrPas(aPort);
  FreeMem(aDevice);
  FreeMem(aDriver);
  FreeMem(aPort);
end;

Procedure OpenPrinter;
Begin
     PrtError:= 0;
     AssignFile(PrtFile, PrintFileName);
     ReWrite(PrtFile);
     If IOResult <> 0 then
        Begin
             PrtError:= 1;
             ShowMessage('프린터를 점검하여 주십시오. ');
        End;
End;

Procedure PrintStr(Strg:String);
Begin
     Write(PrtFile, Strg);
End;

Procedure PrintLnStr(Strg:String);
Begin
     WriteLn(PrtFile, Strg);
End;

Procedure CRLF(Lines:Word);
Var
   Loop : Word;
Begin
     For Loop:= 1 to Lines Do WriteLn(PrtFile);
End;

Procedure FormFeed;
Begin
     Write(PrtFile, #12);
End;

Procedure ClosePrinter;
Begin
     CloseFile(PrtFile);
End;

End.


