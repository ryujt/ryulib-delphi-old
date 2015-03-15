unit SpeakerUtils;

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes;

procedure Sound(aFreq, aDelay: Integer);
procedure NoSound;

implementation

procedure SetPort(address, Value: Word);
var
  bValue: Byte;
begin
  bValue := trunc(Value and 255);
  asm
    mov dx, address
    mov al, bValue
    out dx, al
  end;
end;

function GetPort(address: Word): Word;
var
  bValue: Byte;
begin
  asm
    mov dx, address
    in al, dx
    mov bValue, al
  end;
  GetPort := bValue;
end;

procedure Sound(aFreq, aDelay: Integer);

  procedure DoSound(Freq: Word);
  var
    B: Byte;
  begin
    if Freq > 18 then
    begin
      Freq := Word(1193181 div Longint(Freq));
      B := Byte(GetPort($61));

      if (B and 3) = 0 then
      begin
        SetPort($61, Word(B or 3));
        SetPort($43, $B6);
      end;

      SetPort($42, Freq);
      SetPort($42, Freq shr 8);
    end;
  end;

  procedure Delay(MSecs: Integer);
  var
    FirstTickCount: Longint;
  begin
    FirstTickCount := GetTickCount;
    repeat
      Sleep(1);
      // or use Application.ProcessMessages instead of Sleep
    until ((GetTickCount - FirstTickCount) >= Longint(MSecs));
  end;

begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Winapi.Windows.Beep(aFreq, aDelay);
  end
  else
  begin
    DoSound(aFreq);
    Delay(aDelay);
  end;
end;

procedure NoSound;
var
  Value: Word;
begin
  if not(Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    Value := GetPort($61) and $FC;
    SetPort($61, Value);
  end;
end;

end.
