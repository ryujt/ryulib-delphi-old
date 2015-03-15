unit CompareBytes;

interface

uses
  Windows, SysUtils, Classes;

function CompareFastBytes(const Data1,Data2:Pointer; const DataSize:integer):boolean;
function CompareFastBytesEx(const Data1:Pointer; const Size1:integer; const Data2:Pointer; const Size2:integer): Integer;

implementation

function CompareSlowBytes(Data1,Data2:Pointer; DataSize:integer):boolean;
var
  Loop : integer;
  pItem1, pItem2 : ^Byte;
begin
  pItem1:= Data1;
  pItem2:= Data2;

  Result:= true;
  for Loop := 1 to DataSize do begin
    if pItem1^ <> pItem2^ then begin
      Result:= false;
      Break;
    end;
    Inc(pItem1, 1);
    Inc(pItem2, 1);
  end;
end;

function CompareFastBytes(const Data1,Data2:Pointer; const DataSize:integer):boolean;
var
  Loop : integer;
  pItem1, pItem2 : ^DWord;
begin
  pItem1:= Data1;
  pItem2:= Data2;

  for Loop := 1 to DataSize div 4 do begin
    if pItem1^ <> pItem2^ then begin
      Result:= false;
      Exit;
    end;
    Inc(pItem1, 1);
    Inc(pItem2, 1);
  end;

  Result:= CompareSlowBytes(pItem1, pItem2, DataSize mod 4);
end;

function CompareFastBytesEx(const Data1:Pointer; const Size1:integer; const Data2:Pointer; const Size2:integer): Integer;
var
  Loop, Size : integer;
  pWord1, pWord2 : ^DWord;
  pByte1, pByte2 : ^Byte;
begin
  Result := 0;

  if Size1 > Size2 then Size := Size2
  else Size := Size1;

  pWord1 := Data1;
  pWord2 := Data2;
  for Loop := 1 to Size div 4 do begin
    if pWord1^ <> pWord2^ then begin
      if pWord1^ > pWord2^ then Result := 1
      else  Result := -1;
      Exit;
    end;

    Inc(pWord1);
    Inc(pWord2);
  end;

  pByte1 := Pointer(pWord1);
  pByte2 := Pointer(pWord2);
  for Loop := 1 to Size mod 4 do begin
    if pByte1^ <> pByte2^ then begin
      if pByte1^ > pByte2^ then Result := 1
      else  Result := -1;
      Exit;
    end;

    Inc(pByte1);
    Inc(pByte2);
  end;

  if Size1 > Size2 then Result := 1
  else if Size1 < Size2 then Result := -1;
end;

{$IFDEF DEBUG}
var
  Item1 : word = $FFFF;
  Item2 : word = $FFFF;
  Item3 : word = $1230;
  Item4 : word = $1234;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  Assert(CompareFastBytesEx(@Item1, 1, @Item2, 1) = 0,  'CompareFastBytesEx(@Item1, 1, @Item2, 1) = 0');
  Assert(CompareFastBytesEx(@Item1, 1, @Item2, 2) = -1, 'CompareFastBytesEx(@Item1, 1, @Item2, 2) = -1');

  // 엔디안 문제로 실제 비교는 $3012 vs $3412 가 된다.
  Assert(CompareFastBytesEx(@Item3, 2, @Item4, 2) = -1, 'CompareFastBytesEx(@Item3, 2, @Item4, 2) = -1');
  {$ENDIF}
end.
