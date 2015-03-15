Unit Bit;


Interface

uses
  Windows, Classes, SysUtils;

// 각 Value와 같은 형태의 변수를 이진수로 표현되는 문자열로 바꿔준다.
Function  ByteToBinary(Value:Byte):AnsiString;
Function  WordToBinary(Value:Word):AnsiString;
Function  LongIntToBinary(Value:LongInt):AnsiString;

// 각 Value와 같은 형태의 변수를 16진수로 표현되는 문자열로 바꿔준다.
Function  DigitToHex(Value:Byte):AnsiChar;
Function  ByteToHex(Value:Byte):AnsiString;
Function  WordToHex(Value:Word):AnsiString;
Function  DWordToHex(Value:DWord):AnsiString;
Function  LongToHex(Value:LongInt):AnsiString;
Function  RealToHex(Value:Real):AnsiString;

// 이진수 또는 16진수를 Result와 같은 형태의 값으로 바꾸어 준다.
Function  BinaryToByte(Value:AnsiString):Byte;
Function  BinaryToWord(Value:AnsiString):Word;
Function  HexToByte(Value:AnsiString):Byte;
Function  HexToWord(Value:AnsiString):word;
Function  HexToDWord(Value:AnsiString):DWord;

Function  ArrayToWord(var Value):Word;
Function  ArrayToInt(var Value):Integer;

Implementation

Function  ByteToBinary(Value:Byte):AnsiString;
Var
   Loop : Integer;
Begin
  Result:= '';
  For Loop:= 7 downto 0 do
      If (Value and (1 shl Loop)) <> 0 then Result:= Result+'1'
      Else Result:= Result+'0';
End;

Function  WordToBinary(Value:Word):AnsiString;
Var
   Loop : Integer;
Begin
  Result:= '';
  For Loop:= 15 downto 0 do
      If (Value and (1 shl Loop)) <> 0 then Result:= Result+'1'
      Else Result:= Result+'0';
End;

Function  LongIntToBinary(Value:LongInt):AnsiString;
Var
   Loop : Integer;
Begin
  Result:= '';
  For Loop:= 31 downto 0 do
      If (Value and (1 shl Loop)) <> 0 then Result:= Result+'1'
      Else Result:= Result+'0';
End;

Function  DigitToHex(Value:Byte):AnsiChar;
Const
     HexStr : AnsiString = '0123456789ABCDEF';
Begin
  If Value in [0..15] then Result:= HexStr[Value+1]
  Else Result:= '*';
End;

Function  ByteToHex(Value:Byte):AnsiString;
Begin
  Result:= DigitToHex((Value and $F0) Shr 4)+
           DigitToHex( Value and $0F);
End;

Function  WordToHex(Value:Word):AnsiString;
Begin
  Result:= DigitToHex((Value and $F000) Shr 12)+
           DigitToHex((Value and $0F00) Shr  8)+
           DigitToHex((Value and $00F0) Shr  4)+
           DigitToHex( Value and $000F);
End;

function DWordToHex(Value:DWord):AnsiString;
var
  Temp : Word;
begin
  Temp:= Value shr 16;
  Result:= WordToHex(Temp);
  Temp:= Value and $FFFF;
  Result:= Result+WordToHex(Temp);
end;

Function  LongToHex(Value:LongInt):AnsiString;
Var
   Temp : Word;
Begin
  Temp:= Value Shr 16;
  Result:= WordToHex(Temp);
  Temp:= Value And $FFFF;
  Result:= Result+WordToHex(Temp);
End;

Function  RealToHex(Value:Real):AnsiString;
Var
   Loop : Byte;
   Data : Packed Array [1..6] of Byte;
Begin
  Move(Value, Data, 6);
  Result:= '';
  For Loop:= 1 to 6 Do Result:= Result+ByteToHex(Data[Loop]);
End;

function BinaryToByte(Value:AnsiString):byte;
var
   Loop : Integer;
begin
  Result := 0;

  Value := StringOfChar(' ', 8-Length(Value)) + Value;

  for Loop := 1 to 8 do
    Result := Result + ((Byte(Value[Loop])-48) shl (8-Loop)); 
end;

Function  BinaryToWord(Value:AnsiString):Word;
Var
   Loop : Integer;
Begin
  Result:= 0;
  For Loop:= 1 to 16-Length(Value) do Value:= '0'+Value;
  For Loop:= 1 to 16 do
      Result:= Result+Byte((Value[Loop] = '1')) shl 16-Loop;
End;

Function  HTB(Var Ch:AnsiChar):Byte;
Begin
  Ch:= UpCase(Ch);
  If Ch in ['0'..'9'] then HTB:=Byte(Ch)-48
  Else If Ch in ['A'..'F'] then HTB:=Byte(Ch)-55
  Else HTB:=0;  // 에러처리는 따로 하지 않았음
End;

Function  HexToByte(Value:AnsiString):Byte;
Var
   Loop, Mul, Size : Byte;
Begin
  Result:= 0;
  Size:= Length(Value);
  If Size > 2 then Exit;  // 에러처리는 따로 하지 않았음

  Mul:= 1;
  For Loop:= 1 to Size do
      Begin
        Result:= Result+HTB(Value[Size+1-Loop])*Mul;
        Mul:= Mul Shl 4;
      End;
End;

Function  HexToWord(Value:AnsiString):word;
Var
  Loop, Mul, Size : word;
Begin
  Result:= 0;
  Size:= Length(Value);
  If Size > 4 then Exit;  // 에러처리는 따로 하지 않았음

  Mul:= 1;
  For Loop:= 1 to Size do
      Begin
        Result:= Result+HTB(Value[Size+1-Loop])*Mul;
        Mul:= Mul Shl 4;
      End;
End;

function HexToDWord(Value:AnsiString):DWord;
var
  Loop, Mul, Size : Word;
begin
  Result:= 0;
  Size:= Length(Value);
  If Size > 4 then Exit;  // 에러처리는 따로 하지 않았음

  Mul:= 1;
  for Loop:= 1 to Size do begin
    Result:= Result + HTB(Value[Size+1-Loop])*Mul;
    Mul:= Mul shl 4;
  end;
end;

Function  ArrayToWord(var Value):Word;
Type
  TBuffer = Packed Array [1..2] of Byte;
var
  Buffer :^TBuffer;
Begin
  Buffer:= @Value;
  Result:= Buffer^[1] shl 8 + Buffer^[2];
End;

Function  ArrayToInt(var Value):Integer;
Type
  TBuffer = Packed Array [1..4] of Byte;
var
  Buffer :^TBuffer;
Begin
  Buffer:= @Value;
  Result:= Buffer^[1] shl 24 + Buffer^[2] shl 16 + Buffer^[3] shl 8 + Buffer^[4];
End;

End.
