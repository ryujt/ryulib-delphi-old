unit Cal;

{  적용날자
     1. 음력 : 1901. 2.19 - 2041. 2. 1
     2. 양력 : 1901. 1. 1 - 2040.12.30  }

interface

uses
  Classes, SysUtils, DateUtils;
  
type
  TWeek = (wkSunday, wkMonday, wkTuesday,
           wkWednesday, wkThursday, wkFriday, wkSaturday);

function GetDate(DateTime:TDateTime):TDateTime;
function GetTime(DateTime:TDateTime):TDateTime;

function EumToYaeng(var DateFrom,DateTo:TDateTime):boolean;
function YaengToEum(var DateFrom,DateTo:TDateTime):boolean;

function DayOfYear(Year:integer):integer;
function DayOfMonth(Year,Month:integer):integer;
function DayOfTwo(DateFrom,DateTo:TDateTime):integer;

function WeekOftheDay(Date:TDateTime):TWeek;
function GanJe(Year:integer):string;

function TenDigitTimeToDateTime(ADate,ATime:integer):TDateTime;
  
Implementation

Uses
  Strg;

Const
  Mm : Array[ 0..12 ] Of Word =
       (   0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365 );
  Vm : Array[ 0..6 ] Of Word =
       (   0,  29,  30,  58,  59,  59,  60 );
  Vs : Array[ 1..12 ] Of Word =
       (  31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30, 31 );
  Em : Array[ 1..140 , 1..13 ] Of Word =
       (
{  1}     (  1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{  2}     (  2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 355 ) ,
{  3}     (  1, 2, 1, 2, 3, 2, 1, 1, 2, 2, 1, 2, 383 ) ,
{  4}     (  2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 354 ) ,
{  5}     (  2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 355 ) ,
{  6}     (  1, 2, 2, 4, 1, 2, 1, 2, 1, 2, 1, 2, 384 ) ,
{  7}     (  1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 354 ) ,
{  8}     (  2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1, 2, 355 ) ,
{  9}     (  1, 5, 1, 2, 1, 2, 1, 2, 2, 2, 1, 2, 384 ) ,
{ 10}     (  1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{ 11}     (  2, 1, 2, 1, 1, 5, 1, 2, 2, 1, 2, 2, 384 ) ,
{ 12}     (  2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 354 ) ,
{ 13}     (  2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 354 ) ,
{ 14}     (  2, 2, 1, 2, 5, 1, 2, 1, 2, 1, 1, 2, 384 ) ,
{ 15}     (  2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 355 ) ,
{ 16}     (  1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 354 ) ,
{ 17}     (  2, 3, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 384 ) ,
{ 18}     (  2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 2, 355 ) ,
{ 19}     (  1, 2, 1, 1, 2, 1, 5, 2, 2, 1, 2, 2, 384 ) ,
{ 20}     (  1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 354 ) ,
{ 21}     (  2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 354 ) ,
{ 22}     (  2, 1, 2, 2, 3, 2, 1, 1, 2, 1, 2, 2, 384 ) ,
{ 23}     (  1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 354 ) ,
{ 24}     (  2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 354 ) ,
{ 25}     (  2, 1, 2, 5, 2, 1, 2, 2, 1, 2, 1, 2, 385 ) ,
{ 26}     (  1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{ 27}     (  2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 355 ) ,
{ 28}     (  1, 5, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 384 ) ,
{ 29}     (  1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 354 ) ,
{ 30}     (  1, 2, 2, 1, 1, 5, 1, 2, 1, 2, 2, 1, 383 ) ,
{ 31}     (  2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 354 ) ,
{ 32}     (  2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 355 ) ,
{ 33}     (  1, 2, 2, 1, 6, 1, 2, 1, 2, 1, 1, 2, 384 ) ,
{ 34}     (  1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 355 ) ,
{ 35}     (  1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{ 36}     (  2, 1, 4, 1, 2, 1, 2, 1, 2, 2, 2, 1, 384 ) ,
{ 37}     (  2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{ 38}     (  2, 2, 1, 1, 2, 1, 4, 1, 2, 2, 1, 2, 384 ) ,
{ 39}     (  2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 354 ) ,
{ 40}     (  2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 354 ) ,
{ 41}     (  2, 2, 1, 2, 2, 4, 1, 1, 2, 1, 2, 1, 384 ) ,
{ 42}     (  2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 2, 355 ) ,
{ 43}     (  1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 355 ) ,
{ 44}     (  1, 1, 2, 4, 1, 2, 1, 2, 2, 1, 2, 2, 384 ) ,
{ 45}     (  1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 354 ) ,
{ 46}     (  2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 354 ) ,
{ 47}     (  2, 5, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 384 ) ,
{ 48}     (  2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 354 ) ,
{ 49}     (  2, 2, 1, 2, 1, 2, 3, 2, 1, 2, 1, 2, 384 ) ,
{ 50}     (  2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 354 ) ,
{ 51}     (  2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 355 ) ,
{ 52}     (  1, 2, 1, 2, 4, 2, 1, 2, 1, 2, 1, 2, 384 ) ,
{ 53}     (  1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 355 ) ,
{ 54}     (  1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 354 ) ,
{ 55}     (  2, 1, 4, 1, 1, 2, 1, 2, 1, 2, 2, 2, 384 ) ,
{ 56}     (  1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 354 ) ,
{ 57}     (  2, 1, 2, 1, 2, 1, 1, 5, 2, 1, 2, 2, 384 ) ,
{ 58}     (  1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 354 ) ,
{ 59}     (  1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 354 ) ,
{ 60}     (  2, 1, 2, 1, 2, 5, 2, 1, 2, 1, 2, 1, 384 ) ,
{ 61}     (  2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 355 ) ,
{ 62}     (  1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{ 63}     (  2, 1, 2, 3, 2, 1, 2, 1, 2, 2, 2, 1, 384 ) ,
{ 64}     (  2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 355 ) ,
{ 65}     (  1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 354 ) ,
{ 66}     (  1, 2, 5, 2, 1, 1, 2, 1, 1, 2, 2, 1, 383 ) ,
{ 67}     (  2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 355 ) ,
{ 68}     (  1, 2, 2, 1, 2, 1, 5, 2, 1, 2, 1, 2, 384 ) ,
{ 69}     (  1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 354 ) ,
{ 70}     (  2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1, 2, 355 ) ,
{ 71}     (  1, 2, 1, 1, 5, 2, 1, 2, 2, 2, 1, 2, 384 ) ,
{ 72}     (  1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{ 73}     (  2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 1, 354 ) ,
{ 74}     (  2, 2, 1, 5, 1, 2, 1, 1, 2, 2, 1, 2, 384 ) ,
{ 75}     (  2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 354 ) ,
{ 76}     (  2, 2, 1, 2, 1, 2, 1, 5, 2, 1, 1, 2, 384 ) ,
{ 77}     (  2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 354 ) ,
{ 78}     (  2, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 355 ) ,
{ 79}     (  2, 1, 1, 2, 1, 6, 1, 2, 2, 1, 2, 1, 384 ) ,
{ 80}     (  2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 355 ) ,
{ 81}     (  1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 354 ) ,
{ 82}     (  2, 1, 2, 3, 2, 1, 1, 2, 2, 1, 2, 2, 384 ) ,
{ 83}     (  2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 354 ) ,
{ 84}     (  2, 1, 2, 2, 1, 1, 2, 1, 1, 5, 2, 2, 384 ) ,
{ 85}     (  1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 354 ) ,
{ 86}     (  1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 354 ) ,
{ 87}     (  2, 1, 2, 2, 1, 5, 2, 2, 1, 2, 1, 2, 385 ) ,
{ 88}     (  1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{ 89}     (  2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 355 ) ,
{ 90}     (  1, 2, 1, 1, 5, 1, 2, 2, 1, 2, 2, 2, 384 ) ,
{ 91}     (  1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 354 ) ,
{ 92}     (  1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 354 ) ,
{ 93}     (  1, 2, 5, 2, 1, 2, 1, 1, 2, 1, 2, 1, 383 ) ,
{ 94}     (  2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 355 ) ,
{ 95}     (  1, 2, 2, 1, 2, 2, 1, 5, 2, 1, 1, 2, 384 ) ,
{ 96}     (  1, 2, 1, 2, 2, 1, 2, 1, 2, 2, 1, 2, 355 ) ,
{ 97}     (  1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{ 98}     (  2, 1, 1, 2, 3, 2, 2, 1, 2, 2, 2, 1, 384 ) ,
{ 99}     (  2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{100}     (  2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 354 ) ,
{101}     (  2, 2, 2, 3, 2, 1, 1, 2, 1, 2, 1, 2, 384 ) ,
{102}     (  2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 354 ) ,
{103}     (  2, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 355 ) ,
{104}     (  1, 5, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 384 ) ,
{105}     (  1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 355 ) ,
{106}     (  1, 1, 2, 1, 2, 1, 5, 2, 2, 1, 2, 2, 384 ) ,
{107}     (  1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 354 ) ,
{108}     (  2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 354 ) ,
{109}     (  2, 2, 1, 1, 5, 1, 2, 1, 2, 1, 2, 2, 384 ) ,
{110}     (  2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 354 ) ,
{111}     (  2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 354 ) ,
{112}     (  2, 1, 6, 2, 1, 2, 1, 1, 2, 1, 2, 1, 384 ) ,
{113}     (  2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 355 ) ,
{114}     (  1, 2, 1, 2, 1, 2, 1, 2, 5, 2, 1, 2, 384 ) ,
{115}     (  1, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 355 ) ,
{116}     (  1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 354 ) ,
{117}     (  2, 1, 1, 2, 3, 2, 1, 2, 1, 2, 2, 2, 384 ) ,
{118}     (  1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 354 ) ,
{119}     (  2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 354 ) ,
{120}     (  2, 1, 2, 5, 2, 1, 1, 2, 1, 2, 1, 2, 384 ) ,
{121}     (  1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 354 ) ,
{122}     (  2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 355 ) ,
{123}     (  1, 5, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 384 ) ,
{124}     (  1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 354 ) ,
{125}     (  2, 1, 2, 1, 1, 5, 2, 1, 2, 2, 2, 1, 384 ) ,
{126}     (  2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 355 ) ,
{127}     (  1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 354 ) ,
{128}     (  1, 2, 2, 1, 5, 1, 2, 1, 1, 2, 2, 1, 383 ) ,
{129}     (  2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 355 ) ,
{130}     (  1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 354 ) ,
{131}     (  2, 1, 5, 2, 1, 2, 2, 1, 2, 1, 2, 1, 384 ) ,
{132}     (  2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 355 ) ,
{133}     (  1, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 2, 384 ) ,
{134}     (  1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 354 ) ,
{135}     (  2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 354 ) ,
{136}     (  2, 2, 1, 2, 1, 4, 1, 1, 2, 1, 2, 2, 384 ) ,
{137}     (  2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 354 ) ,
{138}     (  2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 354 ) ,
{139}     (  2, 2, 1, 2, 5, 2, 1, 2, 1, 2, 1, 1, 384 ) ,
{140}     (  2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2, 356 )
       );

Const
     Gan1 : Packed Array[1..12] Of String =
            ('자','축','인','묘','진','사','오','미','신','율','술','해');
     Gan2 : Packed Array[1..10] Of String =
            ('갑','을','병','정','무','기','경','신','임','계');


function GetDate(DateTime:TDateTime):TDateTime;
var
  y, m, d : word;
begin
  DecodeDate(DateTime, y, m, d);
  Result := EncodeDate(y, m, d);
end;

function GetTime(DateTime:TDateTime):TDateTime;
var
  h, n, s, z : word;
begin
  DecodeTime(DateTime, h, n, s, z);
  Result := EncodeTime(h, n, s, z);
end;

type
  TDate = record
    Year, Month, Day : word;
    Check : integer;
  end;

function EumToYaeng(var DateFrom,DateTo:TDateTime):boolean;
var
  Ch : char;
  Sum  : boolean;
  I, J, K, Ly, Lm, SW, SS, Ld, Lc, Sd, Sy, Sm, T : LongInt;
  S1, S2 : TDate;
begin
  DecodeDate(DateFrom, S1.Year, S1.Month, S1.Day);
  DecodeDate(DateTo,   S2.Year, S2.Month, S2.Day);

     Ly:=S1.Year;
     Lm:=S1.Month;
     Ld:=S1.Day;
     Lc:=S1.Check;
     If (Em[Ly,Lm] >= 3) And (Lc = 1) then Ch:='*' Else Ch:=' ';
     Result:= False;
     If Ly > 1900 then
        Begin
             Sum:=True;
             Ly :=Ly-1900;
        End
     Else Sum:=False;
     If (1 > Ly) And (Ly > 140) then Exit;
     If (1 > Lm) And (Lm > 12 ) then Exit;
     If (1 > Ld) And (Ld > 30 ) then Exit;
     Result:= True;
     T:=Ld+49;
     SS:=Ly+1900;
     For I:=1 to Ly-1 Do T:=T+Em[I,13];
     For I:=1 to Lm-1 Do T:=T+Vm[Em[LY,I]];
     If Ch = '*' then
     If (Em[LY,Lm] = 3) Or (Em[Ly,Lm] = 4) then T:=T+29
     Else T:=T+30;
     J:=T Div 1461;
     T:=T-1461*J;
     K:=T Div 365;
     T:=T-365*K;
     Sy:=4*J+K+1;
     If (Sy = 4*(Sy Div 4)) And (T > 59) then SW:=1
     Else SW:=0;
     Sm:=1;
     While (T > Mm[Sm]+Sw) Do Sm:=Sm+1;
     Sd:=T-Mm[Sm-1]-Sw;
     If (Sy = 4*(Sy Div 4)) And (T = 60) then Sd:=29;
     If Sd = 0 then
        Begin
             Sm:=Sm-1;
             If Sm < 1 then
                Begin
                     Sm:=12;
                     Sy:=Sy-1;
                End;
             Sd:=Vs[Sm];
             If Sm = 2 then
                Begin
                     If ((SS Mod 4) = 0)    And
                        ((SS Mod 100) <> 0) And
                        ((SS Mod 400) = 0 ) then Sd:=29
                     Else Sd:=28;
                End;
        End;
     If Sum then S2.Year:=Sy+1900
     Else S2.Year:=Sy;
     S2.Month:=Sm;
     S2.Day  :=Sd;
     S2.Check:= 0;
end;

function YaengToEum(var DateFrom,DateTo:TDateTime):boolean;
var
  Ch : char;
  Sum : boolean;
  Ly, Lm, Ld, Sd, Sy, Sm, T : LongInt;
  S1, S2 : TDate;
begin
  DecodeDate(DateFrom, S1.Year, S1.Month, S1.Day);
  DecodeDate(DateTo,   S2.Year, S2.Month, S2.Day);

     Sy:=S1.Year;
     Sm:=S1.Month;
     Sd:=S1.Day;
     Result:= False;
     IF Sy > 1900 then
        Begin
             Sum:=True;
             Sy :=Sy-1900;
        End
     Else Sum:=False;
     If (1 > Sy) And (Sy > 140) Then Exit;
     If (1 > Sm) And (Sm > 12 ) Then Exit;
     If (1 > Sd) And (Sd > 30 ) Then Exit;
     Result:= True;
     T:=365*(Sy-1)+Mm[Sm-1]+Sd+(Sy Div 4);
     If (Sy = 4*(Sy div 4)) And (30*Sm+Sd < 90) then T:=T+1;
     T:=T-49;
     Ly:=1;
     While (T > Em[Ly,13]) Do
           Begin
                T:=T-Em[Ly,13];
                Ly:=Ly+1;
           End;
     Lm:=1;
     While (T > Vm[Em[Ly,Lm]]) Do
           Begin
                T:=T-Vm[Em[Ly,Lm]];
                Lm:=Lm+1;
           End;
     Ld:=T;
     Ch:=' ';
     If ((Em[Ly,Lm] = 3) Or (Em[Ly,Lm] = 4)) And (T > 29) then
        Begin
             Ld:=T-29;
             Ch:='*';
        End;
     If ((Em[Ly,Lm] = 5) Or (Em[Ly,Lm] = 6)) And (T > 30) then
        Begin
             Ld:=T-30;
             Ch:='*';
        End;
     If Sum then S2.Year:=Ly+1900
     Else S2.Year:=Ly;
     S2.Month:=Lm;
     S2.Day  :=Ld;
     If Ch = '*' then S2.Check:=1
     Else S2.Check:=0;
End;

function DayOfYear(Year:integer):integer;
begin
  If ((Year Mod 4) = 0) then
     Begin
       If ((Year Mod 100) = 0) then
          Begin
            If ((Year Mod 400) = 0) then Result :=366
            Else Result :=365;
          End
        Else Result :=366;
       End
  Else Result :=365;
end;

function DayOfMonth(Year,Month:integer):integer;
begin
  case Month Of
       2 : if DayOfYear(Year) = 365 then Result := 28
           else Result := 29;
       1, 3, 5, 7, 8, 10, 12 : Result := 31;
       4, 6, 9, 11           : Result := 30;
       else
         raise Exception.Create('DayOfMonth: 지정한 날짜에 에러가 있습니다.');
  end;
end;

function DayOfTwo(DateFrom,DateTo:TDateTime):integer;
var
  Loop : Integer;
  S1, S2 : TDate;
begin
  DecodeDate(DateFrom, S1.Year, S1.Month, S1.Day);
  DecodeDate(DateTo,   S2.Year, S2.Month, S2.Day);

  Result:= 0;

  for Loop:= S1.Year to S2.Year-1 do
    Result:= Result + DayOfYear(Loop);

  for Loop:= 1 to S1.Month-1 do
    Result:= Result - DayOfMonth(S1.Year, Loop);

  Result:= Result - S1.Day;

  for Loop:= 1 to S2.Month-1 do
    Result:= Result + DayOfMonth(S2.Year, Loop);

  Result:= Result + S2.Day;
end;

function WeekOftheDay(Date:TDateTime):TWeek;
var
  DayCount, Loop : LongInt;
  S1 : TDate;
begin
  DecodeDate(Date, S1.Year, S1.Month, S1.Day);

  with S1 do begin
    DayCount:=0;
    For Loop:=0 to Year-1  Do DayCount:=DayCount+DayOfYear(Loop);
    For Loop:=1 to Month-1 Do DayCount:=DayCount+DayOfMonth(Year, Loop);
    DayCount:=(DayCount+Day-3) Mod 7;
    Result:= TWeek(DayCount);
  end;
end;

function GanJe(Year:integer):string;
var
  I1, I2 : Integer;
begin
     I1 :=(Year + 2337) mod 10;
     I2 :=(Year + 2337) mod 12;
     If I1 = 0 then I1 :=10;
     If I2 = 0 then I2 :=12;
     GanJe := Gan2[I1] + Gan1[I2];
end;

function TenDigitTimeToDateTime(ADate,ATime:integer):TDateTime;
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond : Word;
begin
  Year := ADate div 10000;
  Month := (ADate div 100) mod 100;
  Day := ADate mod 100;

  Hour := ATime div 10000;
  Minute := (ATime div 100) mod 100;
  Second := ATime mod 100;
  MilliSecond := 0;

  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, MilliSecond);
end;

begin
  {$IFDEF DEBUG}
    Assert(FormatDateTime('yyyymmddhhnnss', TenDigitTimeToDateTime(20110630, 123456)) = '20110630123456', 'Error: TenDigitTimeToDateTime');
  {$ENDIF}
end.


