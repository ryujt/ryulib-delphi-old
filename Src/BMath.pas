unit BMath;

interface

uses
  SysUtils, Math;

var
  IVar : packed array[0..50] of Real;

function CReal(x : Real) : Boolean;
function Tan(x : Real) : Real;
function ArcCos(x : Real) : Real;
function ArcSin(x : Real) : Real;
function Fact(n : Integer) : Real;
function nPr(n, r : Integer) : Real;
function nCr(n, r : Integer) : Real;
function nHr(n, r : Integer) : Real;
function nTr(n, r : Integer) : Real;
function Sinh(x : Real) : Real;
function Cosh(x : Real) : Real;
function Tanh(x : Real) : Real;
function Pol(x, y : Real) : Real;
function Log(x : Real) : Real;
function Lg(x, y : Real) : Real;
function Max(x, b : Real) : Real;
function Min(x, b : Real) : Real;
procedure Bound(var X : Integer; BS, BE : Integer);
function MaxBound(x, b : Integer) : Integer;
function MinBound(x, b : Integer) : Integer;
function BoxIn(DataX, DataY, X1, Y1, X2, Y2 : Integer) : Boolean;

implementation

function CReal;
var
  Logic : real;
begin
  Logic := x - round(x);
  if Logic = 0 then CReal := False
  else CReal := True
end;

function Tan;
begin
  Tan := sin(x) / cos(x)
end;

function ArcCos;
begin
  if x <> 0 then ArcCos := ArcTan(sqrt(1 / sqr(x) - 1))
  else ArcCos := pi / 2;
end;

function ArcSin;
begin
  if x <> 1 then ArcSin := ArcTan(sqrt(1 / (1 - sqr(x)) - 1))
  else ArcSin := 0;
end;

function Fact;
var
  F1 : Real;
begin
  F1 := 1;
  for n := 1 to n do F1 := F1 * n;
  Fact := F1
end;

function nPr;
begin
  nPr := Fact(n) / Fact(n - r)
end;

function nCr;
begin
  nCr := nPr(n, r) / Fact(r)
end;

function nHr;
begin
  nHr := nCr(n + r - 1, r)
end;

function nTr;
begin
  nTr := Power(n, r);
end;

function Sinh;
begin
  sinh := (Exp(x) - Exp(-x)) / 2
end;

function Cosh;
begin
  Cosh := (Exp(x) + Exp(-x)) / 2
end;

function Tanh;
begin
  Tanh := Sinh(x) / Cosh(x)
end;

function Pol;
begin
  Pol := Sqrt(sqr(x) + sqr(y))
end;

function Log;
begin
  Log := Ln(x) / Ln(10)
end;

function Lg;
begin
  Lg := Ln(y) / ln(x)
end;

function Max;
begin
  if x > b then Max := b
  else Max := x;
end;

function Min;
begin
  if x < b then Min := b
  else Min := x;
end;

procedure Bound(var X : Integer; BS, BE : Integer);

begin
  if X > BE then X := BE;
  if X < BS then X := BS;
end;

function Maxbound;
begin
  if x > b then Maxbound := b
  else Maxbound := x;
end;

function Minbound;
begin
  if x < b then Minbound := b
  else Minbound := x;
end;

function BoxIn(DataX, DataY, X1, Y1, X2, Y2 : integer) : boolean;
begin
  Result :=
    ((DataX >= X1) and (DataX <= X2)) and
    ((DataY >= Y1) and (DataY <= Y2));
end;

end.

