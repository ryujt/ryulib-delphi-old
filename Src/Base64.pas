unit Base64;

interface

uses
  SysUtils, IdCoder, IdCoder3to4, IdCoderMIME;

function Base64Decode(const Text : string): string;
function Base64Encode(const Text : string): string;

implementation

function Base64Decode(const Text : string): string;
var
  Decoder : TIdDecoderMime;
begin
  Decoder := TIdDecoderMime.Create(nil);
  try
    Result := Decoder.DecodeString(Text);
  finally
    FreeAndNil(Decoder)
  end
end;

function Base64Encode(const Text : string): string;
var
  Encoder : TIdEncoderMime;
begin
  Encoder := TIdEncoderMime.Create(nil);
  try
    Result := Encoder.EncodeString(Text);
  finally
    FreeAndNil(Encoder);
  end
end;

end.

const Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function Encode64(S: AnsiString): AnsiString;
var
	i: Integer;
	a: Integer;
	x: Integer;
	b: Integer;
begin
	Result := '';

	a := 0;
	b := 0;

	for i := 1 to Length(s) do begin
		x := Ord(s[i]);
		b := b * 256 + x;
		a := a + 8;
		while (a >= 6) do begin
			a := a - 6;
			x := b div (1 shl a);
			b := b mod (1 shl a);
			Result := Result + copy(Codes64,x + 1,1);
		end;
	end;

	if a > 0 then	begin
		x := b shl (6 - a);
		Result := Result + copy(Codes64,x + 1,1);
	end;

	a := Length(Result) mod 4;
	if a = 2 then Result := Result + '=='
	else if a = 3 then Result := Result + '=';
end;

function Decode64(S: AnsiString): AnsiString;
var
	i: Integer;
	a: Integer;
	x: Integer;
	b: Integer;
begin
	Result := '';

	a := 0;
	b := 0;

	for i := 1 to Length(s) do begin
		x := Pos(s[i], codes64) - 1;
		if x >= 0 then begin
			b := b * 64 + x;
			a := a + 6;
			if a >= 8 then begin
				a := a - 8;
				x := b shr a;
				b := b mod (1 shl a);
				x := x mod 256;
				Result := Result + chr(x);
			end;
		end else Exit; // finish at unknown
	end;
end
