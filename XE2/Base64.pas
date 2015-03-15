unit Base64;

interface

uses
  SysUtils, IdCoder, IdCoder3to4, IdCoderMIME;

function Base64Decode(const Text : string; AEncoding:TEncoding = nil): string;
function Base64Encode(const Text : string; AEncoding:TEncoding = nil): string;

implementation

function Base64Decode(const Text : string; AEncoding:TEncoding): string;
var
  Decoder : TIdDecoderMime;
begin
  Decoder := TIdDecoderMime.Create(nil);
  try
    Result := Decoder.DecodeString(Text, AEncoding);
  finally
    FreeAndNil(Decoder)
  end
end;

function Base64Encode(const Text : string; AEncoding:TEncoding): string;
var
  Encoder : TIdEncoderMime;
begin
  Encoder := TIdEncoderMime.Create(nil);
  try
    Result := Encoder.EncodeString(Text, AEncoding);
  finally
    FreeAndNil(Encoder);
  end
end;

end.
