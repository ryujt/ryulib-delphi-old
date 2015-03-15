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
