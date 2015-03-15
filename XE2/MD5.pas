unit MD5;

interface

uses
  Wcrypt2,
  Classes, SysUtils;

type
  TMD5 = packed array [0..15] of byte;
  PMD5 = ^TMD5;

var
  IsWorking : boolean = false;

procedure GetMD5(var AMD5:TMD5; AData:pointer; ASize:integer);
function GetMD5String(AData:pointer; ASize:integer): string; overload;
function GetMD5String(AText:string): string; overload;

implementation

var
  hCryptProvider: HCRYPTPROV;
  hHash: HCRYPTHASH;
  bHash: array[0..$7f] of Byte;
  dwHashBytes: Cardinal;
  pbContent: PByte;

procedure GetMD5(var AMD5:TMD5; AData:pointer; ASize:integer);
begin
  dwHashBytes := SizeOf(TMD5);
  pbContent := AData;

  if not CryptCreateHash(hCryptProvider, CALG_MD5, 0, 0, @hHash) then Exit;
  try
    if CryptHashData(hHash, pbContent, ASize, 0) then begin
      if CryptGetHashParam(hHash, HP_HASHVAL, @bHash[0], @dwHashBytes, 0) then begin
        Move(bHash[0], AMD5, SizeOf(TMD5));
      end;
    end;
  finally
    CryptDestroyHash(hHash);
  end;
end;

function GetMD5String(AData: pointer; ASize: integer): string;
var
  Loop : integer;
begin
  dwHashBytes := SizeOf(TMD5);
  pbContent := AData;

  Result := '';

  if not CryptCreateHash(hCryptProvider, CALG_MD5, 0, 0, @hHash) then Exit;
  try
    if CryptHashData(hHash, pbContent, ASize, 0) then begin
      if CryptGetHashParam(hHash, HP_HASHVAL, @bHash[0], @dwHashBytes, 0) then begin
        for Loop := 0 to dwHashBytes - 1 do Result := Result + Format('%.2x', [bHash[Loop]]);
      end;
    end;
  finally
    CryptDestroyHash(hHash);
  end;
end;

function GetMD5String(AText:string): string;
var
  ssData : TStringStream;
begin
  ssData := TStringStream.Create(AText);
  try
    Result := GetMD5String(ssData.Memory, ssData.Size);
  finally
    ssData.Free;
  end;
end;

initialization
  IsWorking := CryptAcquireContext(@hCryptProvider, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT or CRYPT_MACHINE_KEYSET);

finalization
  if IsWorking then CryptReleaseContext(hCryptProvider, 0);
end.
