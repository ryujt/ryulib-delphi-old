unit MD5;

interface

uses
  Wcrypt2, IdHashMessageDigest, idHash,
  Classes, SysUtils;

type
  TMD5 = packed array [0..15] of byte;
  PMD5 = ^TMD5;

var
  IsWorking : boolean = false;

function FileToMD5(const AFileName:string):string;
procedure GetMD5(var AMD5:TMD5; AData:pointer; ASize:integer);
function GetMD5String(AData:pointer; ASize:integer): string; overload;
function GetMD5String(const AText:string): string; overload;

implementation

function FileToMD5(const AFileName:string):string;
var
  IdMD5: TIdHashMessageDigest5;
  FS: TFileStream;
begin
  try
    IdMD5 := TIdHashMessageDigest5.Create;
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
       Result := IdMD5.HashStreamAsHex(FS)
    finally
      FS.Free;
      IdMD5.Free;
    end;
  except
    Result := '';
  end;
end;

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

function GetMD5String(const AText:string): string;
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
