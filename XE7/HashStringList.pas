{
  * 출처 : http://www.codeway.co.kr/board/bbs/tb.php/Delphi_PDS/414
  * 제작자 : 미노
}

unit HashStringList;

interface

uses
  Classes, SysUtils, IniFiles;

type
  THashStringList = class(TStringList)
  private
    FValueHash: TStringHash;
    FHashSize: Integer;
    procedure AddHash(const S: string; Index: Integer);
    procedure DeleteHash(Index: Integer);
  protected
  public
    constructor Create(HashSize: Integer);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    function Contains(const S: string): Boolean;
    procedure Remove(const S: string);
  end;

implementation

{ THashStringList }

function THashStringList.Add(const S: string): Integer;
begin
  Result:= inherited Add(S);
  AddHash(S, Result);
end;

procedure THashStringList.AddHash(const S: string; Index: Integer);
begin
  FValueHash.Add(S, Index);
end;

function THashStringList.Contains(const S: string): Boolean;
begin
  Result:= IndexOf(S) <> -1;
end;

constructor THashStringList.Create(HashSize: Integer);
begin
  // 대소문자를 구별한다.
  CaseSensitive:= True;
  FHashSize:= HashSize;
  FValueHash := TStringHash.Create(FHashSize);
end;

procedure THashStringList.Delete(Index: Integer);
begin
  DeleteHash(Index);
  inherited Delete(Index);
end;

procedure THashStringList.DeleteHash(Index: Integer);
begin
  Remove(Self[Index]);
end;

destructor THashStringList.Destroy;
begin
  FValueHash.Free;
  inherited Destroy;
end;

function THashStringList.IndexOf(const S: string): Integer;
begin
  Result := FValueHash.ValueOf(S);
end;

procedure THashStringList.Remove(const S: string);
begin
  FValueHash.Remove(S);
end;

end.
