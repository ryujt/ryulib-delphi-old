unit SearchDir;

interface

uses
  Classes, SysUtils;

type
  TIterateProcedure = reference to procedure(Path:string; SearchRec:TSearchRec; var NeedStop:boolean);

{*
  지정 된 경로와 그 하위 폴더에 있는 모든 파일을 찾는다.
  ARecursive = false이면 하위 폴더는 찾지 않는다.
}
procedure SearchFiles(APath:string; ARecursive:boolean; AProcedure:TIterateProcedure);

{*
  지정 된 경로와 그 하위에 있는 폴더만 찾는다.
  ARecursive = false이면 하위 폴더는 찾지 않는다.
}
procedure SearchFolders(APath:string; ARecursive:boolean; AProcedure:TIterateProcedure);

implementation

procedure SearchFiles(APath:string; ARecursive:boolean; AProcedure:TIterateProcedure);
var
  SearchRec : TSearchRec;
  iSearchResult : integer;
  isDirectory : boolean;
  isNeedStop : boolean;
begin
  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';

  isNeedStop := false;

  iSearchResult := FindFirst(APath + '*.*', faAnyFile, SearchRec);
  while iSearchResult = 0 do begin
    // 디랙토리 중 무시해야할 것들
    if (SearchRec.Name = '.' ) or (SearchRec.Name = '..') then begin
      iSearchResult := FindNext(SearchRec);
      Continue;
    end;

    isDirectory := (SearchRec.Attr and faDirectory) = faDirectory;

    if isDirectory then begin
      if ARecursive then
        SearchFiles(APath + SearchRec.Name + '\', ARecursive, AProcedure);
    end else begin
      AProcedure(APath, SearchRec, isNeedStop);
      if isNeedStop then Break;
    end;

    iSearchResult:= FindNext(SearchRec);
  end;

  FindClose(SearchRec);
end;

procedure SearchFolders(APath:string; ARecursive:boolean; AProcedure:TIterateProcedure);
var
  SearchRec : TSearchRec;
  iSearchResult : integer;
  isDirectory : boolean;
  isNeedStop : boolean;
begin
  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';

  isNeedStop := false;

  iSearchResult := FindFirst(APath + '*.*', faAnyFile, SearchRec);
  while iSearchResult = 0 do begin
    // 디랙토리 중 무시해야할 것들
    if (SearchRec.Name = '.' ) or (SearchRec.Name = '..') then begin
      iSearchResult := FindNext(SearchRec);
      Continue;
    end;

    isDirectory := (SearchRec.Attr and faDirectory) = faDirectory;

    if isDirectory then begin
      AProcedure(APath, SearchRec, isNeedStop);

      if isNeedStop then Break;

      if ARecursive then
        SearchFolders(APath + SearchRec.Name + '\', ARecursive, AProcedure);
    end;

    iSearchResult:= FindNext(SearchRec);
  end;

  FindClose(SearchRec);
end;

end.
