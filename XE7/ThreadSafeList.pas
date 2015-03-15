unit ThreadSafeList;

interface

uses
  Classes, SysUtils;

type
  TThreadSafeList = class (TThreadList)
  private
    function GetCount: integer;
  public
    function GetFirst:pointer;
    function RemoveFirst:pointer;

    procedure ClearAndFree;

    property Count : integer read GetCount;
  end;

implementation

{ TThreadSafeList }

procedure TThreadSafeList.ClearAndFree;
var
  List : TList;
  Loop: Integer;
begin
  List := LockList;
  try
    for Loop := 0 to List.Count-1 do TObject(List[Loop]).Free;
    List.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadSafeList.GetCount: integer;
var
  List : TList;
begin
  List := LockList;
  try
    Result := List.Count;
  finally
    UnlockList;
  end;
end;

function TThreadSafeList.GetFirst: pointer;
var
  List : TList;
begin
  List := LockList;
  try
    if List.Count = 0 then Result := nil
    else Result := List[0];
  finally
    UnlockList;
  end;
end;

function TThreadSafeList.RemoveFirst: pointer;
var
  List : TList;
begin
  List := LockList;
  try
    if List.Count = 0 then begin
      Result := nil
    end else begin
      Result := List[0];
      List.Delete(0);
    end;
  finally
    UnlockList;
  end;
end;

end.
