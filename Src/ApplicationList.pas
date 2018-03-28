unit ApplicationList;

interface

uses
  Windows, Classes, SysUtils, Variants;

type
  TApplicationList = class (TComponent)
  private
    FList : TStringList;
    function GetCount: integer;
    function GetText: string;
    function GetNames(Index: integer): string;
    function GetHandles(Index: integer): THandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update;

    property Handles[Index:integer] : THandle read GetHandles;
    property Names[Index:integer] : string read GetNames;
  published
    property Count : integer read GetCount;
    property Text : string read GetText;
  end;

implementation

{ TApplicationList }

constructor TApplicationList.Create(AOwner: TComponent);
begin
  inherited;

  FList := TStringList.Create;
end;

destructor TApplicationList.Destroy;
begin
  FList.Free;

  inherited;
end;

function TApplicationList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TApplicationList.GetHandles(Index: integer): THandle;
begin
  Result := StrToIntDef(FList.Names[Index], 0);
end;

function TApplicationList.GetNames(Index: integer): string;
begin
  Result := FList.ValueFromIndex[Index];
end;

function TApplicationList.GetText: string;
begin
  Result := FList.Text;
end;

function EnumWindowsProc(Handle:HWND; Param:integer):boolean; stdcall;
var
  Style : DWORD;
  Caption: array [0..255] of Char;
  AppList : TApplicationList;
  isVisible, isToolWin, isAppWin, isOwned: Boolean;
begin
  Result := true;

  if Handle = NULL then begin
    Result := false;
    Exit;
  end;

  Style := GetWindowLong(Handle, GWL_EXSTYLE);
  isVisible := IsWindowVisible(Handle);
  isToolWin := (Style and WS_EX_TOOLWINDOW) = WS_EX_TOOLWINDOW;
  isAppWin := (Style and WS_EX_APPWINDOW) = WS_EX_APPWINDOW;
  isOwned := GetWindow(Handle, GW_OWNER) > 0;

  if(not (isVisible and (isAppWin or (not isToolWin and not isOwned)))) then Exit;

  if GetWindowText(Handle, Caption, 256) > 0 then begin
    AppList := TApplicationList(Param);
    AppList.FList.Values[IntToStr(Handle)] := StrPas(Caption);
  end;
end;

procedure TApplicationList.Update;
begin
  FList.Clear;
  EnumWindows(@EnumWindowsProc, Integer(Self));
end;

end.
