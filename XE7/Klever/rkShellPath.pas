unit rkShellPath;

//  ShellPath by Roy Magne Klever
//  © 2010 by Roy Magne Klever. All rights reserved
//
//  WEB: www.rmklever.com
//  Mail: roymagne@rmklever.com
//
//  Portions of this software was written by
//  Gerald Koeder
//  WEB: www.innovative-bytes.net
//
//  version 1.0, October 2010
//
//  Licensed under MPL 1.1 licence

interface

uses
  SysUtils, Classes, rkPathViewer, Menus, Forms, ComCtrls, ShellApi,
  ShlObj, ActiveX, Dialogs, Windows, ImgList, StdCtrls;

const
  // Need IE5.5 installed
  SHACF_DEFAULT = $0;
  SHACF_FILESYSTEM = $1;
  SHACF_URLHISTORY = $2;
  SHACF_URLMRU = $4;
  SHACF_URLALL = (SHACF_URLHISTORY Or SHACF_URLMRU);
  SHACF_FILESYS_ONLY = $10;
  SHACF_FILESYS_DIRS = $20;
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;
  // ---

type
  // Object which holds all the needed Shellinfos and will be later
  // attached to the StringList as Object.
  tShellObject = class(tObject)
    fPIDL: pItemIDList;
    fDisplayname: String;
    fIconIndex: Integer;
  private
    Procedure SetAbsolutePIDL(Value: pItemIDList);
  public
    constructor Create;
    destructor Destroy; override;
    Property AbsolutePIDL: pItemIDList read fPIDL write SetAbsolutePIDL;
    Property IconIndex: Integer read fIconIndex;
    property DisplayName: String read fDisplayname;
  end;

  // Main-Class for the Parser
  tShellParser = Class(tObject)
  private
    // Holds the Path-Items
    FPathList: TStringList;
    // Holds the Sub-Items (if any ...)
    FSubPathList: TStringList;
  public
    FDesktop: String;
    constructor Create;
    destructor Destroy; override;
    procedure EnumSubPaths(PIDL: pItemIDList);
    procedure LoadPath(Path: string);
    procedure LoadPIDL(PIDL: pItemIDList);
    property PathList: TStringList read FPathList write FPathList;
    property SubPathList: TStringList read FSubPathList write FSubPathList;
  End;

  TrkShellPath = class(TComponent)
  private
    { Private declarations }
    FAutoComplete: Boolean;
    FImages: TCustomImageList;
    FPath: String;
    FPathViewer: TrkPathViewer;
    FPopPath: String;
    FShellParser: tShellParser;
    procedure EditedPath(Sender: tObject; var APath: string);
    procedure PathClick(Sender: tObject; PathIdx: Integer);
    procedure PathEdit(Sender: tObject; AEdit: TEdit; var APath: string);
    procedure PathPopup(Sender: tObject; var Path: string; Off, Idx: Integer;
      var APopupMenu: TPopupMenu);
    procedure PathPopupClick(Sender: tObject);
    procedure UpdatePath;
    procedure SetPathViewer(const Value: TrkPathViewer);
    procedure SetPath(const Value: String);
    function SysImageListHandle(const Path: string;
      const WantLargeIcons: Boolean): Windows.THandle;
    function GetPIDL: pItemIDList;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure LoadPIDL(PIDL: pItemIDList);
    property PIDL: pItemIDList read GetPIDL;
  published
    { Published declarations }
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete
      default True;
    property PathViewer: TrkPathViewer read FPathViewer write SetPathViewer;
    property Path: String read FPath write SetPath;
  end;

const
  Shell32 = 'shell32.dll';

procedure Register;

implementation

{$WARN SYMBOL_PLATFORM OFF}

type
  TFolderNameType = (fnNormal, fnInFolder, fnForEditing, fnForAddressBar,
    fnForParsing);

function ILClone(PIDL: pItemIDList): pItemIDList; stdcall;
external Shell32 index 18;

function ILCombine(pidl1, pidl2: pItemIDList): pItemIDList; stdcall;
external Shell32 index 25;

function ILRemoveLastID(PIDL: pItemIDList): LongBool; stdcall;
external Shell32 index 17;

function SHAutoComplete(hwndEdit: HWND; dwFlags: dWord): LongInt; stdcall;
external 'shlwapi.dll';

Procedure FreeItemIDList(Var aPIdL: pItemIDList);
Var
  ppMalloc: iMalloc;
Begin
  SHGetMalloc(ppMalloc);
  ppMalloc.Free(aPIdL);
  aPIdL := nil;
  ppMalloc := nil;
End;

var
  Folder: IShellFolder;

  // Modified PidlToPath to get also a path-Sting for Network-Folders, ...
function PidlToPath(IdList: pItemIDList): String;
Var
  ItemIDListLast: pItemIDList;
  ShellFolder: IShellFolder;

  function GetFolderName(Folder: IShellFolder; PIDL: pItemIDList;
    tp: TFolderNameType): WideString;
  var
    t: dWord;
    str: TStrRet;
  begin
    case tp of
      fnInFolder:
        t := SHGDN_INFOLDER;
      fnForEditing:
        t := SHGDN_FOREDITING;
      fnForAddressBar:
        t := SHGDN_FORADDRESSBAR;
      fnForParsing:
        t := SHGDN_FORPARSING;
    else
      t := SHGDN_NORMAL;
    end;
    if Succeeded(Folder.GetDisplayNameOf(PIDL, t, str)) then
      case str.uType of
        STRRET_CSTR:
          result := str.cStr;
        STRRET_WSTR:
          begin
            result := str.pOleStr;
            CoTaskMemFree(str.pOleStr)
          end;
        STRRET_OFFSET:
          result := PChar(PIDL) + str.uOffset;
      else
        result := ''
      end
    else
      result := '';
  end;

begin
  result := '';
  if assigned(IdList) then
  Begin
    // First, try to get the name using the GetFolderName-Function
    if Succeeded(SHBindToParent(IdList, IID_IShellFolder, Pointer(ShellFolder),
        ItemIDListLast)) then
      result := GetFolderName(ShellFolder, ItemIDListLast, fnForParsing);
    // If the Result was a GUID-String (Desktop for example) or the Result was
    // empty, try SHGetPathFromIdList
    if (Pos('::', result) = 1) or (result = '') then
    Begin
      result := '';
      SetLength(result, MAX_PATH);
      if SHGetPathFromIdList(IdList, PChar(result)) then
      begin
        SetLength(result, StrLen(PChar(result)));
        result := IncludeTrailingBackslash(result);
      end
      else
        result := '';
    End;
  End;
end; // PidlToPath

// Expands an env-string like %appdata% into the correct path
function ExpandEnvStr(const szInput: string): string;
const
  MAXSIZE = 32768;
begin
  SetLength(result, MAXSIZE);
  SetLength(result, ExpandEnvironmentStrings(PChar(szInput), @result[1],
      length(result)));
end;

// Get the iUnknown-Interface from a given pItemIDList
function GetInterfaceForObj(const ItemIDList: pItemIDList): IUnknown;
var
  Desktop: IShellFolder;
  DesktopIDList: pItemIDList;
begin
  SHGetDesktopFolder(Desktop);
  try
    // Check if the current pItemIDList is the Desktop (needs special handling)
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopIDList);
    Try
      If Desktop.CompareIDs(0, DesktopIDList, ItemIDList) = 0 then
        result := Desktop
      else
        Desktop.BindToObject(ItemIDList, nil, IID_IShellFolder, result);
    Finally
      if assigned(DesktopIDList) then
        FreeItemIDList(DesktopIDList);
    End;
  finally
    Desktop := nil;
  end;
end;

procedure Register;
begin
  RegisterComponents('rmKlever', [TrkShellPath]);
end;

{ TrkShellPath }

procedure TrkShellPath.AfterConstruction;
var
  TmpHandle: THandle;
begin
  inherited;
  TmpHandle := SysImageListHandle('', False);
  if TmpHandle <> 0 then
  begin
    FImages.Handle := TmpHandle;
    FImages.ShareImages := True;
  end;
end;

constructor TrkShellPath.Create(AOwner: TComponent);
begin
  inherited;
  FAutoComplete := True;
  FShellParser := tShellParser.Create;
  FImages := TCustomImageList.Create(nil);
  FImages.DrawingStyle := dsTransparent;
end;

destructor TrkShellPath.Destroy;
begin
  FImages.Free;
  FShellParser.Free;
  inherited;
end;

procedure TrkShellPath.EditedPath(Sender: tObject; var APath: string);
var
  s: String;
begin
  s := Path;
  FShellParser.LoadPath(APath);
  if FShellParser.PathList.Count = 0 then
    FShellParser.LoadPath(s);
  UpdatePath;
end;

function TrkShellPath.GetPIDL: pItemIDList;
begin
  if FShellParser.PathList.Count > 0 then
    result := tShellObject(FShellParser.PathList.Objects[0]).AbsolutePIDL
  else
    result := nil;
end;

procedure TrkShellPath.LoadPIDL(PIDL: pItemIDList);
begin
  FShellParser.LoadPIDL(ILClone(PIDL));
  UpdatePath;
end;

procedure TrkShellPath.PathClick(Sender: tObject; PathIdx: Integer);
var
  i: Integer;
begin
  i := FShellParser.PathList.Count - PathIdx - 1;
  FShellParser.LoadPIDL(ILClone(tShellObject(FShellParser.PathList.Objects[i])
        .AbsolutePIDL));
  UpdatePath;
end;

procedure TrkShellPath.PathEdit(Sender: TObject; AEdit: TEdit;
  var APath: string);
var
  i: Integer;
  s: String;
begin
  if FAutoComplete then
    SHAutoComplete(AEdit.Handle,
      SHACF_FILESYS_DIRS or SHACF_URLHISTORY or SHACF_URLMRU or
        SHACF_AUTOSUGGEST_FORCE_ON or SHACF_AUTOAPPEND_FORCE_ON);
  APath:= PidlToPath(tShellObject(FShellParser.PathList.Objects[0]).AbsolutePIDL);
  if APath = '' then
  begin
    APath:= Path;
    i:= Length(FShellParser.FDesktop);
    if Length(APath) > i then
    begin
      s:= Copy(APath, 1, i);
      if UpperCase(s) = FShellParser.FDesktop then
        Delete(APath, 1, i);
      if APath[1] = '\' then
        Delete(APath, 1, 1);
    end;
  end;
end;

procedure TrkShellPath.PathPopup(Sender: TObject; var Path: string;
  Off, Idx: Integer; var APopupMenu: TPopupMenu);
var
  i, Aidx: Integer;
  AMenuItem: TMenuItem;
  aPIdL: pItemIDList;

  procedure AddMenuItem(FName: string; Icon: Integer);
  begin
    AMenuItem := TMenuItem.Create(APopupMenu);
    AMenuItem.Caption := FName;
    AMenuItem.ImageIndex := Icon;
    AMenuItem.Tag := Aidx;
    Inc(Aidx);
    APopupMenu.Items.Add(AMenuItem);
  end;

begin
  i := FShellParser.PathList.Count - Idx;
  if Path = '*' then
  begin
    if (Off > 0) then
    begin
      Aidx := -(Off);
      for i := FShellParser.PathList.Count -
        Off to FShellParser.PathList.Count - 1 do
      begin
        AddMenuItem(tShellObject(FShellParser.PathList.Objects[i]).DisplayName,
          tShellObject(FShellParser.PathList.Objects[i]).IconIndex);
      end;
      AddMenuItem('-', -1);
    end;
    Aidx := 0;
    ShGetFolderLocation(0, CSIDL_DESKTOP, 0, 0, aPIdL);
    FShellParser.EnumSubPaths(aPIdL);
    FreeItemIDList(aPIdL);
    for i := 0 to FShellParser.SubPathList.Count - 1 do
    begin
      AddMenuItem(tShellObject(FShellParser.SubPathList.Objects[i])
        .DisplayName, tShellObject(FShellParser.SubPathList.Objects[i])
        .IconIndex);
    end;
  end
  else
  begin
    Aidx := 0;
    i := i - 1;
    FShellParser.EnumSubPaths(tShellObject(FShellParser.PathList.Objects[i])
        .AbsolutePIDL);
    for i := 0 to FShellParser.SubPathList.Count - 1 do
    begin
      AddMenuItem(tShellObject(FShellParser.SubPathList.Objects[i])
          .DisplayName, tShellObject(FShellParser.SubPathList.Objects[i])
          .IconIndex);
    end;
  end;
  FPopPath := Path;
end;

procedure TrkShellPath.PathPopupClick(Sender: tObject);
var
  i: Integer;
begin
  // Load clicked path for parsing use LoadPIDL
  i := (Sender as TMenuItem).Tag;
  if i < 0 then
  begin
    i := -i;
    i := FShellParser.PathList.Count - i;
    FShellParser.LoadPIDL(ILClone(tShellObject(FShellParser.PathList.Objects[i])
          .AbsolutePIDL));
  end
  else
    FShellParser.LoadPIDL
      (ILClone(tShellObject(FShellParser.SubPathList.Objects[i]).AbsolutePIDL));
  UpdatePath;
end;

procedure TrkShellPath.UpdatePath;
var
  i: Integer;
  s, t: String;
begin
  if not(csLoading in ComponentState) and assigned(FPathViewer) then
  begin
    FShellParser.EnumSubPaths(tShellObject(FShellParser.PathList.Objects[0])
        .AbsolutePIDL);
    FPathViewer.HasSubItems := FShellParser.SubPathList.Count > 0;
    s:= '';
    for i:= FShellParser.PathList.Count - 1 downto 0 do
      s:= s + FShellParser.PathList[i] + FPathViewer.PathSepertor;
    t:= PidlToPath(tShellObject(FShellParser.PathList.Objects[0]).AbsolutePIDL);
    if t = '' then
      FPath := s
    else
      FPath := t;
    FPathViewer.ImageIdx:= tShellObject(FShellParser.PathList.Objects[0]).IconIndex;
    FPathViewer.Path:= s;
  end;
end;

procedure TrkShellPath.SetPath(const Value: String);
begin
  FShellParser.LoadPath(Value);
  UpdatePath;
end;

procedure TrkShellPath.SetPathViewer(const Value: TrkPathViewer);
var
  DesktopIDList: pItemIDList;
begin
  FPathViewer := Value;
  FPAthViewer.HideFirstLevel:= True;
  FPathViewer.Images := FImages;
  FPathViewer.OnEditedPath := EditedPath;
  FPathViewer.OnPathClick := PathClick;
  FPathViewer.OnPathEdit := PathEdit;
  FPathViewer.OnPopup := PathPopup;
  FPathViewer.OnPopupClick := PathPopupClick;
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopIDList);
  LoadPIDL(DesktopIDList);
  FreeItemIDList(DesktopIDList);
  UpdatePath;
end;

function TrkShellPath.SysImageListHandle(const Path: string;
  const WantLargeIcons: Boolean): Windows.THandle;
var
  FI: ShellApi.TSHFileInfo;
  Flags: Windows.UINT;
begin
  Flags := ShellApi.SHGFI_SYSICONINDEX;
  if WantLargeIcons then
    Flags := Flags or ShellApi.SHGFI_LARGEICON
  else
    Flags := Flags or ShellApi.SHGFI_SMALLICON;
  result := ShellApi.SHGetFileInfo(PChar(Path), 0, FI, SizeOf(FI), Flags);
end;

constructor tShellObject.Create;
begin
  inherited;
end;

destructor tShellObject.Destroy;
begin
  inherited;
  if assigned(fPIDL) then
    FreeItemIDList(fPIDL);
end;

procedure tShellObject.SetAbsolutePIDL(Value: pItemIDList);
var
  SFI: TSHFileInfo;
begin
  if assigned(Value) then
  begin
    SHGetFileInfo(PChar(Value), 0, SFI, SizeOf(TSHFileInfo),
      SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_PIDL or SHGFI_DISPLAYNAME);
    fDisplayname := SFI.szDisplayName;
    fIconIndex := SFI.iIcon;
    fPIDL := Value;
  end
  else
  Begin
    fDisplayname := '';
    fIconIndex := 0;
    fPIDL := NIL;
  End;
end;

// Constructor
constructor tShellParser.Create;
var
  DesktopPIDL: pItemIDList;
  SFI: TSHFileInfo;
begin
  inherited;
  FPathList := TStringList.Create;
  FPathList.OwnsObjects := True;
  FSubPathList := TStringList.Create;
  FSubPathList.OwnsObjects := True;
  // Get uppercase name of desktop...
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopPIDL);
  SHGetFileInfo(PChar(DesktopPIDL), 0, SFI, SizeOf(SFI),
    SHGFI_DISPLAYNAME or SHGFI_PIDL);
  FDesktop := UpperCase(SFI.szDisplayName);
end;

// Destructor
destructor tShellParser.Destroy;
begin
  if assigned(FPathList) then
    FPathList.Free;
  if assigned(FSubPathList) then
    FSubPathList.Free;
  inherited;
end;

// Sorting of pidls...
function ComparePIDLs(Item1, Item2: Pointer): Integer;
begin
  result := SmallInt(Folder.CompareIDs(0, Item1, Item2));
end;

// Enum all direct childobjects from a pItemIDList and store the results
// in a ObjectStringList
procedure tShellParser.EnumSubPaths(PIDL: pItemIDList);
const
  SHCONTF = SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN;
Var
  EnumList: IEnumIDList;
  NewItem: pItemIDList;
  Dummy: Cardinal;
  ShellObject: tShellObject;
  PidlList: TList;
  i: Integer;

begin
  PidlList := TList.Create;
  try
    if assigned(PIDL) then
    Begin
      FSubPathList.Clear;
      Folder := GetInterfaceForObj(PIDL) as IShellFolder;
      if assigned(Folder) then
      begin
        if Folder.EnumObjects(Application.Handle, SHCONTF, EnumList) = S_OK then
          while EnumList.Next(1, NewItem, Dummy) = S_OK do
            PidlList.Add(NewItem);
        // Sort it ...
        PidlList.Sort(ComparePIDLs);
        // Add it to FSubPathList
        for i := 0 to PidlList.Count - 1 do
        begin
          ShellObject := tShellObject.Create;
          ShellObject.AbsolutePIDL := ILClone(ILCombine(PIDL, PidlList[i]));
          FSubPathList.AddObject(ShellObject.DisplayName, ShellObject);
          CoTaskMemFree(PidlList[i]);
        end;
      end;
    End;
  finally
    PidlList.Free;
  end;
end;

// Translate a Path into a pItemIDList
procedure tShellParser.LoadPath(Path: string);
Var
  PathPIDL: pItemIDList;
  ShlList: TStringList;
  SFI: TSHFileInfo;
  i: Integer;
  s: String;
  // Checks if an Object with the searched name is available in the given root
  Function GetItemIDListFromObjectname(Root: pItemIDList;
    Objectname: String): pItemIDList;
  Var
    Folder: IShellFolder;
    EnumList: IEnumIDList;
    NewItem: pItemIDList;
    Dummy: Cardinal;
  const
    SHCONTF = SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN;
  Begin
    result := NIL;
    if assigned(Root) then
    Begin
      // get the iShellFolder-Interface for the given root-idList
      Folder := GetInterfaceForObj(Root) as IShellFolder;
      // if we got an iShellFolder-Interface we can go further
      if assigned(Folder) then
      Begin
        // Iterate trough all objects in the given root and check for a match ...
        if Folder.EnumObjects(Application.Handle, SHCONTF, EnumList) = S_OK then
        Begin
          while EnumList.Next(1, NewItem, Dummy) = S_OK do
          begin
            result := ILClone(ILCombine(Root, NewItem));
            SHGetFileInfo(PChar(result), 0, SFI, SizeOf(SFI),
              SHGFI_DISPLAYNAME or SHGFI_PIDL);
            // if we got a match exit the function. The ItemIDList will be in the result
            if UpperCase(SFI.szDisplayName) = UpperCase(Objectname) then
              Exit;
          end;
          // if we got no match free the mem and set the result to nil
          if Assigned(result) then
            FreeItemIDList(result);
          result := NIL;
        End;
      End;
    End;
  End;

begin
  // Clear the ObjectStringList
  PathList.Clear;
  // Clear the SubObjectList
  SubPathList.Clear;
  // Lets check if we can translate the current path into a pidl. This is the easiest way
  // for normal paths like systemfolders, network-folders, etc.
  {
  s:= Path;
  if s <> '' then
    if s[Length(s)] = '\' then
      Delete(s, Length(s), 1);
  if (UpperCase(s) = FDesktop) and (Length(s) = Length(FDesktop)) or (s = '') then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, PathPIDL);
    if Assigned(PathPIDL) then
    begin
      LoadPIDL(PathPIDL);
      FreeItemIDList(PathPIDL);
      Exit;
    end;
  end;
  }

  PathPIDL := ILCreateFromPath(pWideChar(ExpandEnvStr(Path)));
  If assigned(PathPIDL) then
    LoadPIDL(PathPIDL)
  else

  // It seems its no 'normal' path. Perhaps the user has entered some path
  // like 'library' or 'network'. Now, we're checking if a folder rooted
  // at desktop exists named like the first path-part and - if yes - try to parse
  // the path till the end
  begin
    ShlList := TStringList.Create;
    Try
      // seperate the path in its parts
      s:= StringReplace(Path, '\\', '|', [rfReplaceAll]);
      ShlList.Text:= Trim(StringReplace(s, '\', #13, [rfReplaceAll]));
      ShlList.Text:= StringReplace(ShlList.Text, '|', '\\', [rfReplaceAll]);

      {
      NonRegularFolderlist.Text := StringReplace(NonRegularFolderlist.Text,
        '\\', '|', [rfReplaceAll]);
      NonRegularFolderlist.Delimiter := '\';
      NonRegularFolderlist.DelimitedText := ExcludeTrailingPathDelimiter(Path);
      NonRegularFolderlist.Text := StringReplace(NonRegularFolderlist.Text,
        '|', '\\', [rfReplaceAll]);
      }

      // get the desktop as first root
      SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, PathPIDL);
      // iterate trough the folderlist and try to get the pItemIDList for each
      // entry while building an absolute pItemIDList for further processing

      // Check if first item is desktop then delete it if it is...
      if FDesktop = UpperCase(ShlList[0]) then
        ShlList.Delete(0);

      for i := 0 to ShlList.Count - 1 do
      Begin
        PathPIDL := GetItemIDListFromObjectname(PathPIDL,
          ShlList[i]);
      end;
      if assigned(PathPIDL) then
        LoadPIDL(PathPIDL);
    Finally
      if assigned(PathPIDL) then
        FreeItemIDList(PathPIDL);
      ShlList.Free;
    End;
  End;
end;

// Fill the internal ObjectStringList with the parsed path from a
// given pItemIDList
procedure tShellParser.LoadPIDL(PIDL: pItemIDList);
Var
  PathPIDL: pItemIDList;
  ShellObject: tShellObject;
begin
  if assigned(PIDL) then
  Begin
    // Clear the ObjectStringList
    PathList.Clear;
    // Clear the SubObjectList
    SubPathList.Clear;
    // Create a working copy of the current PIDL
    PathPIDL := ILClone(PIDL);
    Try
      // Create a ShellObject for each ID in the pItemIDList and
      // fill the ObjectStringList
      Repeat
      Begin
        ShellObject := tShellObject.Create;
        ShellObject.AbsolutePIDL := ILClone(PathPIDL);
        PathList.AddObject(ShellObject.DisplayName, ShellObject);
      End
      Until ILRemoveLastID(PathPIDL) = False;
    Finally
      FreeItemIDList(PIDL);
    End;
  End;
end;

end.
