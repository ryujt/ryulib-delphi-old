unit rkSmartPath;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
// SmartPath by Roy Magne Klever
// © 2011 by Roy Magne Klever. All rights reserved
//
// This file is not distributable without permission by Roy Magne Klever
// WEB: www.rmklever.com
// Mail: roymagne@rmklever.com
//
// version 2.5, August 2011
//
// MPL 1.1 licenced

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms, Messages,
  ShellAPI, ImgList, Dialogs, Menus, StdCtrls, Math, ActiveX;

const
  CM_REFRESH = WM_USER + 1295; // Custom Message...
{$IFDEF VER180}
  CSIDL_MYMUSIC = $000D;
  CSIDL_MYVIDEO = $000E;
{$ENDIF}
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
  TspItemState = (spNormal, spButtonPressed, spArrowPressed, spButtonHot, spArrowHot);
  TspArrow = (spaNone, spaNormal, spaBack);
  TspPaths = set of (spDesktop, spDocuments, spPictures, spMusic, spVideos);
  TOnErrorEvent = procedure(Sender: TObject; AException: Byte) of object;
  TOnOwnerDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AText: string;
    AState: TspItemState; AButton: Boolean) of object;
  TOnPathEditEvent = procedure(Sender: TObject; AEdit: TEdit; var APath: string) of object;
  TOnPathEditedEvent = procedure(Sender: TObject; var APath: string) of object;

  TrkSmartPath = class(TCustomControl)
  private
    { Private declarations }
    FAllowEdit: Boolean;
    FAllowKeyNav: Boolean;
    FArrowStyle: TspArrow;
    FAuto: Boolean;
    FAutoComplete: Boolean;
    FBmp: TBitmap;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FBtnIdx: Integer;
    FBtnState: TspItemState;
    FButton: Boolean;
    FButtonDown: Boolean;
    FBtnWidth: Integer;
    FClickSetPath: Boolean;
    FCol1: TColor;
    FCol2: TColor;
    FCol3: TColor;
    FCol4: TColor;
    FCol5: TColor;
    FCol6: TColor;
    FCol7: TColor;
    FCol8: TColor;
    FCol9: TColor;
    FCol10: TColor;
    FCol11: TColor;
    FColorEdit: TColor;
    FColorEnter: TColor;
    FColorExit: TColor;
    FCustomPaths: TStringList;
    FCustomPopup: TPopupMenu;
    FDefault: Boolean;
    FDesktopFolder: string;
    FDirMustExist: Boolean;
    FDropDown: Boolean;
    FDropBtnWidth: Integer;
    FDropBtnState: TspItemState;
    FEmptyPathIcon: Integer;
    FEmptyPathText: string;
    FFramed: Boolean;
    FFromRight: Boolean;
    FHideFirst: Boolean;
    FImages: TImageList;
    FImagesBtn: TCustomImageList;
    FImageIdx: Integer;
    FImageIdxBtn: Integer;
    FInButton: Boolean;
    FKeys: Boolean;
    FLastPoint: TPoint;
    FMenu: TPopupMenu;
    FMenuLookup: TStringList;
    FNetDiskWait: Boolean;
    FNewFolder: string;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnError: TOnErrorEvent;
    FOwnerDraw: Boolean;
    FPath: string;
    FPathEdit: TEdit;
    FPathIdx: Integer;
    FPaths: TStringList;
    FPathsShow: TStringList;
    FPopUp: Boolean;
    FPopUpX: Integer;
    FShowArrow: Boolean;
    FShowNewFolder: Boolean;
    FSpecialFolders: TspPaths;
    FState: TspItemState;
    FTransparent: Boolean;
    FOnBtnClick: TNotifyEvent;
    FOnDropBtnClick: TNotifyEvent;
    FOnOwnerDraw: TOnOwnerDrawEvent;
    FOnPathChanged: TNotifyEvent;
    FOnPathEdit: TOnPathEditEvent;
    FOnEditedPath: TOnPathEditedEvent;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMERASEBKGND(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure PaintPath;
    procedure SetPath(Value: string);
    procedure SetCustomPaths(Value: TStringList);
    procedure SetPathsToAdd(Value: TspPaths);
    function CalcPath: Integer;
    procedure CMRefresh(var Message: TMessage); message CM_REFRESH;
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFramed(const Value: Boolean);
    procedure SetDefault(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure DoKey(const Key: Word; Shift: TShiftState);
    procedure SetColorBtn(const Index: Integer; const Value: TColor);
    procedure PaintButton(ACanvas: TCanvas; ARect: TRect; AText: string; AState: TspItemState;
      AButton: Boolean);
    procedure SetButton(const Value: Boolean);
    procedure SetDropDown(const Value: Boolean);
    procedure SetHideFirst(const Value: Boolean);
    procedure SetFromRight(const Value: Boolean);
    procedure SetColorEdit(const Value: TColor);
    procedure SetColorEnter(const Value: TColor);
    procedure SetColorExit(const Value: TColor);
  protected
    { Protected declarations }
    FPopupPos: TPoint;
    FPathOff: Integer;
    FCreateFolder: Boolean;
    InCreate: Boolean;
    InEdit: Boolean;
    InShutdown: Boolean;
    InCtrl: Boolean;
    procedure FMenuPopup(Sender: TObject);
    procedure FMenuClick(Sender: TObject);
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    { Public declarations }
    FHotPt: TPoint;
    FHotPos: Integer;
    FPopupPath: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function CalcState: Boolean;
    function GetSpecialFolderPath(folder: Integer): string;
    function PathAtXY(X, Y: Integer): string;
    function SysImageListHandle(const Path: string; const WantLargeIcons: Boolean)
      : Windows.THandle;
    procedure DoEdit(CreateFolder: Boolean);
    procedure ExitEditMode;
    procedure UpdateView;
    property Images: TImageList read FImages;
    property ImageIdx: Integer read FImageIdx;
    property PopupPath: string read FPopupPath;
    property PathOff: Integer read FPathOff;
    property PathParts: TStringList read FPaths;
    property PathDisplay: TStringList read FPathsShow;
  published
    { Published declarations }
    property Align;
    property AllowEdit: Boolean read FAllowEdit write FAllowEdit default True;
    property AllowKeyNav: Boolean read FAllowKeyNav write FAllowKeyNav default False;
    property Anchors;
    property Auto: Boolean read FAuto write FAuto default True;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BorderWidth;
    property ClickSetPath: Boolean read FClickSetPath write FClickSetPath default True;
    property BtnGreyGrad1: TColor index 0 read FCol1 write SetColorBtn default clBtnText;
    property BtnGreyGrad2: TColor index 1 read FCol2 write SetColorBtn default clBtnText;
    property BtnNormGrad1: TColor index 2 read FCol3 write SetColorBtn default clBtnText;
    property BtnNormGrad2: TColor index 3 read FCol4 write SetColorBtn default clBtnText;
    property BtnHotGrad1: TColor index 4 read FCol5 write SetColorBtn default clBtnText;
    property BtnHotGrad2: TColor index 5 read FCol6 write SetColorBtn default clBtnText;
    property BtnPenGray: TColor index 6 read FCol7 write SetColorBtn default clBtnText;
    property BtnPenNorm: TColor index 7 read FCol8 write SetColorBtn default clBtnText;
    property BtnPenShade1: TColor index 8 read FCol9 write SetColorBtn default clBtnText;
    property BtnPenShade2: TColor index 9 read FCol10 write SetColorBtn default clBtnText;
    property BtnPenArrow: TColor index 10 read FCol11 write SetColorBtn default clBtnText;
    property Button: Boolean read FButton write SetButton default False;
    property ColorEdit: TColor read FColorEdit write SetColorEdit default clWindow;
    property ColorEnter: TColor read FColorEnter write SetColorEnter default $00FFFCF2;
    property ColorExit: TColor read FColorExit write SetColorExit default $00FDF4EA;
    property Ctl3D;
    property CustomPopupMenu: TPopupMenu read FCustomPopup write FCustomPopup;
    property ComputerAsDefault: Boolean read FDefault write SetDefault;
    property DirMustExist: Boolean read FDirMustExist write FDirMustExist;
    property DropDown: Boolean read FDropDown write SetDropDown default False;
    property Enabled;
    property EmptyPathIcon: Integer read FEmptyPathIcon write FEmptyPathIcon;
    property EmptyPathText: string read FEmptyPathText write FEmptyPathText;
    property Favorites: TStringList read FCustomPaths write SetCustomPaths;
    property Font;
    property Framed: Boolean read FFramed write SetFramed default True;
    property Height;
    property HideFirstLevel: Boolean read FHideFirst write SetHideFirst default False;
    property ImagesBtn: TCustomImageList read FImagesBtn write FImagesBtn;
    property ImageIdxBtn: Integer read FImageIdxBtn write FImageIdxBtn default - 1;
    property Left;
    property NetDiskWait: Boolean read FNetDiskWait write FNetDiskWait default False;
    property NewFolderName: string read FNewFolder write FNewFolder;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property ParentColor;
    property ParentBackground;
    property ParentCtl3d;
    property ParentFont;
    property Path: string read FPath write SetPath;
    property PopupMenu;
    property RightToLeft: Boolean read FFromRight write SetFromRight default False;
    property ShowHint;
    property ShowNewFolder: Boolean read FShowNewFolder write FShowNewFolder default True;
    property SpecialFolders: TspPaths read FSpecialFolders write SetPathsToAdd;
    property TabOrder;
    property TabStop;
    property Top;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property Width;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
    property OnClick;
    property OnDblClick;
    property OnDropBtnClick: TNotifyEvent read FOnDropBtnClick write FOnDropBtnClick;
    property OnEditedPath: TOnPathEditedEvent read FOnEditedPath write FOnEditedPath;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnOwnerDraw: TOnOwnerDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
    property OnPathChanged: TNotifyEvent read FOnPathChanged write FOnPathChanged;
    property OnPathEdit: TOnPathEditEvent read FOnPathEdit write FOnPathEdit;
    property OnResize;
  end;

procedure PaintArrow(Canvas: TCanvas; X, Y: Integer; Down, Right: Boolean);

procedure Register;

implementation

uses
  ShlObj, SHFolder;

function SHAutoComplete(hwndEdit: HWND; dwFlags: dWord): LongInt; stdcall;
external 'shlwapi.dll';

function PathToPIDL(APath: WideString): PItemIDList;
// Takes the passed Path and attempts to convert it to the equavalent PIDL
var
  Desktop: IShellFolder;
  pchEaten, dwAttributes: ULONG;
begin
  Result := nil;
  SHGetDesktopFolder(Desktop);
  dwAttributes := 0;
  if Assigned(Desktop) then
  begin
    if Desktop.ParseDisplayName(0, nil, PWideChar(APath), pchEaten, Result, dwAttributes)
      <> NOERROR then
      Result := nil
  end
end;

function TrkSmartPath.GetSpecialFolderPath(folder: Integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  Path: array [0 .. MAX_PATH] of Char;
begin
  if SUCCEEDED(SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @Path[0])) then
    Result := Path
  else
    Result := '';
end;

procedure TrkSmartPath.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FAllowKeyNav then
    DoKey(Key, Shift);
end;

function TrkSmartPath.SysImageListHandle(const Path: string;
  const WantLargeIcons: Boolean): Windows.THandle;
// Gets handle to system image list for a specified path
var
  FI: ShellAPI.TSHFileInfo;
  Flags: Windows.UINT;
begin
  Flags := ShellAPI.SHGFI_SYSICONINDEX;
  if WantLargeIcons then
    Flags := Flags or ShellAPI.SHGFI_LARGEICON
  else
    Flags := Flags or ShellAPI.SHGFI_SMALLICON;
  Result := ShellAPI.SHGetFileInfo(PChar(Path), 0, FI, SizeOf(FI), Flags);
end;

procedure TrkSmartPath.UpdateView;
begin
  PaintPath;
end;

function AddBackSlash(const AString: string): string;
begin
  Result := '';
  if (AString = '') then
    Exit;
  if (AString[Length(AString)] <> '\') then
    Result := AString + '\'
  else
    Result := AString;
end;

function GetNewFolderName(const Path, FolderName: string): string;
// returns a unique folder name for this path, path needs a backslash
var
  i: Integer;
begin
  Result := Path + FolderName;
  i := 2;
  while DirectoryExists(Result) do
  begin
    Inc(i);
    Result := Path + FolderName + ' ' + IntToStr(i);
  end;
end;

procedure TrkSmartPath.ExitEditMode;
begin
  if not InEdit then
    Exit;
  FPathEdit.Free;
  if FHotPos = -2 then
    Color := FColorExit
  else
    Color := FColorEnter;
  Invalidate;
  if HasParent then
    SetFocus;
  InEdit := False;
end;

procedure TrkSmartPath.DoEdit(CreateFolder: Boolean);
var
  i: Integer;
  txt: string;
  w: Integer;
begin
  w := 8;
  if FButton then
    w := w + FBtnWidth;
  if FDropDown then
    w := w + FDropBtnWidth;
  FCreateFolder := CreateFolder;
  FPathEdit := TEdit.Create(nil);
  try
    InEdit := True;
    FPathEdit.AutoSize := False;
    FPathEdit.BorderStyle := bsNone;
    FPathEdit.Color := FColorEdit;
    FPathEdit.Ctl3D := True;
    FPathEdit.Parent := Self;
    FPathEdit.ParentFont := False;
    FPathEdit.Visible := False;
    FPathEdit.OnKeyDown := EditKeyDown;
    FPathEdit.OnKeyPress := EditKeyPress;
    FPathEdit.OnExit := EditExit;
    if FAutoComplete then
      SHAutoComplete(FPathEdit.Handle,
        SHACF_FILESYS_DIRS or SHACF_URLHISTORY or SHACF_URLMRU or SHACF_AUTOSUGGEST_FORCE_ON
          or SHACF_AUTOAPPEND_FORCE_ON);
    if FButton and FFromRight then
      FPathEdit.Left := w - 4
    else
      FPathEdit.Left := 4;
    FPathEdit.Width := ClientWidth - w;
    FPathEdit.Font.Assign(Font);
    FPathEdit.Height := FPathEdit.Font.Size + 6;
    FPathEdit.Top := ((ClientHeight - FPathEdit.Height) div 2);
    FPathEdit.AutoSelect := not CreateFolder;
    if CreateFolder then
    begin
      txt := AddBackSlash(Path);
      i := Length(txt);
      txt := GetNewFolderName(txt, FNewFolder);
      FPathEdit.Text := txt;
      FPathEdit.SelStart := i;
      FPathEdit.SelLength := Length(txt) - i;
    end
    else
      FPathEdit.Text := FPath;
    FPathEdit.Visible := True;
    PaintPath;
    FPathEdit.SetFocus;
  except
    FPathEdit.Free;
    Raise ;
  end;
end;

procedure TrkSmartPath.DoKey(const Key: Word; Shift: TShiftState);
var
  mx, my, i: Integer;
begin
  if (InEdit) or (FPaths.Count = 0) then
    Exit;
  FButtonDown := False;
  FKeys := True;
  FLastPoint := Point(-1, -1);
  if FFromRight then
    mx := ClientWidth - 22
  else
    mx := 22;
  my := ClientHeight shr 1;
  if FHotPos = -2 then
  begin
    FHotPt := Point(mx, my);
    CalcState;
    Invalidate;
    Exit;
  end;

  if Key = VK_RIGHT then
  begin
    if FFromRight then
    begin
      FHotPos := FHotPos - 1;
      if FHotPos < FPathOff - 1 then
        FHotPos := FPaths.Count - 1;
    end
    else
    begin
      FHotPos := FHotPos + 1;
      if FHotPos >= FPaths.Count then
        FHotPos := -1;
    end;
    if (FHotPos = FPathOff - 1) and FFromRight then
    begin
      mx := ClientWidth - 22;
      FHotPos := -1;
    end
    else
    begin
      for i := FPathOff to FHotPos do
        if FFromRight then
          mx := mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
        else
          mx := mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
      if FHotPos <> -1 then
        if FFromRight then
          mx := mx + 14
        else
          mx := mx - 14;
    end;
    FHotPt := Point(mx, my);
    CalcState;
    Invalidate;
  end
  else if Key = VK_LEFT then
  begin
    if FFromRight then
    begin
      FHotPos := FHotPos + 1;
      if FHotPos >= FPaths.Count then
        FHotPos := -1;
    end
    else
    begin
      FHotPos := FHotPos - 1;
      if FHotPos < FPathOff - 1 then
        FHotPos := FPaths.Count - 1;
    end;

    if FHotPos = FPathOff - 1 then
    begin
      if FFromRight then
        mx := ClientWidth - 22
      else
        mx := 22;
      FHotPos := -1;
    end
    else
      for i := FPathOff to FHotPos do
        if FFromRight then
          mx := mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
        else
          mx := mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
    if FHotPos <> -1 then
      if FFromRight then
        mx := mx + 14
      else
        mx := mx - 14;
    FHotPt := Point(mx, my);
    CalcState;
    Invalidate;
  end
  else if Key = VK_DOWN then
  begin
    FBtnIdx := FHotPos;
    if FHotPos = FPathOff - 1 then
    begin
      if FFromRight then
        mx := ClientWidth - 22
      else
        mx := 22;
      FHotPos := -1;
    end
    else
      for i := FPathOff to FHotPos do
        if FFromRight then
          mx := mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
        else
          mx := mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
    FHotPt := Point(mx, my);
    CalcState;
    FPopUp := True;
    FButtonDown := True;
    PaintPath;
    if (FPopUp) and (FPopupPath <> '') then
    begin
      if (FAuto) then
        FMenu.Popup(FPopupPos.X, FPopupPos.Y)
      else if Assigned(FCustomPopup) then
        FCustomPopup.Popup(FPopupPos.X, FPopupPos.Y);
      PostMessage(Handle, CM_REFRESH, 0, 0);
    end;
  end
  else if Key = VK_RETURN then
  begin
    for i := FPathOff to FHotPos do
      if FFromRight then
        mx := mx - FBmp.Canvas.TextWidth(FPaths[i]) - 23
      else
        mx := mx + FBmp.Canvas.TextWidth(FPaths[i]) + 23;
    if FFromRight then
      mx := mx + 14
    else
      mx := mx - 14;
    FHotPt := Point(mx, my);
    FBtnIdx := FHotPos;
    FButtonDown := True;
    CalcState;
    if (FState = spButtonPressed) and (FClickSetPath) and (FInButton) then
    begin
      for i := FPathOff to FHotPos do
        mx := mx + FBmp.Canvas.TextWidth(FPathsShow[i]) + 23;
      mx := mx - 14;
      FHotPt := Point(mx, my);
      FButtonDown := True;
      CalcState;
      if (FState = spButtonPressed) and (FClickSetPath) then
        Path := FPopupPath;
    end;
  end
end;

procedure TrkSmartPath.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  done: Boolean;
begin
  if Key = VK_Escape then
    done := True
  else if Key = VK_RETURN then
  begin
    done := True;
    if FCreateFolder then
    begin
      if not CreateDir(FPathEdit.Text) then
        if Assigned(FOnError) then
          OnError(Self, 0)
        else
          ShowMessage('Create folder failed. An error occured. ErrorCode: ' + IntToStr
              (GetLastError));
    end;
    SetPath(FPathEdit.Text);
  end
  else
    done := False;
  if done then
    ExitEditMode;
end;

procedure TrkSmartPath.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TrkSmartPath.EditExit(Sender: TObject);
begin
  ExitEditMode;
end;

procedure MultiSzToStrings(const MultiSz: PChar; const Strings: Classes.TStrings);
var
  P: PChar;
begin
  if not Assigned(MultiSz) then
    Exit;
  P := MultiSz;
  while P^ <> #0 do
  begin
    Strings.Add(P);
    Inc(P, SysUtils.StrLen(P) + 1);
  end;
end;

function GetDriveNumber(const Drive: string): Integer;
{ Gets the zero based drive number for a local drive. Drive must be a valid local drive (A:\ to Z:\). }
begin
  Result := -1;
  if Drive <> '' then
    Result := Ord(UpCase(Drive[1])) - Ord('A');
end;

function IsDriveReady(const Drive: string): Boolean;
{ Checks if a local drive is ready. Drive must be a valid local drive (A:\ to Z:\). }
var
  ErrorMode: Word; // current error mode
  DriveNum: Integer; // zero based number of drive
begin
  Result := False;
  // Get zero based drive number
  DriveNum := GetDriveNumber(Drive);
  if DriveNum = -1 then
    Exit;
  ErrorMode := Windows.SetErrorMode(Windows.SEM_FAILCRITICALERRORS);
  try
    // DiskSize requires 1 based drive numbers
    // returns -1 for invalid drives
    if SysUtils.DiskSize(DriveNum + 1) <> -1 then
      Result := True;
  finally
    Windows.SetErrorMode(ErrorMode);
  end;
end;

function CompareNatural(s1, s2: String): Integer;

  function ExtractNr(n: Integer; var txt: String): Int64;
  begin
    while (n <= Length(txt)) and ((txt[n] >= '0') and (txt[n] <= '9')) do
      n := n + 1;
    Result := StrToInt64Def(Copy(txt, 1, (n - 1)), 0);
    Delete(txt, 1, (n - 1));
  end;

var
  b: Boolean;
begin
  Result := 0;
  s1 := LowerCase(s1);
  s2 := LowerCase(s2);
  if (s1 <> s2) and (s1 <> '') and (s2 <> '') then
  begin
    b := False;
    while (not b) do
    begin
      if ((s1[1] >= '0') and (s1[1] <= '9')) and ((s2[1] >= '0') and (s2[1] <= '9')) then
        Result := Sign(ExtractNr(1, s1) - ExtractNr(1, s2))
      else
        Result := Sign(Integer(s1[1]) - Integer(s2[1]));
      b := (Result <> 0) or (Min(Length(s1), Length(s2)) < 2);
      if not b then
      begin
        Delete(s1, 1, 1);
        Delete(s2, 1, 1);
      end;
    end;
  end;
  if Result = 0 then
  begin
    if (Length(s1) = 1) and (Length(s2) = 1) then
      Result := Sign(Integer(s1[1]) - Integer(s2[1]))
    else
      Result := Sign(Length(s1) - Length(s2));
  end;
end;

function SortMe(List: TStringList; i1, i2: Integer): Integer;
begin
  Result := CompareNatural(List[i1], List[i2]);
end;

function DriveTypeFromPath(const Path: string): Integer;
var
  Drive: string; // the drive name
begin
  Drive := SysUtils.ExtractFileDrive(Path) + '\';
  Result := Integer(Windows.GetDriveType(PChar(Drive)));
end;

procedure TrkSmartPath.FMenuPopup(Sender: TObject);
const
  Flag: Cardinal = SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_PIDL or SHGFI_TYPENAME or
    SHGFI_DISPLAYNAME;
  SSI: Integer = SizeOf(TSHFileInfo);
var
  AMenuItem: TMenuItem;
  SFI: TSHFileInfo;
  SR: TSearchRec;
  IsFound: Boolean;
  i, j, idx: Integer;
  FName, s: string;
  Drives: PChar;
  BufSize: Integer;
  List: TStringList;
  PIDL: PItemIDList;

  procedure AddMenuItem(PName, FName: string; Icon: Integer);
  begin
    AMenuItem := TMenuItem.Create(FMenu);
    AMenuItem.Caption := FName;
    AMenuItem.OnClick := FMenuClick;
    AMenuItem.ImageIndex := Icon;
    AMenuItem.Tag := idx;
    Inc(idx);
    FMenu.Items.Add(AMenuItem);
    FMenuLookup.Add(PName);
  end;

begin
  FPopUpX := FHotPos;
  Screen.Cursor := crHourglass;
  for i := FMenu.Items.Count - 1 downto 0 do
    FMenu.Items[i].Free;
  FMenuLookup.Clear;
  idx := 0;
  if FPopupPath = 'HomeFolder' then
  begin
    if FPathOff > 0 then
    begin
      for i := FPathOff - 1 downto 0 do
      begin
        FName := '';
        for j := 0 to i do
          FName := FName + FPaths[j] + '\';
        PIDL := PathToPIDL(FName);
        SHGetFileInfo(PChar(PIDL), 0, SFI, SSI, Flag);
        CoTaskMemFree(PIDL);
        AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
      end;
      AddMenuItem('- ', '-', -1);
    end;
    if FShowNewFolder then
    begin
      AddMenuItem('*', FNewFolder, -1);
      AddMenuItem('- ', '-', -1);
    end;
    if spDesktop in FSpecialFolders then
    begin
      FName := GetSpecialFolderPath(CSIDL_DESKTOPDIRECTORY);
      s := ExtractFileName(FName);

      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
    end;
    if spDocuments in FSpecialFolders then
    begin
      FName := GetSpecialFolderPath(CSIDL_PERSONAL);
      s := ExtractFileName(FName);
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
    end;
    if spPictures in FSpecialFolders then
    begin
      FName := GetSpecialFolderPath(CSIDL_MYPICTURES);
      s := ExtractFileName(FName);
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
    end;
    if spMusic in FSpecialFolders then
    begin
      FName := GetSpecialFolderPath(CSIDL_MYMUSIC);
      s := ExtractFileName(FName);
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
    end;
    if spVideos in FSpecialFolders then
    begin
      FName := GetSpecialFolderPath(CSIDL_MYVIDEO);
      s := ExtractFileName(FName);
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
      CoTaskMemFree(PIDL);
    end;
    AddMenuItem('- ', '-', -1);
    for i := 0 to FCustomPaths.Count - 1 do
    begin
      FName := FCustomPaths[i];
      s := ExtractFileName(FName);
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
    end;
    AddMenuItem('- ', '-', -1);
    List := TStringList.Create;
    BufSize := Windows.GetLogicalDriveStrings(0, nil);
    GetMem(Drives, BufSize * SizeOf(Char));
    try
      if Windows.GetLogicalDriveStrings(BufSize, Drives) = 0 then
        SysUtils.RaiseLastOSError;
      MultiSzToStrings(Drives, List);
    finally
      FreeMem(Drives);
    end;
    for i := 0 to List.Count - 1 do
    begin
      FName := List[i];
      if not FNetDiskWait then
      begin
        IsFound := (DriveTypeFromPath(FName) = DRIVE_REMOTE);
        if not IsFound then
          IsFound := IsDriveReady(FName);
      end
      else
        IsFound := IsDriveReady(FName);
      if IsFound then
      begin
        PIDL := PathToPIDL(FName);
        SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
        CoTaskMemFree(PIDL);
        AddMenuItem(FName, SFI.szDisplayName, SFI.iIcon);
      end;
    end;
    List.Free;
  end
  else
  begin
    List := TStringList.Create;
    IsFound := FindFirst(FPopupPath + '*.*', faAnyFile - faHidden, SR) = 0;
    while IsFound do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
        if ((SR.Attr and faDirectory) <> 0) then
          List.Add(SR.Name);
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);
    List.CustomSort(SortMe);
    for i := 0 to List.Count - 1 do
    begin
      FName := FPopupPath + List[i];
      PIDL := PathToPIDL(FName);
      SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, SFI, SSI, Flag);
      CoTaskMemFree(PIDL);
      AddMenuItem(List[i], SFI.szDisplayName, SFI.iIcon);
    end;
    List.Free;
  end;
  Screen.Cursor := crDefault;
end;

procedure TrkSmartPath.FMenuClick(Sender: TObject);
var
  txt: string;
begin
  if FPopupPath = 'HomeFolder' then
  begin
    txt := FMenuLookup[(Sender as TMenuItem).Tag];
    if txt = '*' then
      DoEdit(True)
    else
      Path := txt
  end
  else
    Path := FPopupPath + FMenuLookup[(Sender as TMenuItem).Tag];
end;

procedure TrkSmartPath.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      FBmp.Canvas.Font.Assign(Font);
      PaintPath;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

function TrkSmartPath.PathAtXY(X, Y: Integer): string;
begin
  FHotPt.X := X;
  FHotPt.Y := Y;
  CalcState;
  if FPopupPath = 'HomeFolder' then
    Result := ''
  else
    Result := FPopupPath;
end;

procedure PreMultiplyBitmap(Bitmap: TBitmap);
var
  X, Y: Integer;
  Pixel: PRGBQuad;
begin
  with Bitmap do
    for Y := Height - 1 downto 0 do
    begin
      Pixel := ScanLine[Y];
      for X := Width - 1 downto 0 do
      begin
        Pixel.rgbReserved := 255;
        Pixel.rgbBlue := MulDiv(Pixel.rgbBlue, Pixel.rgbReserved, 255);
        Pixel.rgbGreen := MulDiv(Pixel.rgbGreen, Pixel.rgbReserved, 255);
        Pixel.rgbRed := MulDiv(Pixel.rgbRed, Pixel.rgbReserved, 255);
        Inc(Pixel);
      end;
    end;
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0 .. 1024] of TRGBTriple;
  TGradientColors = array [0 .. 255] of TRGBTriple;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3, y1, i, GSize: Integer;
  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
  GradientBmp: TBitmap;
begin
  rc1 := GetRValue(ColorToRGB(StartColor));
  gc1 := GetGValue(ColorToRGB(StartColor));
  bc1 := GetBValue(ColorToRGB(StartColor));
  rc2 := GetRValue(ColorToRGB(EndColor));
  gc2 := GetGValue(ColorToRGB(EndColor));
  bc2 := GetBValue(ColorToRGB(EndColor));
  rc3 := rc1 + (((rc2 - rc1) * 15) div 9);
  gc3 := gc1 + (((gc2 - gc1) * 15) div 9);
  bc3 := bc1 + (((bc2 - bc1) * 15) div 9);
  if rc3 < 0 then
    rc3 := 0
  else if rc3 > 255 then
    rc3 := 255;
  if gc3 < 0 then
    gc3 := 0
  else if gc3 > 255 then
    gc3 := 255;
  if bc3 < 0 then
    bc3 := 0
  else if bc3 > 255 then
    bc3 := 255;
  GradientBmp := TBitmap.Create;
  GradientBmp.PixelFormat := pf24bit;
  GradientBmp.Width := 1;
  GradientBmp.Height := (ARect.Bottom - ARect.Top) - 1;
  GSize := GradientBmp.Height;
  y1 := GSize div 2;
  for i := 0 to GSize - 1 do
  begin
    Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
    if (i < y1 - 1) then
    begin
      GradCol.rgbtRed := rc1 + (rc2 - rc1) * i div y1;
      GradCol.rgbtGreen := gc1 + (gc2 - gc1) * i div y1;
      GradCol.rgbtBlue := bc1 + (bc2 - bc1) * i div y1;
    end
    else
    begin
      GradCol.rgbtRed := rc3 + (rc2 - rc3) * i div GSize;
      GradCol.rgbtGreen := gc3 + (gc2 - gc3) * i div GSize;
      GradCol.rgbtBlue := bc3 + (bc2 - bc3) * i div GSize;
    end;
    Row[0] := GradCol;
  end;
  Canvas.StretchDraw(ARect, GradientBmp);
  GradientBmp.Free;
end;

function TrkSmartPath.CalcState: Boolean;
var
  i, n: Integer;
  txt: string;
  R: TRect;
  ShowArrow, done, bool: Boolean;
  popX, popY: Integer;
  FSysArrow: Boolean;
  Old: TspItemState;
begin
  Result := False;
  if (FPaths.Count = 0) or InCreate then
    Exit;
  FPathOff := CalcPath;
  FArrowStyle := spaBack;
  FSysArrow := FArrowStyle <> spaNone;

  if FFromRight then
  begin
    R.Right := FBmp.Width - 22;
    R.Left := R.Right;
  end
  else
  begin
    R.Left := 22;
    R.Right := R.Left;
  end;
  Old := FState;
  n := FPathOff - 1;
  bool := FSysArrow;
  ShowArrow := True;
  FState := spNormal;
  FInButton := (FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight) and (FButtonDown);
  FInButton := FInButton or FKeys;
  txt := '';
  done := False;
  while (not done) do
  begin
    done := (n = FPathsShow.Count - 1);
    if bool then
      if FFromRight then
        R.Left := R.Right - 14
      else
        R.Right := R.Left + 14;
    if not bool then
    begin
      Inc(n);
      if (n > -1) and (n < FPaths.Count) then
        txt := FPathsShow[n];
      if FFromRight then
        R.Left := R.Right - FBmp.Canvas.TextWidth(txt) - 9
      else
        R.Right := R.Left + FBmp.Canvas.TextWidth(txt) + 9;
    end;
    if done then
      ShowArrow := (FShowArrow) or (FPathOff = FPaths.Count);
    if ShowArrow then
    begin
      if (FHotPt.X >= R.Left) and (FHotPt.X <= R.Right) then
      begin
        if FButtonDown then
        begin
          if bool then
            FState := spArrowPressed
          else
            FState := spButtonPressed;
        end
        else
        begin
          if bool then
            FState := spArrowHot
          else
            FState := spButtonHot;
        end;
        if FFromRight then
          popX := R.Right + 21
        else
          popX := R.Right - 35;
        popY := ClientHeight - 1;
        FPopupPos := ClientToScreen(Point(popX, popY));
        if (bool) and (n = FPathOff - 1) then
        begin
          FPopupPath := 'HomeFolder';
          FPathIdx := -1;
        end
        else
        begin
          FPopupPath := '';
          for i := 0 to n do
            if i = 0 then
              FPopupPath := FPaths[i] + '\'
            else
              FPopupPath := FPopupPath + FPaths[i] + '\';
          FPathIdx := n;
        end;
        FHotPos := n;
        if (FButtonDown) and ((FHotPos <> FBtnIdx) or (not FInButton)) then
          if (FState = spButtonPressed) or (FState = spArrowPressed) then
            FState := spNormal;
      end;
    end;
    if FFromRight then
      R.Right := R.Left
    else
      R.Left := R.Right;
    bool := not bool;
  end;
  Result := Old <> FState;

  Old := FBtnState;
  FBtnState := spNormal;
  if (FState = spNormal) and (FButton) then // Check if in button...
  begin
    R := ClientRect;
    if FFromRight then
      FInButton := (FHotPt.X >= R.Left) and (FHotPt.X <= R.Left + FBtnWidth)
    else
      FInButton := (FHotPt.X >= R.Right - FBtnWidth) and (FHotPt.X <= R.Right);
    FInButton := FInButton and ((FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight));
    if FInButton then
      if FButtonDown then
        FBtnState := spButtonPressed
      else
        FBtnState := spButtonHot;
    Result := Result or (Old <> FBtnState);
  end;

  Old := FDropBtnState;
  FDropBtnState := spNormal;
  if (FState = spNormal) and (FBtnState = spNormal) and (FDropDown) then
  begin
    R := ClientRect;
    if FButton then
      i := FBtnWidth
    else
      i := 0;
    if FFromRight then
      FInButton := (FHotPt.X >= R.Left + i) and (FHotPt.X <= R.Left + i + FDropBtnWidth)
    else
      FInButton := (FHotPt.X >= R.Right - i - FDropBtnWidth) and (FHotPt.X <= R.Right - i);
    FInButton := FInButton and ((FHotPt.Y > 0) and (FHotPt.Y <= ClientHeight));
    if FInButton then
      if FButtonDown then
        FDropBtnState := spButtonPressed
      else
        FDropBtnState := spButtonHot;
    Result := Result or (Old <> FDropBtnState);
  end;
end;

procedure PaintArrow(Canvas: TCanvas; X, Y: Integer; Down, Right: Boolean);
begin
  with Canvas do
  begin
    if Down then
    begin
      MoveTo(X + 3, Y + 3);
      LineTo(X + 7, Y + 3);
      LineTo(X + 5, Y + 5);
      LineTo(X + 3, Y + 3);
      LineTo(X + 6, Y + 5);
      MoveTo(X + 2, Y + 2);
      LineTo(X + 9, Y + 2);
    end
    else if Right then
    begin
      MoveTo(X + 4, Y);
      LineTo(X + 4, Y + 4);
      LineTo(X + 2, Y + 2);
      LineTo(X + 4, Y);
      LineTo(X + 3, Y + 3);
      LineTo(X + 5, Y - 1);
      LineTo(X + 5, Y + 6);
    end
    else
    begin
      MoveTo(X + 4, Y);
      LineTo(X + 4, Y + 4);
      LineTo(X + 6, Y + 2);
      LineTo(X + 4, Y);
      LineTo(X + 5, Y + 3);
      LineTo(X + 3, Y - 1);
      LineTo(X + 3, Y + 6);
    end;
  end;
end;

procedure PaintBackArrow(Canvas: TCanvas; X, Y: Integer; Down, Right: Boolean);
begin
  with Canvas do
  begin
    if Right then
    begin
      MoveTo(X + 6, Y);
      LineTo(X + 8, Y + 2);
      LineTo(X + 5, Y + 5);
      MoveTo(X + 5, Y);
      LineTo(X + 7, Y + 2);
      LineTo(X + 4, Y + 5);
      MoveTo(X + 2, Y);
      LineTo(X + 4, Y + 2);
      LineTo(X + 1, Y + 5);
      MoveTo(X + 1, Y);
      LineTo(X + 3, Y + 2);
      LineTo(X + 0, Y + 5);
    end
    else
    begin
      MoveTo(X + 2, Y);
      LineTo(X + 0, Y + 2);
      LineTo(X + 3, Y + 5);
      MoveTo(X + 3, Y);
      LineTo(X + 1, Y + 2);
      LineTo(X + 4, Y + 5);
      MoveTo(X + 6, Y);
      LineTo(X + 4, Y + 2);
      LineTo(X + 7, Y + 5);
      MoveTo(X + 7, Y);
      LineTo(X + 5, Y + 2);
      LineTo(X + 8, Y + 5);
    end;
  end;
end;

procedure TrkSmartPath.PaintButton(ACanvas: TCanvas; ARect: TRect; AText: string;
  AState: TspItemState; AButton: Boolean);
var
  c1, c2: TColor;
  Grey: Boolean;
  R: TRect;
  Y: Integer;
  uFormat: UINT;
begin
  R := ARect;
  if not AButton then
    R.Left := R.Left - 1;
  if AState <> spNormal then
  begin
    ACanvas.Font.Color := clBlack;
    Grey := AButton and (AState = spArrowHot);
    if Grey then
    begin
      c1 := FCol1;
      c2 := FCol2;
    end
    else
    begin
      if (AState = spArrowHot) then
      begin
        c1 := FCol3;
        c2 := FCol4;
      end
      else
      begin
        c1 := FCol5;
        c2 := FCol6;
      end;
    end;
    if Grey then
      ACanvas.Pen.Color := FCol7
    else
      ACanvas.Pen.Color := FCol8;
    ACanvas.MoveTo(R.Left, R.Top);
    ACanvas.LineTo(R.Left, R.Bottom);
    ACanvas.MoveTo(R.Right - 1, R.Top);
    ACanvas.LineTo(R.Right - 1, R.Bottom);
    R.Left := R.Left + 2;
    R.Right := R.Right - 2;
    R.Top := R.Top + 1;
    R.Bottom := R.Bottom - 1;
    if (AState = spArrowHot) or (AState = spButtonHot) then
      FillGradient(ACanvas, R, c1, c2)
    else
    begin
      InflateRect(R, 1, 1);
      FillGradient(ACanvas, R, c1, c2);
      InflateRect(R, -1, -1);
      R.Top := R.Top - 1;
      R.Bottom := R.Bottom + 1;
      ACanvas.Pen.Color := FCol9;
      ACanvas.MoveTo(R.Left - 1, R.Bottom - 1);
      ACanvas.LineTo(R.Left - 1, R.Top);
      ACanvas.LineTo(R.Right + 1, R.Top);
      ACanvas.MoveTo(R.Right, R.Top + 1);
      ACanvas.Pen.Color := FCol10;
      ACanvas.LineTo(R.Left, R.Top + 1);
      ACanvas.LineTo(R.Left, R.Bottom - 1);
      ACanvas.LineTo(R.Right + 1, R.Bottom - 1);
    end;
  end;
  if AButton then
  begin
    ACanvas.Brush.Style := bsClear;
    if (AState = spArrowPressed) or (AState = spButtonPressed) then
    begin
      R.Left := R.Left + 1;
      R.Top := R.Top + 1;
      R.Bottom := R.Bottom + 1;
    end;
    if FAllowKeyNav then
      uFormat := DT_CENTER or DT_VCENTER or DT_SINGLELINE
    else
      uFormat := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), R, uFormat);
  end
  else
  begin
    Y := (((ARect.Bottom - ARect.Top) - 7) div 2) + 2;
    if AState = spNormal then
      ACanvas.Pen.Color := ACanvas.Font.Color
    else
      ACanvas.Pen.Color := FCol11;
    if AText = 'back' then
      PaintBackArrow(ACanvas, ARect.Right - 12, Y,
        (AState = spButtonPressed) or (AState = spArrowPressed), FFromRight)
    else
      PaintArrow(ACanvas, ARect.Right - 12, Y,
        (AState = spButtonPressed) or (AState = spArrowPressed), FFromRight);
  end;
end;

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
{$IFDEF DFS_COMPILER_2}
    GetViewportOrgEx(DC, @Position);
{$ELSE}
    GetViewportOrgEx(DC, Position);
{$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TrkSmartPath.PaintPath;
var
  i, n, w, X: Integer;
  txt: string;
  R: TRect;
  ShowArrow, done, bool: Boolean;
begin
  if InCreate then
    Exit;

  CalcState;

  if InEdit then
    Color := FColorEdit
  else if InCtrl then
    Color := FColorEnter
  else
    Color := FColorExit;

  FBmp.Width := ClientWidth;
  FBmp.Height := ClientHeight;
  FBmp.Canvas.Brush.Color := Color;
  FBmp.Canvas.Brush.Style := bsSolid;
  FBmp.Canvas.Pen.Color := Color;
  R := ClientRect;
  if Transparent then
    DrawParentImage(Self, FBmp.Canvas)
  else
  begin
    if FFramed then
      FBmp.Canvas.Pen.Color := FBorderColor;
    FBmp.Canvas.Rectangle(R);
  end;
  if FFramed then
    InflateRect(R, -1, -1);
  if not InEdit then
  begin
    i := ((ClientHeight - 16) shr 1);
    if (FImageIdx <> -1) and (Assigned(FImages)) then
      if (FImageIdx > -1) and (FImageIdx < FImages.Count) then
        if FFromRight then
          FImages.Draw(FBmp.Canvas, FBmp.Width - 20, i, FImageIdx)
        else
          FImages.Draw(FBmp.Canvas, 4, i, FImageIdx);
    if FFromRight then
    begin
      R.Right := FBmp.Width - 22;
      R.Left := R.Right;
    end
    else
    begin
      R.Left := 22;
      R.Right := R.Left;
    end;
    txt := '';
    n := FPathOff - 1;
    if FPathOff > 0 then
      txt := 'back';
    bool := FArrowStyle <> spaNone;
    done := False;
    ShowArrow := True;
    while not done do
    begin
      done := (n = FPaths.Count - 1);
      if bool then
        if FFromRight then
          R.Left := R.Right - 14
        else
          R.Right := R.Left + 14;
      if not bool then
      begin
        Inc(n);
        if (n > -1) and (n < FPaths.Count) then
          txt := FPathsShow[n];
        if FFromRight then
          R.Left := R.Right - FBmp.Canvas.TextWidth(txt) - 9
        else
          R.Right := R.Left + FBmp.Canvas.TextWidth(txt) + 9;
      end;

      if done then
        ShowArrow := (FShowArrow) or (FPathOff = FPaths.Count);
      if ShowArrow then
        if (FState <> spNormal) and (n = FHotPos) then
        begin
          if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
            FOnOwnerDraw(Self, FBmp.Canvas, R, txt, FState, not bool)
          else
          begin
            FBmp.Canvas.Font.Assign(Font);
            PaintButton(FBmp.Canvas, R, txt, FState, not bool);
          end;
        end
        else
        begin
          if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
            FOnOwnerDraw(Self, FBmp.Canvas, R, txt, spNormal, not bool)
          else
          begin
            FBmp.Canvas.Font.Assign(Font);
            PaintButton(FBmp.Canvas, R, txt, spNormal, not bool);
          end;
        end;
      if FFromRight then
        R.Right := R.Left + 1
      else
        R.Left := R.Right;
      bool := not bool;
    end;
  end;

  w := 0;
  if (FButton) then
  begin
    w := FBtnWidth;
    R := ClientRect;
    if FFramed then
      InflateRect(R, -1, -1);
    if FFromRight then
    begin
      R.Right := R.Left + w;
      X := R.Right;
    end
    else
    begin
      R.Left := R.Right - w;
      X := R.Left;
    end;

    i := 0;
    n := 0;
    if Assigned(FImagesBtn) then
    begin
      i := R.Left + ((w - FImagesBtn.Width) shr 1);
      n := R.Top + ((ClientHeight - FImagesBtn.Height) shr 1);
    end;

    if FBtnState = spNormal then
    begin
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', spNormal, True)
      else if Assigned(FImagesBtn) and (FImageIdxBtn <> -1) then
        FImagesBtn.Draw(FBmp.Canvas, i + 1, n - 1, FImageIdxBtn);
    end
    else
    begin
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', FBtnState, True)
      else
      begin
        FBmp.Canvas.Pen.Color := FBorderColor;
        FBmp.Canvas.MoveTo(X, R.Top);
        FBmp.Canvas.LineTo(X, R.Bottom);

        PaintButton(FBmp.Canvas, R, '', FBtnState, True);
        if Assigned(FImagesBtn) and (FImageIdxBtn <> -1) then
          if FBtnState = spButtonHot then
            FImagesBtn.Draw(FBmp.Canvas, i + 1, n - 1, FImageIdxBtn)
          else
            FImagesBtn.Draw(FBmp.Canvas, i + 2, n, FImageIdxBtn)
      end;
    end;
  end;

  if FDropDown then
  begin
    R := ClientRect;
    if FFramed then
      InflateRect(R, -1, -1);
    if FFromRight then
    begin
      R.Left := w + 1;
      R.Right := R.Left + FDropBtnWidth;
    end
    else
    begin
      R.Right := R.Right - w;
      R.Left := R.Right - FDropBtnWidth;
    end;
    i := R.Left + ((FDropBtnWidth - 7) shr 1);
    n := R.Top + ((ClientHeight - 7) shr 1);

    if FDropBtnState = spNormal then
    begin
      FBmp.Canvas.Pen.Color := Font.Color;
      PaintArrow(FBmp.Canvas, i - 1, n - 1, True, False);
    end
    else
    begin
      R.Right := R.Left + FDropBtnWidth + 1;
      if Assigned(FOnOwnerDraw) and (FOwnerDraw) then
        FOnOwnerDraw(Self, FBmp.Canvas, R, '', FDropBtnState, True)
      else
      begin
        PaintButton(FBmp.Canvas, R, '', FDropBtnState, True);
        FBmp.Canvas.Pen.Color := Font.Color;
        if FDropBtnState = spButtonHot then
          PaintArrow(FBmp.Canvas, i - 1, n - 1, True, False)
        else
          PaintArrow(FBmp.Canvas, i, n, True, False);
      end;
    end;
  end;

  PreMultiplyBitmap(FBmp);

  // if InEdit then
  // ExcludeClipRect(Canvas.Handle, FPathEdit.ClientRect.Left + FPAthEdit.Left, FPathEdit.ClientRect.Top + FPathEdit.Top,
  // FPathEdit.ClientRect.Right  + FPAthEdit.Left, FPathEdit.ClientRect.Bottom + FPathEdit.Top);

  // if InEdit then
  // FBmp.Canvas.Rectangle(FPathEdit.ClientRect);

  BitBlt(Canvas.Handle, 0, 0, FBmp.Width, FBmp.Height, FBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TrkSmartPath.WMERASEBKGND(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TrkSmartPath.WMGetDlgCode(var message: TWMGetDlgCode);
begin
  inherited;
  // Answer Delphi that this component wants to handle its own arrow key press:
  message.Result := DLGC_WANTARROWS;
end;

procedure TrkSmartPath.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TrkSmartPath.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TrkSmartPath.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if Assigned(OnEnter) then
    OnEnter(Self);
  FPopUpX := -2;
  if FAllowKeyNav then
    DoKey(VK_HOME, []);
end;

procedure TrkSmartPath.CMExit(var Message: TCMExit);
begin
  if Assigned(OnExit) then
    OnExit(Self);
  FKeys := False;
  FHotPos := -2;
  FHotPt := Point(-1, -1);
  FState := spNormal;
  FPathIdx := -1;
  FPopupPath := '';
  FButtonDown := False;
  FPopUp := False;
  Invalidate;
  inherited;
end;

procedure TrkSmartPath.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not(csLoading in ComponentState) then
    Resize;
end;

procedure TrkSmartPath.CMRefresh(var Message: TMessage);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if not FKeys then
  begin
    FHotPos := -2;
    FHotPt := Point(-1, -1);
    FState := spNormal;
  end;
  FKeys := False;
  FPathIdx := -1;
  FPopupPath := '';
  FButtonDown := False;
  FPopUp := False;
  Invalidate;
  inherited;
end;

procedure TrkSmartPath.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrkSmartPath.SetButton(const Value: Boolean);
begin
  FButton := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetFramed(const Value: Boolean);
begin
  FFramed := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetFromRight(const Value: Boolean);
begin
  FFromRight := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetHideFirst(const Value: Boolean);
begin
  FHideFirst := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetOwnerDraw(const Value: Boolean);
begin
  FOwnerDraw := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetPath(Value: string);
var
  i: Integer;
  s, P: String;
  PIDL: PItemIDList;
  SFI: TSHFileInfo;
  SR: TSearchRec;
begin
  if FDirMustExist and (not(DirectoryExists(Value))) then
    Exit;
  if Value = '' then
  begin
    ComputerAsDefault := FDefault;
    FPath := '';
    FPaths.Clear;
    FPaths.Add(FEmptyPathText);
    FPathsShow.Clear;
    FPathsShow.Add(FEmptyPathText);
    FImageIdx := FEmptyPathIcon;
    FPathOff := 0;
    FShowArrow := False;
  end
  else
  begin
    if (FPath = Value) then
      Exit;
    FPath := AddBackSlash(Value);
    FPaths.Clear;
    if Copy(FPath, 1, 2) = '\\' then
    begin
      FPaths.Text := Trim(StringReplace(FPath, '\', #13, [rfReplaceAll]));
      FPaths.Text := '\\' + FPaths.Text;
    end
    else
      FPaths.Text := Trim(StringReplace(FPath, '\', #13, [rfReplaceAll]));
    PIDL := PathToPIDL(FPath);
    SHGetFileInfo(PChar(PIDL), 0, SFI, SizeOf(SFI),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME or SHGFI_TYPENAME);
    CoTaskMemFree(PIDL);
    FImageIdx := SFI.iIcon;
    FButtonDown := False;
    FShowArrow := False;
    s := '';
    FPathsShow.Clear;
    for i := 0 to FPaths.Count - 1 do
    begin
      s := s + FPaths[i] + '\';
      SHGetFileInfo(PChar(s), 0, SFI, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME);
      P := SFI.szDisplayName;
      if P = '' then
        P := FPaths[i];
      FPathsShow.Add(P);
    end;
    // Empty folder?
    if FindFirst(Value + '\*.*', faDirectory, SR) = 0 then
      while (FindNext(SR) = 0) and (not FShowArrow) do
        if (SR.Name <> '.') and (SR.Name <> '..') then
          FShowArrow := ((SR.Attr and faDirectory) <> 0);
    FindClose(SR);
    FPathOff := CalcPath;
  end;
  Invalidate;
  if Assigned(FOnPathChanged) then
    FOnPathChanged(Self);
end;

procedure TrkSmartPath.SetColorBtn(const Index: Integer; const Value: TColor);
begin
  case Index of
  0:
    FCol1 := Value;
  1:
    FCol2 := Value;
  2:
    FCol3 := Value;
  3:
    FCol4 := Value;
  4:
    FCol5 := Value;
  5:
    FCol6 := Value;
  6:
    FCol7 := Value;
  7:
    FCol8 := Value;
  8:
    FCol9 := Value;
  9:
    FCol10 := Value;
  10:
    FCol11 := Value;
else
end;
  Invalidate;
end;

procedure TrkSmartPath.SetColorEdit(const Value: TColor);
begin
  FColorEdit := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetColorEnter(const Value: TColor);
begin
  FColorEnter := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetColorExit(const Value: TColor);
begin
  FColorExit := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetCustomPaths(Value: TStringList);
begin
  FCustomPaths.Assign(Value);
end;

procedure TrkSmartPath.SetDefault(const Value: Boolean);
var
  PIDL: PItemIDList;
  FileInfo: TSHFileInfo;
begin
  FDefault := Value;
  if Value = True then
    SHGetSpecialFolderLocation(Application.Handle, CSIDL_DRIVES, PIDL)
  else
    Exit;
  if SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(TSHFileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME) <> 0 then
  begin
    EmptyPathIcon := FileInfo.iIcon;
    EmptyPathText := FileInfo.szDisplayName;
  end;
  CoTaskMemFree(PIDL);
  Invalidate;
end;

procedure TrkSmartPath.SetDropDown(const Value: Boolean);
begin
  FDropDown := Value;
  Invalidate;
end;

procedure TrkSmartPath.SetPathsToAdd(Value: TspPaths);
begin
  FSpecialFolders := Value;
end;

procedure TrkSmartPath.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TrkSmartPath.AfterConstruction;
var
  TmpHandle: THandle;
begin
  inherited;
  if InCreate then
    Exit;
  TmpHandle := SysImageListHandle('', False);
  if TmpHandle <> 0 then
  begin
    FImages.Handle := TmpHandle;
    FImages.ShareImages := True;
  end;
end;

function TrkSmartPath.CalcPath: Integer;
// Calculate how many parts of the path that will fit in view
var
  i, w, btns: Integer;
begin
  Result := 0;
  btns := 0;
  if (FPaths.Count = 0) or InCreate then
    Exit;
  FBmp.Canvas.Font.Assign(Font);
  w := 22 + 14;
  if FButton then
    btns := FBtnWidth;
  if FDropDown then
    btns := btns + FDropBtnWidth;

  for i := 0 to FPathsShow.Count - 1 do
    w := w + FBmp.Canvas.TextWidth(FPathsShow[i]) + 23;
  if not FShowArrow then
    w := w - 14;
  if w < ClientWidth - (8 + btns) then
  begin
    if (FPathsShow.Count > 1) and (Result = 0) then
      if FHideFirst then
        Result := 1;
    Exit;
  end;
  w := w - (ClientWidth - (8 + btns));
  i := 0;
  while (w > 0) and (i < FPaths.Count - 1) do
  begin
    w := w - (FBmp.Canvas.TextWidth(FPathsShow[i]) + 23);
    i := i + 1;
  end;
  if w > 0 then
    Result := FPaths.Count
  else
    Result := i;
  if (FPaths.Count > 1) and (Result = 0) then
    if FHideFirst then
      Result := 1;
end;

constructor TrkSmartPath.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InCreate := True;
  Width := 150;
  Height := 25;
  ControlStyle := ControlStyle + [csClickEvents, csReplicatable, csNeedsBorderPaint,
    csCaptureMouse];
  // Color := clGray;
  ComputerAsDefault := True;

  FAuto := True;
  FAllowEdit := True;
  FAllowKeyNav := False;
  FAutoComplete := True;

  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf32Bit;
  FBorderColor := clSilver;
  FBorderStyle := bsNone;
  FBtnIdx := -1;
  FBtnWidth := 25;
  FButton := False;
  FButtonDown := False;
  FClickSetPath := True;
  FCol1 := $F2F2F2;
  FCol2 := $E3E3E3;
  FCol3 := $FDF6EA;
  FCol4 := $F4D9A6;
  FCol5 := $FDF6EA;
  FCol6 := $FADFAC;
  FCol7 := $8F8F8F;
  FCol8 := $B17F3C;
  FCol9 := $927764;
  FCol10 := $EACF9C;
  FCol11 := clBlack;
  FColorEdit := clWindow;
  FColorEnter := $00FFFCF2;
  FColorExit := $00FDF4EA;
  FCustomPaths := TStringList.Create;
  FDesktopFolder := GetSpecialFolderPath(CSIDL_PERSONAL);
  FDirMustExist := True;
  FDropDown := False;
  FDropBtnWidth := 18;
  FDropBtnState := spNormal;
  FEmptyPathIcon := -1;
  FEmptyPathText := '';
  FFramed := True;
  FFromRight := False;
  FHideFirst := False;
  FHotPos := -2; // -1 = HomeMenu
  FImages := TImageList.Create(nil);
  FImages.DrawingStyle := dsTransparent;
  FImageIdx := -1;
  FImagesBtn := nil;
  FImageIdxBtn := -1;
  FMenu := TPopupMenu.Create(nil);
  FMenu.OnPopup := FMenuPopup;
  FMenu.Images := FImages;
  FMenuLookup := TStringList.Create;
  FNetDiskWait := False;
  FNewFolder := 'NewFolder';
  FOwnerDraw := False;
  FPath := '';
  FPathIdx := -1;
  FPaths := TStringList.Create;
  FPathOff := -1;
  FPathsShow := TStringList.Create;
  FPopupPath := '';
  FState := spNormal;
  FShowArrow := False;
  FShowNewFolder := True;
  FSpecialFolders := [spDocuments, spDesktop];
  FTransparent := False;
  Color := FColorExit;
  ParentColor := False;
  ParentCtl3d := True;
  ParentFont := True;
  Path := FDesktopFolder;
  InCreate := False;
  InEdit := False;
  InCtrl := False;
end;

destructor TrkSmartPath.Destroy;
begin
  InShutdown := True;
  ExitEditMode;
  FMenu.Free;
  FMenuLookup.Free;
  FImages.Free;
  FCustomPaths.Free;
  FPaths.Free;
  FPathsShow.Free;
  FBmp.Free;
  inherited Destroy;
end;

procedure TrkSmartPath.Resize;
begin
  if InShutdown or InCreate then
    Exit;
  FPathOff := CalcPath;
  Invalidate;
end;

procedure TrkSmartPath.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of dWord = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TrkSmartPath.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not InCreate then
    SetFocus;
  if (Button = mbLeft) then
  begin
    FHotPt := Point(X, Y);
    CalcState;

    if InEdit then
    begin
      if (FBtnState <> spNormal) then
        ExitEditMode;
      FButtonDown := True;
      CalcState;
      PaintPath;
      Exit;
    end;
    FBtnIdx := FHotPos;
    if (FPopUpX = FHotPos) and (FState <> spNormal) and (FBtnState <> spNormal) then
    begin
      FPopUpX := -2;
      Exit;
    end;
    if (FState = spArrowHot) then
      FPopUp := True
    else
      FPopUp := False;
    FButtonDown := True;
    if not InEdit then
      PaintPath;
    if (FPopUp) and (FPopupPath <> '') then
    begin
      if FFromRight then
        FMenu.Alignment := paRight
      else
        FMenu.Alignment := paLeft;
      if (FAuto) then
        FMenu.Popup(FPopupPos.X, FPopupPos.Y)
      else if Assigned(FCustomPopup) then
        FCustomPopup.Popup(FPopupPos.X, FPopupPos.Y);
      PostMessage(Handle, CM_REFRESH, 0, 0);
    end;
    if (FState = spNormal) and (FBtnState = spNormal) and (FDropBtnState = spNormal) and
      (FAllowEdit = True) and (not InEdit) then
      DoEdit(False);
  end;
  if (FState = spNormal) then
    FPopupPath := FPath;
  inherited;
end;

procedure TrkSmartPath.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FPopUp then
    Exit;
  FHotPt := Point(X, Y);
  CalcState;
  if (FState = spButtonPressed) and (FClickSetPath) and (FInButton) then
  begin
    Path := FPopupPath
  end
  else if (FBtnState = spButtonPressed) and (FInButton) then
  begin
    if Assigned(FOnBtnClick) then
      FOnBtnClick(Self);
  end
  else if (FDropBtnState = spButtonPressed) and (FInButton) then
    if Assigned(FOnDropBtnClick) then
      FOnDropBtnClick(Self);
  FButtonDown := False;
  FPopUp := False;
  Invalidate;
  inherited;
end;

procedure TrkSmartPath.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FKeys then
  begin
    if (FLastPoint.X = -1) and (FLastPoint.Y = -1) then
      FLastPoint := Point(X, Y)
    else if (Abs(FLastPoint.X - X) > 5) or (Abs(FLastPoint.Y - Y) > 5) then
      FKeys := False;
  end
  else
  begin
    FHotPt := Point(X, Y);
    FHotPos := -2;
    if CalcState then
      Invalidate;
  end;
  inherited;
end;

procedure TrkSmartPath.CMMouseEnter(var Message: TMessage);
begin
  InCtrl := True;
  FPopUpX := -2;
  Invalidate;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  inherited
end;

procedure TrkSmartPath.CMMouseLeave(var Message: TMessage);
begin
  InCtrl := False;
  if FPopUp then
    Exit;
  FKeys := False;
  FButtonDown := False;
  FPopUp := False;
  FHotPt := Point(-1, -1);
  FHotPos := -2;
  FState := spNormal;
  Invalidate;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('rmklever', [TrkSmartPath]);
end;

end.
