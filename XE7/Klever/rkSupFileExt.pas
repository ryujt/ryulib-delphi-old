unit rkSupFileExt;

interface

uses
  Windows, Classes, Sysutils;

type
  EShortcutError = class(Exception);

  TExtFileEvent = procedure(Sender: TObject; name: string) of object;

  TExtOptions = Set of
    (
    extIgnoreLink,
    extResolveLink,
    extFileMustExist
    );

  TrkSupFileExt = Class(TComponent)
  private
          { Private declarations }
    FSupFileExt		: TStringList;
    FOptions            : TExtOptions;
    procedure   SetExtensions(Value: TStringList);
  protected
          { Protected declarations }
    FOnValidExt         : TExtFileEvent;
    FOnNotValidExt      : TExtFileEvent;
  public
          { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ResolveLink(name: string): string;
    function    ValidFile(name: string): boolean;
    function    GetValidFile(name: string): string;
    procedure   ValidateFileList(fileList: TStrings);
  published
          { Published declarations }
    // Properties
    property SupFileExt: TStringList read FSupFileExt write SetExtensions;
    property Options:    TExtOptions read FOptions write FOptions;
    // Events
    property OnValidExt: TExtFileEvent read FOnValidExt write FOnValidExt;
    property OnNotValidExt: TExtFileEvent read FOnNotValidExt write FOnNotValidExt;
  end;

procedure Register;

implementation

uses
  ComObj, ActiveX, ShellApi, ShlObj;

constructor TrkSupFileExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSupFileExt:= TStringList.Create;
  FSupFileExt.Add('Add your supported file extensions here');
  FOptions:= [extResolveLink];
end;

destructor TrkSupFileExt.Destroy;
begin
  FSupFileExt.Free;
  inherited Destroy;
end;

procedure TrkSupFileExt.SetExtensions(Value: TStringList);
var
  counter: Integer;
begin
  if Value.Count - 1 > High(Byte) then Exit;  // Not more than 255 extensions
  for counter:= 0 to Value.Count - 1 do Value[counter]:= LowerCase(Trim(Value[counter]));
  FSupFileExt.Assign(Value);
end;

function GetLinkTarget(linkFile: string): string;
const
  IID_IPersistFile: TGUID = (
    D1:$0000010B; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
var
  Ems: HResult;
  Psl: IShellLink;
  Ppf: IPersistFile;
  WFileName: WideString;
  Data: TWin32FindData;
  Buffer: array [0..MAX_PATH-1] of char;
begin
  Ems:= CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkA, psl);
  if Ems <> 0 then Result:= '';
  Ems:= psl.QueryInterface(IID_IPersistFile, ppf);
  if Ems <> 0 then Result:= '';
  WFileName:= linkFile;
  Ems:= ppf.Load(PWChar(WFileName), STGM_READ);
  if Ems <> 0 then begin
    raise EShortcutError.Create('Unable to open link file');
    Result:= '';
  end;
  Ems:= psl.Resolve(0, SLR_ANY_MATCH);
  if Ems <> 0 then Result:= '';
  Ems:= psl.GetPath(Buffer, Sizeof(Buffer), Data, SLGP_UNCPRIORITY);
  If Ems<>0 then begin
    raise EShortcutError.Create('Error in getting Target');
    Result:= '';
  end;
  Result:= Buffer;
end;

// Public functions starts here

// This routine will return the Target name of a ShortCut link file
//
function TrkSupFileExt.ResolveLink(name: string): string;
begin
  Result:= GetLinkTarget(name);
end;

// This routine will check if name contains a valid ext. If it does
// it will return true otherwise it will return false.
//
function TrkSupFileExt.ValidFile(name: string): boolean;
var
  filename, ext: string;
begin
  filename:= name;
  ext:= LowerCase(ExtractFileExt(name));
  if ext = '.lnk' then begin
    if (extIgnoreLink in FOptions) then filename:= ''
    else
    if (extResolveLink in FOptions) then begin
      filename:= ResolveLink(name);
      ext:= LowerCase(ExtractFileExt(filename));
    end;
  end;
  if filename = '' then  Result:= false
  else
  Result:= SupFileExt.IndexOf(ext) <> -1;
end;

// This routine will check if name contains a valid ext. If it does
// it will return the filename otherwise it will return a nil string
//
function TrkSupFileExt.GetValidFile(name: string): string;
var
  filename, ext: string;
begin
  filename:= name;
  ext:= LowerCase(ExtractFileExt(name));
  if ext = '.lnk' then begin
    if (extIgnoreLink in FOptions) then filename:= ''
    else
    if (extResolveLink in FOptions) then begin
      filename:= ResolveLink(name);
      ext:= LowerCase(ExtractFileExt(filename));
    end;
  end;
  if filename = '' then  Result:= ''
  else begin
    if (extFileMustExist in FOptions) then begin
      if FileExists(Filename) then
        if SupFileExt.IndexOf(ext) <> -1 then Result:= filename else Result:= '';
    end else
      if SupFileExt.IndexOf(ext) <> -1 then Result:= filename else Result:= '';
  end;
end;

// This routine takes a a stringlist and sort it into valid and nonvalid
// files. If the extension is valid then it will execute OnValidExt if
// the ext is not valid then it executes OnNotValidExt.
//
procedure TrkSupFileExt.ValidateFileList(fileList: TStrings);
var
  filename: string;
  i: integer;
begin
  for i:= 0 to fileList.Count - 1 do begin
    filename:= GetValidFile(fileList[i]);
    if filename = '' then
      if Assigned(FOnNotValidExt) then FOnNotValidExt(self, fileList[i]);
    if filename <> '' then
      if Assigned(FOnValidExt) then FOnValidExt(self, filename);
  end;
end;

procedure Register;
begin
  RegisterComponents('rmkCollection', [TrkSupFileExt]);
end;

end.

