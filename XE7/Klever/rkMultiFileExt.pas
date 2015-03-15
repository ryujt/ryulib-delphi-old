unit rkMultiFileExt;

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

  TrkMultiFileExt = Class;

  TrkMultiExtItem = Class(TCollectionItem)
  Private
    FExtensions:TStringList;
    FOptions:   TExtOptions;
    FTag:       Integer;
    FActive:    Boolean;
    procedure   SetExtensions(Value: TStringList);
  Public
    Constructor Create(Collection: TCollection); Override;
    Destructor  Destroy; Override;
  Protected
    FOnValidExt         : TExtFileEvent;
  Published
    property  Extensions: TStringList read FExtensions write SetExtensions;
    property  Options:    TExtOptions read FOptions write FOptions;
    Property  Active: Boolean read FActive write FActive default TRUE;
    Property  Tag: Integer read FTag write Ftag;
    property  OnValidExt: TExtFileEvent read FOnValidExt write FOnValidExt;
  End;

  TrkMultiExtItems = Class(TCollection)
  private
    FOwner:     TrkMultiFileExt;
  protected
    Function    GetItem(Index:Integer): TrkMultiExtItem;
    Function    GetOwner:TPersistent; override;
  public
    Property    Items[Index:Integer]: TrkMultiExtItem read GetItem; default;
    Function    Add: TrkMultiExtItem;
    Constructor Create(AOwner: TrkMultiFileExt);
  End;

  TrkMultiFileExt = Class(TComponent)
  Private
    FData:      TrkMultiExtItems;
    FIndex:     Integer;
    Procedure   SetData(Value: TrkMultiExtItems);
  Protected
    FOnNotValidExt      : TExtFileEvent;
  Public
    Constructor Create(AOwner:TComponent);override;
    Destructor  Destroy;Override;
    function    ResolveLink(name: string): string;
    function    ValidFile(name: string): boolean;
    function    GetValidFile(name: string): string;
    procedure   ValidateFileList(fileList: TStrings);
  Published
    property    FileExt: TrkMultiExtItems read FData write SetData;
    property    OnNotValidExt: TExtFileEvent read FOnNotValidExt write FOnNotValidExt;
  End;

procedure Register;

implementation

uses
  Dialogs, ComObj, ActiveX, ShellApi, ShlObj;

Constructor TrkMultiExtItem.Create(Collection: TCollection);
Begin
  inherited Create(Collection);
  FActive:=True;
  FExtensions:= TStringList.Create;
  FOptions:= [extResolveLink, extFileMustExist];
End;

Destructor TrkMultiExtItem.Destroy;
Begin
  FExtensions.Free;
  inherited Destroy;
End;

Constructor TrkMultiFileExt.Create(AOwner:TComponent);
Begin
  inherited Create(AOwner);
  FData:= TrkMultiExtItems.Create(Self);
end;

Destructor TrkMultiFileExt.Destroy;
Begin
  FData.free;
  inherited;
end;

Procedure TrkMultiFileExt.SetData(Value: TrkMultiExtItems);
Begin
  FData.Assign(Value);
End;

////////////////////////////////////////////////////////////
// TrkMultiExtItems
//
Constructor TrkMultiExtItems.Create(AOwner: TrkMultiFileExt);
begin
  inherited Create(TrkMultiExtItem);
  Fowner:=AOwner;
End;

Function TrkMultiExtItems.Add: TrkMultiExtItem;
Begin
  result:= TrkMultiExtItem(inherited Add);
End;

Function TrkMultiExtItems.GetItem(index:Integer): TrkMultiExtItem;
Begin
  result:= TrkMultiExtItem(inherited GetItem(index));
End;

Function TrkMultiExtItems.GetOwner:TPersistent;
begin
  result:= TPersistent(Fowner);
End;

procedure TrkMultiExtItem.SetExtensions(Value: TStringList);
var
  counter: Integer;
begin
  if Value.Count - 1 > High(Byte) then Exit;  // Not more than 255 extensions
  for counter:= 0 to Value.Count - 1 do Value[counter]:= LowerCase(Trim(Value[counter]));
  FExtensions.Assign(Value);
end;


// Support routine... Extract Target from Shortcut...
//
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
function TrkMultiFileExt.ResolveLink(name: string): string;
begin
  Result:= GetLinkTarget(name);
end;

// This routine will check if name contains a valid ext. If it does
// it will return true otherwise it will return false.
//
function TrkMultiFileExt.ValidFile(name: string): boolean;
var
  resFilename, filename, ext: string;
  i, j: integer;
begin
  resFilename:= name;
  ext:= LowerCase(ExtractFileExt(name));
  if ext = '.lnk' then resFilename:= ResolveLink(name);
  filename:= resFilename;
  ext:= LowerCase(ExtractFileExt(filename));

  // Got the extension. Now we must search for it among the FileExt items
  j:= -1;
  for i:= 0 to FileExt.Count-1 do
    if self.FileExt[i].Extensions.IndexOf(ext) <> -1 then j:= i;

  if (j = -1) or (self.FileExt[j].Active = false) then Result:= false
  else begin
    filename:= name;
    with self.FileExt[j] do begin
      ext:= LowerCase(ExtractFileExt(name));
      if ext = '.lnk' then begin
        if (extIgnoreLink in FOptions) then filename:= ''
        else
        if (extResolveLink in FOptions) then begin
          filename:= resFilename;
          ext:= LowerCase(ExtractFileExt(filename));
        end;
      end;
      if filename = '' then  Result:= false
      else begin
        if (extFileMustExist in FOptions) then begin
          if FileExists(Filename) then
            if Extensions.IndexOf(ext) <> -1 then Result:= true else Result:= false;
        end else
          if Extensions.IndexOf(ext) <> -1 then Result:= true else Result:= false;
      end;
    end;
  end;
end;

// This routine will check if name contains a valid ext. If it does
// it will return the filename otherwise it will return a nil string
//
function TrkMultiFileExt.GetValidFile(name: string): string;
var
  filename, ext, resFilename: string;
  i, j: integer;
begin
  FIndex:= -1;
  resFilename:= name;
  ext:= LowerCase(ExtractFileExt(name));
  if ext = '.lnk' then resFilename:= ResolveLink(name);
  filename:= resFilename;
  ext:= LowerCase(ExtractFileExt(filename));

  // Got the extension. Now we must search for it among the FileExt items
  j:= -1;
  for i:= 0 to FileExt.Count-1 do begin
    if self.FileExt[i].Extensions.IndexOf(ext) <> -1 then j:= i;
  end;
  if (j = -1) or (self.FileExt[j].Active = false) then Result:= ''
  else begin
    FIndex:= j;
    filename:= name;
    with self.FileExt[j] do begin
      ext:= LowerCase(ExtractFileExt(name));
      if ext = '.lnk' then begin
        if (extIgnoreLink in FOptions) then filename:= ''
        else
        if (extResolveLink in FOptions) then begin
          filename:= resFilename;
          ext:= LowerCase(ExtractFileExt(filename));
        end;
      end;
      if filename = '' then  Result:= ''
      else begin
        if (extFileMustExist in FOptions) then begin
          if FileExists(Filename) then
            if Extensions.IndexOf(ext) <> -1 then Result:= filename else Result:= '';
        end else
          if Extensions.IndexOf(ext) <> -1 then Result:= filename else Result:= '';
      end;
    end;
  end;
end;

// This routine takes a a stringlist and sort it into valid and nonvalid
// files. If the extension is valid then it will execute OnValidExt if
// the ext is not valid then it executes OnNotValidExt.
//
procedure TrkMultiFileExt.ValidateFileList(fileList: TStrings);
var
  filename: string;
  i: integer;
begin
  for i:= 0 to fileList.Count - 1 do begin
    filename:= GetValidFile(fileList[i]);
    if FIndex <> -1 then begin
      with self.FileExt[FIndex] do begin
        showMessage(filename);
        if filename <> '' then
          if Assigned(FOnValidExt) then FOnValidExt(self, filename);
      end;
    end;
    if filename = '' then
      if Assigned(FOnNotValidExt) then FOnNotValidExt(self, fileList[i]);
  end;
end;

procedure Register;
begin
  RegisterComponents('rmkCollection', [TrkMultiFileExt]);
end;

end.

