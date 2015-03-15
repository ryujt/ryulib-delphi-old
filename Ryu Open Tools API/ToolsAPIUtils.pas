unit ToolsAPIUtils;

interface

uses
  Strg, Sys,
  Windows, SysUtils, Classes, ToolsAPI;

type
  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: String;
  public
    constructor Create(const Source: String);
    function GetSource: String;
    function GetAge: TDateTime;
  end;

  TWizard = class (TNotifierObject, IOTAWizard,
    IOTARepositoryWizard, IOTARepositoryWizard60, IOTARepositoryWizard80,
    IOTAFormWizard)
  private
    FIcon, FIDString, FName, FAuthor, FCommnet : string;
  public // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute; virtual; abstract;
  public // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  public // IOTARepositoryWizard60
    function GetDesigner: string;
    property Designer: string read GetDesigner;
  public // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  public
    constructor Create(AIcon,AIDString,AName,AAuthor,ACommnet:string); reintroduce; virtual;
  end;

  TUnitCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  protected
    FClassName, FAncestorName : string;
  public // IOTACreator
    function GetCreatorType: String; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: String;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  public // IOTAModuleCreator
    function GetAncestorName: String;
    function GetImplFileName: String;
    function GetIntfFileName: String;
    function GetFormName: String; virtual;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: String): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; virtual; abstract;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create(AClassName,AAncestorName:string); reintroduce; virtual;
  end;

  TFormCreator = class(TUnitCreator)
  public
    function GetCreatorType: String; override;
    function GetFormName: String; override;
  end;

function GetTemplatePath:string;

function GetActiveProject: IOTAProject;
function FindSourceEditor(SourceModule: IOTAModule): IOTASourceEditor;
function GetSourceModuleLength(SourceEditor: IOTASourceEditor): longint;

procedure SourceModuleToStream(SourceModule:IOTAModule; Stream:TStream);
function SourceModuleToString(SourceModule:IOTAModule):AnsiString;

implementation

function GetTemplatePath:string;
begin
  Result := RegReadString(HKEY_LOCAL_MACHINE, 'SoftWare\RyuLib\Delphi\OpenToolsAPI\', 'Path');
  SetLastChar(Result, '\');
end;

function GetActiveProject: IOTAProject;
var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;

  AModuleServices := (BorlandIDEServices as IOTAModuleServices);

  for i := 0 to AModuleServices.ModuleCount - 1 do begin
    AModule := AModuleServices.Modules[i];
    if AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK then Break;
  end;

  if Assigned(AProjectGroup) then Result := AProjectGroup.ActiveProject;

  AModuleServices := nil;
  AModule := nil;
  AProjectGroup := nil;
end;

function FindSourceEditor(SourceModule: IOTAModule): IOTASourceEditor;
var
  Loop: integer;
begin
  Loop := 0;
  while (Loop < SourceModule.GetModuleFileCount) do begin
    if SourceModule.GetModuleFileEditor(Loop).QueryInterface(IOTASourceEditor, Result) = S_OK then Break;
    Inc(Loop);
  end;
end;

function GetSourceModuleLength(SourceEditor: IOTASourceEditor): longint;
var
  Buffer: char;
  Reader: IOTAEditReader;
begin
  Result := 0;

  Reader := SourceEditor.CreateReader;
  while Reader.getText(result, @Buffer, 1) > 0 do Inc(Result);

  Reader := nil;
end;

procedure SourceModuleToStream(SourceModule:IOTAModule; Stream:TStream);
var
  moduleSize: longint;
  SourceEditor: IOTASourceEditor;
  Reader: IOTAEditReader;
  iPos: longint;
  Buffer : char;
begin
  SourceEditor := FindSourceEditor(SourceModule);
  if SourceEditor = nil then Exit;

  moduleSize := GetSourceModuleLength(SourceEditor);
  Stream.Size := moduleSize;

  SourceEditor := nil;
  SourceEditor := FindSourceEditor(SourceModule);
  Reader := SourceEditor.CreateReader;

  iPos := 0;
  while Reader.GetText(iPos, @Buffer, 1) > 0 do begin
    Stream.Write(Buffer, SizeOf(Buffer));
    Inc(iPos);
  end;
end;

function SourceModuleToString(SourceModule:IOTAModule):AnsiString;
var
  SourceEditor: IOTASourceEditor;
  Reader: IOTAEditReader;
  iPos: longint;
  Buffer : char;
begin
  SourceEditor := FindSourceEditor(SourceModule);
  if SourceEditor = nil then Exit;

  SetLength(Result, GetSourceModuleLength(SourceEditor));

  SourceEditor := nil;
  SourceEditor := FindSourceEditor(SourceModule);
  Reader := SourceEditor.CreateReader;

  Reader.GetText(0, @Result[1], Length(Result));
end;

{ TOTAFile }

constructor TOTAFile.Create(const Source: String);
begin
  inherited Create;

  FSource := Source;
end;

function TOTAFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TOTAFile.GetSource: String;
begin
  Result := FSource;
end;

{ TWizard }

constructor TWizard.Create(AIcon, AIDString, AName, AAuthor, ACommnet: string);
begin
  inherited Create;

  FIcon := AIcon;
  FIDString := AIDString;
  FName := AName;
  FAuthor := AAuthor;
  FCommnet := ACommnet;
end;

function TWizard.GetAuthor: string;
begin
  Result := FAuthor;
end;

function TWizard.GetComment: string;
begin
  Result := FCommnet;
end;

function TWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  Manager: IOTAGalleryCategoryManager;
begin
  Result := nil;

  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then Result := Manager.FindCategory('Ryu');
end;

function TWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'RYU');
  if (Result = 0) then MessageBox(0, 'Could not load icon LNSSTREAMABLEFORM', '', MB_OK);
end;

function TWizard.GetIDString: string;
begin
  Result := FIDString;
end;

function TWizard.GetName: string;
begin
  Result := FName;
end;

function TWizard.GetPage: string;
begin
    Result := 'New';
end;

function TWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TWizard.GetState: TWizardState;
begin
  Result := [];
end;

{ TUnitCreator }

constructor TUnitCreator.Create(AClassName, AAncestorName: string);
begin
  inherited Create;

  FClassName := AClassName;
  FAncestorName := AAncestorName;
end;

procedure TUnitCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
  // Do nothing
end;

function TUnitCreator.GetAncestorName: String;
begin
  Result := FAncestorName;
end;

function TUnitCreator.GetCreatorType: String;
begin
  Result := sUnit;
end;

function TUnitCreator.GetExisting: Boolean;
begin
  Result := false;
end;

function TUnitCreator.GetFileSystem: String;
begin
  Result := '';
end;

function TUnitCreator.GetFormName: String;
begin
  Result := '';
end;

function TUnitCreator.GetImplFileName: String;
begin
  Result := '';
end;

function TUnitCreator.GetIntfFileName: String;
begin
  Result := '';
end;

function TUnitCreator.GetMainForm: Boolean;
begin
  Result := false;
end;

function TUnitCreator.GetOwner: IOTAModule;
var
  module: IOTAModule;
  newModule: IOTAModule;
begin
  module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if (Assigned(module)) then begin
    if (module.QueryInterface(IOTAProject, newModule) = S_OK) then Result := newModule
    else if (module.OwnerCount > 0) then begin
      newModule := module.OwnerModules[0];
      if (Assigned(newModule)) then
        if (newModule.QueryInterface(IOTAProject, Result) <> S_OK) then Result := nil;
    end;
  end;
end;

function TUnitCreator.GetShowForm: Boolean;
begin
  Result := false;
end;

function TUnitCreator.GetShowSource: Boolean;
begin
  Result := true;
end;

function TUnitCreator.GetUnnamed: Boolean;
begin
  Result := true;
end;

function TUnitCreator.NewFormFile(const FormIdent,
  AncestorIdent: String): IOTAFile;
begin
  Result := nil;
end;

function TUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: String): IOTAFile;
begin
  Result := nil;
end;

{ TFormCreator }

function TFormCreator.GetCreatorType: String;
begin
  Result := sForm;
end;

function TFormCreator.GetFormName: String;
begin
  Result := FClassName;
end;

end.
