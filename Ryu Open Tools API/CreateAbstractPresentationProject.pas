unit CreateAbstractPresentationProject;

interface

procedure Register;

implementation

uses
  OTAFile, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateAbstractPresentationProject = class(TNotifierObject, IOTAWizard,
    IOTARepositoryWizard, IOTARepositoryWizard60, IOTARepositoryWizard80,
    IOTAFormWizard)
  public // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
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
  end;

  TAbstractPresentationProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator)
  private
  public // IOTACreator
    function GetCreatorType: String;
    function GetExisting: Boolean;
    function GetFileSystem: String;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    function GetFileName: string;
  public // IOTAProjectCreator
    function GetOptionFileName: string; deprecated;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
  public
    constructor Create;
  end;

procedure Register;
begin
  RegisterPackageWizard(TCreateAbstractPresentationProject.Create);
end;

{ TCreateAbstractPresentationProject }

procedure TCreateAbstractPresentationProject.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TAbstractPresentationProjectCreator.Create);
end;

function TCreateAbstractPresentationProject.GetAuthor: string;
begin
  Result := 'Ryu';
end;

function TCreateAbstractPresentationProject.GetComment: string;
begin
  Result := 'Create Abstract Presentation Project';
end;

function TCreateAbstractPresentationProject.GetDesigner: string;
begin
  Result := dVCL;
end;

function TCreateAbstractPresentationProject.GetGalleryCategory: IOTAGalleryCategory;
var
  cat: IOTAGalleryCategory;
  catMgr: IOTAGalleryCategoryManager;
begin
  catMgr := (BorlandIDEServices as IOTAGalleryCategoryManager);
  if (not Assigned(catMgr)) then
    MessageBox(0, 'Could not get reference to CategoryGalleryManager', '', MB_OK);

  cat := catMgr.FindCategory(sCategoryDelphiNew);
  if (not Assigned(cat)) then
    MessageBox(0, 'Could not get reference to sCategoryDelphiNewFiles', '', MB_OK);

  Result := cat;
end;

function TCreateAbstractPresentationProject.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'RYU');
  if (Result = 0) then MessageBox(0, 'Could not load icon LNSSTREAMABLEFORM', '', MB_OK);
end;

function TCreateAbstractPresentationProject.GetIDString: string;
begin
  Result := 'CreateAbstractPresentationProject';
end;

function TCreateAbstractPresentationProject.GetName: string;
begin
  Result := 'Presentation Project';
end;

function TCreateAbstractPresentationProject.GetPage: string;
begin
    Result := 'New';
end;

function TCreateAbstractPresentationProject.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TCreateAbstractPresentationProject.GetState: TWizardState;
begin
  Result := [];
end;

{ TAbstractPresentationProjectCreator }

constructor TAbstractPresentationProjectCreator.Create;
begin
  inherited;

//  FSingletonClassName := InputBox('Class name', 'Class name', 'Singleton');
end;

function TAbstractPresentationProjectCreator.GetCreatorType: String;
begin
  Result := '';
end;

function TAbstractPresentationProjectCreator.GetExisting: Boolean;
begin
  Result := false;
end;

function TAbstractPresentationProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TAbstractPresentationProjectCreator.GetFileSystem: String;
begin
  Result := '';
end;

function TAbstractPresentationProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TAbstractPresentationProjectCreator.GetOwner: IOTAModule;
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

function TAbstractPresentationProjectCreator.GetShowSource: Boolean;
begin
  Result := true;
end;

function TAbstractPresentationProjectCreator.GetUnnamed: Boolean;
begin
  Result := true;
end;

procedure TAbstractPresentationProjectCreator.NewDefaultModule;
begin
  //
end;

function TAbstractPresentationProjectCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TAbstractPresentationProjectCreator.NewProjectResource(
  const Project: IOTAProject);
begin
  //
end;

function TAbstractPresentationProjectCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TOTAFile.Create('');
end;

end.
