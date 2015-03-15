unit CreateSingletonUnit;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateSingletonUnitWizard = class(TWizard)
  public
    procedure Execute; override;
  end;

  TSingletonUnitCreator = class(TUnitCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateSingletonUnitWizard.Create(
//      'RYU',
//      'SingletonUnitCreator',
//      'Singleton Unit',
//      'Ryu',
//      'Create Singleton Unit Wizard'
//    )
//  );
//end;

{ TCreateSingletonUnitWizard }

procedure TCreateSingletonUnitWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TSingletonUnitCreator.Create(
      InputBox('Class name', 'Class name', 'ClassName'),
      'Object'
    )
  );
end;

{ TSingletonUnitCreator }

function TSingletonUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.Singleton.pas';
  if not FileExists(FileName) then begin
    MessageDlg(Format('File not found. (%s)', [FileName]), mtError, [mbOk], 0);
    Source := '';
  end else begin
    Source := LoadFileAsText(FileName);
    Source := StringReplace(Source, '@ClassName', FClassName, [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := TOTAFile.Create(Source);
end;

end.
