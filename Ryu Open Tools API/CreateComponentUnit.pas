unit CreateComponentUnit;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateComponentUnitWizard = class(TWizard)
  public
    procedure Execute; override;
  end;

  TComponentUnitCreator = class(TUnitCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateComponentUnitWizard.Create(
//      'RYU',
//      'CreateComponentUnitWizard',
//      'Component Unit',
//      'Ryu',
//      'Create Component Unit Wizard'
//    )
//  );
//end;

{ TCreateComponentUnitWizard }

procedure TCreateComponentUnitWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TComponentUnitCreator.Create(
      InputBox('Class name', 'Class name', 'ClassName'),
      'Component'
    )
  );
end;

{ TComponentUnitCreator }

function TComponentUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.ComponentUnit.pas';
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
