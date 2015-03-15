unit CreateClassUnit;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateClassUnitWizard = class(TWizard)
  public
    procedure Execute; override;
  end;

  TClassUnitCreator = class(TUnitCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateClassUnitWizard.Create(
//      'RYU',
//      'CreateClassUnitWizard',
//      'Class Unit',
//      'Ryu',
//      'Create Class Unit Wizard'
//    )
//  );
//end;

{ TCreateClassUnitWizard }

procedure TCreateClassUnitWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TClassUnitCreator.Create(
      InputBox('Class name', 'Class name', 'ClassName'),
      'Object'
    )
  );
end;

{ TClassUnitCreator }

function TClassUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.ClassUnit.pas';
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
