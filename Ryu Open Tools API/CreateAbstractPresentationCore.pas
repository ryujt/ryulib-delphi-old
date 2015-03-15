unit CreateAbstractPresentationCore;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateAbstractPresentationCore = class(TWizard)
  public
    procedure Execute; override;
  end;

  TAbstractPresentationCoreCreator = class(TUnitCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateAbstractPresentationCore.Create(
//      'RYU',
//      'CreateAbstractPresentationCore',
//      'Presentation Core',
//      'Ryu',
//      'Create Abstract Presentation Core'
//    )
//  );
//end;

{ TCreateAbstractPresentationCore }

procedure TCreateAbstractPresentationCore.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TAbstractPresentationCoreCreator.Create(
      '',
      'Object'
    )
  );
end;

{ TAbstractPresentationCoreCreator }

function TAbstractPresentationCoreCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.AbstractPresentationCore.pas';
  if not FileExists(FileName) then begin
    MessageDlg(Format('File not found. (%s)', [FileName]), mtError, [mbOk], 0);
    Source := '';
  end else begin
    Source := LoadFileAsText(FileName);
  end;

  Result := TOTAFile.Create(Source);
end;

end.
