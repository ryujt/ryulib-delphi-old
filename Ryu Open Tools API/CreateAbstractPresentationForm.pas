unit CreateAbstractPresentationForm;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateAbstractPresentationForm = class(TWizard)
  public
    procedure Execute; override;
  end;

  TAbstractPresentationFormCreator = class(TFormCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateAbstractPresentationForm.Create(
//      'RYU',
//      'CreateAbstractPresentationForm',
//      'Presentation Form',
//      'Ryu',
//      'Create Abstract Presentation Form'
//    )
//  );
//end;

{ TCreateAbstractPresentationForm }

procedure TCreateAbstractPresentationForm.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TAbstractPresentationFormCreator.Create(
      InputBox('Form name', 'Form name', 'fmMain'),
      'Form'
    )
  );
end;

{ TAbstractPresentationFormCreator }

function TAbstractPresentationFormCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.AbstractPresentationForm.pas';
  if not FileExists(FileName) then begin
    MessageDlg(Format('File not found. (%s)', [FileName]), mtError, [mbOk], 0);
    Source := '';
  end else begin
    Source := LoadFileAsText(FileName);
    Source := StringReplace(Source, '@UnitName', ModuleIdent, [rfReplaceAll, rfIgnoreCase]);
    Source := StringReplace(Source, '@FormName', FClassName, [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := TOTAFile.Create(Source);
end;

end.
