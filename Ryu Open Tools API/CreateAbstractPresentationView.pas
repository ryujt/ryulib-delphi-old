unit CreateAbstractPresentationView;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateAbstractPresentationView = class(TWizard)
  public
    procedure Execute; override;
  end;

  TAbstractPresentationViewCreator = class(TUnitCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateAbstractPresentationView.Create(
//      'RYU',
//      'CreateAbstractPresentationView',
//      'Presentation View',
//      'Ryu',
//      'Create Abstract Presentation View'
//    )
//  );
//end;

{ TCreateAbstractPresentationView }

procedure TCreateAbstractPresentationView.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TAbstractPresentationViewCreator.Create(
      '',
      'Component'
    )
  );
end;

{ TAbstractPresentationViewCreator }

function TAbstractPresentationViewCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.AbstractPresentationView.pas';
  if not FileExists(FileName) then begin
    MessageDlg(Format('File not found. (%s)', [FileName]), mtError, [mbOk], 0);
    Source := '';
  end else begin
    Source := LoadFileAsText(FileName);
  end;

  Result := TOTAFile.Create(Source);
end;

end.
