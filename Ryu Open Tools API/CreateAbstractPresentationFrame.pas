unit CreateAbstractPresentationFrame;

interface

uses
  ToolsAPIUtils, Disk,
  Windows, SysUtils, ToolsAPI, Dialogs;

type
  TCreateAbstractPresentationFrame = class(TWizard)
  public
    procedure Execute; override;
  end;

  TAbstractPresentationFrameCreator = class(TFormCreator)
  public
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: String): IOTAFile; override;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterPackageWizard(
//    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
//    TCreateAbstractPresentationFrame.Create(
//      'RYU',
//      'CreateAbstractPresentationFrame',
//      'Presentation Frame',
//      'Ryu',
//      'Create Abstract Presentation Frame'
//    )
//  );
//end;

{ TCreateAbstractPresentationFrame }

procedure TCreateAbstractPresentationFrame.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    // AClassName, AAncestorName
    TAbstractPresentationFrameCreator.Create(
      InputBox('Frame name', 'Frame name', 'frMain'),
      'Frame'
    )
  );
end;

{ TAbstractPresentationFrameCreator }

function TAbstractPresentationFrameCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: String): IOTAFile;
var
  FileName, Source : string;
begin
  FileName := GetTemplatePath + 'RyuOpenToolsAPI.Template.AbstractPresentationFrame.pas';
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
