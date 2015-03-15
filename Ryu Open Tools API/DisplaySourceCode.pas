unit DisplaySourceCode;

interface

procedure Register;

implementation

uses
  ToolsAPIUtils, ToolsAPI, Menus, IStreams,
  Windows, SysUtils, Classes, Dialogs;

type
  TDisplaySourceCode = class(TNotifierObject, IOTANotifier, IOTAKeyboardBinding)
  public // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    procedure KeyBoardHandler(const Context: IOTAKeyContext; KeyCode: TShortcut;
      var BindingResult: TKeyBindingResult);
  public // IOTAMenuWizard
    function GetMenuText: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
  end;

procedure Register;
begin
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TDisplaySourceCode.Create);
end;

procedure TDisplaySourceCode.BindKeyboard(const BindingServices
  : IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding(
    [ShortCut(Ord('O'), [ssShift, ssCtrl])],
    KeyBoardHandler, nil,
    kfImplicitShift or kfImplicitModifier or kfImplicitKeypad,
    '', ''
  );
end;

procedure TDisplaySourceCode.Execute;
var
  CurrentModule, SourceModule: IOTAModule;
  ModuleServices: IOTAModuleServices;
begin
  if BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) <> S_OK then Exit;

  CurrentModule := ModuleServices.CurrentModule;
  if CurrentModule = nil then Exit;

  ShowMessage(SourceModuleToString(CurrentModule));
end;

function TDisplaySourceCode.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TDisplaySourceCode.GetDisplayName: string;
begin
  Result := 'Create Object wizard';
end;

function TDisplaySourceCode.GetIDString: string;
begin
  Result := 'TCreateObjectWizard';
end;

function TDisplaySourceCode.GetMenuText: string;
begin
  Result := 'Create Object wizard';
end;

function TDisplaySourceCode.GetName: string;
begin
  Result := 'Create Object wizard';
end;

function TDisplaySourceCode.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TDisplaySourceCode.KeyBoardHandler(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  Execute;
  BindingResult := krHandled;
end;

end.
