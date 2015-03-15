unit CreateObjectWizard;

interface

procedure Register;

implementation

uses
  ToolsAPIUtils, ToolsAPI, Menus, IStreams,
  Windows, SysUtils, Classes, Dialogs;

type
  TCreateObjectWizard = class(TNotifierObject, IOTANotifier, IOTAKeyboardBinding)
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
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TCreateObjectWizard.Create);
end;

procedure TCreateObjectWizard.BindKeyboard(const BindingServices
  : IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding(
    [ShortCut(Ord('O'), [ssShift, ssCtrl])],
    KeyBoardHandler, nil,
    kfImplicitShift or kfImplicitModifier or kfImplicitKeypad,
    '', ''
  );
end;

procedure TCreateObjectWizard.Execute;
var
  CurrentModule: IOTAModule;
  ModuleServices: IOTAModuleServices;
begin
  if BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) <> S_OK then Exit;

  CurrentModule := ModuleServices.CurrentModule;
  if CurrentModule = nil then Exit;

  ShowMessage(SourceModuleToString(CurrentModule));
end;

function TCreateObjectWizard.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TCreateObjectWizard.GetDisplayName: string;
begin
  Result := 'Create Object wizard';
end;

function TCreateObjectWizard.GetIDString: string;
begin
  Result := 'TCreateObjectWizard';
end;

function TCreateObjectWizard.GetMenuText: string;
begin
  Result := 'Create Object wizard';
end;

function TCreateObjectWizard.GetName: string;
begin
  Result := 'Create Object wizard';
end;

function TCreateObjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TCreateObjectWizard.KeyBoardHandler(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  Execute;
  BindingResult := krHandled;
end;

end.
