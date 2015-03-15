unit RyuCodeInsightManager;

interface

implementation

uses
  DebugTools, ToolsAPIUtils, ToolsAPI, Menus, IStreams,
  Windows, SysUtils, Classes, Dialogs;

type
  TRyuCodeInsightManager = class(TNotifierObject, IOTANotifier, IOTACodeInsightManager)
  public // IOTACodeInsightManager100
    function GetName: string;
    function GetIDString: string;
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function EditorTokenValidChars(PreValidating: Boolean): TSysCharSet;
    procedure AllowCodeInsight(var Allow: Boolean; const Key: Char);
    function PreValidateCodeInsight(const Str: string): Boolean;
    function IsViewerBrowsable(Index: Integer): Boolean;
    function GetMultiSelect: Boolean;
    procedure GetSymbolList(out SymbolList: IOTACodeInsightSymbolList);
    procedure OnEditorKey(Key: Char; var CloseViewer: Boolean; var Accept: Boolean);
    function HandlesFile(const AFileName: string): Boolean;
    function GetLongestItem: string;
    procedure GetParameterList(out ParameterList: IOTACodeInsightParameterList);
    procedure GetCodeInsightType(AChar: Char; AElement: Integer; out CodeInsightType: TOTACodeInsightType;
      out InvokeType: TOTAInvokeType);
    function InvokeCodeCompletion(HowInvoked: TOTAInvokeType; var Str: string): Boolean;
    function InvokeParameterCodeInsight(HowInvoked: TOTAInvokeType; var SelectedIndex: Integer): Boolean;
    procedure ParameterCodeInsightAnchorPos(var EdPos: TOTAEditPos);
    function ParameterCodeInsightParamIndex(EdPos: TOTAEditPos): Integer;
    function GetHintText(HintLine, HintCol: Integer): string;
    function GotoDefinition(out AFileName: string; out ALineNum: Integer; Index: Integer = -1): Boolean;
    procedure Done(Accepted: Boolean; out DisplayParams: Boolean);
  public // IOTACodeInsightManager
    function GetOptionSetName: string;
  end;

  TSymbolList = class (TInterfacedObject, IOTACodeInsightSymbolList)
  public // IOTACodeInsightSymbolList
    procedure Clear;
    function GetCount: Integer;
    function GetSymbolIsReadWrite(I: Integer): Boolean;
    function GetSymbolIsAbstract(I: Integer): Boolean;
    function GetViewerSymbolFlags(I: Integer): TOTAViewerSymbolFlags;
    function GetViewerVisibilityFlags(I: Integer): TOTAViewerVisibilityFlags;
    function GetProcDispatchFlags(I: Integer): TOTAProcDispatchFlags;
    procedure SetSortOrder(const Value: TOTASortOrder);
    function GetSortOrder: TOTASortOrder;
    function FindIdent(const AnIdent: string): Integer;
    function FindSymIndex(const Ident: string; var Index: Integer): Boolean;
    procedure SetFilter(const FilterText: string);
    function GetSymbolText(Index: Integer): string;
    function GetSymbolTypeText(Index: Integer): string;
    function GetSymbolClassText(I: Integer): string;
  end;

{ TRyuCodeInsightManager }

procedure TRyuCodeInsightManager.AllowCodeInsight(var Allow: Boolean;
  const Key: Char);
begin
  Allow := true;
end;

procedure TRyuCodeInsightManager.Done(Accepted: Boolean;
  out DisplayParams: Boolean);
begin

end;

function TRyuCodeInsightManager.EditorTokenValidChars(
  PreValidating: Boolean): TSysCharSet;
begin

end;

procedure TRyuCodeInsightManager.GetCodeInsightType(AChar: Char;
  AElement: Integer; out CodeInsightType: TOTACodeInsightType;
  out InvokeType: TOTAInvokeType);
begin
  CodeInsightType := citCodeInsight;
  InvokeType := itAuto;
end;

function TRyuCodeInsightManager.GetEnabled: Boolean;
begin
  Result := true;
end;

function TRyuCodeInsightManager.GetHintText(HintLine, HintCol: Integer): string;
begin
  Result := '';
end;

function TRyuCodeInsightManager.GetIDString: string;
begin
  Result := 'RyuCodeInsightManager';
end;

function TRyuCodeInsightManager.GetLongestItem: string;
begin

end;

function TRyuCodeInsightManager.GetMultiSelect: Boolean;
begin
  Result := false;
end;

function TRyuCodeInsightManager.GetName: string;
begin
  Result := 'Ryu Code Insight Manager';
end;

function TRyuCodeInsightManager.GetOptionSetName: string;
begin

end;

procedure TRyuCodeInsightManager.GetParameterList(
  out ParameterList: IOTACodeInsightParameterList);
begin
  ParameterList := nil;
end;

procedure TRyuCodeInsightManager.GetSymbolList(
  out SymbolList: IOTACodeInsightSymbolList);
begin
  SymbolList := TSymbolList.Create;
end;

function TRyuCodeInsightManager.GotoDefinition(out AFileName: string;
  out ALineNum: Integer; Index: Integer): Boolean;
begin
  AFileName := '';
  ALineNum := 0;
  Result := true;
end;

function TRyuCodeInsightManager.HandlesFile(const AFileName: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(AFileName)) = '.pas'
end;

function TRyuCodeInsightManager.InvokeCodeCompletion(HowInvoked: TOTAInvokeType;
  var Str: string): Boolean;
begin
  Result := true;
end;

function TRyuCodeInsightManager.InvokeParameterCodeInsight(
  HowInvoked: TOTAInvokeType; var SelectedIndex: Integer): Boolean;
begin
  Result := false;
end;

function TRyuCodeInsightManager.IsViewerBrowsable(Index: Integer): Boolean;
begin
  Result := true;
end;

procedure TRyuCodeInsightManager.OnEditorKey(Key: Char; var CloseViewer,
  Accept: Boolean);
begin
  if Key = #13 then begin
    Accept := true;
    CloseViewer := true;

  end else if Key = #27 then begin
    Accept := false;
    CloseViewer := true;
  end;
end;

procedure TRyuCodeInsightManager.ParameterCodeInsightAnchorPos(
  var EdPos: TOTAEditPos);
begin

end;

function TRyuCodeInsightManager.ParameterCodeInsightParamIndex(
  EdPos: TOTAEditPos): Integer;
begin

end;

function TRyuCodeInsightManager.PreValidateCodeInsight(
  const Str: string): Boolean;
begin
  Result := true;
end;

procedure TRyuCodeInsightManager.SetEnabled(Value: Boolean);
begin

end;

{ TSymbolList }

procedure TSymbolList.Clear;
begin

end;

function TSymbolList.FindIdent(const AnIdent: string): Integer;
begin

end;

function TSymbolList.FindSymIndex(const Ident: string;
  var Index: Integer): Boolean;
begin

end;

function TSymbolList.GetCount: Integer;
begin
  Result := 0;
end;

function TSymbolList.GetProcDispatchFlags(I: Integer): TOTAProcDispatchFlags;
begin

end;

function TSymbolList.GetSortOrder: TOTASortOrder;
begin

end;

function TSymbolList.GetSymbolClassText(I: Integer): string;
begin

end;

function TSymbolList.GetSymbolIsAbstract(I: Integer): Boolean;
begin

end;

function TSymbolList.GetSymbolIsReadWrite(I: Integer): Boolean;
begin

end;

function TSymbolList.GetSymbolText(Index: Integer): string;
begin

end;

function TSymbolList.GetSymbolTypeText(Index: Integer): string;
begin

end;

function TSymbolList.GetViewerSymbolFlags(I: Integer): TOTAViewerSymbolFlags;
begin

end;

function TSymbolList.GetViewerVisibilityFlags(
  I: Integer): TOTAViewerVisibilityFlags;
begin

end;

procedure TSymbolList.SetFilter(const FilterText: string);
begin

end;

procedure TSymbolList.SetSortOrder(const Value: TOTASortOrder);
begin

end;

var
  ManagerIndex : integer;

initialization
  ManagerIndex := (BorlandIDEServices as IOTACodeInsightServices).AddCodeInsightManager(TRyuCodeInsightManager.Create);

finalization
  (BorlandIDEServices as IOTACodeInsightServices).RemoveCodeInsightManager(ManagerIndex);
end.
