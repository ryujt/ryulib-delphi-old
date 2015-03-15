/// 델파이 클래스를 DLL로 빌드하고 해당 DLL를 Import 하는 유닛을 자동으로 생성합니다.
unit _fmMain;

interface

uses
  Scanner, FunctionList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TLineState = (lsPrivate, lsPublic);

  TfmMain = class(TForm)
    Panel1: TPanel;
    moSrc: TMemo;
    Splitter1: TSplitter;
    moProject: TMemo;
    btExecute: TButton;
    PopupMenuOfProject: TPopupMenu;
    miSelectAll: TMenuItem;
    moTemplateOfProject: TMemo;
    moUnit: TMemo;
    Splitter2: TSplitter;
    moTemplateOfUnit: TMemo;
    PopupMenuOfUnit: TPopupMenu;
    MenuItem1: TMenuItem;
    PopupMenuOfSrc: TPopupMenu;
    MenuItem2: TMenuItem;
    procedure btExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    FScanner : TScanner;
    procedure do_Parsing(AText:string);
  private
    FLineState : TLineState;
    FTokens : TStringList;
    procedure get_Tokens(AText:string);
  private
    function get_constructorAnddestructor(AClassName:string):string;
  private
    FFunctions : TFunctionList;
    procedure get_Functions(ATokens:TStringList);
  public
  end;

var
  fmMain: TfmMain;

implementation

function IsPrivate(AText:string):boolean;
begin
  Result :=
    (AText = 'private') or
    (AText = 'protected');
end;

function IsPublic(AText:string):boolean;
begin
  Result :=
    (AText = 'public') or
    (AText = 'published');
end;

function IsFunction(AText:string):boolean;
begin
  Result :=
    (AText = 'procedure') or
    (AText = 'function');
end;

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
begin
  do_Parsing(moSrc.Text);
//  moUnit
end;

procedure TfmMain.do_Parsing(AText: string);
var
  sDllName, sProject, sUnitName, sUnit : string;
begin
  get_Tokens(AText);
  get_Functions(FTokens);

  sDllName := 'lib' + Copy(FFunctions.ClassName, 2, Length(FFunctions.ClassName));

  sProject :=
    get_constructorAnddestructor(FFunctions.ClassName) +
    FFunctions.DllText +
    FFunctions.ExportsText;

  sProject := StringReplace(moTemplateOfProject.Text, '@Body', sProject, [rfIgnoreCase]);
  sProject := StringReplace(sProject, '@DllName', sDllName, [rfIgnoreCase]);

  moProject.Text := sProject;

  sUnitName := Copy(FFunctions.ClassName, 2, Length(FFunctions.ClassName));

  sUnit := moTemplateOfUnit.Text;
  sUnit := StringReplace(sUnit, '@UnitName', sUnitName, [rfReplaceAll, rfIgnoreCase]);
  sUnit := StringReplace(sUnit, '@MethodInterface', FFunctions.MethodInterfaceText, [rfIgnoreCase]);
  sUnit := StringReplace(sUnit, '@ExternalFunctions', FFunctions.ExternalsText, [rfIgnoreCase]);
  sUnit := StringReplace(sUnit, '@MethodImplementation', FFunctions.MethodImplementationText, [rfIgnoreCase]);
  moUnit.Text := sUnit;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTokens := TStringList.Create;
  FFunctions := TFunctionList.Create;

  FScanner := TScanner.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTokens);
  FreeAndNil(FFunctions);
  FreeAndNil(FScanner);
end;

function TfmMain.get_constructorAnddestructor(AClassName: string): string;
begin
  Result :=
    'function _CreateObject:pointer;' + #13#10 +
    'begin' + #13#10 +
    Format('  Result := %s.Create(nil);', [AClassName]) + #13#10 +
    'end;' + #13#10 + #13#10;

  Result := Result +
    'function _DestroyObject(AHandle:pointer);' + #13#10 +
    'begin' + #13#10 +
    '  TObject(AHandle).Free;' + #13#10 +
    'end;' + #13#10 + #13#10;
end;

procedure TfmMain.get_Functions(ATokens: TStringList);
var
  isFunctionStarted : boolean;
  iLeftParentheseCount : integer;
  Loop: Integer;
  slTemp : TStringList;
begin
  FFunctions.Clear;

  isFunctionStarted := false;

  slTemp := TStringList.Create;
  try
    for Loop := 0 to FTokens.Count-1 do begin
      if IsFunction(LowerCase(FTokens[Loop])) then begin
        isFunctionStarted := true;
        iLeftParentheseCount := 0;

        slTemp.Add(FTokens[Loop]);

        Continue;
      end;

      if isFunctionStarted then begin
        slTemp.Add(FTokens[Loop]);

        if FTokens[Loop] = '(' then begin
          iLeftParentheseCount := iLeftParentheseCount + 1;
          if iLeftParentheseCount > 1 then
            raise Exception.Create('TfmMain.get_Functions: iLeftParentheseCount > 1');
        end else if FTokens[Loop] = ')' then begin
          iLeftParentheseCount := iLeftParentheseCount - 1;
          if iLeftParentheseCount < 0 then
            raise Exception.Create('TfmMain.get_Functions: iLeftParentheseCount < 0');
        end;

        if (iLeftParentheseCount = 0) and (FTokens[Loop] = ';') then begin
          FFunctions.Add(slTemp);

          slTemp.Clear;
          isFunctionStarted := false;
        end;
      end;
    end;
  if slTemp.Count > 0 then
    raise Exception.Create('TfmMain.get_Functions: slTemp.Count > 0');
  finally
    slTemp.Free;
  end;
end;

procedure TfmMain.get_Tokens(AText:string);
begin
  FLineState := lsPublic;
  FTokens.Clear;

  FScanner.SetText(AText);

  FScanner.GetNextToken;
  FFunctions.ClassName := FScanner.Token.Text;

  while FScanner.IsEOF = false do begin
    FScanner.GetNextToken;

    if IsPrivate(FScanner.Token.LowerCaseText) then FLineState := lsPrivate
    else if IsPublic(FScanner.Token.LowerCaseText) then FLineState := lsPublic;

    if FLineState = lsPublic then FTokens.Add(FScanner.Token.Text);
  end;
end;

procedure TfmMain.MenuItem1Click(Sender: TObject);
begin
  moUnit.SelectAll;
end;

procedure TfmMain.MenuItem2Click(Sender: TObject);
begin
  moSrc.SelectAll;
end;

procedure TfmMain.miSelectAllClick(Sender: TObject);
begin
  moProject.SelectAll;
end;

end.
