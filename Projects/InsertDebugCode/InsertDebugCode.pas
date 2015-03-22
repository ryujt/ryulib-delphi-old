unit InsertDebugCode;

interface

uses
  Scanner,
  RyuLibBase, SearchDir, Disk,
  SysUtils, Classes;

type
  TInsertDebugCode = class
  private
    procedure do_InsertDebugCode(AFileName:string);
  private
    FOnWork: TStringEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(APath:string);
  public
    property OnWork : TStringEvent read FOnWork write FOnWork;
  end;

implementation

uses
  ScanMgr, ScanMethod;

{ TInsertDebugCode }

constructor TInsertDebugCode.Create;
begin
  inherited;

end;

destructor TInsertDebugCode.Destroy;
begin

  inherited;
end;

procedure TInsertDebugCode.do_InsertDebugCode(AFileName: string);
var
  isFirstUsesExpected : boolean;
  isImplementationArea : boolean;
begin
  if Assigned(FOnWork) then FOnWork(Self, AFileName);

  TScanMgr.Obj.SetText( AFileName, LoadFileAsText(AFileName) );

  isFirstUsesExpected := true;
  isImplementationArea := false;

  TScanMgr.Obj.GetNextToken;
  while not TScanMgr.Obj.IsEOF do begin
    if TScanMgr.Obj.isUses then begin
      isFirstUsesExpected := false;
      TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText + #13#10 + '  CodeSiteLogging, ';
      TScanMgr.Obj.GetNextToken;
      Continue;
    end;

    if TScanMgr.Obj.isImplementation then begin
      isImplementationArea := true;
      TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
      TScanMgr.Obj.GetNextToken;
      Continue;
    end;

    if not isImplementationArea then begin
      TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;
      TScanMgr.Obj.GetNextToken;
      Continue;
    end;

    if TScanMgr.Obj.isMethodBegin then TScanMethod.Obj.Execute(0)
    else TScanMgr.Obj.Source := TScanMgr.Obj.Source + TScanMgr.Obj.CurrentToken.OriginalText;

    TScanMgr.Obj.GetNextToken;
  end;

  SaveTextToFile( AFileName, TScanMgr.Obj.Source );
end;

procedure TInsertDebugCode.Execute(APath: string);
begin
  SearchFiles( APath, true,
    procedure(Path:string; SearchRec:TSearchRec; var NeedStop:boolean)
    var
      isPascalFile : boolean;
    begin
      isPascalFile := ExtractFileExt( LowerCase(SearchRec.Name) ) = '.pas';
      if isPascalFile then do_InsertDebugCode(Path + SearchRec.Name);
    end
  );
end;

end.
