unit _fmMain;

interface

uses
  InsertDebugCode,
  Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btExecute: TButton;
    moResult: TMemo;
    procedure btExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOldPath : string;
  private
    FInsertDebugCode : TInsertDebugCode;
    procedure on_FInsertDebugCode_Work(Sender:TObject; const AValue:string);
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  SelectPathDlg;

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
var
  sPath : string;
begin
  sPath := Trim( SelectPath(FOldPath) );

  if sPath = '' then Exit;

  moResult.Clear;

  FOldPath := sPath;

  FInsertDebugCode.Execute( sPath );

  moResult.Lines.Add( 'Done!');
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FOldPath := GetExecPath + 'TestFile';

  FInsertDebugCode := TInsertDebugCode.Create;
  FInsertDebugCode.OnWork := on_FInsertDebugCode_Work;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FInsertDebugCode);
end;

procedure TfmMain.on_FInsertDebugCode_Work(Sender: TObject;
  const AValue: string);
begin
  moResult.Lines.Add( 'OnWork - ' + AValue );
end;

end.
