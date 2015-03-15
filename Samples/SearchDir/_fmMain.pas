unit _fmMain;

interface

uses
  SearchDir,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Panel1: TPanel;
    btOpen: TButton;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
  private
    FSearchDir : TSearchDir;
    procedure on_FindFile(Sender:TObject; APath:string; AFileInfo:TSearchRec);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FSearchDir.Search(ExtractFilePath(OpenDialog.FileName));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSearchDir := TSearchDir.Create(Self);
  FSearchDir.SearchSubDirectories := false;
  FSearchDir.OnFindFile := on_FindFile;
end;

procedure TfmMain.on_FindFile(Sender: TObject; APath: string;
  AFileInfo: TSearchRec);
begin
  if LowerCase(ExtractFileExt(AFileInfo.Name)) = '.pas' then
    moMsg.Lines.Add(APath + '\' + AFileInfo.Name);
end;

end.
