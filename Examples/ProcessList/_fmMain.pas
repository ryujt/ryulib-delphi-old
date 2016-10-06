unit _fmMain;

interface

uses
  ProcessUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TfmMain = class(TForm)
    moList: TMemo;
    Panel1: TPanel;
    btUpdate: TButton;
    PopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    procedure btUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  private
    FProcessList : TProcessList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btUpdateClick(Sender: TObject);
var
  Loop : Integer;
begin
  FProcessList.Update;

  moList.Clear;
  for Loop := 0 to FProcessList.Count-1 do begin
    moList.Lines.Add( FProcessList.Names[Loop] );
    moList.Lines.Add( '    * Class Name: ' + FProcessList.WindowClassName[Loop] );
    moList.Lines.Add( '    * Path: ' + ExtractFilePath(FProcessList.FullNames[Loop]) );
    moList.Lines.Add( '' );
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FProcessList := TProcessList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FProcessList.Free;
end;

procedure TfmMain.miSelectAllClick(Sender: TObject);
begin
  moList.SelectAll;
end;

end.
