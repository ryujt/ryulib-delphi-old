unit SelectPathDlg;

interface

uses
  Vcl.ComCtrls, Vcl.Shell.ShellCtrls,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, XiButton;

type
  TfmSelectPathDlg = class(TForm)
    plClient: TPanel;
    Panel1: TPanel;
    edPath: TEdit;
    Panel2: TPanel;
    tmStart: TTimer;
    btOk: TXiButton;
    btCancel: TXiButton;
    procedure btOkClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edPathKeyPress(Sender: TObject; var Key: Char);
  private
    FIsSelected : boolean;
    FShellTreeView: TShellTreeView;
    procedure on_ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function SelectPath(ADefaultPath:string):string;

implementation

function SelectPath(ADefaultPath:string):string;
var
  fmSelectPathDlg : TfmSelectPathDlg;
begin
  Result := '';

  fmSelectPathDlg := TfmSelectPathDlg.Create(nil);
  try
    fmSelectPathDlg.edPath.Text := ADefaultPath;
    fmSelectPathDlg.FShellTreeView.Path := ADefaultPath;
    fmSelectPathDlg.ShowModal;
    if fmSelectPathDlg.FIsSelected then Result := Trim(fmSelectPathDlg.edPath.Text);
  finally
    fmSelectPathDlg.Free;
  end;
end;

{$R *.dfm}

{ TfmSelectPathDlg }

procedure TfmSelectPathDlg.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmSelectPathDlg.btOkClick(Sender: TObject);
begin
  FIsSelected := true;
  Close;
end;

constructor TfmSelectPathDlg.Create(AOwner: TComponent);
begin
  inherited;

  FIsSelected := false;

  FShellTreeView := TShellTreeView.Create(Self);
  FShellTreeView.Align := alClient;
  FShellTreeView.Parent := plClient;
  FShellTreeView.OnChange := on_ShellTreeViewChange;
end;

destructor TfmSelectPathDlg.Destroy;
begin
  FreeAndNil(FShellTreeView);

  inherited;
end;

procedure TfmSelectPathDlg.edPathKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    FShellTreeView.Path := edPath.Text;
  end;
end;

procedure TfmSelectPathDlg.FormShow(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TfmSelectPathDlg.on_ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  edPath.Text := FShellTreeView.Path;
  Node.Expanded := true;
end;

end.
