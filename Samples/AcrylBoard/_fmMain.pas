unit _fmMain;

interface

uses
  AcrylBoard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ExtDlgs;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btClear: TButton;
    btOpen: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
  private
    FAcrylBoard : TAcrylBoard;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btClearClick(Sender: TObject);
begin
  FAcrylBoard.Clear;
end;

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then begin
    FAcrylBoard.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    FAcrylBoard.Invalidate;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FAcrylBoard := TAcrylBoard.Create(Self);
  FAcrylBoard.Align := alClient;
  FAcrylBoard.Parent := Self;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAcrylBoard);
end;

end.
