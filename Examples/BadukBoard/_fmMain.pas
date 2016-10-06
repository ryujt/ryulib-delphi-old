unit _fmMain;

interface

uses
  BadukBoard,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FStoneState : TStoneState;
    FBadukBoard : TBadukBoard;
    procedure on_BoardClick(Sender:TObject; IndexX,IndexY:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FStoneState := ssBlack;

  FBadukBoard := TBadukBoard.Create(Self);
  FBadukBoard.Align := alClient;
  FBadukBoard.Parent := Self;
  FBadukBoard.OnBoardClick := on_BoardClick;
end;

procedure TfmMain.on_BoardClick(Sender: TObject; IndexX, IndexY: integer);
begin
  FBadukBoard.PutStone(IndexX, IndexY, FStoneState);
  ToggleStoneState(FStoneState);
end;

end.
