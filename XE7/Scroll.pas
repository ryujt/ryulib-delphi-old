unit Scroll;

interface

uses
  Windows, Classes, SysUtils, Controls, Forms;

type
  TScroll = class
  private
    FMouseDownPos : TPoint;
  private
    FControl: TScrollingWinControl;
  public
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property Control : TScrollingWinControl read FControl write FControl;
  end;

implementation

{ TScroll }

procedure TScroll.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownPos.X := X;
  FMouseDownPos.Y := Y;
end;

procedure TScroll.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FControl) then FControl.ScrollBy(X-FMouseDownPos.X, Y-FMouseDownPos.Y);
end;

end.
