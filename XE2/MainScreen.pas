unit MainScreen;

interface

uses
  CompareBytes, RyuGraphics,
  Windows, SysUtils, Classes, Forms;

type
  TMainScreen = class
  private
    FOldMainFormPos : TPoint;
    FRect : TRect;
  private
    function GetRect: TRect;
  public
    constructor Create;

    class function Obj:TMainScreen;

    property Rect : TRect read GetRect;
  end;

implementation

var
  MyObject : TMainScreen = nil;

{ TMainScreen }

constructor TMainScreen.Create;
begin
  inherited;

  FOldMainFormPos := Point(0, 0);
  FRect := FindMonitorRect(0, 0);
end;

function TMainScreen.GetRect: TRect;
begin
  if Application.MainForm <> nil then begin
    FRect := Classes.Rect(
      Application.MainForm.Left, Application.MainForm.Top,
      Application.MainForm.Left + Application.MainForm.Width, Application.MainForm.Top + Application.MainForm.Height
    );
    FOldMainFormPos := Point(Application.MainForm.Left, Application.MainForm.Top);
  end else begin
    FRect := FindMonitorRect(FOldMainFormPos.X, FOldMainFormPos.Y);
  end;

  Result := FRect;
end;

class function TMainScreen.Obj: TMainScreen;
begin
  if MyObject = nil then MyObject := TMainScreen.Create;
  Result := MyObject;
end;

end.