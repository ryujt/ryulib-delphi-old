unit AppBar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  WM_ShowAppBar = WM_User + 1;

type
  TAppBar = class(TForm)
  private
    procedure on_FormShow(Sender: TObject);
    procedure on_FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure do_Show; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    class var BarSize : integer;
    class var Edge : integer;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ShellAPI;

{ TAppBar }

constructor TAppBar.Create(AOwner: TComponent);
var
  MyTaskBar  : TAppBarData;
  Monitor : TMonitor;
begin
  inherited;

  Monitor := Screen.MonitorFromPoint(Point(Application.MainForm.Left, Application.MainForm.Top));

  FormStyle := fsStayOnTop;

  case Edge of
    ABE_LEFT: begin
      if Monitor <> nil then Height := Monitor.Height;

      Width := BarSize;
      Left:= 0;
      Top := 0;
    end;

    ABE_RIGHT: begin
      if Monitor <> nil then begin
        Left := Monitor.Width - BarSize;
        Height := Monitor.Height;
      end;

      Width := BarSize;
      Top := 0;
    end;

    ABE_TOP: begin
      if Monitor <> nil then Width := Monitor.Width;

      Height:= BarSize;
      Left:= 0;
      Top := 0;
    end;

    ABE_BOTTOM: begin
      if Monitor <> nil then begin
        Width := Monitor.Width;
        Top := Monitor.Height - BarSize;
      end;

      Height := BarSize;
      Left := 0;
    end;
  end;

  FillChar(MyTaskBar, SizeOf(TAppBarData), 0);
  MyTaskBar.cbSize := SizeOf(TAppBarData);
  MyTaskBar.hWnd   := Handle;
  MyTaskBar.uCallbackMessage := WM_USER + 777;
  MyTaskBar.uEdge  := Edge;
  MyTaskBar.rc     := Rect(Left, Top, Width, Height);
  SHAppBarMessage(ABM_NEW, MyTaskBar);
  SHAppBarMessage(ABM_ACTIVATE, MyTaskBar);
  SHAppBarMessage(ABM_SETPOS, MyTaskBar);

  Application.ProcessMessages;

  Self.OnShow := on_FormShow;
  Self.OnClose := on_FormClose;

  Show;
end;

procedure TAppBar.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.ExStyle  := Params.ExStyle or WS_EX_TOOLWINDOW;
  Params.Style    := (Params.Style OR WS_POPUP) AND (NOT WS_DLGFRAME);
end;

procedure TAppBar.do_Show;
begin
end;

procedure TAppBar.on_FormClose(Sender: TObject; var Action: TCloseAction);
var
  MyTaskBar : TAppBarData;
begin
  FillChar(MyTaskBar, SizeOf(TAppBarData), 0);
  MyTaskBar.cbSize := SizeOf(TAppBarData);
  MyTaskBar.hWnd   := Self.Handle;
  SHAppBarMessage(ABM_Remove, MyTaskBar);

  Application.MainForm.Show;
  Action := caFree;
end;

procedure TAppBar.on_FormShow(Sender: TObject);
begin
  BorderStyle := bsNone;
  do_Show;
end;

end.
