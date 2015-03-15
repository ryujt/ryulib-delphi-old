unit BitmapOutputCanvas;

interface

uses
  RyuLibBase,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus;

type
  TBitmapOutputCanvas = class (TForm)
  private
    FOnStretchChanged: TNotifyEvent;
    FOnErrorMsg: TStringEvent;
  protected
    function GetScreenHint: string; virtual; abstract;
    procedure SetScreenHint(const Value: string); virtual; abstract;
  public
    procedure Terminate; virtual;

    function GetCanvasWidth:integer; virtual; abstract;
    function GetCanvasHeight:integer; virtual; abstract;

    procedure DrawBitmap(ABitmap:TBitmap); virtual; abstract;
  published
    property ScreenHint : string read GetScreenHint write SetScreenHint;
    property OnStretchChanged : TNotifyEvent read FOnStretchChanged write FOnStretchChanged;
    property OnErrorMsg : TStringEvent read FOnErrorMsg write FOnErrorMsg;
  end;

implementation

{ TBitmapOutputCanvas }

procedure TBitmapOutputCanvas.Terminate;
begin
  //
end;

end.
