unit BoxLabel;

interface

uses
  Messages, Windows, SysUtils, Classes, Contnrs, Controls, Graphics;
  
type
  TBoxLabel = class(TGraphicControl)
  private
    FCaption: string;
    procedure SetCaption(const Value: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption : string read FCaption write SetCaption;
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBoxLabel]);
end;

{ TBoxLabel }

constructor TBoxLabel.Create(AOwner: TComponent);
begin
  inherited;

  Width := 51;
  Height := 21;

  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clBlack;
end;

procedure TBoxLabel.Paint;
var
  X, Y : integer;
begin
  Canvas.Rectangle(0, 0, Width, Height);

  X := (Width div 2) - (Canvas.TextWidth(Caption) div 2);
  Y := (Height div 2) - (Canvas.TextHeight(Caption) div 2);
  Canvas.TextOut(X, Y, Caption);
end;

procedure TBoxLabel.SetCaption(const Value: string);
begin
  FCaption := Value;
  Repaint;
end;

end.
