unit TileView;

interface

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TTileView = class (TCustomPanel)
  private
    FList : TList<TWinControl>;
    procedure arrange_Items;
  private
    FPanel : TPanel;
    procedure on_PanelResized(Sender:TObject);
  private
    FScrollBar : TScrollBar;
    procedure on_Scroll(Sender:TObject);
  private
    FTileHeight: integer;
    FTileWidth: integer;
    FMarginVertical: integer;
    FMarginHorizontal: integer;
    procedure SetTileHeight(const Value: integer);
    procedure SetTileWidth(const Value: integer);
    function GetCount: integer;
    procedure SetMarginHorizontal(const Value: integer);
    procedure SetMarginVertical(const Value: integer);
    function GetItems(Index: integer): TWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AControl:TWinControl);
    procedure Remove(AControl:TWinControl);

    property Count : integer read GetCount;
    property Items[Index:integer] : TWinControl read GetItems;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  published
    property TileWidth : integer read FTileWidth write SetTileWidth;
    property TileHeight : integer read FTileHeight write SetTileHeight;
    property MarginVertical : integer read FMarginVertical write SetMarginVertical;
    property MarginHorizontal : integer read FMarginHorizontal write SetMarginHorizontal;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TTileView]);
end;

{ TTileView }

procedure TTileView.Add(AControl: TWinControl);
begin
  FList.Add(AControl);
  arrange_Items;

  AControl.Parent := FPanel;
end;

procedure TTileView.arrange_Items;
var
  Loop, iCountInRow, iIndex, iX, iY : Integer;
begin
  iCountInRow := Width div FTileWidth;

  if iCountInRow < 1 then iCountInRow := 1;

  FScrollBar.Max := FList.Count div iCountInRow;

  for Loop := 0 to FList.Count-1 do begin
    FList[Loop].Width  := FTileWidth - FMarginHorizontal;
    FList[Loop].Height := FTileHeight - FMarginVertical;

    iIndex := Loop - (FScrollBar.Position * iCountInRow);
    if iIndex < 0 then begin
      FList[Loop].Left := - FTileWidth;
      FList[Loop].Top  := - FTileHeight;

      Continue;
    end;

    iX := (iIndex mod iCountInRow) * FTileWidth;
    iY := (iIndex div iCountInRow) * FTileHeight;

    FList[Loop].Left := iX + FMarginHorizontal;
    FList[Loop].Top  := iY + FMarginVertical;
  end;
end;

procedure TTileView.Clear;
var
  Loop: Integer;
begin
  FScrollBar.Position := 0;
  for Loop := 0 to FList.Count-1 do FList[Loop].Parent := nil;
  FList.Clear;
  arrange_Items;
end;

constructor TTileView.Create(AOwner: TComponent);
begin
  inherited;

  FMarginHorizontal := 0;
  FMarginVertical := 0;

  FTileWidth := 320 div 2;
  FTileHeight := 240 div 2;

  FList := TList<TWinControl>.Create;

  FScrollBar := TScrollBar.Create(Self);
  FScrollBar.Kind := sbVertical;
  FScrollBar.Align := alRight;
  FScrollBar.Parent := Self;
  FScrollBar.OnChange := on_Scroll;

  FPanel := TPanel.Create(Self);
  FPanel.DoubleBuffered := true;
  FPanel.BevelOuter := bvNone;
  FPanel.Align := alClient;
  FPanel.Parent := Self;
  FPanel.OnResize := on_PanelResized;
end;

destructor TTileView.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FPanel);
  FreeAndNil(FScrollBar);

  inherited;
end;

function TTileView.GetCount: integer;
begin
  Result := FList.Count;
end;

function TTileView.GetItems(Index: integer): TWinControl;
begin
  Result := FList[Index];
end;

procedure TTileView.on_PanelResized(Sender: TObject);
begin
  arrange_Items;
end;

procedure TTileView.on_Scroll(Sender: TObject);
begin
  arrange_Items;
end;

procedure TTileView.Remove(AControl: TWinControl);
begin
  AControl.Parent := nil;
  FList.Remove(AControl);
  arrange_Items;
end;

procedure TTileView.SetMarginHorizontal(const Value: integer);
begin
  FMarginHorizontal := Value;
end;

procedure TTileView.SetMarginVertical(const Value: integer);
begin
  FMarginVertical := Value;
end;

procedure TTileView.SetTileHeight(const Value: integer);
begin
  FTileHeight := Value;
  arrange_Items;
end;

procedure TTileView.SetTileWidth(const Value: integer);
begin
  FTileWidth := Value;
  arrange_Items;
end;

end.
