unit VirtualDeskTop;

interface

uses
  Windows, Classes, SysUtils, Controls, ExtCtrls;

type
  TVirtualDeskTop = class (TCustomPanel)
  private
    FTargetHandle: THandle;
    procedure SetTargetHandle(const Value: THandle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    property DockManager;
  published
    property TargetHandle : THandle read FTargetHandle write SetTargetHandle;

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
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
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
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
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
  end;

implementation

{ TVirtualDeskTop }

constructor TVirtualDeskTop.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;
end;

destructor TVirtualDeskTop.Destroy;
begin
  TargetHandle := 0;

  inherited;
end;

procedure TVirtualDeskTop.SetTargetHandle(const Value: THandle);
begin
  if FTargetHandle <> 0 then begin
    Windows.SetParent(FTargetHandle, 0);
    // Todo : 원래 위치와 상태로 복원하기
  end;

  if Value <> 0 then begin
    Windows.SetParent(Value, Handle);
    SetWindowPos(Value, HWND_TOP, 0, 0, Width, Height, SWP_NOOWNERZORDER);
  end;

  FTargetHandle := Value;
end;

end.
