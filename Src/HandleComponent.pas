unit HandleComponent;

interface

uses
  Windows, Messages, Classes, SysUtils;

type
  THandleComponent = class (TComponent)
  private
    FHandle : THandle;
  protected
    procedure do_WndProc(var Message:TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Handle : THandle read FHandle;
  end;

implementation

{ THandleComponent }

constructor THandleComponent.Create(AOwner: TComponent);
begin
  inherited;

  FHandle := AllocateHWND(do_WndProc);
end;

destructor THandleComponent.Destroy;
begin
  DeAllocateHWND(FHandle);

  inherited;
end;

procedure THandleComponent.do_WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

end.

