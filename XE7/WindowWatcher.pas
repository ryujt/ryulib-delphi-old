unit WindowWatcher;

interface

uses
  Windows, Classes, SysUtils, ExtCtrls;

type
  TWindowWatcher = class (TComponent)
  private
    FWindowHandle : HWND;
    FTimer : TTimer;
    FIsWindowVisibleOld : boolean;
    procedure on_Timer(Sender:TObject);
  private
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    function GetVisible: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start(AWindowTitle:string);
    procedure Stop;
  published
    property IsVisible : boolean read GetVisible;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
    property OnHide : TNotifyEvent read FOnHide write FOnHide;
  end;

implementation

{ TWindowWatcher }

constructor TWindowWatcher.Create(AOwner: TComponent);
begin
  inherited;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.Enabled := true;
end;

destructor TWindowWatcher.Destroy;
begin
  Stop;

  FreeAndNil(FTimer);

  inherited;
end;

function TWindowWatcher.GetVisible: boolean;
begin
  Result := IsWindowVisible(FWindowHandle);
end;

procedure TWindowWatcher.on_Timer(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    if FIsWindowVisibleOld then begin
      FIsWindowVisibleOld := IsVisible;
      if Assigned(FOnHide) and (not FIsWindowVisibleOld) then FOnHide(Self);
    end else begin
      FIsWindowVisibleOld := IsVisible;
      if Assigned(FOnShow) and (FIsWindowVisibleOld) then FOnShow(Self);
    end;
  finally
    FTimer.Enabled := true;
  end;
end;

procedure TWindowWatcher.Start(AWindowTitle:string);
begin
  FWindowHandle := FindWindow(nil, PChar(AWindowTitle));
  if FWindowHandle = 0 then Exit;
  
  FIsWindowVisibleOld := IsWindowVisible(FWindowHandle);

  FTimer.OnTimer := on_Timer;
end;

procedure TWindowWatcher.Stop;
begin
  FTimer.OnTimer := nil;
end;

end.
