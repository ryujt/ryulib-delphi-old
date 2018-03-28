unit CountLock;

interface

uses
  SysUtils, Classes;

type
  TCountLock = class
  private
    FCount : integer;
  public
    constructor Create;

    procedure Inc;
    procedure Dec;

    procedure WaitFor;
  end;

implementation

{ TCountLock }

constructor TCountLock.Create;
begin
  inherited;

  FCount := 0;
end;

procedure TCountLock.Dec;
begin
  TMonitor.Enter( Self );
  try
    FCount := FCount - 1;
    TMonitor.PulseAll( Self );
  finally
    TMonitor.Exit( Self );
  end;
end;

procedure TCountLock.Inc;
begin
  TMonitor.Enter( Self );
  try
    FCount := FCount + 1;
  finally
    TMonitor.Exit( Self );
  end;
end;

procedure TCountLock.WaitFor;
begin
  TMonitor.Enter( Self );
  try
    while FCount > 0 do begin
      TMonitor.Wait( Self, INFINITE );
    end;
  finally
    TMonitor.Exit( Self );
  end;
end;

end.
