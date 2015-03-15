unit Room;

interface

uses
  DebugTools,
  SysUtils, Classes;

type
  TRoom = class
  private
  public
    Key : string;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRoom }

constructor TRoom.Create;
begin
  inherited;

  {$IFDEF DEBUG}
  Trace( 'TRoom.Create' );
  {$ENDIF}
end;

destructor TRoom.Destroy;
begin
  {$IFDEF DEBUG}
  Trace( 'TRoom.Destroy' );
  {$ENDIF}

  inherited;
end;

end.
