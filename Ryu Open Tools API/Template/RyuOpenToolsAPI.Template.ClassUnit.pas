unit @ClassName;

interface

uses
  SysUtils, Classes;

type
  T@ClassName = class
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ T@ClassName }

constructor T@ClassName.Create;
begin
  inherited;

end;

destructor T@ClassName.Destroy;
begin

  inherited;
end;

end.
