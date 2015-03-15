unit @ClassName;

interface

uses
  SysUtils, Classes;

type
  T@ClassName = class (TComponent)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ T@ClassName }

constructor T@ClassName.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor T@ClassName.Destroy;
begin

  inherited;
end;

end.
