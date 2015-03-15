unit @UnitName;

interface

uses
  ValueList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  T@FormName = class(TForm)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  @FormName: T@FormName;

implementation

uses
  Core;

{$R *.dfm}

constructor T@FormName.Create(AOwner: TComponent);
begin
  inherited;

  TCore.Obj.View.Add(Self);
end;

destructor T@FormName.Destroy;
begin
  TCore.Obj.View.Remove(Self);

  inherited;
end;

end.
