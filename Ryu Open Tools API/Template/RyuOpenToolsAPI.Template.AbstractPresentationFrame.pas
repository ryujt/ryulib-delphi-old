unit @UnitName;

interface

uses
  FrameBase, ValueList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls;

type
  T@FormName = class(TFrame, IFrameBase)
  private
    procedure BeforeShow;
    procedure AfterShow;
    procedure BeforeClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Core;

{$R *.dfm}

{ T@FormName }

procedure T@FormName.AfterShow;
begin

end;

procedure T@FormName.BeforeShow;
begin

end;

procedure T@FormName.BeforeClose;
begin

end;

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