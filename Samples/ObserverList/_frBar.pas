unit _frBar;

interface

uses
  ValueList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TfrBar = class(TFrame)
    pbVolume: TProgressBar;
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure rp_CurrentVolume(AValueList:TValueList);
  end;

implementation

uses
  View;

{$R *.dfm}

{ TfrBar }

constructor TfrBar.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);
end;

destructor TfrBar.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrBar.rp_CurrentVolume(AValueList: TValueList);
begin
  pbVolume.Position := AValueList.Integers['Volume'];
end;

end.
