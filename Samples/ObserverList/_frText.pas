unit _frText;

interface

uses
  ValueList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrText = class(TFrame)
    lbVolume: TLabel;
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

{ TfrText }

constructor TfrText.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);
end;

destructor TfrText.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrText.rp_CurrentVolume(AValueList: TValueList);
begin
  lbVolume.Caption := AValueList.Values['Volume'];
end;

end.
