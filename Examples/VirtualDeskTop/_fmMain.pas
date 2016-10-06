unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualDeskTop, ApplicationList;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    cbAppList: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbAppListKeyPress(Sender: TObject; var Key: Char);
    procedure cbAppListDropDown(Sender: TObject);
    procedure cbAppListChange(Sender: TObject);
  private
    FVirtualDeskTop : TVirtualDeskTop;
    FApplicationList : TApplicationList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.cbAppListChange(Sender: TObject);
begin
  if cbAppList.ItemIndex >= 0 then
    FVirtualDeskTop.TargetHandle := Integer(cbAppList.Items.Objects[cbAppList.ItemIndex])
  else
    FVirtualDeskTop.TargetHandle := 0;
end;

procedure TfmMain.cbAppListDropDown(Sender: TObject);
var
  Loop: Integer;
begin
  FApplicationList.Update;

  cbAppList.Items.Clear;
  cbAppList.Items.Add('공유할 프로그램을 선택하세요');

  for Loop := 0 to FApplicationList.Count - 1 do begin
    if FApplicationList.Handles[Loop] = Handle then Continue;
    if FApplicationList.Names[Loop] = Caption then Continue;

    cbAppList.Items.AddObject(FApplicationList.Names[Loop], TObject(FApplicationList.Handles[Loop]));
  end;
end;

procedure TfmMain.cbAppListKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FApplicationList := TApplicationList.Create(Self);

  FVirtualDeskTop := TVirtualDeskTop.Create(Self);
  FVirtualDeskTop.Align := alClient;
  FVirtualDeskTop.Parent := Self;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FApplicationList.Free;
  FVirtualDeskTop.Free;
end;

end.
