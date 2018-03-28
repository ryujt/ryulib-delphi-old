unit SaveFormInfo;

interface

uses
  Disk, RyuGraphics,
  SysUtils, Classes, Forms;

type
  TSaveFormInfo = class (TComponent)
  private
    FFormName: string;
  public
    constructor Create(AOwner: TComponent; ALeft,ATop,AWidth,AHeight:integer; AFormName:string=''); reintroduce;
    destructor Destroy; override;

    procedure SaveInfo;
  end;

implementation

{ TSaveFormInfo }

constructor TSaveFormInfo.Create(AOwner: TComponent; ALeft,ATop,AWidth,AHeight:integer; AFormName: string);
var
  iTemp, iLeft, iTop, iWidth, iHeight : integer;
  ParentForm : TForm;
begin
  inherited Create(AOwner);

  ParentForm := Pointer(Owner);

  if AFormName = '' then FFormName := ParentForm.ClassName
  else FFormName := AFormName;

  iLeft := IniInteger(GetExecPath + 'FormInfo.ini', FFormName, 'Left', ALeft);
  iTop := IniInteger(GetExecPath + 'FormInfo.ini', FFormName, 'Top', ATop);

  iWidth := AWidth;
  iTemp := IniInteger(GetExecPath + 'FormInfo.ini', FFormName, 'Width', AWidth);
  if iTemp <> 0 then iWidth := iTemp;

  iHeight := AHeight;
  iTemp := IniInteger(GetExecPath + 'FormInfo.ini', FFormName, 'Height', AHeight);
  if iTemp <> 0 then iHeight := iTemp;

  if IsWindowMaximized(iLeft, iTop, iWidth, iHeight) then begin
    ParentForm.Left := iLeft;
    ParentForm.Top  := iTop;

    ParentForm.WindowState := wsMaximized;
    Exit;
  end;

  if IsWindowInMonitorAreas(iLeft, iTop, iWidth, iHeight) then begin
    ParentForm.Left := iLeft;
    ParentForm.Top  := iTop;

    ParentForm.Width  := iWidth;
    ParentForm.Height := iHeight;
  end;
end;

destructor TSaveFormInfo.Destroy;
begin
  SaveInfo;

  inherited;
end;

procedure TSaveFormInfo.SaveInfo;
var
  ParentForm : TForm;
begin
  if Self = nil then Exit;

  ParentForm := Pointer(Owner);

  WriteIniInt(GetExecPath + 'FormInfo.ini', FFormName, 'Left', ParentForm.Left);
  WriteIniInt(GetExecPath + 'FormInfo.ini', FFormName, 'Top',  ParentForm.Top);

  WriteIniInt(GetExecPath + 'FormInfo.ini', FFormName, 'Width',  ParentForm.Width);
  WriteIniInt(GetExecPath + 'FormInfo.ini', FFormName, 'Height', ParentForm.Height);
end;

end.
