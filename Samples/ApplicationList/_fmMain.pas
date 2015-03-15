unit _fmMain;

interface

uses
  ApplicationList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    plBottom: TPanel;
    Label1: TLabel;
    lbResolution: TLabel;
    bt1024x768: TSpeedButton;
    bt1280x800: TSpeedButton;
    btNot1024x768: TSpeedButton;
    btNot1280x800: TSpeedButton;
    cbApplication: TComboBox;
    plDeskCam: TPanel;
    procedure cbApplicationChange(Sender: TObject);
    procedure cbApplicationDropDown(Sender: TObject);
    procedure cbApplicationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbApplicationKeyPress(Sender: TObject; var Key: Char);
    procedure cbApplicationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FExWindow : THandle;
    FApplicationList : TApplicationList;
    procedure Release_ExWindow;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.SpeedButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    1 : begin
      Application.MainForm.ClientWidth  := 1024;
      Application.MainForm.ClientHeight :=  768 + 30;
    end;

    2 : begin
      Application.MainForm.ClientWidth  := 1280;
      Application.MainForm.ClientHeight :=  800 + 30;
    end;
  end;
end;

procedure TfmMain.cbApplicationChange(Sender: TObject);
begin
  if cbApplication.ItemIndex = -1 then Exit;

  Release_ExWindow;

  if cbApplication.ItemIndex = 0 then Exit;

  FExWindow := Integer(FApplicationList.Handles[cbApplication.ItemIndex - 1]);
  Winapi.Windows.SetParent(FExWindow, plDeskCam.Handle);
  SetWindowPos(FExWindow, HWND_TOP, 0, 0, plDeskCam.Width, plDeskCam.Height, SWP_NOSIZE);
end;

procedure TfmMain.cbApplicationDropDown(Sender: TObject);
var
  Loop: Integer;
begin
  FApplicationList.Update;

  cbApplication.Items.Clear;
  cbApplication.Items.AddObject('*** None ***', TObject(-1));

  for Loop := 0 to FApplicationList.Count-1 do
    cbApplication.Items.AddObject(FApplicationList.Names[Loop], TObject(FApplicationList.Handles[Loop]));
end;

procedure TfmMain.cbApplicationKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0;
end;

procedure TfmMain.cbApplicationKeyPress(Sender: TObject; var Key: Char);
begin
  key := #0;
end;

procedure TfmMain.cbApplicationKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0;
end;

procedure TfmMain.Release_ExWindow;
begin
  if FExWindow <> 0 then begin
    Winapi.Windows.SetParent(FExWindow, 0);
    SetWindowPos(FExWindow, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE);
    FExWindow := 0;
  end;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Release_ExWindow;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FExWindow := 0;
  FApplicationList := TApplicationList.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FApplicationList);
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  lbResolution.Caption := Format('%d x %d', [plDeskCam.Width, plDeskCam.Height]);

  if (plDeskCam.Width = 1024) and (plDeskCam.Height = 768) then bt1024x768.Down := true
  else btNot1024x768.Down := true;

  if (plDeskCam.Width = 1280) and (plDeskCam.Height = 800) then bt1280x800.Down := true
  else btNot1280x800.Down := true;
end;

end.
