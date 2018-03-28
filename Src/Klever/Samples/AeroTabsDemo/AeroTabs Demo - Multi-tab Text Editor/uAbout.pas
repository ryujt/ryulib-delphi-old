 (*

        Application: TrkAeroTabbed Text Editor

        Version: 1.0

        Author: Jeff Hansen

        -------------------

        Unit: uAbout.pas

        Notes:  The Icons are from the awesome Icon Search Engine: Iconspedia.com.
                On there you will find tons of free icons.

 *)


unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, rkGlassButton, StdCtrls;

type
  TfrmAbout = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnNew: TrkGlassButton;
    Label6: TLabel;
    Label7: TLabel;
    procedure btnNewClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Procedure ShowAbout;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

Procedure ShowAbout;
Var
 Dlg : TfrmAbout;
Begin

 Dlg := TFrmAbout.Create(Nil);
 try
   Dlg.ShowModal;
 finally
   Dlg.Free;
 end;

End;


procedure TfrmAbout.btnNewClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
