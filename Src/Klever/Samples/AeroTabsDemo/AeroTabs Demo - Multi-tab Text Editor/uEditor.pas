 (*

        Application: TrkAeroTabbed Text Editor

        Version: 1.0

        Author: Jeff Hansen

        -------------------

        Unit: uEditor.pas

        Notes:  This is the Editor Frame - Make a single change as it was a form
                (Which essentially it is), and use it for all documents. :)

 *)

unit uEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, rkGlassButton, rkVistaPanel, ImgList;

type
  TfrmEditor = class(TFrame)
    pnlTop: TrkVistaPanel;
    btnSave: TrkGlassButton;
    btnOpen: TrkGlassButton;
    redtEditor: TRichEdit;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    imglEditor: TImageList;
    btnBold: TrkGlassButton;
    btnItalic: TrkGlassButton;
    btnUnderline: TrkGlassButton;
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure redtEditorSelectionChange(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
    procedure btnUnderlineClick(Sender: TObject);
    procedure redtEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure redtEditorChange(Sender: TObject);
  private
    procedure ChangeTabCaption(NewCaption: String);
    { Private declarations }
  public
    HasChanged : Boolean;
    procedure SaveContent(Filename: String);
    { Public declarations }
  end;


implementation

Uses
  uMain; // We need to find the parent tabsheet in order to change the Tab-caption.

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
/// ChangeCaption
////////////////////////////////////////////////////////////////////////////////
Procedure TfrmEditor.ChangeTabCaption(NewCaption: String);
Var
  TS: TTabSheet;
  I: Integer;
Begin

  // Always good manners to ask first. :)
  if Parent is TTabSheet then
  begin
    // Assign TS to the corresponding Tabsheet!
    TS := TTabSheet(Parent);
    // Get the Page Index of the tabsheet!
    I := TS.PageIndex;
    // Voila, change the caption of the Aero Tab!! :)
    frmMain.AeroTabs.Tabs[I] := NewCaption;
    frmMain.AeroTabs.Refresh;
  end;

End;


////////////////////////////////////////////////////////////////////////////////
/// SaveContent - Routine to Save the content, since we dont know if its plaintext or RTF format.
////////////////////////////////////////////////////////////////////////////////
Procedure TfrmEditor.SaveContent(Filename : String);
Begin
  // if we got RTF
  if ExtractFileExt(Filename) = '.rtf' then
  // Save as RTF (duh)
  redtEditor.Lines.SaveToFile(Filename)
  else
  // Else, save it as plaintext
  begin
    With TStringlist.Create do
    begin
      Text := redtEditor.Text;
      SaveToFile(Filename);
    end;
  end;

  ChangeTabCaption(ExtractFileName(FileName));
  HasChanged := False;
End;



////////////////////////////////////////////////////////////////////////////////
/// btnOpenClick
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.btnOpenClick(Sender: TObject);
begin
 // If the user chooses to open a file
 if dlgOpen.Execute then
 begin
   // Do as he says - OPEN SESAME!
   redtEditor.Lines.LoadFromFile(dlgOpen.FileName);
   // Change the caption of the Aero Tab!
   ChangeTabCaption(ExtractFileName(dlgOpen.FileName));
 end;
end;


////////////////////////////////////////////////////////////////////////////////
/// btnSaveClick
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.btnSaveClick(Sender: TObject);
begin

 // If the user chooses to save the file
 if dlgSave.Execute then
 begin
   // Change the caption of the Aero Tab!
   ChangeTabCaption(ExtractFileName(dlgSave.FileName));
   // Do as he asks, and save it!
   SaveContent(dlgSave.FileName);
 end;
end;


////////////////////////////////////////////////////////////////////////////////
/// EditorOnChange
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.redtEditorChange(Sender: TObject);
Var
 CurCaption : String;
begin
 // Yes, the OnChange event changed it.. :P
 HasChanged := True;

 // Get the Caption
 CurCaption := frmMain.AeroTabs.Tabs[TTabSheet(Parent).PageIndex];
 // If the Caption already has the Changed star, exit
 if CurCaption[1] = '*' then Exit;

 // Okay, it doesent, give it then!
 ChangeTabCaption('* '+CurCaption);


end;




////////////////////////////////////////////////////////////////////////////////
/// redtEditorOnKeyDown
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.redtEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if ssCtrl in Shift then
 Begin
    // Save
    if Key = Ord('S') then
    btnSaveClick(Sender);

    // Open
    if Key = Ord('O') then
    btnOpenClick(Sender);

    // Bold
    if Key = Ord('B') then
    btnBoldClick(Sender);

    // Italic
    if Key = Ord('K') then // "K", because "I" is used for Indent!
    btnItalicClick(Sender);

    // Underline
    if Key = Ord('U') then
    btnUnderlineClick(Sender);
 End;

end;









////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
/// redtEditorSelectionChange
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.redtEditorSelectionChange(Sender: TObject);
begin
 // Change the UI Buttons accordingly
 btnBold.Down := (fsBold in redtEditor.SelAttributes.Style);
 btnItalic.Down := (fsItalic in redtEditor.SelAttributes.Style);
 btnUnderline.Down := (fsUnderline in redtEditor.SelAttributes.Style);

end;


////////////////////////////////////////////////////////////////////////////////
/// Bold
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.btnBoldClick(Sender: TObject);
begin

 // Bold toggle
 if btnBold.Down then
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style - [fsBold]
 else
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style + [fsBold];

 btnBold.Down := not btnBold.Down;

 // Note to Roy - Try adding TSpeedButton behavior to the TrkGlassButton - the
 // RichEdit looses focus, so I have to manually focus it again! ;)
 redtEditor.SetFocus;
end;



////////////////////////////////////////////////////////////////////////////////
/// Italic
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.btnItalicClick(Sender: TObject);
begin
 // Italic toggle
 if btnItalic.Down then
 Begin
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style - [fsItalic];
 End
 else
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style + [fsItalic];

 btnItalic.Down := not btnItalic.Down;
 redtEditor.SetFocus;

end;

////////////////////////////////////////////////////////////////////////////////
/// Underline
////////////////////////////////////////////////////////////////////////////////
procedure TfrmEditor.btnUnderlineClick(Sender: TObject);
begin
 // Underline toggle
 if btnUnderline.Down then
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style - [fsUnderline]
 else
 redtEditor.SelAttributes.Style := redtEditor.SelAttributes.Style + [fsUnderline];

 btnUnderline.Down := not btnUnderline.Down;
 redtEditor.SetFocus;

end;

end.
