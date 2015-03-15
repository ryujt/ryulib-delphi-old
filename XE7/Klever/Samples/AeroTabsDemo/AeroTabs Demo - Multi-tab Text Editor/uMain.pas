 (*

        Application: TrkAeroTabbed Text Editor

        Version: 1.0

        Author: Jeff Hansen

        -------------------

        Unit: uMain.pas

        Notes:  This demo is for demonstrating Roy M. Klever's awesome components.
                The whole concept is about using Frames for a multi-document-interface
                since it allows you to easily access components by their names
                (like redtEditor), rather than Editor1, Editor2, which is a dynamic
                mess. I hope this demo will enlighten you on the use for TrkAeroTabs.

                All respect to Roy, keep making these awesome components dude!

 *)

unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, rkAeroTabs, ComCtrls, rkGlassButton, StdCtrls, ExtCtrls, ImgList;

type
  TfrmMain = class(TForm)
    AeroTabs: TrkAeroTabs;
    pcMain: TPageControl;
    tsWelcome: TTabSheet;
    gbMain: TGroupBox;
    btnNew: TrkGlassButton;
    btnOpen: TrkGlassButton;
    btnAbout: TrkGlassButton;
    sbMain: TStatusBar;
    dlgOpen: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    pnlContainer: TPanel;
    imglMain: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure AeroTabsAddClick(Sender: TObject);
    procedure AeroTabsTabChange(Sender: TObject);
    procedure AeroTabsCloseTab(Sender: TObject; Index: Integer;
      var Close: Boolean);
    procedure btnOpenClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    { Private declarations }
    Procedure AddDocument(Filename : String);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  // The Untitled count, start with one!
  iUntitled : Integer = 1;

implementation

Uses
  uEditor, // We need the Editor Frame!
  uAbout;

{$R *.dfm}


////////////////////////////////////////////////////////////////////////////////
/// UpdateStatusBar - I always have one of these. :)
/// - By not doing TfrmMain.UpdateStatusBar, you can call this everywhere, as
/// long as you add uMain to your Uses Clause. :)
////////////////////////////////////////////////////////////////////////////////
Procedure UpdateStatusBar(FromCloseEvent : Boolean = False);
Var
 NewCount : Integer;
Begin
 // The new Tab count
 NewCount := frmMain.AeroTabs.Tabs.Count-1;
 // The OnClose event does not have the correct tabcount when this is being called
 // - Adjust it!
 if FromCloseEvent then
 NewCount := NewCount-1;

 // If we only have one tab open (The welcome screen), adjust the statusbar!
 if frmMain.AeroTabs.Tabs.Count = 1 then
 frmMain.sbMain.Panels[1].Text := 'No Documents Open!'
 else
 // Else, set the document count. :)
 frmMain.sbMain.Panels[1].Text := 'Documents Open: ' +
                                  IntToStr(NewCount);

End;



////////////////////////////////////////////////////////////////////////////////
/// AddDocument - Used for Opening, AND New documents!
////////////////////////////////////////////////////////////////////////////////
Procedure TfrmMain.AddDocument(Filename : String);
Var
 TS : TTabsheet;
 Editor : TfrmEditor;
Begin

 // Create the Tabsheet - Owned by the Main Pagecontrol!
 TS := TTabSheet.Create(pcMain);
 // Assign the TS to the Main Pagecontrol!
 TS.PageControl := pcMain;
 // We dont want the tab to be visible in the Pagecontrol!
 TS.TabVisible := False;

 // Create the editor - Owned by the Tabsheet!
 Editor := TfrmEditor.Create(TS);
 // Assign the Editors parent to the tabsheet!
 Editor.Parent := TS;
 // Null the has Changed variable
 Editor.HasChanged := False;

 // If we are opening a document...
 if Filename <> '' then
 begin
   // Okay, now add the Tab to the AeroTabs control!
   AeroTabs.AddTab(ExtractFileName(Filename));
   // Load the text into the Editor - See how easy it is with frames?
   Editor.redtEditor.Lines.LoadFromFile(Filename);
 end
 // Okay, its a New Document!
 else
 Begin
 // Add it!
 AeroTabs.AddTab('Untitled '+IntToStr(iUntitled));
 // Increment the Untitled count. :)
 Inc(iUntitled);
 End;

 // Set the Active Tab in the Pagecontrol!
 pcMain.ActivePage := TS;

 // Update the statusbar. :)
 UpdateStatusBar;

End;


////////////////////////////////////////////////////////////////////////////////
/// AeroTabsOnAddClick
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.AeroTabsAddClick(Sender: TObject);
begin
 ///////////////////////////////////////////////////////
 ///  Please note this procedure is being used by    ///
 ///  the Plus button, and the Create New Document   ///
 ///  button.                                        ///
 ///////////////////////////////////////////////////////

 AddDocument('');

end;

////////////////////////////////////////////////////////////////////////////////
/// AeroTabsCloseTab
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.AeroTabsCloseTab(Sender: TObject; Index: Integer;
  var Close: Boolean);
Var
 Editor : TFrmEditor;
begin
  // If its the Welcome tabsheet, prompt to close the application!
  if Index = 0 then
  if MessageDlg('Are you sure you want to close the Application?'+sLineBreak+
                ' All unsaved data will be lost!',mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
   frmMain.Close;
  end else Exit;

  // The Tabsheet only has one control, which is the Editor. Therefore its safe
  // to asume that the index is 0 :)
  Editor := TfrmEditor(pcMain.Pages[Index].Components[0]);

  // If the document has changed, prompt for save!
  if (Editor.HasChanged) and (MessageDlg('Would you like to save this document before closing it?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
  begin
   if Editor.dlgSave.Execute then
   Editor.SaveContent(Editor.dlgSave.FileName);
  end;


  // Free the Tabsheet!
  pcMain.Pages[Index].Free;
  // Signal the close!
  Close := True;

  // Update the statusbar
  UpdateStatusBar(True);
end;



////////////////////////////////////////////////////////////////////////////////
/// AeroTabsTabChange
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.AeroTabsTabChange(Sender: TObject);
begin
  // Change the Active Tab in the PageControl aswell!
  pcMain.ActivePageIndex := AeroTabs.ActiveTab;
end;


////////////////////////////////////////////////////////////////////////////////
/// btnOpenClick
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
 // If the user wants to truly open a document..
 if dlgOpen.Execute then
 begin
  // Do it!
  AddDocument(dlgOpen.FileName);
 end;
end;


////////////////////////////////////////////////////////////////////////////////
/// btnAboutClick
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
 ShowAbout;
end;


////////////////////////////////////////////////////////////////////////////////
/// FormCreate
////////////////////////////////////////////////////////////////////////////////
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // If the application is running Windows Vista or higher, change the
  // background color of AeroTabs to Black, so it is glass-compatible.
  // - We do this, to ensure that it looks good on XP and lower. :)
  if CheckWin32Version(5,4) then
  AeroTabs.ColorBackground := clBlack;
end;

end.
