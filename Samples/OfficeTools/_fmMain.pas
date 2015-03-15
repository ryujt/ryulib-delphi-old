unit _fmMain;

interface

uses
  OfficeTools, Strg,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    OpenDialog: TOpenDialog;
    btOpen: TButton;
    procedure btOpenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then PPT2JPG(OpenDialog.FileName, DeleteRightPlus(OpenDialog.FileName, '.'));
end;

end.

uses
  SysUtils,
  Variants,
  Dialogs,
  ActiveX,
  Windows,
  UDC_TLB,
  Office_TLB,
  PowerPoint_TLB;

  procedure PrintPowerPointToJPEG(PowerPointFilePath: string);
  var
    objUDC: IUDC;
    Printer: IUDCPrinter;
    Profile: IProfile;
    PowerPointApp: PowerPointApplication;
    Presentation: PowerPointPresentation;
    PrintOptions: PowerPoint_TLB.PrintOptions;
  begin
    //Create a UDC object and get its interfaces
    objUDC := CoAPIWrapper.Create;
    Printer := objUDC.get_Printers('Universal Document Converter');
    Profile := Printer.Profile;

    //Use Universal Document Converter API to change settings of converterd document
    Profile.PageSetup.Orientation := PO_LANDSCAPE;

    Profile.FileFormat.ActualFormat := FMT_JPEG;
    Profile.FileFormat.JPEG.ColorSpace := CS_TRUECOLOR;

    Profile.OutputLocation.Mode := LM_PREDEFINED;
    Profile.OutputLocation.FolderPath := 'c:\UDC Output Files';
    Profile.OutputLocation.FileName := '&[DocName(0)] -- &[Date(0)] -- &[Time(0)].&[ImageType]';
    Profile.OutputLocation.OverwriteExistingFile := False;

    Profile.PostProcessing.Mode := PP_OPEN_FOLDER;

    //Run Microsoft Excel as COM-server
    PowerPointApp := CoPowerPointApplication.Create;

    //Open document from file
    Presentation := PowerPointApp.Presentations.Open(PowerPointFilePath, msoTrue,  msoTrue, msoFalse);

    //Print all slides from the presentation
    PrintOptions := Presentation.PrintOptions;
    PrintOptions.PrintInBackground := msoFalse;
    PrintOptions.ActivePrinter := 'Universal Document Converter';

    Presentation.PrintOut(0, Presentation.Slides.Count, '', 1, msoFalse);

    //Close the presentation
    Presentation.Close();

    //Close Microsoft PowerPoint
    PowerPointApp.Quit();
  end;
