unit _fmMain;

interface

uses
  Zlib64Utils, Zlib64,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    BtnCompress: TButton;
    Edit1: TEdit;
    BtnDecompress: TButton;
    Edit2: TEdit;
    procedure BtnCompressClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnDecompressClick(Sender: TObject);
  private
    CompressedStream : TMemoryStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BtnCompressClick(Sender: TObject);
var
  SrcStream : TStringStream;
  a : TZStreamRec;
begin
  CompressedStream.Clear;
  SrcStream := TStringStream.Create(Edit1.Text);
  ShrinkStreamSlow(SrcStream, CompressedStream);
  ShowMessage(Format('Original Length : %d'#13#10'Compress Length : %d',
    [SrcStream.Size, CompressedStream.Size]));

  SrcStream.Free;
end;

procedure TForm1.BtnDecompressClick(Sender: TObject);
var
  DestStream : TStringStream;
begin
  DestStream := TStringStream.Create;
  ExpandStream(CompressedStream, DestStream);
  Edit2.Text := DestStream.DataString;
  DestStream.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CompressedStream := TMemoryStream.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CompressedStream.Free;
end;

end.
