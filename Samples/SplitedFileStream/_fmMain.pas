unit _fmMain;

interface

uses
  Disk, SplitedFileStream, CompareBytes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btSrc: TButton;
    btSplit: TButton;
    btDst: TButton;
    btTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btSrcClick(Sender: TObject);
    procedure btSplitClick(Sender: TObject);
    procedure btDstClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    msSrc : TMemoryStream;
    msDst : TMemoryStream;
    procedure MakeSrc;
    procedure Split;
    procedure MakeDst;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure MakeRandomBuffer(AData:pointer; ASize:integer);
var
  Loop: Integer;
  pData : PByte absolute AData;
begin
  for Loop := 1 to ASize do begin
    pData^ := Random(256);
    Inc(pData);
  end;
end;

procedure TfmMain.btDstClick(Sender: TObject);
begin
  MakeDst;
  msDst.SaveToFile(GetExecPath+'Dst.data');
end;

procedure TfmMain.btSplitClick(Sender: TObject);
begin
  Split;
end;

procedure TfmMain.btSrcClick(Sender: TObject);
begin
  MakeSrc;
end;

procedure TfmMain.btTestClick(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 1 to 1000 do begin
    MakeSrc;
    Split;
    MakeDst;

    if not CompareFastBytes(msSrc.Memory, msDst.Memory, msSrc.Size) then
      raise Exception.Create('Error - msSrc.Memory <> msDst.Memory');
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  msSrc := TMemoryStream.Create;
  msDst := TMemoryStream.Create;
end;

procedure TfmMain.MakeDst;
var
  iSize, iRead : integer;
  Stream : TSplitedFileStream;
  Buffer : array [0..2048-1] of byte;
begin
  Stream := TSplitedFileStream.Create;
  Stream.SplitSize := 128 * 1024;
  Stream.OpenRead(GetExecPath+'Split.data');

  msDst.Clear;

  repeat
    iSize := 256 + Random(1024);

    iRead := Stream.Read(Buffer, iSize);

    msDst.Write(Buffer, iRead);
  until Stream.IsEOF;

  FreeAndNil(Stream);
end;

procedure TfmMain.MakeSrc;
var
  Buffer : array [0..1024-1] of byte;
  Loop: Integer;
begin
  msSrc.Clear;

  for Loop := 1 to 1024 do begin
    MakeRandomBuffer(@Buffer, SizeOf(Buffer));
    msSrc.Write(Buffer, SizeOf(Buffer));
  end;
end;

procedure TfmMain.Split;
var
  iSize, iRead : integer;
  Stream : TSplitedFileStream;
  Buffer : array [0..2048-1] of byte;
begin
  Stream := TSplitedFileStream.Create;
  Stream.SplitSize := 128 * 1024;
  Stream.OpenWrite(GetExecPath+'Split.data');

  msSrc.Position := 0;

  while msSrc.Position <= msSrc.Size do begin
    iSize := 256 + Random(1024);

    iRead := msSrc.Read(Buffer, iSize);
    if iRead = 0 then Break;

    Stream.Write(Buffer, iRead);
  end;

  FreeAndNil(Stream);
end;

end.
