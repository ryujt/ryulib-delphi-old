unit DBPNG;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls, DB, DBCtrls,
  PngImage;

type
  TDBPNG = class (TImage)
  private
    procedure do_LoadBitmapFile(FileName:string; Stream:TStream);
    procedure on_DataChanged(Sender:TObject);
  private
    FDataLink : TFieldDataLink;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetDataField: string;
    procedure SetDataField(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName:string);
    procedure LoadFromStream(Stream:TStream);
  published
    property DataField : string read GetDataField write SetDataField;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TDBPNG]);
end;

{ TDBPNG }

constructor TDBPNG.Create(AOwner: TComponent);
begin
  inherited;

  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := on_DataChanged;
end;

destructor TDBPNG.Destroy;
begin
  FDataLink.Free;

  inherited;
end;

procedure TDBPNG.do_LoadBitmapFile(FileName: string; Stream: TStream);
var
  Bitmap : TBitmap;
  PNG : TPNGObject;
begin
  Bitmap := TBitmap.Create;
  PNG := TPNGObject.Create;
  try
    Bitmap.LoadFromFile(FileName);
    PNG.Assign(Bitmap);
    PNG.SaveToStream(Stream);
    Stream.Position := 0;
  finally
    PNG.Free;
    Bitmap.Free;
  end
end;

function TDBPNG.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBPNG.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPNG.LoadFromFile(FileName: string);
var
  msData : TMemoryStream;
  BlobField : TBlobField;
begin
  BlobField := TBlobField(FDataLink.Field);

  msData := TMemoryStream.Create;
  try
    if LowerCase(ExtractFileExt(FileName)) = '.bmp' then begin
      do_LoadBitmapFile(FileName, msData);
      BlobField.LoadFromStream(msData);
    end else
      BlobField.LoadFromFile(FileName);
  finally
    msData.Free;
  end;
end;

procedure TDBPNG.LoadFromStream(Stream: TStream);
var
  BlobField : TBlobField;
begin
  BlobField := TBlobField(FDataLink.Field);
  BlobField.LoadFromStream(Stream);
end;

procedure TDBPNG.on_DataChanged(Sender: TObject);
var
  PNG : TPNGObject;
  BlobField : TBlobField;
  msData : TMemoryStream;
begin
  BlobField := TBlobField(FDataLink.Field);

  PNG := TPNGObject.Create;
  msData := TMemoryStream.Create;
  try
    BlobField.SaveToStream(msData);
    msData.Position := 0;

    if msData.Size > 0 then begin
      PNG.LoadFromStream(msData);
      Picture.Bitmap.Assign(PNG);
    end else begin
      Picture.Bitmap.Width  := Width;
      Picture.Bitmap.Height := Height;
      Picture.Bitmap.Canvas.Brush.Color := clWhite;
      Picture.Bitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
  finally
    PNG.Free;
    msData.Free;
  end
end;

procedure TDBPNG.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBPNG.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

end.
