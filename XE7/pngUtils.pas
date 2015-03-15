unit PNGUtils;

interface

uses
  Classes, SysUtils, pngImage, DB, Graphics;

procedure FieldToBitmap(Field:TField; Bitmap:TBitmap);
procedure StreamToBitmap(Stream:TStream; Bitmap:TBitmap);

implementation

procedure FieldToBitmap(Field:TField; Bitmap:TBitmap);
var
  PNG : TPNGObject;
  msData : TMemoryStream;
  BlobField : TBlobField absolute Field;
begin
  if BlobField = nil then Exit;

  PNG := TPNGObject.Create;
  msData := TMemoryStream.Create;
  try
    BlobField.SaveToStream(msData);
    if msData.Size = 0 then begin
      Bitmap.Width  := 0;
      Bitmap.Height := 0;
      Exit;
    end;

    msData.Position := 0;

    PNG.LoadFromStream(msData);
    Bitmap.Assign(PNG);
  finally
    PNG.Free;
    msData.Free;
  end
end;

procedure StreamToBitmap(Stream:TStream; Bitmap:TBitmap);
var
  PNG : TPNGObject;
begin
  PNG := TPNGObject.Create;
  try
    if Stream.Size = 0 then begin
      Bitmap.Width  := 0;
      Bitmap.Height := 0;
      Exit;
    end;

    Stream.Position := 0;

    PNG.LoadFromStream(Stream);
    Bitmap.Assign(PNG);
  finally
    PNG.Free;
  end
end;

end.
