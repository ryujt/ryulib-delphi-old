unit ImageTools;

interface

uses
  Classes, SysUtils, Graphics, PngImage, JPeg;

procedure JPegFileToBitmap(AFileName:String; ABitmap:TBitmap);
procedure PngFileToBitmap(AFileName:String; ABitmap:TBitmap);
function LoadFileToBitmap(AFileName:String; ABitmap:TBitmap):boolean;

procedure JPegStreamToBitmap(AStream:TStream; ABitmap:TBitmap);
procedure PngStreamToBitmap(AStream:TStream; ABitmap:TBitmap);
function LoadStreamToBitmap(AStream:TMemoryStream; ABitmap:TBitmap):boolean;

procedure JPegFileToPng(AFileName:String; APNGObject:TPNGObject);
procedure BmpFileToPng(AFileName:String; APNGObject:TPNGObject);
function LoadFileToPng(AFileName:String; AStream:TStream):boolean;

implementation

type
  TImageType = (itBmp, itJPeg, itPng);

procedure JPegFileToBitmap(AFileName:String; ABitmap:TBitmap);
var
  JPeg : TJPEGImage;
begin
  JPeg := TJPEGImage.Create;
  try
    JPeg.LoadFromFile(AFileName);
    ABitmap.Assign(JPeg);
  finally
    JPeg.Free;
  end
end;

procedure PngFileToBitmap(AFileName:String; ABitmap:TBitmap);
var
  Png : TPNGObject;
begin
  Png := TPNGObject.Create;
  try
    Png.LoadFromFile(AFileName);
    ABitmap.Assign(Png);
  finally
    Png.Free;
  end
end;

function LoadFileToBitmap(AFileName:String; ABitmap:TBitmap):boolean;
var
  Ext : string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  try
    if Pos(Ext, '.jpeg; .jpg') > 0 then JPegFileToBitmap(AFileName, ABitmap)
    else if Pos(Ext, '.png') > 0 then PngFileToBitmap(AFileName, ABitmap)
    else ABitmap.LoadFromFile(AFileName);
    Result := true;
  except
    Result := false;
  end;
end;

procedure JPegStreamToBitmap(AStream:TStream; ABitmap:TBitmap);
var
  JPeg : TJPEGImage;
begin
  JPeg := TJPEGImage.Create;
  try
    AStream.Position := 0;
    JPeg.LoadFromStream(AStream);
    ABitmap.Assign(JPeg);
  finally
    JPeg.Free;
  end
end;

procedure PngStreamToBitmap(AStream:TStream; ABitmap:TBitmap);
var
  Png : TPNGObject;
begin
  Png := TPNGObject.Create;
  try
    AStream.Position := 0;
    Png.LoadFromStream(AStream);
    ABitmap.Assign(Png);
  finally
    Png.Free;
  end
end;

function CheckHeader(AData:pointer):TImageType;
var
  pByte : ^byte;
  Header : ShortString;
begin
  // BMP 검사
  Header[0] := #2; // SetLength(Header, 3);
  Move(AData^, Header[1], 2);
  if LowerCase(Header) = 'bm' then begin
    Result := itBmp;
    Exit;
  end;

  // PNG 검사
  pByte := AData;
  Inc(pByte);
  Header[0] := #3; // SetLength(Header, 3);
  Move(pByte^, Header[1], 3);
  if LowerCase(Header) = 'png' then begin
    Result := itPng;
    Exit;
  end;

  Result := itJPeg;
end;

function LoadStreamToBitmap(AStream:TMemoryStream; ABitmap:TBitmap):boolean;
begin
  try
    case CheckHeader(AStream.Memory) of
      itJPeg : JPegStreamToBitmap(AStream, ABitmap);
      itPng : PngStreamToBitmap(AStream, ABitmap);
      else ABitmap.LoadFromStream(AStream);
    end;
    Result := true;
  except
    Result := false;
  end;
end;

procedure JPegFileToPng(AFileName:String; APNGObject:TPNGObject);
var
  JPeg : TJPEGImage;
  Bitmap : TBitmap;
begin
  JPeg := TJPEGImage.Create;
  Bitmap := TBitmap.Create;
  try
    JPeg.LoadFromFile(AFileName);
    Bitmap.Assign(JPeg);
    APNGObject.Assign(Bitmap);
  finally
    JPeg.Free;
    Bitmap.Free;
  end
end;

procedure BmpFileToPng(AFileName:String; APNGObject:TPNGObject);
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile(AFileName);
    APNGObject.Assign(Bitmap);
  finally
    Bitmap.Free;
  end
end;

function LoadFileToPng(AFileName:String; AStream:TStream):boolean;
var
  Ext : string;
  PNGObject : TPNGObject;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));

  PNGObject := TPNGObject.Create;
  try
    try
      if Pos(Ext, '.jpeg; .jpg') > 0 then JPegFileToPng(AFileName, PNGObject)
      else if Pos(Ext, '.png') > 0 then PNGObject.LoadFromFile(AFileName)
      else BmpFileToPng(AFileName, PNGObject);

      PNGObject.SaveToStream(AStream);
      Result := true;
    except
      Result := false;
    end;
  finally
    PNGObject.Free;
  end;
end;

end.
