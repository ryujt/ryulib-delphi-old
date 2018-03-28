{
  IJL Use Unit
  Last Update : 6.12.2010
  Made in ThinkPlant by Lyn
}
unit IJLUtils;

interface

uses
  IJL,
  Windows, SysUtils, Classes;

// 아래 두 함수 모두 24bit Bitmap을 사용합니다.
procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer=100);
procedure BitmapToJpegCopy(AJpegBuffer:Pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer=100);

procedure JpegToBitmap(ASrc:pointer; ASrcSize:integer; var ADst:pointer; var ADstSize:integer; var AWidth,AHeight:integer);
procedure JpegToBitmapCopy(ASrc:pointer; ASrcSize:integer; ADst:pointer; var ADstSize,AWidth,AHeight:integer);

implementation

procedure BitmapToJpeg(ASrc:pointer; AWidth,AHeight:integer; var ADst:pointer; var ADstSize:integer; AQuality:integer);
var
  ErrorCode : Integer;
  JpegBuffer : Pointer;
  IJLData: TJPEG_CORE_PROPERTIES;
begin
  ErrorCode := ijlInit(@IJLData);
  if ErrorCode <> IJL_OK then raise Exception.Create('ijlInit Error');

  try
    IJLData.DIBChannels := 3;
    IJLData.DIBPadBytes := 0;
    IJLData.DIBWidth := AWidth;
    IJLData.DIBHeight := -AHeight;
    IJLData.DIBBytes := ASrc;
    IJLData.DIBColor := IJL_BGR;

    GetMem(JpegBuffer, 3 * AWidth * AHeight * 2);
    try
      IJLData.JPGWidth := AWidth;
      IJLData.JPGHeight := AHeight;
      IJLData.JPGFile := nil;
      IJLData.JPGBytes := JpegBuffer;
      IJLData.JPGSizeBytes := 3 * AWidth * AHeight * 2;
      IJLData.JPGChannels := 3;
      IJLData.JPGColor := IJL_YCBCR;
      IJLData.JPGSubsampling := IJL_411;
      IJLData.jquality := AQuality;

      ErrorCode := ijlWrite(@IJLData, IJL_JBUFF_WRITEWHOLEIMAGE);
      if ErrorCode <> IJL_OK then raise Exception.Create('ijlWrite Error');

      ADstSize := IJLData.JPGSizeBytes;

      GetMem(ADst, IJLData.JPGSizeBytes);
      Move(IJLData.JPGBytes^, ADst^, IJLData.JPGSizeBytes);
    finally
      FreeMem(JpegBuffer);
    end;
  finally
    ijlFree(@IJLData);
  end;
end;

procedure BitmapToJpegCopy(AJpegBuffer:Pointer; ASrc:pointer; AWidth,AHeight:integer; ADst:pointer; var ADstSize:integer; AQuality:integer);
var
  ErrorCode : Integer;
  IJLData: TJPEG_CORE_PROPERTIES;
begin
  ErrorCode := ijlInit(@IJLData);
  if ErrorCode <> IJL_OK then raise Exception.Create('ijlInit Error');

  try
    IJLData.DIBChannels := 3;
    IJLData.DIBPadBytes := 0;
    IJLData.DIBWidth := AWidth;
    IJLData.DIBHeight := -AHeight;
    IJLData.DIBBytes := ASrc;
    IJLData.DIBColor := IJL_BGR;

    IJLData.JPGWidth := AWidth;
    IJLData.JPGHeight := AHeight;
    IJLData.JPGFile := nil;
    IJLData.JPGBytes := AJpegBuffer;
    IJLData.JPGSizeBytes := 3 * AWidth * AHeight * 2;
    IJLData.JPGChannels := 3;
    IJLData.JPGColor := IJL_YCBCR;
    IJLData.JPGSubsampling := IJL_411;
    IJLData.jquality := AQuality;

    ErrorCode := ijlWrite(@IJLData, IJL_JBUFF_WRITEWHOLEIMAGE);
    if ErrorCode <> IJL_OK then raise Exception.Create('ijlWrite Error');

    ADstSize := IJLData.JPGSizeBytes;
    Move(IJLData.JPGBytes^, ADst^, IJLData.JPGSizeBytes);
  finally
    ijlFree(@IJLData);
  end;
end;

procedure JpegToBitmap(ASrc:pointer; ASrcSize:integer; var ADst:pointer; var ADstSize:integer; var AWidth,AHeight:integer);
var
  IJLData: TJPEG_CORE_PROPERTIES;
  ErrorCode : Integer;
begin
  ErrorCode := ijlInit(@IJLData);
  if ErrorCode <> IJL_OK then raise Exception.Create('ijlInit Error');

  try
    IJLData.JPGFile := nil;
    IJLData.JPGBytes := ASrc;
    IJLData.JPGSizeBytes := ASrcSize;
    ErrorCode := ijlRead(@IJLData, IJL_JBUFF_READPARAMS);
    if ErrorCode <> IJL_OK then raise Exception.Create('ijlRead Error');

    case IJLData.JPGChannels of
      1: begin
        IJLData.JPGColor := IJL_G;
        IJLData.DIBColor := IJL_BGR;
        IJLData.DIBChannels := 3;
      end;
      3: begin
        IJLData.JPGColor := IJL_YCBCR;
        IJLData.DIBColor := IJL_BGR;
        IJLData.DIBChannels := 3;
      end;
    end;

    ADstSize := (IJLData.JPGWidth * IJLData.JPGHeight * IJLData.DIBChannels);// + sizeof(BITMAPFILEHEADER);
    GetMem(ADst, ADstSize);

    IJLData.DIBWidth := IJLData.JPGWidth;
    IJLData.DIBHeight := -IJLData.JPGHeight;
    IJLData.DIBChannels := 3;
    IJLData.DIBPadBytes := 0;
    IJLData.DIBBytes := ADst;
    ErrorCode := ijlRead(@IJLData,  IJL_JBUFF_READWHOLEIMAGE);
    if ErrorCode <> IJL_OK then raise Exception.Create('ijlRead Error');

    AWidth := IJLData.DIBWidth;
    AHeight := -IJLData.DIBHeight;
  finally
    ijlFree(@IJLData);
  end;
end;

procedure JpegToBitmapCopy(ASrc:pointer; ASrcSize:integer; ADst:pointer; var ADstSize,AWidth,AHeight:integer); overload;
var
  IJLData: TJPEG_CORE_PROPERTIES;
  ErrorCode : Integer;
begin
  ErrorCode := ijlInit(@IJLData);
  if ErrorCode <> IJL_OK then raise Exception.Create('ijlInit Error');

  try
    IJLData.JPGFile := nil;
    IJLData.JPGBytes := ASrc;
    IJLData.JPGSizeBytes := ASrcSize;
    ErrorCode := ijlRead(@IJLData, IJL_JBUFF_READPARAMS);
    if ErrorCode <> IJL_OK then raise Exception.Create('ijlRead Error');

    case IJLData.JPGChannels of
      1: begin
        IJLData.JPGColor := IJL_G;
        IJLData.DIBColor := IJL_BGR;
        IJLData.DIBChannels := 3;
      end;
      3: begin
        IJLData.JPGColor := IJL_YCBCR;
        IJLData.DIBColor := IJL_BGR;
        IJLData.DIBChannels := 3;
      end;
    end;

    ADstSize := (IJLData.JPGWidth * IJLData.JPGHeight * IJLData.DIBChannels);// + sizeof(BITMAPFILEHEADER);

    IJLData.DIBWidth := IJLData.JPGWidth;
    IJLData.DIBHeight := -IJLData.JPGHeight;
    IJLData.DIBChannels := 3;
    IJLData.DIBPadBytes := 0;
    IJLData.DIBBytes := ADst;
    ErrorCode := ijlRead(@IJLData,  IJL_JBUFF_READWHOLEIMAGE);
    if ErrorCode <> IJL_OK then raise Exception.Create('ijlRead Error');

    AWidth := IJLData.DIBWidth;
    AHeight := -IJLData.DIBHeight;
  finally
    ijlFree(@IJLData);
  end;
end;

end.
