unit LoadImageFromURL;

interface

uses
  DebugTools, RyuGraphics, HandleComponent, SimpleThread, Base64, Disk,
  IdHTTP, IdURI, IdSSLOpenSSL, JPeg, PngImage,
  Windows, Messages, Classes, SysUtils, Types, Graphics, ExtCtrls;

type
  TCallBack = reference to procedure(AGraphic:TGraphic);
  TCallBackBitmap = reference to procedure(ABitmap:TBitmap);

procedure AfterLoadImageFromURL(AURL:string; ACallBack:TCallBack);

procedure ImageFromURL(const AURL:string; AImage:TImage);

function LoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap):boolean; overload;
function LoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap; AWidth,AHeight:integer):boolean; overload;

//procedure AsyncLoadImageFromUrlToBitmap(AURL:string; ACallBack:TCallBackBitmap); overload;
procedure AsyncLoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap); overload;
procedure AsyncLoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap; AWidth,AHeight:integer); overload;

implementation

const
  WM_Downloaded = WM_USER + 1;
  WM_Error = WM_USER + 2;

type
  TDownloader = class (THandleComponent)
  private
    FURL : string;
    FCallBack : TCallBack;
    procedure do_WM_Downloaded(var AMsg:TMessage); message WM_Downloaded;
    procedure do_WM_Error(var AMsg:TMessage); message WM_Error;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Execute(Sender:TObject);
  public
    constructor Create(AURL:string; ACallBack:TCallBack); reintroduce;
  end;

{ TDownloader }

constructor TDownloader.Create(AURL:string; ACallBack:TCallBack);
begin
  inherited Create(nil);

  FURL := Trim(AURL);
  FCallBack := ACallBack;

  FSimpleThread := TSimpleThread.Create(on_Execute);
  FSimpleThread.Name := 'TDownloader';
end;

procedure TDownloader.do_WM_Downloaded(var AMsg: TMessage);
var
  Graphic : TGraphic;
begin
  Graphic := Pointer(AMsg.WParam);
  try
    FCallBack(Graphic);
  finally
    Graphic.Free;
  end;
  FreeAndNil(Self);
end;

procedure TDownloader.do_WM_Error(var AMsg: TMessage);
begin
  FCallBack(nil);
  FreeAndNil(Self);
end;

procedure TDownloader.on_Execute(Sender:TObject);
const
  JPEG_SOI_Marker : word = $D8FF;
var
  IdHTTP : TIdHTTP;
  Graphic : TGraphic;
  msData : TMemoryStream;
  pMarker : PWord;
begin
  IdHTTP := TIdHTTP.Create(nil);
  msData := TMemoryStream.Create;
  try
    try
      with TIdURI.Create(FURL) do
      try
        if LowerCase(Protocol) = 'https' then
          IdHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHTTP);
      finally
        Free;
      end;

      // 웹서버에서 악성코드로 인식을 방지하기 위해 크롬 브라우져로 위장 한다.
      IdHTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.60 Safari/537.1 CoolNovo/2.0.4.16';

      IdHTTP.Get(FURL, msData);

      if msData.Size = 0 then raise Exception.Create('Download failed.');

      pMarker := msData.Memory;

      if pMarker^ = JPEG_SOI_Marker then Graphic := TJPEGImage.Create
      else Graphic := TPNGImage.Create;

      msData.Position := 0;
      Graphic.LoadFromStream(msData);
      PostMessage(Handle, WM_Downloaded, Integer(Graphic), 0);
    except
      PostMessage(Handle, WM_Error, 0, 0);
    end;
  finally
    IdHTTP.Disconnect;

    IdHTTP.Free;
    msData.Free;
  end;
end;

procedure AfterLoadImageFromURL(AURL:string; ACallBack:TCallBack);
begin
  TDownloader.Create(AURL, ACallBack);
end;

procedure ImageFromURL(const AURL:string; AImage:TImage);
var
  Graphic : TGraphic;
  FileName : string;
begin
  FileName := GetTempDirectory + Base64Encode(AURL);

  if FileExists(FileName) then begin
    Graphic := LoadGraphicFromFile(FileName);
    try
      AImage.Picture.Assign(Graphic);
    finally
      Graphic.Free;
    end;

    Exit;
  end;

  AfterLoadImageFromURL(
    AURL,
    procedure (AGraphic:TGraphic) begin
      try
        if AGraphic <> nil then begin
          AImage.Picture.Assign(AGraphic);
          AImage.Repaint;
        end;
      except
        Trace('ImageFromURL failed.');
      end;

      try
        AGraphic.SaveToFile(FileName);
      except
        Trace('Saving ImageFromURL failed.');
      end;
    end
  );
end;

function LoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap):boolean;
const
  JPEG_SOI_Marker : word = $D8FF;
var
  IdHTTP : TIdHTTP;
  pMarker : PWord;
  Graphic : TGraphic;
  msData : TMemoryStream;
begin
  Result := false;

  AURL := Trim(AURL);
  if AURL = '' then Exit;

  IdHTTP := TIdHTTP.Create(nil);
  msData := TMemoryStream.Create;
  try
    try
      with TIdURI.Create(AURL) do
      try
        if LowerCase(Protocol) = 'https' then
          IdHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHTTP);
      finally
        Free;
      end;

      IdHTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.60 Safari/537.1 CoolNovo/2.0.4.16';

      IdHTTP.Get(AURL, msData);

      if msData.Size = 0 then Exit;

      pMarker := msData.Memory;

      if pMarker^ = JPEG_SOI_Marker then Graphic := TJPEGImage.Create
      else Graphic := TPNGImage.Create;

      try
        msData.Position := 0;
        Graphic.LoadFromStream(msData);
        ABitmap.Assign(Graphic);
      finally
        Graphic.Free;
      end;
    except
      Exit;
    end;

    Result := true;
  finally
    IdHTTP.Disconnect;

    IdHTTP.Free;
    msData.Free;
  end;
end;

function LoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap; AWidth,AHeight:integer):boolean;
var
  Src, Dst : TBitmap;
  TargetSize : TPoint;
begin
  Result := false;

  AURL := Trim(AURL);
  if AURL = '' then Exit;

  if AWidth * AHeight <> 0 then begin
    Src := TBitmap.Create;
    Dst := TBitmap.Create;
    try
      Result := LoadImageFromUrlToBitmap(AURL, Src);

      if Result then begin
        Src.PixelFormat := pf32bit;
        Dst.PixelFormat := pf32bit;

        TargetSize := RatioSize(Point(Src.Width, Src.Height), Point(AWidth, AHeight));

        Dst.Width := TargetSize.X;
        Dst.Height := TargetSize.Y;

        try
          SmoothResize(Src, Dst);
          ABitmap.Assign(Dst);
        except
          Result := false;
        end;
      end;
    finally
      Src.Free;
      Dst.Free;
    end;
  end else begin
    Result := LoadImageFromUrlToBitmap(AURL, ABitmap);
  end;
end;

procedure AsyncLoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap);
begin
  AURL := Trim(AURL);
  if AURL = '' then Exit;

  AfterLoadImageFromURL(AURL,
    procedure (AGraphic:TGraphic) begin
      if AGraphic <> nil then ABitmap.Assign(AGraphic);
    end
  );
end;

procedure AsyncLoadImageFromUrlToBitmap(AURL:string; ABitmap:TBitmap; AWidth,AHeight:integer); overload;
var
  Src, Dst : TBitmap;
  TargetSize : TPoint;
begin
  AURL := Trim(AURL);
  if AURL = '' then Exit;

  AfterLoadImageFromURL(AURL,
    procedure (AGraphic:TGraphic) begin
      if AGraphic = nil then Exit;

      if AWidth * AHeight <> 0 then begin
        Src := TBitmap.Create;
        Dst := TBitmap.Create;
        try
          Src.Assign(AGraphic);

          Src.PixelFormat := pf32bit;
          Dst.PixelFormat := pf32bit;

          TargetSize := RatioSize(Point(Src.Width, Src.Height), Point(AWidth, AHeight));

          Dst.Width := TargetSize.X;
          Dst.Height := TargetSize.Y;

          try
            SmoothResize(Src, Dst);
            ABitmap.Assign(Dst);
          except
          end;
        finally
          Src.Free;
          Dst.Free;
        end;
      end else begin
        ABitmap.Assign(AGraphic);
      end;
    end
  );
end;

end.
