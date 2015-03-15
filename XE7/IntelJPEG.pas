{******************************************************************************)

                 ijl(Intel JPEG Library) Graphic Class
                      (2002/2)propws@hanmail.net

  수정자 : 홍환민

  이 클래스를 사용하기 위해서는 ijl15.dll 파일이 실행파일이 있는 디렉토리나
  윈도우즈 시스템 디렉토리에 있어야 한다.

  - 2003/10/10 홍환민 : TJpegImage라는 클래스명을 TIntelJPEGImage로 변경.
  - 2003/10/10 홍환민 : 이 유닛의 파일명 변경. 간단히 소스 정리.
(******************************************************************************}
unit IntelJPEG;

interface

uses
  Windows, SysUtils, Classes, Graphics, ijl, initCPUInfo;

type
  Eijlerror = class(Exception);

  TJpegQuality = 0..integer(100);

  TIntelJPEGImage = class(TBitmap)
  private
    FQuality: TJpegQuality;
    fComment: string;
  protected
    function LoadProcess(filename:string; Buff:pointer; BuffSize:integer):integer; virtual;
    function SaveProcess(filename:string; Buff:pointer; BuffSize:integer):integer; virtual;
    procedure ReadProp(var jprop:TJPEG_CORE_PROPERTIES); virtual;
    procedure WriteProp(var jprop:TJPEG_CORE_PROPERTIES); virtual;
    procedure ReadData(Stream: TStream);override;
    procedure WriteData(Stream: TStream);override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromFile(const Filename:string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToFile(const Filename:string); override;

    property  Quality: TJpegQuality read FQuality write FQuality default 75;
    property  Comment: string read fComment write fComment; // 기록만 가능
  end;

procedure GetJPEGSize(filename: string; var Width, Height: Integer);

implementation

procedure Chkijl(code:integer);
begin
  if code < 0 then
    raise Eijlerror.Create(ijlErrorStr(code));
end;

function GetPadBytes(Width,Channels:integer):integer;
begin
  Result := ((((Width*Channels)+3) div 4)*4)-(Width*Channels);
end;

function GetProcesserType:integer;
begin
  result:=0;  // Pentium 외 CPU
  if CPUInfo.VendorID='GenuineIntel' then begin
    if CPUInfo.Family=5 then result:=1; // Pentium
    if CPUInfo.Family=6 then begin
      result:=2;  // Pentium pro
      if CPUInfo.Model > 1 then result:=4;  // Pentium2
    end;
    if(cfMMX in CPUInfo.Features)and(result < 3)then result:=3; // Pentium MMX
    if cfSSE in CPUInfo.Features then result:=5;  // Pentium3
    if cfSSE2 in CPUInfo.Features then result:=6; // Pentium4
  end;
end;

procedure SetProcesserType(CPUKey:integer);
var
  Key: HKEY;
  Dummy: Integer;
begin
  RegCreateKeyEx(HKEY_LOCAL_MACHINE,'SOFTWARE\Intel Corporation\PLSuite\IJLib',
    0,nil,REG_OPTION_NON_VOLATILE,KEY_WRITE,nil,Key,@Dummy);
  RegSetValueEx(Key,'USECPU',0,REG_DWORD,@CPUKey,4);
  RegCloseKey(Key);
end;

{ TIntelJPEGImage }

constructor TIntelJPEGImage.Create;
begin
  inherited Create;

  Quality := 75;
end;

// ReadProp과 WriteProp을 override하면 ijl을 세부조정 할수 있다.

procedure TIntelJPEGImage.ReadProp(var jprop: TJPEG_CORE_PROPERTIES);
begin
// empty
end;

procedure TIntelJPEGImage.WriteProp(var jprop: TJPEG_CORE_PROPERTIES);
begin
// empty
end;

function TIntelJPEGImage.LoadProcess(filename:string; Buff:pointer; BuffSize:integer):integer;
var
  jprop: TJPEG_CORE_PROPERTIES;
begin
  Chkijl(ijlinit(@jprop));
  try
    // jpeg 기초정보 구하기
    if filename='' then begin
      jprop.JPGBytes:=Buff;
      jprop.JPGSizeBytes:=BuffSize;
      Chkijl(ijlread(@jprop,IJL_JBUFF_READPARAMS));
    end else begin
      jprop.JPGFile:=PChar(filename);
      Chkijl(ijlread(@jprop,IJL_JFILE_READPARAMS));
    end;
    jprop.DIBChannels:=3;
    jprop.DIBColor:=IJL_BGR;
    jprop.DIBWidth:=jprop.JPGWidth;
    jprop.DIBHeight:=-integer(jprop.JPGHeight);
    ReadProp(jprop);

    // 비트맵 설정
    Width := 0;
    PixelFormat := pf24bit;
    Width := jprop.DIBWidth;
    Height := -Integer(jprop.DIBHeight);
    jprop.DIBBytes := ScanLine[Height-1];
    jprop.DIBPadBytes := GetPadBytes(Width, jprop.DIBChannels);

    // 이미지 읽기
    if filename = '' then
    begin
      Chkijl(ijlRead(@jprop,IJL_JBUFF_READWHOLEIMAGE));
    end else
    begin
      Chkijl(ijlRead(@jprop,IJL_JFILE_READWHOLEIMAGE));
    end;

    Result := jprop.jprops.state.entropy_bytes_processed;
    Modified := True;
  finally
    ijlfree(@jprop);
  end;
end;

procedure TIntelJPEGImage.LoadFromStream(Stream: TStream);
var
  Buff:pointer;
  SavePos, BuffSize, ReadSize: Integer;
begin
  SavePos :=Stream.Position;
  BuffSize := Stream.Size - SavePos;
  if Stream is TCustomMemoryStream then
    with TCustomMemoryStream(Stream) do
       ReadSize:=LoadProcess('',pointer(integer(Memory)+Position),BuffSize)
  else begin
     GetMem(Buff,BuffSize);
     try
       Stream.ReadBuffer(Buff^,BuffSize);
       ReadSize:=LoadProcess('',Buff,BuffSize);
     finally
       FreeMem(Buff,BuffSize);
     end;
  end;
  Stream.Position:=SavePos + ReadSize;
end;

procedure TIntelJPEGImage.LoadFromFile(const Filename: string);
begin
  LoadProcess(Filename,nil,0);
end;

function TIntelJPEGImage.SaveProcess(filename: string; Buff: pointer; BuffSize: integer): integer;
var
  jprop: TJPEG_CORE_PROPERTIES;
begin
  if PixelFormat <> pf24bit then
    raise Eijlerror.Create('24bit 이미지만 JPEG로 변환할수 있습니다.');

  Chkijl(ijlinit(@jprop));
  try
    jprop.DIBBytes := ScanLine[Height-1];
    jprop.DIBWidth := Width;
    jprop.DIBHeight := -Integer(Height);
    jprop.DIBChannels := 3;
    jprop.DIBColor := IJL_BGR;
    jprop.DIBPadBytes := GetPadBytes(Width,jprop.DIBChannels);
    jprop.JPGWidth := Width;
    jprop.JPGHeight := Height;
    jprop.JPGChannels := 3;
    jprop.JPGColor := IJL_YCBCR;
    jprop.jquality := Quality;
    jprop.jprops.jpeg_comment := PChar(Comment);
    jprop.jprops.jpeg_comment_Size := Length(Comment);
    Writeprop(jprop);
    if filename = '' then
    begin
      jprop.JPGBytes := Buff;
      jprop.JPGSizeBytes := BuffSize;
      Chkijl(ijlwrite(@jprop, IJL_JBUFF_WRITEWHOLEIMAGE));
    end else
    begin
      jprop.JPGFile := PChar(filename);
      Chkijl(ijlwrite(@jprop, IJL_JFILE_WRITEWHOLEIMAGE));
    end;

    Result := jprop.JPGSizeBytes;
  finally
    ijlfree(@jprop);
  end;
end;

procedure TIntelJPEGImage.SaveToStream(Stream: TStream);
var
  Buff: Pointer;
  BuffSize, JPGSize: Integer;
begin
  BuffSize := Width * Height + 1024;
  GetMem(Buff, BuffSize);
  try
    JPGSize := SaveProcess('', Buff, BuffSize);
    Stream.WriteBuffer(Buff^, JPGSize);
  finally
    FreeMem(Buff,BuffSize);
  end;
end;

procedure TIntelJPEGImage.SaveToFile(const Filename: string);
begin
  SaveProcess(FileName, nil, 0);
end;

procedure GetJPEGSize(filename: string; var Width, Height: Integer);
var
  jprop: TJPEG_CORE_PROPERTIES;
begin
  Chkijl(ijlinit(@jprop));
  try
    jprop.JPGFile:=PChar(filename);
    Chkijl(ijlread(@jprop,IJL_JFILE_READPARAMS));
    Width:=jprop.JPGWidth;
    Height:=jprop.JPGHeight;
  finally
    ijlfree(@jprop);
  end;
end;

procedure TIntelJPEGImage.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TIntelJPEGImage.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;


initialization
  // 프로세서(CPU) 종류를 구하여, 설정한다.
  SetProcesserType(GetProcesserType);

  // TPicture에 파일 포맷을 등록한다.
  TPicture.RegisterFileFormat('jpg' , 'JPEG Image File (*.jpg)' , TIntelJPEGImage);
  TPicture.RegisterFileFormat('jpeg', 'JPEG Image File (*.jpeg)', TIntelJPEGImage);
finalization
  TPicture.UnregisterGraphicClass(TIntelJPEGImage);

end.
