unit ZLibUtils;

interface

uses
  SysUtils, Classes, ZLIBEX;

const
  BufferSize = 1024 * 16;

procedure ShrinkData(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer; Speed:TZCompressionLevel);
procedure ShrinkDataToBuffer(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer; Speed:TZCompressionLevel);

procedure ShrinkDataSlow(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer);
procedure ShrinkDataToBufferSlow(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer);
procedure ShrinkDataToBufferFast(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer);

procedure ShrinkDataFast(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer);

procedure ExpandData(DataIn:pointer; SizeIn:integer; var DataOut:pointer;
          var SizeOut:integer);
procedure ExpandDataToBuffer(DataIn:pointer; SizeIn:integer; DataOut:pointer;
          var SizeOut:integer);

procedure ShrinkStream(StreamSrc,StreamDst:TStream; Speed:TZCompressionLevel);
procedure ShrinkStreamSlow(StreamSrc,StreamDst:TStream);
procedure ShrinkStreamNormal(StreamSrc,StreamDst:TStream);
procedure ShrinkStreamFast(StreamSrc,StreamDst:TStream);
procedure ExpandStream(StreamSrc,StreamDst:TStream);

procedure ShrinkFileToStream(FileName:string; Dst:TStream);
procedure ExpandToFile(Src:TStream; FileName:string);

procedure ShrinkFile(ASrc,ADst:string);
procedure ExpandFile(ASrc,ADst:string);

implementation

procedure ShrinkData(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer; Speed:TZCompressionLevel);
var
  StreamIn, StreamOut : TMemoryStream;
begin
  StreamIn:= TMemoryStream.Create;
  StreamOut:= TMemoryStream.Create;
  try
    StreamIn.Write(DataIn^, SizeIn);
    StreamIn.Position:= 0;

    ShrinkStream(StreamIn, StreamOut, Speed);

    SizeOut:= StreamOut.Size;
    if SizeOut > 0 then begin
      GetMem(DataOut, SizeOut);
      StreamOut.Read(DataOut^, SizeOut);
    end else begin
      DataOut := nil;
      SizeOut := 0;
    end;
  finally
    StreamIn.Free;
    StreamOut.Free;
  end;
end;

procedure ShrinkDataToBuffer(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer; Speed:TZCompressionLevel);
var
  StreamIn, StreamOut : TMemoryStream;
begin
  StreamIn := TMemoryStream.Create;
  StreamOut := TMemoryStream.Create;
  try
    StreamIn.Write(DataIn^, SizeIn);
    StreamIn.Position:= 0;

    ShrinkStream(StreamIn, StreamOut, Speed);

    SizeOut := StreamOut.Size;
    if SizeOut > 0 then StreamOut.Read(DataOut^, SizeOut);
  finally
    StreamIn.Free;
    StreamOut.Free;
  end;
end;

procedure ShrinkDataSlow(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer);
begin
  ShrinkData(DataIn, SizeIn, DataOut, SizeOut, zcMax);
end;

procedure ShrinkDataToBufferSlow(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer);
begin
  ShrinkDataToBuffer(DataIn, SizeIn, DataOut, SizeOut, zcMax);
end;

procedure ShrinkDataToBufferFast(DataIn:pointer; SizeIn:integer;
          DataOut:pointer; var SizeOut:integer);
begin
  ShrinkDataToBuffer(DataIn, SizeIn, DataOut, SizeOut, zcDefault);
end;

procedure ShrinkDataFast(DataIn:pointer; SizeIn:integer;
          var DataOut:pointer; var SizeOut:integer);
begin
  ShrinkData(DataIn, SizeIn, DataOut, SizeOut, zcFastest);
end;

procedure ExpandData(DataIn:pointer; SizeIn:integer; var DataOut:pointer;
          var SizeOut:integer);
var
  msIn, msOut : TMemoryStream;
begin
  msIn:= TMemoryStream.Create;
  msOut:= TMemoryStream.Create;
  try
    msIn.Write(DataIn^, SizeIn);
    msIn.Position:= 0;
    ExpandStream(msIn, msOut);

    SizeOut:= msOut.Size;
    if SizeOut > 0 then begin
      GetMem(DataOut, SizeOut);
      msOut.Read(DataOut^, SizeOut);
    end else
      DataOut:= nil;
  finally
    msIn.Free;
    msOut.Free;
  end;
end;

procedure ExpandDataToBuffer(DataIn:pointer; SizeIn:integer; DataOut:pointer;
          var SizeOut:integer);
var
  msIn, msOut : TMemoryStream;
begin
  msIn:= TMemoryStream.Create;
  msOut:= TMemoryStream.Create;
  try
    msIn.Write(DataIn^, SizeIn);
    msIn.Position:= 0;
    ExpandStream(msIn, msOut);

    SizeOut:= msOut.Size;
    if SizeOut > 0 then msOut.Read(DataOut^, SizeOut);
  finally
    msIn.Free;
    msOut.Free;
  end;
end;

procedure ShrinkStream(StreamSrc,StreamDst:TStream; Speed:TZCompressionLevel);
var
  ZStream : TZCompressionStream;
begin
  ZStream:= TZCompressionStream.Create(StreamDst, Speed);
  try
    ZStream.CopyFrom(StreamSrc, StreamSrc.Size);
  finally
    ZStream.Free;
  end;
  StreamDst.Position:= 0;
end;

procedure ShrinkStreamSlow(StreamSrc,StreamDst:TStream);
begin
  ShrinkStream(StreamSrc, StreamDst, zcMax);
end;

procedure ShrinkStreamNormal(StreamSrc,StreamDst:TStream);
begin
  ShrinkStream(StreamSrc, StreamDst, zcDefault);
end;

procedure ShrinkStreamFast(StreamSrc,StreamDst:TStream);
begin
  ShrinkStream(StreamSrc, StreamDst, zcFastest);
end;

procedure ExpandStream(StreamSrc,StreamDst:TStream);
var
  iSize : Integer;
  ZStream : TZDecompressionStream;
  Buffer : Packed Array [0..BufferSize-1] of Byte;
begin
  ZStream:= TZDecompressionStream.Create(StreamSrc);
  try
    iSize:= ZStream.Read(Buffer, BufferSize);
    while iSize > 0 do begin
      StreamDst.Write(Buffer, iSize);
      iSize:= ZStream.Read(Buffer, BufferSize);
    end;
    StreamDst.Position:= 0;
  finally
    ZStream.Free;
  end;
end;

procedure ShrinkFileToStream(FileName:string; Dst:TStream);
var
  fsData : TFileStream;
begin
  fsData := TFileStream.Create(FileName, fmOpenRead);
  try
    ShrinkStreamSlow(fsData, Dst);
  finally
    fsData.Free;
  end;
end;

procedure ExpandToFile(Src:TStream; FileName:string);
var
  fsData : TFileStream;
begin
  fsData := TFileStream.Create(FileName, fmCreate);
  try
    ExpandStream(Src, fsData);
  finally
    fsData.Free;
  end;
end;

procedure ShrinkFile(ASrc,ADst:string);
var
  fsSrc, fsDst : TFileStream;
begin
  fsSrc := TFileStream.Create(ASrc, fmOpenRead);
  fsDst := TFileStream.Create(ADst, fmCreate);
  try
    ShrinkStreamSlow(fsSrc, fsDst);
  finally
    fsSrc.Free;
    fsDst.Free;
  end;
end;

procedure ExpandFile(ASrc,ADst:string);
var
  fsSrc, fsDst : TFileStream;
begin
  fsSrc := TFileStream.Create(ASrc, fmOpenRead);
  fsDst := TFileStream.Create(ADst, fmCreate);
  try
    ExpandStream(fsSrc, fsDst);
  finally
    fsSrc.Free;
    fsDst.Free;
  end;
end;

end.
