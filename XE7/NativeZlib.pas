unit NativeZlib;

interface

uses
  ZLib,
  Windows, SysUtils, Classes;

procedure ExpandStream(StreamSrc,StreamDst:TStream);
procedure ExpandFile(ASrc,ADst:string);

implementation

const
  BufferSize = 1024 * 16;

procedure ExpandStream(StreamSrc,StreamDst:TStream);
var
  iSize : Integer;
  ZStream : TDecompressionStream;
  Buffer : Packed Array [0..BufferSize-1] of Byte;
begin
  ZStream:= TDecompressionStream.Create(StreamSrc);
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
