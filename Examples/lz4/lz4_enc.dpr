program lz4_enc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  lz4d,
  lz4d.lz4,
  lz4d.lz4s,
  System.SysUtils,
  Classes;

var
  msData : TMemoryStream;
  pBufferOut : pointer;
  iBuffferSize, iSizeOut : integer;
  fsIn, fsOut : TFileStream;

begin
  msData := TMemoryStream.Create;
  fsIn := TFileStream.Create(ParamStr(1), fmOpenRead);

  msData.CopyFrom(fsIn, fsIn.Size);

  iBuffferSize := LZ4_compressBound(fsIn.Size);
  GetMem(pBufferOut, msData.Size);

  iSizeOut := TLZ4.Stream_Encode( msData.Memory, pBufferOut, msData.Size, iBuffferSize);

  WriteLn( Format('iSizeOut: %d', [iSizeOut]) );

  fsOut := TFileStream.Create(ParamStr(1) + '.lz4', fmCreate);
  fsOut.Write(pBufferOut^, iSizeOut);

  FreeMem(pBufferOut);
  msData.Free;
  fsIn.Free;
  fsOut.Free;
end.
