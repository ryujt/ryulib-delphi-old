program lz4_test;

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
  iSizeOut : integer;
  fsIn, fsOut : TFileStream;

begin
  msData := TMemoryStream.Create;
  fsIn := TFileStream.Create(ParamStr(1), fmOpenRead);

  msData.CopyFrom(fsIn, fsIn.Size);
  msData.Position := 0;

  GetMem(pBufferOut, msData.Size);

  iSizeOut := TLZ4.Stream_Encode( msData.Memory, pBufferOut, msData.Size, msData.Size);

  WriteLn( Format('iSizeOut: %d', [iSizeOut]) );

end.
