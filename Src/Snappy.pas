unit Snappy;

interface

uses
  Windows, SysUtils, Classes;

type
  TSnappyStatus = (
    SNAPPY_OK = 0,
    SNAPPY_INVALID_INPUT = 1,
    SNAPPY_BUFFER_TOO_SMALL = 2
  );

{*
   주어진 데이터를 압축한다.
   @param input 압축 할 데이터의 포인터
   @param input_length 압축 할 데이터의 크기
   @param compressed 압축 결과 데이터를 저장 할 버퍼의 포인터
   @param output_length 압축 결과를 저장 할 버퍼의 크기이다.  압축 후에는 압축 된 데이터의 크기가 저장 된다.

   - Example:
     output_length := max_compressed_length(input_length);
     GetMem( output, output_length );
     if compress(input, input_length, output, output_length) <> SNAPPY_OK then
       raise Exception.Create('Error');
     ...
     FreeMem( output );
}
function compress(input:pointer; input_length:DWord; compressed:pointer; var compressed_length:DWord):TSnappyStatus; cdecl;
         external 'snappy.dll' delayed;

{*
   압축 된 데이터의 압축을 해제한다.
   @param compressed 압축 된 데이터의 포인터
   @param compressed_length 압출 된 데이터의 크기
   @param uncompressed 압출 해제 결과 데이터를 저장 할 버퍼의 포인터
   @param uncompressed_length 압출 해제 결과 데이터를 저장 할 버퍼의 크기

   - Example:
    if uncompressed_length(compressed, compressed_length, uncompressed_length) <> SNAPPY_OK then
       raise Exception.Create('Error');
    GetMem( uncompressed, uncompressed_length);
    uncompress(compressed, compressed_length, uncompressed, uncompressed_length) <> SNAPPY_OK then
       raise Exception.Create('Error');
     ...
     FreeMem( uncompressed );
}
function uncompress(compressed:pointer; compressed_length:DWord; uncompressed:pointer; var uncompressed_length:DWord):TSnappyStatus; cdecl;
         external 'snappy.dll' delayed;

function max_compressed_length(source_length:DWord):DWord; cdecl;
         external 'snappy.dll' delayed;

function uncompressed_length(compressed:pointer; compressed_length:DWord; var result_length:DWord):TSnappyStatus; cdecl;
         external 'snappy.dll' delayed;

function  validate_compressed_buffer(compressed:pointer; compressed_length:DWord):TSnappyStatus; cdecl;
         external 'snappy.dll' delayed;

implementation

end.
