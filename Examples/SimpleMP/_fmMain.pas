unit _fmMain;

interface

uses
  DebugTools, SimpleMP, CRC32, Sys,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  BUFFER_SIZE = 1024;

type
  TBufferUnit = array [0..BUFFER_SIZE-1] of byte;

  TfmMain = class(TForm)
    btExecute: TButton;
    btSpeedCheck: TButton;
    moMsg: TMemo;
    procedure btExecuteClick(Sender: TObject);
    procedure btSpeedCheckClick(Sender: TObject);
  private
    Buffers : array [0..Buffer_Size-1] of TBufferUnit;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
var
  iSum, iTemp : integer;
begin
  iSum := 0;

  TSimpleMP.Execute(Self, 100, SystemInfo.dwNumberOfProcessors,
    procedure (Context:pointer; ThreadNo,Index:integer) begin
      iTemp := InterlockedExchangeAdd( iSum, Index + 1 );
      Trace( Format('ThreadNo(%d) - %d: %d', [ThreadNo, Index + 1, iTemp]) );
    end
  );

  moMsg.Lines.Add( Format('1+2+....+100=%d', [iSum]) );
  moMsg.Lines.Add( '' );
end;

procedure TfmMain.btSpeedCheckClick(Sender: TObject);
var
  Loop: Integer;
  iStart, iStop, iTime1, iTime2 : int64;
begin
  QueryPerformanceCounter( iStart );
  for Loop := 0 to BUFFER_SIZE-1 do CRC32Data( @Buffers[Loop], BUFFER_SIZE );
  QueryPerformanceCounter( iStop );

  iTime1 := iStop - iStart;
  moMsg.Lines.Add( Format('Make crc32 without SimpleMP: %d', [iTime1]) );

  QueryPerformanceCounter( iStart );
  TSimpleMP.Execute(Self, BUFFER_SIZE, SystemInfo.dwNumberOfProcessors * 2,
    procedure (Context:pointer; ThreadNo,Index:integer) begin
      CRC32Data( @Buffers[Index], Buffer_Size );
    end
  );
  QueryPerformanceCounter( iStop );

  iTime2 := iStop - iStart;
  moMsg.Lines.Add( Format('Make crc32 with SimpleMP: %d', [iTime2]) );

  moMsg.Lines.Add( Format('SimpleMP is fast %f times.', [iTime1 / iTime2]) );

  moMsg.Lines.Add( '' );
end;

end.
