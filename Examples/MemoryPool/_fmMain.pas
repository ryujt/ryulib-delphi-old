unit _fmMain;

interface

uses
  MemoryPool,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FList : TList;
    FMemoryPool : TBasicMemoryPool;
    procedure do_SystemTest;
    procedure do_MemoryPoolTest;
    procedure do_AV_Check;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.do_SystemTest;
var
  Loop1, Loop2 : Integer;
  Tick : cardinal;
  Data : pointer;
begin
  RandSeed := 0;

  Tick := GetTickCount;

  for Loop2 := 1 to 1000 do begin
    for Loop1 := 1 to 1000 do begin
      GetMem(Data, Random(16 * 1024));
      FList.Add(Data);
    end;

    for Loop1 := 0 to FList.Count-1 do FreeMem(FList[Loop1]);
    FList.Clear;
  end;

  moMsg.Lines.Add(Format('System.GetMem: %d ms', [GetTickCount-Tick]));
end;

procedure TfmMain.do_AV_Check;
const
  MAX_SIZE = 1024;
var
  Data : pointer;
  Loop1, Loop2, Size : Integer;
  Buffer : array [0..MAX_SIZE] of byte;
begin
  RandSeed := 0;

  for Loop2 := 1 to 10000 do begin
    for Loop1 := 1 to 10000 do begin
      Size := Random(MAX_SIZE - 2) + 1;
      Data := FMemoryPool.GetMem( Size );
      Move( Buffer[0], Data^, Size );
    end;
  end;
end;

procedure TfmMain.do_MemoryPoolTest;
var
  Loop1, Loop2, Size : Integer;
  Tick : cardinal;
begin
  RandSeed := 0;

  Tick := GetTickCount;

  for Loop2 := 1 to 1000 do begin
    for Loop1 := 1 to 1000 do begin
      Size := Random(16 * 1024) + 1;
      FMemoryPool.GetMem( Size );
    end;
  end;

  moMsg.Lines.Add(Format('MemoryPool.GetMem: %d ms', [GetTickCount-Tick]));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
  FMemoryPool := TBasicMemoryPool.Create( 1024 * 1024 * 1024 );

  do_SystemTest;
  do_MemoryPoolTest;
  do_AV_Check;
end;

end.
