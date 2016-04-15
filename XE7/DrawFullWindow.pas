{*
  윈도우 창들이 드래그될 때, 전체 화면을 모두 그리느냐 아니면 외곽만 그리느냐?
  프로그램 시작 할 때, Disable을 하고, 프로그램 종료 시에 Restore하면,
  프로그램 시작 이후는 외곽만 그리다가, 종료 이후에는 원래의 사용자 옵션으로 되돌아 간다.
}
unit DrawFullWindow;

interface

uses
  Windows, SysUtils, Classes;

procedure EnableDrawFullWindow;
procedure DisableDrawFullWindow;
procedure RestoreDrawFullWindow;

implementation

var
  OldState : LongBool = false;

procedure EnableDrawFullWindow;
begin
  SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, Ord(true), nil, 0);
end;

procedure DisableDrawFullWindow;
begin
  SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, Ord(false), nil, 0);
end;

procedure RestoreDrawFullWindow;
begin
  SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, Ord(OldState), nil, 0);
end;

initialization
  SystemParametersInfo( SPI_GETDRAGFULLWINDOWS, 0, @OldState, 0 );
finalization
  RestoreDrawFullWindow;
end.
