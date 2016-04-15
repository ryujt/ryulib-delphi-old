/// 윈도우즈 애니메이션 동작을 멈추게 하거나 복구한다.
unit AnimationControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes;

procedure DisableAnimation;
procedure EnableAnimation;
procedure RestoreAnimation;

implementation

var
  OldAnimationInfo : ANIMATIONINFO;

procedure DisableAnimation;
var
  AniInfo : ANIMATIONINFO;
begin
  AniInfo.cbSize := SizeOf(ANIMATIONINFO);
  AniInfo.iMinAnimate := 0;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(ANIMATIONINFO), @AniInfo, 0);
end;

procedure EnableAnimation;
var
  AniInfo : ANIMATIONINFO;
begin
  AniInfo.cbSize := SizeOf(ANIMATIONINFO);
  AniInfo.iMinAnimate := 1;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(ANIMATIONINFO), @AniInfo, 0);
end;

procedure RestoreAnimation;
begin
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(ANIMATIONINFO), @OldAnimationInfo, 0);
end;

initialization
  OldAnimationInfo.cbSize := SizeOf(ANIMATIONINFO);
  SystemParametersInfo(SPI_GETANIMATION, SizeOf(ANIMATIONINFO), @OldAnimationInfo, 0);
finalization
  RestoreAnimation;
end.
