unit OfficeTools;

interface

uses
  Windows, SysUtils, Classes, ComObj, ActiveX;

procedure PPT2JPG(ASrc,ADst:string);
procedure PPT2PNG(ASrc,ADst:string);

implementation

const
  ppSaveAsPresentation = $00000001;
  ppSaveAsPowerPoint7 = $00000002;
  ppSaveAsPowerPoint4 = $00000003;
  ppSaveAsPowerPoint3 = $00000004;
  ppSaveAsTemplate = $00000005;
  ppSaveAsRTF = $00000006;
  ppSaveAsShow = $00000007;
  ppSaveAsAddIn = $00000008;
  ppSaveAsWizard = $00000009;
  ppSaveAsPowerPoint4FarEast = $0000000A;
  ppSaveAsDefault = $0000000B;
  ppSaveAsHtml = $0000000C;
  ppSaveAsJPG  = 17;
  ppSaveAsPNG  = 18;

  ppWindowMinimized = 2;

  msoFalse = TOleEnum(False);
  msoTrue = TOleEnum(True);

procedure PPT2JPG(ASrc,ADst:string);
var
  PPT : variant;
begin
  try
    PPT:= GetActiveOleObject('Powerpoint.Application');
  except
    PPT:= CreateOleObject('Powerpoint.Application');
  end;

  PPT.Visible := True;
  PPT.Presentations.open(ASrc);
  PPT.ActivePresentation.SaveAs(ADst, ppSaveAsJPG, false);
  PPT.Quit;
end;

procedure PPT2PNG(ASrc,ADst:string);
var
  PPT : variant;
begin
  try
    PPT:= GetActiveOleObject('Powerpoint.Application');
  except
    PPT:= CreateOleObject('Powerpoint.Application');
  end;

  PPT.Visible := True;
  PPT.Presentations.open(ASrc);
  PPT.ActivePresentation.SaveAs(ADst, ppSaveAsPNG, false);
  PPT.Quit;
end;

initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
end.

기본값인 96dpi보다 높은 해상도의 그림으로 슬라이드를 내보내려면 레지스트리 값을 추가해야 합니다. 다음 단계에 따라 레지스트리 값을 추가하십시오.

경고 레지스트리 편집기를 잘못 사용하면 심각한 문제가 발생할 수 있으며 문제를 해결하기 위해 운영 체제를 다시 설치해야 할 수도 있습니다. Microsoft는 레지스트리 편집기를 잘못 사용하여 발생하는 문제에 대해 해결을 보증하지 않습니다. 레지스트리 편집기 사용에 따른 모든 책임은 사용자에게 있습니다.
1.	Microsoft Windows 프로그램을 종료합니다.
2.	시작을 누르고 실행을 누릅니다.
3.	열기 상자에 regedit를 입력한 다음 확인을 누릅니다.
4.	레지스트리를 확장하여 다음 키를 찾습니다.
HKEY_CURRENT_USER\Software\Microsoft\Office\14.0\PowerPoint\Options
5.	Options 키를 선택한 상태에서 편집 메뉴의 새로 만들기를 가리킨 다음 DWORD 값을 누릅니다.
6.	ExportBitmapResolution을 입력한 다음 Enter 키를 누릅니다.
7.	ExportBitmapResolution을 선택한 상태에서 편집 메뉴의 수정을 누릅니다.
8.	다음 표를 참조하여 값 데이터 상자에 원하는 해상도 값을 입력합니다.

참고 PowerPoint 에서 내보낼 수 있는 최대 해상도 설정은 307dpi입니다.

10진수 값	픽셀(가로 x 세로)	dpi(가로 및 세로)
50	500 x 375	50dpi
96(기본값)	960 x 720	96dpi
100	1000 x 750	100dpi
150	1500 x 1125	150dpi
200	2000 x 1500	200dpi
250	2500 x 1875	250dpi
300	3000 x 2250	300dpi
9.	10진수를 누른 다음 확인을 누릅니다.
10.	파일 메뉴에서 끝내기를 눌러 레지스트리 편집기를 종료합니다.

{
Constant Value
ppSaveAsAddIn  8
ppSaveAsBMP  19
ppSaveAsDefault  11
ppSaveAsEMF  23
ppSaveAsGIF  16
ppSaveAsHTML  12
ppSaveAsHTMLDual  14
ppSaveAsHTMLv3  13
ppSaveAsJPG  17
ppSaveAsMetaFile  15
ppSaveAsPNG  18
ppSaveAsPowerPoint3  4
ppSaveAsPowerPoint4  3
ppSaveAsPowerPoint4FarEast  10
ppSaveAsPowerPoint7  2
ppSaveAsPresentation  1
ppSaveAsPresForReview  22
ppSaveAsRTF  6
ppSaveAsShow  7
ppSaveAsTemplate  5
ppSaveAsTIF  21
ppSaveAsWebArchive  20

Constant Value
ppWindowMaximized  3
ppWindowMinimized  2
ppWindowNormal  1
//}

