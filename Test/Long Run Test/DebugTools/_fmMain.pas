{*
  DebugTools 유닛에서 발생한 에러는 아닐 것이라 생각되지만,
  연관 된 프로세스에서 에러가 발생한 것으로 의심되어 롱런 테스트를 작성했다.
}
unit _fmMain;

interface

uses
  DebugTools, SimpleThread, RyuLibBase,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 1 to 32 do begin
    TSimpleThread.Create(
      '', nil,
      procedure (ASimpleThread:TSimpleThread)
      begin
        while true do begin
          if TraceCount < 1024 then Trace( 'TfmMain.FormCreate' );
          Sleep(5);
        end;
      end
    );
  end;
end;

end.
