unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    ListBox1: TListBox;
    btStart: TButton;
    Timer1: TTimer;
    procedure btStartClick(Sender: TObject);
  private
    procedure do_WM_User(var Msg:TMessage); message WM_User;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  ThreadPool;

{$R *.dfm}

function InternalThreadFunction(lpThreadParameter: Pointer): Integer; stdcall;
begin
  Result := 0;

//  PostMessage(fmMain.Handle, WM_USER, Integer(lpThreadParameter), 0);

//  Sleep(1000 * 2);

//  PostMessage(fmMain.Handle, WM_USER, Integer(lpThreadParameter), 1);
end;

procedure TfmMain.btStartClick(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 1 to 32 do QueueWorkItem( InternalThreadFunction, Pointer(Loop), 32 );
end;

procedure TfmMain.do_WM_User(var Msg: TMessage);
begin
//  ListBox1.Items.Add( Format('Thread No = %d, status = %d', [Msg.WParam, Msg.LParam]) );
end;

end.
