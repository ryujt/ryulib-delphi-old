unit RichEditPlus;

interface

uses
  DebugTools, RyuLibBase, RichEdit,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ComCtrls;

type
  TRichEditPlus = class (TRichEdit)
  private
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  private
    FOnURLClick: TStringEvent;
    FOnScroll: TNotifyEvent;
  protected
    procedure CreateWnd; override;
  public
  published
    property OnURLClick : TStringEvent read FOnURLClick write FOnURLClick;
    property OnScroll : TNotifyEvent read FOnScroll write FOnScroll;
  end;

implementation

{ TRichEditPlus }

procedure TRichEditPlus.CNNotify(var Msg: TWMNotify);
var
  p: TENLink;
  sURL: string;
begin
  if (Msg.NMHdr^.code <> EN_LINK) then Exit;

  p := TENLink(Pointer(Msg.NMHdr)^);
  if (p.Msg <> WM_LBUTTONDOWN) then Exit;

  try
    SendMessage(Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
    sURL := SelText;
    if Assigned(FOnURLClick) then OnURLClick(Self, sURL);
  except
    Trace('TRichEditPlus.CNNotify: ');
  end;
end;

procedure TRichEditPlus.CreateWnd;
var
  mask: Word;
begin
  inherited;

  SendMessage(Handle, EM_AUTOURLDETECT,1, 0);
  mask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
end;

procedure TRichEditPlus.WMVScroll(var Message: TWMVScroll);
begin
  inherited;

  if Assigned(FOnScroll) then FOnScroll(Self);
end;

end.
