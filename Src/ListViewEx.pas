unit ListViewEx;

(*

ListView with ColumnResize events

Code found on news:borland.* provided by Peter Below (a TeamB member)

Placing a TProgressBar into a TListView

http://delphi.about.com/library/weekly/aa053105a.htm

Here's how to add a progress bar (or any other Delphi
component) to a ListView control. Plus: full source code
to the TListViewEx component (TListView descendant) with
ColumnResize events!


~ Zarko Gajic
http://delphi.about.com


*)


interface

uses
  SysUtils, Classes, Controls, ComCtrls, CommCtrl, Messages, Windows;

type
  TColumnResizeEvent = procedure (Sender:TObject; AColumnIndex,AColumnWidth:Integer) of object;

  TListViewEx = class(TListView)
  private
    FColumnResizeEvent: TColumnResizeEvent;
    FEndColumnResizeEvent: TColumnResizeEvent;
    FBeginColumnResizeEvent: TColumnResizeEvent;
  protected
    procedure DoBeginColumnResize(columnIndex, columnWidth: integer); virtual;
    procedure DoEndColumnResize(columnindex, columnwidth: integer); virtual;
    procedure DoColumnResize(columnindex, columnwidth: integer); virtual;

    procedure WMNotify(var msg: TWMNotify); message WM_NOTIFY;

    function FindColumnIndex(pHeader: pNMHdr): integer;
    function FindColumnWidth(pHeader: pNMHdr): integer;
    procedure CreateWnd; override;
  public
  published
    property OnBeginColumnResize: TColumnResizeEvent read FBeginColumnResizeEvent write FBeginColumnResizeEvent;
    property OnEndColumnResize: TColumnResizeEvent read FEndColumnResizeEvent write FEndColumnResizeEvent;
    property OnColumnResize: TColumnResizeEvent read FColumnResizeEvent write FColumnResizeEvent;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TListViewEx]);
end;

{ TListViewEx }

procedure TListViewEx.CreateWnd;
var
  wnd: HWND;
begin
  inherited;

  wnd := GetWindow(handle, GW_CHILD);
  SetWindowLong(wnd, GWL_STYLE, GetWindowLong(wnd, GWL_STYLE) and not HDS_FULLDRAG);
end;

procedure TListViewEx.DoBeginColumnResize(columnIndex, columnWidth: integer);
begin
  if Assigned(FBeginColumnResizeEvent ) then FBeginColumnResizeEvent(self, columnIndex, columnWidth);
end;

procedure TListViewEx.DoColumnResize(columnindex, columnwidth: integer);
begin
  if Assigned(FColumnResizeEvent ) then FColumnResizeEvent(self, columnIndex, columnWidth);
  Invalidate;
end;

procedure TListViewEx.DoEndColumnResize(columnindex, columnwidth: integer);
begin
  if Assigned(FEndColumnResizeEvent) then FEndColumnResizeEvent(self, columnIndex, columnWidth);
  Invalidate;
end;

function TListViewEx.FindColumnIndex(pHeader: pNMHdr): integer;
var
  hwndHeader: HWND;
  iteminfo: THdItem;
  itemindex: Integer;
  buf: array [0..128] of Char;
begin
  Result := -1;
  hwndHeader := pHeader^.hwndFrom;
  itemindex  := pHDNotify(pHeader)^.Item;
  FillChar(iteminfo, sizeof(iteminfo), 0);

  iteminfo.Mask := HDI_TEXT;
  iteminfo.pszText := buf;
  iteminfo.cchTextMax := sizeof(buf)-1;

  Header_GetItem( hwndHeader, itemindex, iteminfo );

  if CompareStr(Columns[itemindex].Caption, iteminfo.pszText ) = 0 then
    Result := itemindex
  else
  begin
    for itemindex := 0 to Columns.count-1 Do
     If CompareStr(Columns[itemindex].Caption, iteminfo.pszText ) = 0 then
     begin
       Result := itemindex;
       Break;
     end;
  end;
end; //FindColumnIndex

function TListViewEx.FindColumnWidth(pHeader: pNMHdr): integer;
begin
  Result := -1;
  if Assigned(PHDNotify( pHeader )^.pItem ) aND ((PHDNotify( pHeader )^.pItem^.mask and HDI_WIDTH) <> 0) then
    Result := PHDNotify( pHeader )^.pItem^.cxy;
end;

procedure TListViewEx.WMNotify(var msg: TWMNotify);
begin
  inherited;
  case msg.NMHdr^.code Of
    HDN_ENDTRACK:
      DoEndColumnResize(FindColumnIndex(msg.NMHdr), FindColumnWidth(msg.NMHdr));
    HDN_BEGINTRACK:
      DoBeginColumnResize(FindColumnIndex(msg.NMHdr), FindColumnWidth(msg.NMHdr));
    HDN_TRACK:
      DoColumnResize(FindColumnIndex(msg.NMHdr), FindColumnWidth(msg.NMHdr));
  end;
end;

end.
