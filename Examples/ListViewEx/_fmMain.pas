unit _fmMain;

interface

uses
  ListViewEx,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TListView = class (TListViewEx);

  TfmMain = class(TForm)
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
  private
    procedure on_ListView_ColumnResize(Sender:TObject; AColumnIndex,AColumnWidth:Integer);
  public
    procedure Add(ACaption:string);
    procedure Remove(AListItem:TListItem);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Add(ACaption: string);
const
  pbColumnIndex = 1;
  pbMax = 100;
var
  ListItem : TListItem;
  pbRect : TRect;
  ProgressBar : TProgressBar;
begin
  ListItem := ListView.Items.Add;
  ListItem.Caption := ACaption;

  ProgressBar := TProgressBar.Create(nil);
  ProgressBar.Parent := ListView;

  pbRect := ListItem.DisplayRect(drBounds);
  pbRect.Left  := pbRect.Left + ListView.Columns[-1 + pbColumnIndex].Width;
  pbRect.Right := pbRect.Left + ListView.Columns[pbColumnIndex].Width;
  ProgressBar.BoundsRect := pbRect;

  ListItem.Data := ProgressBar;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ListView.OnColumnResize := on_ListView_ColumnResize;

  Add( 'Test A');
  Add( 'Test B');
  Add( 'Test C');
end;

procedure TfmMain.ListViewDblClick(Sender: TObject);
begin
  Remove( ListView.Selected );
end;

procedure TfmMain.on_ListView_ColumnResize(Sender: TObject;
  AColumnIndex, AColumnWidth: Integer);
var
  Loop : integer;
  pb : TProgressBar;
  lv : TListViewEx absolute Sender;
begin
  if AColumnIndex = 0 then begin
    for Loop := 0 to -1 + lv.Items.Count do begin
      pb := TProgressBar(lv.Items[Loop].Data);
      pb.Left := AColumnWidth;
    end;
  end;

  if AColumnIndex = 1 then begin
    for Loop := 0 to -1 + lv.Items.Count do begin
      pb := TProgressBar(lv.Items[Loop].Data);
      pb.Width := AColumnWidth;
    end;
  end;
end;

procedure TfmMain.Remove(AListItem: TListItem);
var
  pb : TProgressBar;
  ListItem: TListItem;
  Loop, iIndex : integer;
begin
  if AListItem = nil then Exit;

  iIndex := AListItem.Index;
  ListView.Items.Delete(iIndex);

  TObject(AListItem.Data).Free;

  for Loop := iIndex to -1 + ListView.Items.Count do begin
    ListItem := ListView.Items.Item[Loop];

    pb := TProgressBar(ListItem.Data);
    pb.Top := pb.Top - (pb.BoundsRect.Bottom - pb.BoundsRect.Top);
  end;
end;

end.
