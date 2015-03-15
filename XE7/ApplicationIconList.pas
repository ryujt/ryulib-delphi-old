unit ApplicationIconList;

interface

uses
  HandleComponent, BTree,
  Windows, Classes, SysUtils, Graphics, ShellApi;

type
  TApplicationIconList = class (THandleComponent)
  private
    FBTree : TBTree;
    FBackgroundColor: TColor;
    function do_Search(AFileName:string):TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure GetBitmap(AFileName:string; ABitmap:TBitmap); overload;
    function GetBitmap(AFileName:string):TBitmap; overload;
  published
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
  end;

implementation

type
  TApplicationIcon = class (TBTreeItem)
  private
  protected
    function Compare(const ACompareTo:TBTreeItem):integer; override;
    procedure Copy(const ACopyTo:TBTreeItem); override;
  public
    FileName : string;
    Bitmap : TBitmap;

    constructor Create;
    destructor Destroy; override;
  end;

{ TApplicationIcon }

function TApplicationIcon.Compare(const ACompareTo: TBTreeItem): integer;
begin
  if Self.FileName > TApplicationIcon(ACompareTo).FileName then Result := 1
  else if Self.FileName < TApplicationIcon(ACompareTo).FileName then Result := -1
  else Result := 0;
end;

procedure TApplicationIcon.Copy(const ACopyTo: TBTreeItem);
begin
  TApplicationIcon(ACopyTo).FileName := Self.FileName;
  TApplicationIcon(ACopyTo).Bitmap.Assign(Self.Bitmap);
end;

constructor TApplicationIcon.Create;
begin
  inherited;

  Bitmap := TBitmap.Create;
end;

destructor TApplicationIcon.Destroy;
begin
  FreeAndNil(Bitmap);

  inherited;
end;

{ TApplicationIconList }

procedure TApplicationIconList.Clear;
begin
  FBTree.Clear;
end;

constructor TApplicationIconList.Create(AOwner: TComponent);
begin
  inherited;

  FBackgroundColor := clWhite;
  FBTree := TBTree.Create;
end;

destructor TApplicationIconList.Destroy;
begin
  Clear;

  FreeAndNil(FBTree);

  inherited;
end;

function TApplicationIconList.do_Search(AFileName: string): TBitmap;
var
  ApplicationIcon, SearchResult : TApplicationIcon;
begin
  Result := nil;

  ApplicationIcon := TApplicationIcon.Create;
  try
    ApplicationIcon.FileName := AFileName;
    if FBTree.Search(ApplicationIcon, TBTreeItem(SearchResult)) then Result := SearchResult.Bitmap;
  finally
    FreeAndNil(ApplicationIcon);
  end;
end;

function TApplicationIconList.GetBitmap(AFileName: string): TBitmap;
var
  Icon: TIcon;
  ApplicationIcon : TApplicationIcon;
begin
  Result := do_Search(AFileName);
  if Result = nil then begin
    Icon := TIcon.Create;
    try
      Icon.Handle := ExtractIcon(Handle, PChar(AFileName), 0);

      ApplicationIcon := TApplicationIcon.Create;
      ApplicationIcon.FileName := AFileName;
      ApplicationIcon.Bitmap.Canvas.Brush.Color := FBackgroundColor;
      ApplicationIcon.Bitmap.Width := Icon.Width;
      ApplicationIcon.Bitmap.Height := Icon.Height;
      ApplicationIcon.Bitmap.Canvas.Draw(0, 0, Icon);
      FBTree.Add(ApplicationIcon);

      Result := ApplicationIcon.Bitmap;
    finally
      FreeAndNil(Icon);
    end;
  end;
end;

procedure TApplicationIconList.GetBitmap(AFileName: string; ABitmap: TBitmap);
var
  Icon: TIcon;
  Bitmap : TBitmap;
  ApplicationIcon : TApplicationIcon;
begin
  Bitmap := do_Search(AFileName);
  if Bitmap <> nil then begin
    ABitmap.Assign(Bitmap);
  end else begin
    Icon := TIcon.Create;
    try
      Icon.Handle := ExtractIcon(Handle, PChar(AFileName), 0);

      ApplicationIcon := TApplicationIcon.Create;
      ApplicationIcon.FileName := AFileName;
      ApplicationIcon.Bitmap.Width := Icon.Width;
      ApplicationIcon.Bitmap.Height := Icon.Height;
      ApplicationIcon.Bitmap.Canvas.Draw(0, 0, Icon);
      FBTree.Add(ApplicationIcon);

      ABitmap.Assign(ApplicationIcon.Bitmap);
    finally
      FreeAndNil(Icon);
    end;
  end;
end;

end.
