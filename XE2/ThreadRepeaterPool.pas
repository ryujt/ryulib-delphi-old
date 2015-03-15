unit ThreadRepeaterPool;

interface

uses
  Classes, SysUtils, ThreadRepeater;

type
  TThreadRepeaterPool = class (TComponent)
  private
    FList : TList;
    procedure create_Pool;
    procedure release_Pool;
    procedure on_Repeat(Sender:TObject);
  private
    FTimeOut: integer;
    FSize: integer;
    FOnRepeat: TNotifyEvent;
    procedure SetTimeOut(const Value: integer);
  public
    constructor Create(AOwner: TComponent; ASize:integer); reintroduce; virtual;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  published
    property Size : integer read FSize; 
    property TimeOut : integer read FTimeOut write SetTimeOut;
    property OnRepeat : TNotifyEvent read FOnRepeat write FOnRepeat; 
  end;

implementation

{ TThreadRepeaterPool }

constructor TThreadRepeaterPool.Create(AOwner: TComponent; ASize:integer);
begin
  inherited Create(AOwner);

  FSize := ASize;

  FList := TList.Create;

  create_Pool;
end;

procedure TThreadRepeaterPool.create_Pool;
var
  Loop: Integer;
begin
  for Loop := 1 to FSize do FList.Add(TThreadRepeater.Create(Self));
end;

destructor TThreadRepeaterPool.Destroy;
begin
  Stop;

  release_Pool;

  FreeAndNil(FList);

  inherited;
end;

procedure TThreadRepeaterPool.on_Repeat(Sender: TObject);
begin
  if Assigned(FOnRepeat) then FOnRepeat(Self);  
end;

procedure TThreadRepeaterPool.release_Pool;
var
  Loop: Integer;
  Repeater : TThreadRepeater;
begin
  for Loop := 0 to FList.Count-1 do begin
    Repeater := Pointer(FList[Loop]);
    Repeater.Free;
  end;

  FList.Clear;
end;

procedure TThreadRepeaterPool.SetTimeOut(const Value: integer);
var
  Loop: Integer;
  Repeater : TThreadRepeater;
begin
  FTimeOut := Value;

  for Loop := 0 to FList.Count-1 do begin
    Repeater := Pointer(FList[Loop]);
    Repeater.TimeOut := Value;
  end;
end;

procedure TThreadRepeaterPool.Start;
var
  Loop: Integer;
  Repeater : TThreadRepeater;
begin
  for Loop := 0 to FList.Count-1 do begin
    Repeater := Pointer(FList[Loop]);
    Repeater.Execute(on_Repeat);
  end;
end;

procedure TThreadRepeaterPool.Stop;
var
  Loop: Integer;
  Repeater : TThreadRepeater;
begin
  for Loop := 0 to FList.Count-1 do begin
    Repeater := Pointer(FList[Loop]);
    Repeater.Stop;
  end;
end;

end.
