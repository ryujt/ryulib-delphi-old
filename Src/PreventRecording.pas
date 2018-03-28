unit PreventRecording;

interface

uses
  ProcessUtils,
  SysUtils, Classes, SyncObjs, ExtCtrls;

type
  TFindRecorderEvent = procedure (Sender:TObject; ARecorderList:TStringList) of object;

  TPreventRecording = class
  private
    FCS : TCriticalSection;
    FRecorderList : TStringList;
    FProcessList : TProcessList;
    FRecorder: TStringList;
    function find_Recorder(AFileName,AClassName:string):integer;
    procedure do_CheckRecorderList;
  private
    FTimer : TTimer;
    procedure on_Timer(Sender:TObject);
  private
    FOnFindRecorder: TFindRecorderEvent;
    function GetInterval: integer;
    procedure SetInterval(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearRecorderList;
    procedure AddRecorder(ACaption,AFileName,AClassName:string);

    procedure Start;
    procedure Stop;
  public
    property Interval : integer read GetInterval write SetInterval;
    property OnFindRecorder : TFindRecorderEvent read FOnFindRecorder write FOnFindRecorder;
  end;

implementation

{ TPreventRecording }

procedure TPreventRecording.AddRecorder(ACaption, AFileName, AClassName: string);
begin
  FCS.Acquire;
  try
    FRecorderList.Add( Format('%s;%s;%s;', [ACaption, AFileName, AClassName]) );
  finally
    FCS.Release;
  end;
end;

procedure TPreventRecording.ClearRecorderList;
begin
  FCS.Acquire;
  try
    FRecorderList.Clear;
  finally
    FCS.Release;
  end;
end;

constructor TPreventRecording.Create;
begin
  inherited;

  FCS := TCriticalSection.Create;
  FRecorderList := TStringList.Create;
  FProcessList := TProcessList.Create;
  FRecorder := TStringList.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := true;
  FTimer.OnTimer := nil;
end;

destructor TPreventRecording.Destroy;
begin
  ClearRecorderList;

  FreeAndNil(FCS);
  FreeAndNil(FRecorderList);
  FreeAndNil(FProcessList);
  FreeAndNil(FRecorder);
  FreeAndNil(FTimer);

  inherited;
end;

function GetString(AText:string; AIndex:integer):string;
var
  StringList : TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.StrictDelimiter := true;
    StringList.Delimiter := ';';
    StringList.DelimitedText := AText;

    Result := StringList[AIndex];
  finally
    StringList.Free;
  end;
end;

procedure TPreventRecording.do_CheckRecorderList;
var
  Loop, iIndex: Integer;
  sCaption, sFileName, sClassName, sPID : string;
begin
  FRecorder.Clear;

  FProcessList.Update;

  for Loop := 0 to FRecorderList.Count-1 do begin
    sCaption   := GetString(FRecorderList[Loop], 0);
    sFileName  := LowerCase( GetString(FRecorderList[Loop], 1) );
    sClassName := LowerCase( GetString(FRecorderList[Loop], 2) );

    iIndex := find_Recorder(sFileName, sClassName);

    if iIndex >= 0 then begin
      sPID := IntToStr(FProcessList.Items[iIndex].th32ProcessID);
      FRecorder.Values[ sPID ] := sCaption;
    end;
  end;

  if Assigned(FOnFindRecorder) then FOnFindRecorder(Self, FRecorder);
end;

function TPreventRecording.find_Recorder(AFileName,
  AClassName: string): integer;
var
  Loop: Integer;
  isCondition : boolean;
  sFileName, sClassName : string;
begin
  Result := -1;

  for Loop := 0 to FProcessList.Count-1 do begin
    sFileName  := LowerCase( FProcessList.Names[Loop] );
    sClassName := LowerCase( FProcessList.WindowClassName[Loop] );

    isCondition := true;

    if AFileName <> '' then isCondition := AFileName = sFileName;

    if isCondition then
      if AClassName <> '' then isCondition := AClassName = sClassName;

    if isCondition then begin
      Result := Loop;
      Break;
    end;
  end;
end;

function TPreventRecording.GetInterval: integer;
begin
  Result := FTimer.Interval;
end;

procedure TPreventRecording.on_Timer(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    FCS.Acquire;
    try
      do_CheckRecorderList;
    finally
      FCS.Release;
    end;
  finally
    FTimer.Enabled := true;
  end;
end;

procedure TPreventRecording.SetInterval(const Value: integer);
begin
  FTimer.Interval := Value;
end;

procedure TPreventRecording.Start;
begin
  FCS.Acquire;
  try
    do_CheckRecorderList;
  finally
    FCS.Release;
  end;

  FTimer.OnTimer := on_Timer;
end;

procedure TPreventRecording.Stop;
begin
  FTimer.OnTimer := nil;
end;

end.
