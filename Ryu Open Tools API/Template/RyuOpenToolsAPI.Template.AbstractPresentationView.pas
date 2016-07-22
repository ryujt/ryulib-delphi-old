/// TView 정의되고 구현된 유닛이다.
unit View;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils, Graphics;

type
  ///  Core에서 UI 객체에게 메시지를 전달하는 과정을 대신 해 준다.
  TView = class (TComponent)
  private
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
  protected
    FObserverList : TObserverList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// 메시지를 수신 할 객체를 등록한다.
    procedure Add(Observer:TObject);  

    /// Observer에게 메시지 전송을 중단한다.
    procedure Remove(Observer:TObject);  

    /// 등록 된 모든 Observer에게 메시지를 전송한다.
    procedure AsyncBroadcast(AMsg:string);

    /// TCore가 초기화 됐다.
    procedure sp_Initialize;

    /// TCore의 종료처리가 시작됐다.
    procedure sp_Finalize;    

    /// 모든 View 객체들이 생성 되었다.
    procedure sp_ViewIsReady;  

    /// 시스템 내부에서 경고 메시지를 출력하고자 할 때 쓰인다.
    procedure sp_SystemMessage(AMsg:string; AColor:TColor=clRed);

    /// 프로그램 종료
    procedure sp_Terminate(AMsg:string);

published
    /// 메시지 전송 중인 가?
    property Active : boolean read GetActive write SetActive;  
  end;

implementation

{ TView }

procedure TView.Add(Observer: TObject);
begin
  FObserverList.Add(Observer);
end;

procedure TView.AsyncBroadcast(AMsg: string);
begin
  FObserverList.AsyncBroadcast( AMsg );
end;

constructor TView.Create(AOwner: TComponent);
begin
  inherited;

  FObserverList := TObserverList.Create(nil);
end;

destructor TView.Destroy;
begin
  FreeAndNil(FObserverList);

  inherited;
end;

function TView.GetActive: boolean;
begin
  Result := FObserverList.Active;
end;

procedure TView.Remove(Observer: TObject);
begin
  FObserverList.Remove(Observer);
end;

procedure TView.SetActive(const Value: boolean);
begin
  FObserverList.Active := Value;
end;

procedure TView.sp_Finalize;
var
  Params : TValueList;
begin
  Params := TValueList.Create;
  try
    Params.Values['Code'] := 'Finalize';
    FObserverList.Broadcast(Params);
  finally
    Params.Free;
  end;
end;

procedure TView.sp_Initialize;
var
  Params : TValueList;
begin
  Params := TValueList.Create;
  try
    Params.Values['Code'] := 'Initialize';
    FObserverList.AsyncBroadcast(Params);
  finally
    Params.Free;
  end;
end;

procedure TView.sp_SystemMessage(AMsg: string; AColor: TColor);
var
  Params : TValueList;
begin
  Params := TValueList.Create;
  try
    Params.Values['Code'] := 'SystemMessage';
    Params.Values['Msg'] := AMsg;
    Params.Integers['Color'] := AColor;

    FObserverList.AsyncBroadcast(Params);
  finally
    Params.Free;
  end;
end;

procedure TView.sp_Terminate(AMsg: string);
var
  Params : TValueList;
begin
  Params := TValueList.Create;
  try
    Params.Values['Code'] := 'Terminate';
    Params.Values['Msg']  := AMsg;

    FObserverList.AsyncBroadcast(Params);
  finally
    Params.Free;
  end;
end;

procedure TView.sp_ViewIsReady;
var
  Params : TValueList;
begin
  Params := TValueList.Create;
  try
    Params.Values['Code'] := 'ViewIsReady';
    FObserverList.AsyncBroadcast(Params);
  finally
    Params.Free;
  end;
end;

end.
