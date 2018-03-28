unit SpinLock;

interface

uses
  Windows;

type
  TSpinLock = class
  private
    FSection : TRTLCriticalSection;
    FSpinCount : cardinal;
  public
    procedure Enter;
    procedure Leave;
    function TryEnter:boolean;

    constructor Create(SpinCount : Cardinal = 2000);
    destructor Destroy; override;

    property SpinCount : cardinal read FSpinCount;
  end;
  
implementation

{ TSpinLock }

constructor TSpinLock.Create(SpinCount: Cardinal);
begin
  inherited Create;

  FSpinCount := SpinCount;
  InitializeCriticalSectionAndSpinCount(FSection, SpinCount);
end;

destructor TSpinLock.Destroy;
begin
  DeleteCriticalSection(FSection);
  
  inherited;
end;

procedure TSpinLock.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TSpinLock.Leave;
begin
  LeaveCriticalSection(FSection);
end;

function TSpinLock.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection);
end;

end.
