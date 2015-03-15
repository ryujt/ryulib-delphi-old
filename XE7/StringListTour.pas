unit StringListTour;

interface

uses
  Classes, SysUtils, Controls;

type
  TStringListTourErrorEvent = procedure (Sender:TObject; var ErrorAction:integer) of object;
    // ErrorAction = mrYes, mrCancel, mrIgnore;

  TStringListTourDropPointEvent = procedure (Sender:TObject; Text:string; var Error:boolean) of object;

  TStringListTour = class
  private
    FList : TStringList;
  private
    FOnError : TStringListTourErrorEvent;
    FOnDropPoint : TStringListTourDropPointEvent;
    FOnCancel : TNotifyEvent;
    FOnComplete : TNotifyEvent;
  public
    constructor Create(StringList:TStringList); reintroduce;

    procedure Start;

    property OnDropPoint : TStringListTourDropPointEvent read FOnDropPoint write FOnDropPoint;
    property OnError : TStringListTourErrorEvent read FOnError write FOnError;
    property OnCancel : TNotifyEvent read FOnCancel write FOnCancel;
    property OnComplete : TNotifyEvent read FOnComplete write FOnComplete;
  end;

implementation

{ TStringListTour }

constructor TStringListTour.Create(StringList:TStringList);
begin
  inherited Create;

  FList := StringList;
end;

procedure TStringListTour.Start;
var
  bEOF, bError : boolean;
  Enum : TStringsEnumerator;
  iErrorActoin : integer;
begin
  if (Assigned(FOnDropPoint) = false) or (Assigned(FOnError) = false) then
    raise Exception.Create('TStringListTour.Start: OnDropPoint,OnError 핸들러를 지정하여 주시기 바랍니다.');

  Enum := FList.GetEnumerator;
  try
    repeat
      bEOF := not Enum.MoveNext;
      if not bEOF then
        repeat
          FOnDropPoint(Self, Enum.Current, bError);
          if bError then begin
            FOnError(Self, iErrorActoin);
            case iErrorActoin of
              mrYes : ;
              mrCancel : begin
                if Assigned(FOnCancel) then FOnCancel(Self);  
                Exit;
              end;
              mrIgnore : bError := false;
            end;
          end;
        until bError = false;
    until bEOF;
  finally
    Enum.Free;
  end;

  if Assigned(FOnComplete) then FOnComplete(Self);
end;

end.
