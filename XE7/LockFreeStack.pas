unit LockFreeStack;

interface

uses
  Windows, SysUtils, Classes;

type
  TLockFreeStack = class
  private
    FHeader : int64;
  private
    function GetCount: integer;
  public
    constructor Create;

    procedure Clear;
    procedure Push(AEntry:pointer);
    function Pop:pointer;

    property Count : integer read GetCount;
  end;

implementation

type
  TSLIST_ENTRY = record
    NextEntry : pointer;
    UserData : pointer;
  end;
  PSLIST_ENTRY = ^TSLIST_ENTRY;

procedure InitializeSListHead(AHeader:pointer); stdcall; external kernel32;
function InterlockedFlushSList(AHeader:pointer):pointer; stdcall; external kernel32;
function InterlockedPopEntrySList(AHeader:pointer):pointer; stdcall; external kernel32;
function InterlockedPushEntrySList(AHeader,AEntry:pointer):pointer; stdcall; external kernel32;
function QueryDepthSList(AHeader:pointer):Word; stdcall; external kernel32;

{ TLockFreeStack }

procedure TLockFreeStack.Clear;
var
  pItem, pRemove : PSLIST_ENTRY;
begin
  pItem := InterlockedFlushSList(@FHeader);

  while pItem = nil do begin
    pRemove := pItem;
    pItem := pItem^.NextEntry;
    Dispose(pRemove);
  end;
end;

constructor TLockFreeStack.Create;
begin
  inherited;

  InitializeSListHead(@FHeader);
end;

function TLockFreeStack.GetCount: integer;
begin
  Result := QueryDepthSList(@FHeader);
end;

function TLockFreeStack.Pop: pointer;
var
  pItem : PSLIST_ENTRY;
begin
  pItem := InterlockedPopEntrySList(@FHeader);
  if pItem = nil then begin
    Result := nil;
  end else begin
    Result := pItem^.UserData;
    Dispose(pItem);
  end;
end;

procedure TLockFreeStack.Push(AEntry: pointer);
var
  pItem : PSLIST_ENTRY;
begin
  New(pItem);
  pItem^.UserData := AEntry;
  InterlockedPushEntrySList(@FHeader, pItem);
end;

{$IFDEF DEBUG}
var
  Stack : TLockFreeStack;
  pItm : pointer;
{$ENDIF}

initialization
{$IFNDEF WIN32}
  raise Exception.Create('This unit surport only 32 bit MS-Windows.');
{$ENDIF}

{$IFDEF DEBUG}
  Stack := TLockFreeStack.Create;
  try
    Stack.Push(Pointer(1));
    Assert(Stack.Count = 1, 'Stack.Count = 1 Error.');


    Stack.Push(Pointer(2));
    Assert(Stack.Count = 2, 'Stack.Count = 2 Error.');


    pItm := Stack.Pop;
    Assert(Stack.Count = 1, 'Stack.Count = 1 Error.');
    Assert(Integer(pItm) = 2, 'pItem^.UserData = 1 Error.');
  finally
    Stack.Free;
  end;
{$ENDIF}
end.
