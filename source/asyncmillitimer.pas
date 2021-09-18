unit AsyncMilliTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Queues, Locker, Incrementations;

type

  { TMilliTimer }

  TMilliTimer = class
  private
    fTerminating : Boolean;
    fDelayList: array of TQueueDelayRecord;
    fDelayListCount: integer;
    DelayLocker: TLocker;         
    fManager : TQueueManager;
    procedure ExecuteDelayMethods;
  public
    procedure AddMethod(const Method: TQueueMethod; const DelayMilliseconds: QWord);
    procedure Clear;

    constructor Create(queueMgr : TQueueManager);
    destructor Destroy; override;
  end;

implementation

{ TMilliTimer }

procedure TMilliTimer.ExecuteDelayMethods;
var
  i: integer;
  CurrTime: QWord;
begin
  DelayLocker.Lock;
  try
    CurrTime := GetTickCount64;
    i := 0;
    while i < fDelayListCount do
      if fDelayList[i].Time <= CurrTime then
      begin
        fManager.AddMethod(fDelayList[i].Method);
        fDelayList[i] := fDelayList[PreDec(fDelayListCount)];
      end
      else
        Inc(i);
    SetLength(fDelayList, fDelayListCount);
  finally
    DelayLocker.Unlock;
    if fTerminating then
    begin
      fManager.DequeueObject(self);
      fTerminating := False;
    end
    else
    begin
      Sleep(1);
      fManager.AddMethod(@ExecuteDelayMethods);
    end;
  end;
end;

procedure TMilliTimer.AddMethod(const Method: TQueueMethod; const DelayMilliseconds: QWord);
begin
  if fTerminating then
     Exit;
  DelayLocker.Lock;
  try
    SetLength(fDelayList, PreInc(fDelayListCount));
    fDelayList[fDelayListCount - 1].Method := Method;
    fDelayList[fDelayListCount - 1].Time := DelayMilliseconds + GetTickCount64;
  finally
    DelayLocker.Unlock;
  end;
end;

procedure TMilliTimer.Clear;
begin
  DelayLocker.Lock;
  try
    fDelayListCount := 0;
    setlength(fDelayList, fDelayListCount);
  finally
    DelayLocker.Unlock;
  end;
end;

constructor TMilliTimer.Create(queueMgr: TQueueManager);
begin
  DelayLocker := TLocker.Create;
  fTerminating := False;
  fManager := queueMgr;
  fDelayListCount := 0;
  SetLength(fDelayList, fDelayListCount);
  fManager.AddMethod(@ExecuteDelayMethods);
end;

destructor TMilliTimer.Destroy;
begin
  Clear;
  fTerminating := True;
  while fTerminating do
    TThread.Yield;
  DelayLocker.WaitForUnlock;
  DelayLocker.Free;
  inherited Destroy;
end;

end.

