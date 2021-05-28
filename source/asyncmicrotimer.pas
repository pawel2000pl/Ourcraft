unit AsyncMicroTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Queues, Locker, ProcessUtils, SynchronizedCounter;

type
  TMicroTimerRecord = record
    Methods : array of TQueueDelayRecord;
    Locker : TLocker;
  end;

  { TMicroTimer }

  TMicroTimer = class
  const
    MicroSecondsPerMilliSecond = 1000;
  private
    fTerminating : Boolean;
    fManager : TQueueManager;
    fCurrentTime : QWord;
    fMethodCount : TSynchronizedCounter;
    fMethods : array[0..MicroSecondsPerMilliSecond-1] of TMicroTimerRecord;
  public
    procedure Execute;
    procedure AddMethod(const Method: TQueueMethod; const DelayMicroseconds: QWord);

    constructor Create(queueMgr : TQueueManager);
    destructor Destroy; override;
  end;

implementation

{ TMicroTimer }

procedure TMicroTimer.Execute;
var
  i, j, c : Integer;
begin
  while fMethodCount.Value > 0 do
  begin
    fCurrentTime := GetMicroseconds;
    for i := low(fMethods) to High(fMethods) do
      with fMethods[i mod MicroSecondsPerMilliSecond] do
      begin      
        j := 0;
        Locker.Lock;
        c := Length(Methods);
        while (j<c) do
           if Methods[j].Time <= fCurrentTime then
           begin
             fManager.AddMethod(Methods[j].Method);
             Dec(c);
             Methods[j] := Methods[c];
             fMethodCount.DecValue();
           end
           else
             Inc(j);
        Locker.UnLock;
      end;
  end;
  if not fTerminating then
  begin
    SleepMicroseconds(1);
    fManager.AddMethod(@Execute);
  end
  else
    fTerminating := False;
end;

procedure TMicroTimer.AddMethod(const Method: TQueueMethod; const DelayMicroseconds: QWord);
var
  temp : TQueueDelayRecord;
begin                              
   fCurrentTime := GetMicroseconds;
   temp.Method:=Method;
   temp.Time := fCurrentTime + DelayMicroseconds;
   with fMethods[(fCurrentTime + DelayMicroseconds) mod MicroSecondsPerMilliSecond] do
   begin
     Locker.Lock;
     Insert(temp, Methods, Length(Methods));
     Locker.Unlock;                         
     fMethodCount.IncValue();
   end;
end;

constructor TMicroTimer.Create(queueMgr: TQueueManager);
var
  i : Integer;
begin
  fTerminating := False;
  fManager := queueMgr;
  for i := Low(fMethods) to High(fMethods) do
      fMethods[i].Locker := TLocker.Create;
  fMethodCount := TSynchronizedCounter.Create();
  fManager.AddMethod(@Execute);
end;

destructor TMicroTimer.Destroy;
var
  i : Integer;
begin
  fTerminating := True;
  while fTerminating do;
  for i := Low(fMethods) to High(fMethods) do
      fMethods[i].Locker.Free;
  fMethodCount.Free;
  inherited Destroy;
end;

end.

