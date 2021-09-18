unit AsyncMicroTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Queues, Locker, ProcessUtils;

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
    fMethodCount : LongWord;
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
  i, j, c, iterations : Integer;
begin
  iterations := 0;
  while (fMethodCount > 0) and (not fTerminating) and (iterations < 16*1024) do
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
             Methods[j] := Methods[c];
             Dec(fMethodCount);
             Dec(c);
           end
           else
             Inc(j);
        SetLength(Methods, c);
        Locker.UnLock;
      end;
    Inc(iterations);
  end;
  if not fTerminating then
  begin
    if fMethodCount = 0 then
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
   if fTerminating then Exit;
   fCurrentTime := GetMicroseconds;
   temp.Method:=Method;
   temp.Time := fCurrentTime + DelayMicroseconds;
   with fMethods[temp.Time mod MicroSecondsPerMilliSecond] do
   begin
     Locker.Lock;
     Insert(temp, Methods, Length(Methods));    
     Inc(fMethodCount);
     Locker.Unlock;
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
  fMethodCount := 0;
  fManager.AddMethod(@Execute);
end;

destructor TMicroTimer.Destroy;
var
  i : Integer;
begin
  fTerminating := True;
  while fTerminating do
    TThread.Yield;
  fManager.DequeueObject(Self);
  for i := Low(fMethods) to High(fMethods) do
      fMethods[i].Locker.Free;
  inherited Destroy;
end;

end.

