{
  Copyright (C) 2020 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit Queues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProcessUtils, Locker, Math, Incrementations;

type

  TQueueManager = class;
  TQueueMethod = procedure of object;

  TQueueRecord = record
    case integer of
      0: (Method: TQueueMethod);
      1: (Properties: TMethod);
      2: (MethodPointer: CodePointer;
        ObjectInstance: TObject);
  end;

  TQueueDelayRecord = record
    Method: TQueueMethod;
    Time: QWord;
  end;

  { TQueueThread }

  TQueueThread = class(TThread)
  private
    fTerminating: boolean;
    fManager: TQueueManager;
    procedure WaitForNext;
  public
    procedure Execute; override;
    constructor Create(Manager: TQueueManager);
    destructor Destroy; override;
  end;

  { TQueueManager }

  TQueueManager = class
  private
    fList: array[word] of TQueueRecord;
    fAddIndex, fExecuteIndex: word;
    Locker: TLocker;
    FCoreCount: integer;

    FThreadCount: integer;
    Threads: array of TQueueThread;

    fRemoveRepeated: boolean;
    fSuspend: boolean;
  public
    property ThreadCount: integer read FThreadCount;
    property CoreCount: integer read FCoreCount;
    property RemoveRepeated: boolean read fRemoveRepeated write fRemoveRepeated;
    function QueueSize: integer;

    procedure Clear; virtual;
    property Suspend: boolean read fSuspend write fSuspend;
    function ExecuteMethod: boolean;
    procedure DequeueObject(obj: TObject); virtual;
    procedure AddMethod(const Method: TQueueMethod); virtual;
    constructor Create(const ThreadsPerCore: integer = 1;
      const AdditionalThreads: integer = 0);
    destructor Destroy; override;
  end;

  { TQueueManagerWithDelays }

  TQueueManagerWithDelays = class(TQueueManager)
  private
    fDelayList: array of TQueueDelayRecord;
    fDelayListCount: integer;
    DelayLocker: TLocker;
    procedure ExecuteDelayMethods;
  public
    procedure Clear; override;
    procedure AddMethodDelay(const Method: TQueueMethod;
      const DelayMilliseconds: QWord);
    constructor Create(const ThreadsPerCore: integer = 1;
      const AdditionalThreads: integer = 0);
    destructor Destroy; override;
  end;

operator = (const a, b: TQueueRecord): boolean; inline;

implementation

{$RangeChecks off}

operator = (const a, b: TQueueRecord): boolean; inline;
begin
  Result := (a.Properties.Code = b.Properties.Code) and
    (a.Properties.Data = b.Properties.Data);
end;

{ TQueueManagerWithDelays }

procedure TQueueManagerWithDelays.ExecuteDelayMethods;
var
  i: integer;
  CurrTime: QWord;
begin
  DelayLocker.Lock;
  try
    CurrTime := GetTickCount64;
    i := 0;
    while i < fDelayListCount do
      if CurrTime >= fDelayList[i].Time then
      begin
        AddMethod(fDelayList[i].Method);
        fDelayList[i] := fDelayList[PreDec(fDelayListCount)];
      end
      else
        Inc(i);
    setlength(fDelayList, fDelayListCount);
  finally
    DelayLocker.Unlock;
    Sleep(1);
    AddMethod(@ExecuteDelayMethods);
  end;
end;

procedure TQueueManagerWithDelays.Clear;
begin
  DelayLocker.Lock;
  try
    fDelayListCount := 0;
    setlength(fDelayList, fDelayListCount);
  finally
    DelayLocker.Unlock;
  end;
  inherited Clear;
end;

procedure TQueueManagerWithDelays.AddMethodDelay(const Method: TQueueMethod;
  const DelayMilliseconds: QWord);
begin
  DelayLocker.Lock;
  try
    setlength(fDelayList, PreInc(fDelayListCount));
    fDelayList[fDelayListCount - 1].Method := Method;
    fDelayList[fDelayListCount - 1].Time := DelayMilliseconds + GetTickCount64;
  finally
    DelayLocker.Unlock;
  end;
end;

constructor TQueueManagerWithDelays.Create(const ThreadsPerCore: integer;
  const AdditionalThreads: integer);
begin
  DelayLocker := TLocker.Create;
  fDelayListCount := 0;
  setlength(fDelayList, fDelayListCount);
  inherited Create(max(0, ThreadsPerCore), max(AdditionalThreads, 0) + 1);
  AddMethod(@ExecuteDelayMethods);
end;

destructor TQueueManagerWithDelays.Destroy;
begin
  Clear;
  DelayLocker.Free;
  inherited Destroy;
end;

{ TQueueManager }

function TQueueManager.QueueSize: integer;
begin
  Result := ($100 + fAddIndex - fExecuteIndex) and $FF;
end;

procedure TQueueManager.Clear;
var
  i: integer;
begin
  Locker.Lock;
  try
    for i := low(fList) to High(fList) do
      fList[i].Method := nil;
  finally
    Locker.Unlock;
  end;
end;

function TQueueManager.ExecuteMethod: boolean;
var
  q: TQueueRecord;
  i: word;
begin
  if Suspend then
    Exit(False);
  try      
    Locker.Lock;
    repeat
      q := fList[PostInc(fExecuteIndex)];
      if fExecuteIndex = fAddIndex then
        Exit(False);
    until (q.Method <> nil);

    if q.Method <> nil then
    begin
      if fRemoveRepeated then
      begin
        i := fExecuteIndex - 1;
        repeat
          if q = fList[i] then
            fList[i].Method := nil;
        until PostInc(i) = fAddIndex;
      end
      else
        fList[Word(fExecuteIndex-1)].Method:=nil;
    end;
  finally
    Locker.Unlock;
  end;

  try
    if q.Method <> nil then
      q.Method();
  except
    on E:Exception do RaiseException('Exception in queue method: ' + E.Message, False);
  end;

  Result := fExecuteIndex <> fAddIndex;
end;

procedure TQueueManager.DequeueObject(obj: TObject);
var
  i: integer;
begin
  Locker.Lock;
  try
    for i := low(FList) to High(FList) do
      if Obj = fList[i].ObjectInstance then
        fList[i].Method := nil;
  finally
    Locker.Unlock;
  end;
end;

procedure TQueueManager.AddMethod(const Method: TQueueMethod);
var
  q: TQueueRecord;
begin
  q.Method := Method;
  Locker.Lock;
  try
    fList[PostInc(fAddIndex)] := q;
  finally
    Locker.Unlock;
  end;
end;

constructor TQueueManager.Create(const ThreadsPerCore: integer;
  const AdditionalThreads: integer);
var
  i: integer;
begin
  Suspend := False;
  fRemoveRepeated := True;
  fAddIndex := 0;
  fExecuteIndex := 0;
  Locker := TLocker.Create;
  FCoreCount := GetCoreCount;
  FThreadCount := max(1, AdditionalThreads + ThreadsPerCore * CoreCount);
  setlength(Threads, ThreadCount);
  Clear;
  for i := 0 to ThreadCount - 1 do
    Threads[i] := TQueueThread.Create(self);
end;

destructor TQueueManager.Destroy;
var
  i: integer;
begin
  for i := 0 to ThreadCount - 1 do
    Threads[i].Free;
  setlength(Threads, 0);
  Locker.WaitForUnlock;
  Locker.Free;
  inherited Destroy;
end;

{ TQueueThread }

procedure TQueueThread.WaitForNext;
begin
   SleepMicroseconds(10);
end;

procedure TQueueThread.Execute;
begin
  repeat
    if not fManager.ExecuteMethod then
      WaitForNext;
  until fTerminating;
end;

constructor TQueueThread.Create(Manager: TQueueManager);
begin
  fTerminating := False;
  fManager := Manager;
  inherited Create(False);
  Priority := tpHigher;
end;

destructor TQueueThread.Destroy;
begin
  fTerminating := True;
  WaitFor;
  inherited Destroy;
end;

end.
