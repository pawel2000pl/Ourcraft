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
  Classes, SysUtils, ProcessUtils, CalcUtils, Locker, Math;

type

  TQueueManager = class;
  TQueueMethod = procedure of object;

  TQueueRecord = record
    case Integer of
      0: (Method : TQueueMethod);
      1: (Properties : TMethod);
      2: (MethodPointer : CodePointer; ObjectInstance : TObject);
  end;

  TQueueDelayRecord = record
    Method : TQueueMethod;
    Time : QWord;
  end;

  { TQueueThread }

  TQueueThread = class(TThread)
  private
    fTerminating : boolean;
    fManager : TQueueManager;
  public
    procedure Execute; override;
    constructor Create(Manager : TQueueManager);
    destructor Destroy; override;
  end;

  { TQueueManager }

  TQueueManager = class
  private
    fList : array[word] of TQueueRecord;
    fAddIndex, fExecuteIndex : word;
    Locker : TLocker;

    ThreadCount : integer;
    Threads : array of TQueueThread;

    fSuspend : boolean;
  public
    procedure Clear; virtual;
    property Suspend : boolean read fSuspend write fSuspend;
    function ExecuteMethod : boolean;
    procedure DequeueObject(obj : TObject); virtual;
    procedure AddMethod(const Method : TQueueMethod); virtual;
    constructor Create(const AdditionalThreads : integer = 0);
    destructor Destroy; override;
  end;

  { TQueueManager2 }

  TQueueManager2 = class(TQueueManager)
  private
    fDelayList : array of TQueueDelayRecord;
    fDelayListCount : integer;
    DelayLocker : TLocker;
    procedure ExecuteDelayMethods;
  public
    procedure Clear; override;
    procedure AddMethodDelay(const Method : TQueueMethod; const DelayMilliseconds : QWord);
    constructor Create(const AdditionalThreads : integer = 0);
    destructor Destroy; override;
  end;
                
  operator = (const a, b : TQueueRecord) : Boolean; inline;

implementation

{$RangeChecks off}

operator = (const a, b : TQueueRecord) : Boolean; inline;
begin
  Result := (a.Properties.Code = b.Properties.Code) and (a.Properties.Data = b.Properties.Data);
end;

{ TQueueManager2 }

procedure TQueueManager2.ExecuteDelayMethods;
var
  i : integer;
  CurrTime : QWord;
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

procedure TQueueManager2.Clear;
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

procedure TQueueManager2.AddMethodDelay(const Method: TQueueMethod;
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

constructor TQueueManager2.Create(const AdditionalThreads : integer);
begin                    
  DelayLocker := TLocker.Create;
  fDelayListCount := 0;
  setlength(fDelayList, fDelayListCount);
  inherited Create(AdditionalThreads + 1);
end;

destructor TQueueManager2.Destroy;
begin
  Clear;
  DelayLocker.Free;
  inherited Destroy;
end;

{ TQueueManager }

procedure TQueueManager.Clear;
var
  i : integer;
begin
  Locker.Lock;
  try
    for i := low(fList) to High(fList) do
      fList[i].Method := nil;
  finally
    Locker.Unlock;
  end;
end;

function TQueueManager.ExecuteMethod : boolean;
var
  q : TQueueRecord;
  i : word;
begin
  Result := False;
  if Suspend then
    exit;
  Locker.Lock;
  try
    repeat
      q := fList[PostInc(fExecuteIndex)];
      if fExecuteIndex = fAddIndex then
        exit;
    until (q.Method <> nil);

    i := fExecuteIndex - 1;
    repeat
      if q = fList[i] then
        fList[i].Method := nil;
    until PostInc(i) = fAddIndex;
  finally
    Locker.Unlock;
  end;

  try
    if q.Method <> nil then
      q.Method();
  except
    RaiseException('Exception in queue method', False);
  end;

  Result := fExecuteIndex <> fAddIndex;
end;

procedure TQueueManager.DequeueObject(obj : TObject);
var
  i : integer;
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
  q : TQueueRecord;
begin
  q.Method := Method;
  Locker.Lock;
  try
    fList[PostInc(fAddIndex)] := q;
  finally
    Locker.Unlock;
  end;
end;

constructor TQueueManager.Create(const AdditionalThreads : integer);
var
  i : integer;
begin
  Suspend := False;
  fAddIndex := 0;
  fExecuteIndex := 0;
  Locker := TLocker.Create;
  ThreadCount := max(1, AdditionalThreads + GetCoreCount);
  setlength(Threads, ThreadCount);     
  Clear;
  for i := 0 to ThreadCount - 1 do
    Threads[i] := TQueueThread.Create(self);
end;

destructor TQueueManager.Destroy;
var
  i : integer;
begin
  for i := 0 to ThreadCount - 1 do
    Threads[i].Free;
  setlength(Threads, 0);
  Locker.WaitForUnlock;
  Locker.Free;
  inherited Destroy;
end;

{ TQueueThread }

procedure TQueueThread.Execute;
begin
  repeat
    if not fManager.ExecuteMethod then
      SleepMicroseconds(10);
  until fTerminating;
end;

constructor TQueueThread.Create(Manager : TQueueManager);
begin
  fTerminating := False;
  fManager := Manager;
  inherited Create(False);
end;

destructor TQueueThread.Destroy;
begin
  fTerminating := True;
  WaitFor;
  inherited Destroy;
end;

end.
