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
  Classes, SysUtils, Locker, RTLEvent;

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

  TQueueExceptionEvent = procedure(Sender : TObject; const Method : TQueueRecord; RaisedException : Exception) of object;

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
    FOnException: TQueueExceptionEvent;
    Locker: TLocker;
    FCoreCount: integer;
    FMethodCount : Integer;
    FEvent : TRTLEvent;
    FStackSizePerThread : PtrUInt;
    FTerminating : Boolean;

    FThreadCount: integer;
    Threads: array of TQueueThread;

    fRemoveRepeated: boolean;
    fSuspend: boolean;
    procedure DoExceptionEvent(Sender : TObject; const Method : TQueueRecord; RaisedException : Exception);
    procedure SetOnException(AValue: TQueueExceptionEvent);
  public
    property ThreadCount: integer read FThreadCount;
    property CoreCount: integer read FCoreCount;
    property RemoveRepeated: boolean read fRemoveRepeated write fRemoveRepeated;
    property QueuedMethodCount : Integer read FMethodCount;
    property OnException : TQueueExceptionEvent read FOnException write SetOnException;
    property Terminagting : Boolean read FTerminating;
    function QueueSize: integer;

    procedure Clear; virtual;
    property Suspend: boolean read fSuspend write fSuspend;
    function ExecuteMethod: boolean;
    procedure DequeueObject(obj: TObject); virtual;
    procedure AddMethod(const Method: TQueueMethod); virtual;
    procedure AddOrExecuteIfOveloaded(const Method: TQueueMethod);
    constructor Create(const ThreadsPerCore: integer = 1; const AdditionalThreads: integer = 0; const StackSizePerThread : PtrUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { EQueueOverload }

  EQueueOverload = class(Exception)
  private
    FQueue : TQueueManager;
    FMethod : TQueueMethod;
  public
    function Retry : Boolean;
    property Queue : TQueueManager read FQueue;
    property Method : TQueueMethod read FMethod;
    constructor Create(AQueue : TQueueManager; const AMethod : TQueueMethod);
  end;

  { TObjectProcedureWithOneParameter }

  generic TObjectProcedureWithOneParameter<TParameter> = class
  type
      TStaticConstProcedure = procedure(const x : TParameter);
      TStaticProcedure = procedure(x : TParameter);
      TObjectConstProcedure = procedure(const x : TParameter) of object;
      TObjectProcedure = procedure(x : TParameter) of object;
  private
      fscp : TStaticConstProcedure;
      fsp : TStaticProcedure;
      focp : TObjectConstProcedure;
      fop : TObjectProcedure;
      fParameter : TParameter;
      procedure InitNil;
  public
      procedure Execute;

      constructor Create(const proc : TStaticConstProcedure; const Parameter : TParameter);
      constructor Create(const proc : TStaticProcedure; const Parameter : TParameter);
      constructor Create(const proc : TObjectConstProcedure; const Parameter : TParameter);
      constructor Create(const proc : TObjectProcedure; const Parameter : TParameter);

      class function Convert(const proc : TStaticConstProcedure; const Parameter : TParameter) : TQueueMethod; overload;
      class function Convert(const proc : TStaticProcedure; const Parameter : TParameter) : TQueueMethod; overload;
      class function Convert(const proc : TObjectConstProcedure; const Parameter : TParameter) : TQueueMethod; overload;
      class function Convert(const proc : TObjectProcedure; const Parameter : TParameter) : TQueueMethod; overload;
  end;

    { TStaticProcedureAsObjectMethod }

    TStaticProcedureAsObjectMethod = class
    private
        FProc : TProcedure;
    public
        procedure Execute;
        function GetMethodPointer : TQueueMethod;
        constructor Create(const Proc : TProcedure);
    end;
        
operator = (const a, b: TQueueRecord): boolean; inline;
function StaticProcedureToObjectMethod(const Proc : TProcedure) : TQueueMethod;

implementation

uses
   Math, ctypes;

{$RangeChecks off}

{$ifdef Linux}                     
{$IfNDef USECTHREADS}{$Hint In case of linking error, add the cThreads unit as the first unit in the project}{$EndIf}
function sysconf(i : cint) : clong; cdecl; external Name 'sysconf';
{$endif}

function GetCoreCount : PtrUInt;
begin
  {$ifdef Linux}
  Result := sysconf(83);
  {$else}
  Result := GetCPUCount;
  {$endif}
end;

operator = (const a, b: TQueueRecord): boolean; inline;
begin
  Result := (a.Properties.Code = b.Properties.Code) and
    (a.Properties.Data = b.Properties.Data);
end;

function PostInc(var i : Integer) : Integer; inline; overload;
begin
    Result := i;
    Inc(i);
end;

function PostInc(var i : word) : word; inline; overload;
begin
    Result := i;
    Inc(i);
end;

{ EQueueOverload }

function EQueueOverload.Retry: Boolean;
begin
  try
    FQueue.AddMethod(FMethod);
    Result := True;
  except
    on E: EQueueOverload do Result := False;
  end;
end;

constructor EQueueOverload.Create(AQueue: TQueueManager; const AMethod: TQueueMethod);
begin
  FQueue := AQueue;
  FMethod := AMethod;
  inherited Create('Queue $' + IntToHex(QWord(FQueue), 16) + ' is overloaded. A method which was attempted to add: ' + IntToHex({%H-}QWord(TMethod(FMethod).Code), 16) +
  ' from an object ' + IntToHex({%H-}QWord(TMethod(FMethod).Data), 16) + ' (' + TObject(TMethod(FMethod).Data).ClassName + ')');
end;

{ TObjectProcedureWithOneParameter }

procedure TObjectProcedureWithOneParameter.InitNil;
begin
  fscp := nil;
  fsp := nil;
  focp := nil;
  fop := nil;
end;

procedure TObjectProcedureWithOneParameter.Execute;
begin
  if fscp <> nil then
     fscp(fParameter)
  else if fsp <> nil then
     fsp(fParameter)
  else if focp <> nil then
     focp(fParameter)
  else if fop <> nil then
     fop(fParameter);
  Free;
end;

constructor TObjectProcedureWithOneParameter.Create(
  const proc: TStaticConstProcedure; const Parameter: TParameter);
begin
   InitNil;
   fParameter:=Parameter;
   fscp := proc;
end;

constructor TObjectProcedureWithOneParameter.Create(
  const proc: TStaticProcedure; const Parameter: TParameter);
begin
   InitNil;
   fParameter:=Parameter;
   fsp := proc;
end;

constructor TObjectProcedureWithOneParameter.Create(
  const proc: TObjectConstProcedure; const Parameter: TParameter);
begin
   InitNil;
   fParameter:=Parameter;
   focp := proc;
end;

constructor TObjectProcedureWithOneParameter.Create(
  const proc: TObjectProcedure; const Parameter: TParameter);
begin
   InitNil;
   fParameter:=Parameter;
   fop := proc;
end;

class function TObjectProcedureWithOneParameter.Convert(
  const proc: TStaticConstProcedure; const Parameter: TParameter): TQueueMethod;
begin
  Result := @TObjectProcedureWithOneParameter.Create(proc, Parameter).Execute;
end;

class function TObjectProcedureWithOneParameter.Convert(
  const proc: TStaticProcedure; const Parameter: TParameter): TQueueMethod;
begin
  Result := @TObjectProcedureWithOneParameter.Create(proc, Parameter).Execute;
end;

class function TObjectProcedureWithOneParameter.Convert(
  const proc: TObjectConstProcedure; const Parameter: TParameter): TQueueMethod;
begin
  Result := @TObjectProcedureWithOneParameter.Create(proc, Parameter).Execute;
end;

class function TObjectProcedureWithOneParameter.Convert(
  const proc: TObjectProcedure; const Parameter: TParameter): TQueueMethod;
begin
  Result := @TObjectProcedureWithOneParameter.Create(proc, Parameter).Execute;
end;

procedure TStaticProcedureAsObjectMethod.Execute;
begin
    FProc();
    Free;
end;

function TStaticProcedureAsObjectMethod.GetMethodPointer : TQueueMethod;
begin
    Exit(@Execute);
end;

constructor TStaticProcedureAsObjectMethod.Create(const Proc : TProcedure);
begin
    FProc := Proc;
end;

function StaticProcedureToObjectMethod(const Proc : TProcedure) : TQueueMethod;
var
    tmp : TStaticProcedureAsObjectMethod;
begin 
    tmp := TStaticProcedureAsObjectMethod.Create(Proc);
    Exit(tmp.GetMethodPointer);
end;

{ TQueueManager }

procedure TQueueManager.DoExceptionEvent(Sender: TObject; const Method: TQueueRecord; RaisedException: Exception);
const
  Trials = $1000;
var
    i : Integer;
    SecondMessage : AnsiString;
begin
  if (FOnException <> nil) and Assigned(FOnException) then
    FOnException(Sender, Method, RaisedException)
    else
    begin
      SecondMessage := '';
      if not Suspend then
        if RaisedException is EQueueOverload then
        begin
          i := 0;
          while (FMethodCount > Length(fList)*0.9) and (PostInc(i) < Trials) do
            TThread.Yield;
          if (FMethodCount > Length(fList)*0.9) and (i < Trials) then
            if (RaisedException as EQueueOverload).Retry then
              SecondMessage := ' and cannot avoid it';
        end;
      Writeln('Exception in queue method: ' + RaisedException.Message + SecondMessage);
    end;
end;

procedure TQueueManager.SetOnException(AValue: TQueueExceptionEvent);
begin
  if (FOnException=AValue) then Exit;
  if not Assigned(FOnException) then
     AValue := nil;
  FOnException:=AValue;
end;

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
    FMethodCount:=0;
  finally
    Locker.Unlock;
  end;
end;

function TQueueManager.ExecuteMethod: boolean;
var
  q: TQueueRecord;
  i: word;
begin
  if Suspend or (FMethodCount = 0) then
    Exit(False);  
  Locker.Lock;
  try
    repeat
      q := fList[PostInc(fExecuteIndex)];
      if fExecuteIndex = fAddIndex then
        Break;
    until (q.Method <> nil);

    if q.Method <> nil then
    begin
      if fRemoveRepeated then
      begin
        i := fExecuteIndex - 1;
        repeat
          if q = fList[i] then
          begin
            fList[i].Method := nil;
            Dec(FMethodCount);
          end;
        until PostInc(i) = fAddIndex;
      end
      else
      begin
        fList[Word(fExecuteIndex-1)].Method:=nil;
        Dec(FMethodCount);
      end;
    end;            

    Result := fExecuteIndex <> fAddIndex;
    if Result then
      FEvent.SetUp
      else
      FEvent.Reset;
  finally
    Locker.Unlock;
  end;

  try
    if (q.MethodPointer <> nil) and (q.ObjectInstance <> nil) then
      q.Method();
  except
    on E:Exception do DoExceptionEvent(Self, q, E);
  end;
end;

procedure TQueueManager.DequeueObject(obj: TObject);
var
  i: integer;
begin
  Locker.Lock;
  try
    for i := low(FList) to High(FList) do
      if Obj = fList[i].ObjectInstance then
      begin
        fList[i].Method := nil;
        Dec(FMethodCount);
      end;
  finally
    Locker.Unlock;
  end;
end;

procedure TQueueManager.AddMethod(const Method: TQueueMethod);
var
  q: TQueueRecord;
begin
  if Terminagting then
     Exit;
  q.Method := Method;
  Locker.Lock;
  try                    
    if FMethodCount >= Length(fList)-1 then
       raise EQueueOverload.Create(self, Method);
    fList[PostInc(fAddIndex)] := q;
    Inc(FMethodCount);
  finally
    Locker.Unlock;   
    FEvent.SetUp;
  end;
end;

procedure TQueueManager.AddOrExecuteIfOveloaded(const Method: TQueueMethod);
begin
  if QueuedMethodCount >= ThreadCount then
     Method()
     else
     AddMethod(Method);
end;

constructor TQueueManager.Create(const ThreadsPerCore: integer;
  const AdditionalThreads: integer; const StackSizePerThread: PtrUInt);
var
  i: integer;
begin
  FStackSizePerThread:=StackSizePerThread;
  FTerminating:=False;
  FOnException:=nil;
  Suspend := False;
  fRemoveRepeated := True;
  fAddIndex := 0;
  fExecuteIndex := 0;
  FMethodCount := 0;
  Locker := TLocker.Create;
  FCoreCount := GetCoreCount;
  FThreadCount := max(1, AdditionalThreads + ThreadsPerCore * CoreCount);
  setlength(Threads, ThreadCount);
  FEvent := TRTLEvent.Create;
  Clear;
  for i := 0 to ThreadCount - 1 do
    Threads[i] := TQueueThread.Create(self);
end;

destructor TQueueManager.Destroy;
var
  i: integer;
begin
  FTerminating := True;
  Clear;
  for i := 0 to ThreadCount - 1 do
    FreeAndNil(Threads[i]);
  setlength(Threads, 0);
  Locker.WaitForUnlock;
  Locker.Free;
  FEvent.Free;
  inherited Destroy;
end;

{ TQueueThread }

procedure TQueueThread.WaitForNext;
begin
  fManager.FEvent.WaitFor(1);
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
  inherited Create(False, fManager.FStackSizePerThread);
end;

destructor TQueueThread.Destroy;
begin
  fTerminating := True;
  fManager.FEvent.SetUp;
  WaitFor;
  inherited Destroy;
end;

end.
