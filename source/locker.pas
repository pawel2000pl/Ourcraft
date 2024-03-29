unit Locker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLocker }

  TLocker = class
  private
    fCriticalSection : TRTLCriticalSection;
    fIsLocked : LongWord;
    function GetIsLocked: Boolean;
  public
    procedure WaitForUnlock;
    property IsLocked : Boolean read GetIsLocked;
    function TryLock : Boolean;
    procedure Lock;
    procedure Unlock;
    constructor Create;
    destructor Destroy; override;
  end;

  { TReadWriteLocker }

  TReadWriteLocker = class(TInterfacedObject, IReadWriteSync)
  private
    WriterSection : TRTLCriticalSection;
    ReadersCount : LongWord;
    FIsWriting : Boolean;
    function CheckIfItIsReading : Boolean; inline;
  public
    property IsWriting : Boolean read FIsWriting;
    property IsReading : Boolean read CheckIfItIsReading;
    function BeginWrite: boolean;
    procedure EndWrite;
    procedure BeginRead;
    procedure EndRead;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TReadWriteLocker }

function TReadWriteLocker.CheckIfItIsReading: Boolean;
begin
  Exit(ReadersCount > 0);
end;

function TReadWriteLocker.BeginWrite: boolean;
begin
   EnterCriticalSection(WriterSection);
   while ReadersCount > 0 do
       TThread.Yield;    
   FIsWriting := True;
   Exit(True);
end;

procedure TReadWriteLocker.EndWrite;
begin                                    
   FIsWriting := False;
   LeaveCriticalSection(WriterSection);
end;

procedure TReadWriteLocker.BeginRead;
begin
   EnterCriticalSection(WriterSection);
   InterlockedIncrement(ReadersCount); 
   LeaveCriticalSection(WriterSection);
end;

procedure TReadWriteLocker.EndRead;
begin
   InterlockedDecrement(ReadersCount);
end;

constructor TReadWriteLocker.Create;
begin
  InitCriticalSection(WriterSection);
  ReadersCount := 0;
  FIsWriting := False;
end;

destructor TReadWriteLocker.Destroy;
begin
  DoneCriticalSection(WriterSection);
  inherited Destroy;
end;

{ TLocker }

function TLocker.GetIsLocked: Boolean;
begin
  Exit(fIsLocked>0);
end;

procedure TLocker.WaitForUnlock;
begin
  Lock;
  Unlock;
end;

function TLocker.TryLock: Boolean;
begin
   Result := LongBool(TryEnterCriticalsection(fCriticalSection));
   if Result then
     InterlockedIncrement(fIsLocked);
end;

procedure TLocker.Lock;
begin
   EnterCriticalsection(fCriticalSection);
   InterlockedIncrement(fIsLocked);
end;

procedure TLocker.Unlock;
begin
   InterlockedDecrement(fIsLocked);
   LeaveCriticalsection(fCriticalSection);
end;

constructor TLocker.Create;
begin
  inherited;
  fIsLocked := 0;
  InitCriticalSection(fCriticalSection);
end;

destructor TLocker.Destroy;
begin
  while IsLocked do
    Unlock;
  DoneCriticalsection(fCriticalSection);
  inherited Destroy;
end;

end.

