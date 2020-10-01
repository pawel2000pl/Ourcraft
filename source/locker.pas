unit Locker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProcessUtils;

type

  { TLocker }

  TLocker = class
  private
    fCriticalSection : TRTLCriticalSection;
    fIsLocked : Boolean;
  public
    procedure WaitForUnlock;
    property IsLocked : Boolean read fIsLocked;
    function TryLock : Boolean;
    procedure Lock;
    procedure Unlock;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TLocker }

procedure TLocker.WaitForUnlock;
begin
  Lock;
  Unlock;
end;

function TLocker.TryLock: Boolean;
begin
   Result := LongBool(TryEnterCriticalsection(fCriticalSection));
   fIsLocked := true;
end;

procedure TLocker.Lock;
begin
   EnterCriticalsection(fCriticalSection);
   fIsLocked := true;
end;

procedure TLocker.Unlock;
begin
   fIsLocked := false;
   LeaveCriticalsection(fCriticalSection);
end;

constructor TLocker.Create;
begin
  inherited;
  fIsLocked := false;
  InitCriticalSection(fCriticalSection);
end;

destructor TLocker.Destroy;
begin
  if IsLocked then
    Unlock;
  DoneCriticalsection(fCriticalSection);
  inherited Destroy;
end;

end.

