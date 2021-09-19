unit MemGuard;

{$mode objfpc}{$H+}

interface

var MaxMemoryAvailable : int64 = 2*1024*1024*1024;  //6GB

function GetMGMemoryAllocated : int64; inline;

implementation

{$RangeChecks off}

var
  MainMemoryManager : TMemoryManager;
  mgMemoryManager : TMemoryManager;
  Allocated : int64 = 0;

procedure RaiseOutOfMemory;
begin
  Writeln(StdErr, 'Memory guard: out of memory');
  runerror(12);
end;

function mgGetMem(Size : ptruint) : Pointer;
begin
  if Size <= 0 then
    exit(nil);
  InterlockedExchangeAdd64(Allocated, Size);
  if Allocated > MaxMemoryAvailable then
    RaiseOutOfMemory;
  Result := MainMemoryManager.Getmem(Size);
end;

function mgFreeMem(P : pointer) : ptruint;
begin
  if p <> nil then
    InterlockedExchangeAdd64(Allocated, -int64(MainMemoryManager.MemSize(P)));
  Result := MainMemoryManager.Freemem(P);
end;

function mgFreeMemSize(p:pointer;Size:ptruint):ptruint;
begin
  if p <> nil then
    InterlockedExchangeAdd64(Allocated, -int64(MainMemoryManager.MemSize(P)));
  Result := MainMemoryManager.FreememSize(P, Size);
end;

function mgReallocMem(var p:pointer;Size:ptruint):Pointer;
begin
  if p = nil then
    InterlockedExchangeAdd64(Allocated, Size)
    else
    InterlockedExchangeAdd64(Allocated, Size - MainMemoryManager.MemSize(p));
  if Allocated > MaxMemoryAvailable then
    RaiseOutOfMemory;
  Result := MainMemoryManager.ReAllocMem(p, size);
end;

function GetMGMemoryAllocated: int64;
begin
  Result := Allocated;
end;

initialization
    GetMemoryManager(MainMemoryManager{%H-});
    MgMemoryManager := MainMemoryManager;

    MgMemoryManager.Getmem:=@mgGetMem;
    MgMemoryManager.Freemem:=@mgFreeMem;
    MgMemoryManager.FreememSize:=@mgFreeMemSize;  
    MgMemoryManager.AllocMem:=@mgGetMem;
    mgMemoryManager.ReAllocMem:=@mgReallocMem;

    SetMemoryManager(MgMemoryManager);

finalization
    if Allocated > 0 then
      writeln(' [MemGuard] : Possible memory leaks: ', Allocated, 'B')
      else
      writeln(' [MemGuard] : No leaks detected');
    SetMemoryManager(MainMemoryManager);

end.

