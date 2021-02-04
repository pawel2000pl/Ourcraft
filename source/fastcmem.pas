unit fastcmem;

{$mode objfpc}
interface
// slightly faster than cmem, all size management code removed.
const
  LibName = 'libc';

function Malloc(Size : ptrint) : Pointer;
  cdecl; external LibName Name 'malloc';
procedure Free(P : pointer);
  cdecl; external LibName Name 'free';
function ReAlloc(P : Pointer; Size : ptrint) : pointer;
  cdecl; external LibName Name 'realloc';
function CAlloc(unitSize, UnitCount : ptrint) : pointer;
  cdecl; external LibName Name 'calloc';
function MemSizeUsed(p : pointer) : ptrint;
  cdecl; external libname Name 'malloc_usable_size';

implementation

function CGetMem(Size : ptruint) : Pointer;
begin
  CGetMem := Malloc(Size);
end;

function CFreeMem(P : pointer) : ptruint;
begin
  CFreeMem := memsizeused(p);
  Free(P);
end;

function CFreeMemSize(P : pointer;{%H-}size : ptruint) : ptruint;
begin
  Free(p);
  CFreeMemSize := MemSizeUsed(p);
end;


function CAllocMem(Size : ptruint) : Pointer;
begin
  CAllocMem := calloc(Size, 1);
end;

function CReAllocMem(var p : pointer; Size : ptruint) : Pointer;
begin
  p := realloc(p, size);
  CReAllocMem := p;
end;

function CMemSize(p : pointer) : ptruint;
begin
  CMemSize := memsizeused(p);
end;

function CGetHeapStatus : THeapStatus;
begin
  CGetHeapStatus := Default(THeapStatus);
end;

function CGetFPCHeapStatus : TFPCHeapStatus;
begin
  CGetFPCHeapStatus := Default(TFPCHeapStatus);
end;

const
  CMemoryManager : TMemoryManager =
    (
    NeedLock : False;
    GetMem : @CGetmem;
    FreeMem : @CFreeMem;
    FreememSize : @CfreeMemSize;
    AllocMem : @CAllocMem;
    ReallocMem : @CReAllocMem;
    MemSize : @CMemSize;
    InitThread : nil;
    DoneThread : nil;
    RelocateHeap : nil;
    GetHeapStatus : @CGetHeapStatus;
    GetFPCHeapStatus : @CGetFPCHeapStatus;
    );

var
  OldMemoryManager : TMemoryManager;

initialization
  OldMemoryManager := Default(TMemoryManager);
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(CmemoryManager);

finalization
  SetMemoryManager(OldMemoryManager);
end.
