unit ProcessUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, BaseUnix, Math, ctypes;

type
  TMemoryInfo = record
    MemTotal : int64;
    MemAvailable : int64;
  end;

function RunProcess(const FileName : ansistring; const Params : array of ansistring;
  const Async : boolean = False) : TPid;
procedure RunCommand(const s : ansistring);
function GetMemInfo : TMemoryInfo;

procedure RaiseException(const msg : ansistring; const Terminate : boolean = True);
function DumpCallStack : ansistring;
function GetCoreCount : PtrUInt;
function StrToIntE(const s : ansistring) : integer;
function StrToFloatE(const s : ansistring; const MinValue : double = NegInfinity;
  const MaxValue : double = Infinity) : double;
function StrToUnsigned(const s : ansistring) : integer;
function GetMicroseconds : qword;
procedure SleepMicroseconds(const Delay : QWord);     
procedure SleepNanoseconds(const Delay : QWord);

implementation

{$ifdef Linux}
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

function GetMemInfo : TMemoryInfo;  //TODO: Windows version
const
  bufSize = 256;
  strA = 'MemTotal:';
  strB = 'MemAvailable:';
var
  F : cint;
  p, k : integer;
  buf : array[0..bufSize - 1] of char;
begin
  try
    try
      F := FpOpen('/proc/meminfo', Open_RdOnly);
      FpRead(F, @buf[0], bufSize);

    finally
      fpClose(F);
    end;

    p := Pos(strA, buf);
    if p > 0 then
    begin
      p += length(strA);
      while not (buf[p] in ['0'..'9']) do
        Inc(p);
      k := p;
      while (buf[k] in ['0'..'9']) do
        Inc(k);
      Result.MemTotal := StrToInt(Copy(buf, p, k - p + 1));
      case buf[k + 1] of
        'k': Result.MemTotal *= 1024;
        'M': Result.MemTotal *= 1024 * 1024;
        'G': Result.MemTotal *= 1024 * 1024 * 1024;
      end;

    end;

    p := Pos(strB, buf);
    if p > 0 then
    begin
      p += length(strB);
      while not (buf[p] in ['0'..'9']) do
        Inc(p);
      k := p;
      while (buf[k] in ['0'..'9']) do
        Inc(k);
      Result.MemAvailable := StrToInt(Copy(buf, p, k - p + 1));
      case buf[k + 1] of
        'k': Result.MemAvailable *= 1024;
        'M': Result.MemAvailable *= 1024 * 1024;
        'G': Result.MemAvailable *= 1024 * 1024 * 1024;
      end;
    end;

  except
    Result.MemAvailable := -1;
    Result.MemTotal := -1;
  end;
end;

function DumpCallStack : ansistring;
var
  I : longint;
  prevbp : Pointer;
  CallerFrame, CallerAddress, bp : Pointer;
  Report : string;
const
  MaxDepth = 20;
begin
  Report := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do
    begin
      CallerAddress := get_caller_addr(bp);
      CallerFrame := get_caller_frame(bp);
      if (CallerAddress = nil) then
        Break;
      Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
      Inc(I);
      if (I >= MaxDepth) or (CallerFrame = nil) then
        Break;
      prevbp := bp;
      bp := CallerFrame;
    end;
  except
    { prevent endless dump if an exception occured }
  end;
  Result := Report;
end;

function GetMicroseconds : qword;
var
  tp : TTimeVal;
  {$IFDEF HAVECLOCKGETTIME}
  ts : TTimeSpec;
  {$ENDIF}

begin
 {$IFDEF HAVECLOCKGETTIME}
  if clock_gettime(CLOCK_MONOTONIC, @ts) = 0 then
  begin
    Result := (int64(ts.tv_sec) * 1000000) + (ts.tv_nsec);
    exit;
  end;
 {$ENDIF}
  fpgettimeofday(@tp, nil);
  Result := (int64(tp.tv_sec) * 1000000) + (tp.tv_usec);
end;

procedure SleepMicroseconds(const Delay : QWord);
begin
  SleepNanoseconds(Delay*1000);
end;

procedure SleepNanoseconds(const Delay : QWord);
var
  Req, Rem : TimeSpec;
begin
  if Delay = 0 then
    exit;
  Rem.tv_sec := delay div 1000000000;
  Rem.tv_nsec := delay mod 1000000000;
  repeat
    req := rem;
  until FpNanoSleep(@req, @rem) = 0;
end;

function StrToIntE(const s : ansistring) : integer;
begin
  if not TryStrToInt(s, Result) then
    RaiseException('"' + s + '" is not a integer value');
end;

function StrToFloatE(const s : ansistring; const MinValue : double = NegInfinity;
  const MaxValue : double = Infinity) : double;
begin
  if not TryStrToFloat(s, Result) then
    RaiseException('"' + s + '" is not a float value', True);
  if not ((Result >= MinValue) and (Result <= MaxValue)) then
    RaiseException(s + ' is not in between: ' + floattostr(MinValue) +
      ' and ' + floattostr(MaxValue), True);
end;

function StrToUnsigned(const s : ansistring) : integer;
begin
  Result := StrToIntE(s);
  if Result < 0 then
    RaiseException('"' + s + '" is not a signed value');
end;

procedure RaiseException(const msg : ansistring; const Terminate : boolean = True);
begin
  writeln(StdErr, 'Error: ', msg);
  if Terminate then
  begin
    writeln(StdErr, 'Stack trace:');
    writeln(StdErr, DumpCallStack);
    Halt(1);
  end;
end;

function RunProcess(const FileName : ansistring; const Params : array of ansistring;
  const Async : boolean = False) : TPid;
var
  l, lfn, i : integer;
  ArgV : PPchar;
  Child : TPid;
  ExeFileName : ansistring;
begin
  Child := fpFork();
  Result := Child;
  if Child = 0 then
  begin
    if FileExists(FileName) then
      ExeFileName := FileName
    else
      ExeFileName := ExeSearch(FileName, {%H-}fpgetenv('PATH'));

    l := length(Params) + 2;
    ArgV := AllocMem(l * SizeOf(PChar));
    ArgV[l - 1] := nil;

    lfn := length(ExeFileName);
    ArgV[0] := AllocMem(lfn + 1);
    Move(ExeFileName[1], ArgV[0][0], lfn);
    ArgV[0][lfn] := #0;

    if l > 0 then
      for i := 1 to l - 2 do
      begin
        lfn := length(Params[i - 1]);
        ArgV[i] := AllocMem(lfn + 1);
        Move(Params[i - 1][1], ArgV[i][0], lfn);
        ArgV[i][lfn] := #0;
      end;

    fpExecv(ArgV[0], ArgV);
    fpExit(0);
  end
  else if not Async then
    fpWaitpid(Child, nil, 0);
end;

procedure RunCommand(const s : ansistring);
var
  Child : TPid;
  ArgV : PPChar;
begin
  Child := fpFork();
  if Child = 0 then
  begin
    ArgV := {%H-}CreateShellArgV(s);
    fpExecv(ArgV[0], ArgV);
    fpExit(0);
  end
  else
    fpWaitpid(Child, nil, 0);
end;

end.
