unit PrimeLib;

{$Mode ObjFpc}

interface

uses
    DoubleInt;

type
    { TPrimeTests }

    generic TPrimeTests<TuInt> = class
    type
        TDoubledInt = specialize TDoubleInt<TuInt>;
        TDoubleSignedInt = specialize TSignedInt<TDoubledInt>;
        TInt = specialize TSignedInt<TuInt>;
        TNWDResult = array[0..2] of TDoubleSignedInt;

        TMultithreadedPrimeGeneratorRecord = record
          Done : Boolean;
          ID : QWord;
          Count : PtrUInt;
          Input, Result : TuInt;
          Event : PRTLEvent;
          cs : TRTLCriticalSection;
        end;
        PMultithreadedPrimeGeneratorRecord = ^TMultithreadedPrimeGeneratorRecord;
    private
        class function NextPrimeThread(P : Pointer) : PtrInt; static;
    public

        class function RandomNumber(const m: TuInt): TuInt; static;

        class function Fermat(const a, p: TuInt): boolean; static; overload;
        class function Fermat(const p: TuInt): boolean; static; overload;
        class function FermatRand(const p: TuInt; const k: PtrUInt = 8; const AbordPointer : PBoolean = nil): boolean; static;

        class function ModPrimes(const p: TuInt): boolean; static;

        class function MillerRabin(const n: TuInt; const k: PtrUInt = 24; const AbordPointer : PBoolean = nil): boolean; static;

        class function RandomPrime(const LessThanHalf: boolean = False): TuInt; static;
        class function NextPrime(const Number: TuInt; const MultiThreadRec : PMultithreadedPrimeGeneratorRecord = nil): TuInt; static; //preffered max 4 threads
        class function NextMultithreadedPrime(const Number: TuInt; const ThreadCount : PtrUInt = 16): TuInt; static;

        class function SimpleEuclides(a, b: TuInt): TuInt; static;
        class function NWDResult(const a, b, c: TDoubleSignedInt): TNWDResult; static; inline;
        class function NWD(const j, k: TDoubleSignedInt; const Depth : PtrUInt = 0): TNWDResult; static; deprecated; //beacause of stack overflow with uInt2048 and greater
        class function InvertModulo(const a, m: TuInt): TuInt; static;
    end;

var
    PrimeThreadCount : PtrUInt = 0; //DO NOT USE - ONLY FOR GENERICS

implementation

class function TPrimeTests.RandomNumber(const m: TuInt): TuInt;
var
    d: TDoubledInt;
    l: PLongWord;
    i: PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TuInt) shr 1 - 1 do
        l[i] := Random($FFFFFFFF);
    Exit(d mod m);
end;

class function TPrimeTests.Fermat(const a, p: TuInt): boolean;
begin
    Result := TDoubledInt.PowerMod(a, p - 1, p) = 1;
end;

class function TPrimeTests.Fermat(const p: TuInt): boolean;
begin
    Result := Fermat(2, p) and Fermat(3, p) and Fermat(5, p) and Fermat(7, p) and Fermat(11, p) and
        Fermat(13, p) and Fermat(17, p);
end;

class function TPrimeTests.FermatRand(const p: TuInt; const k: PtrUInt;
  const AbordPointer: PBoolean): boolean;
var
    i, a: PtrUInt;
begin
    for i := 1 to k do
    begin
        a := Random($FFFFFFF0) + 2;
        if (SimpleEuclides(a, p) <> 1) or (not Fermat(a, p)) or ((AbordPointer <> nil) and AbordPointer^) then
            Exit(False);
    end;
    Exit(True);
end;

class function TPrimeTests.MillerRabin(const n: TuInt; const k: PtrUInt;
  const AbordPointer: PBoolean): boolean;
var
    s, i, r: PtrUInt;
    tmp, a, d: TuInt;
    n1: TuInt;
    all: boolean;
begin
    if n and 1 = 0 then
        Exit(False);
    tmp := 1;
    s := 0;
    n1 := n - 1;
    while n1 mod tmp = 0 do
    begin
        tmp := tmp shl 1;
        Inc(s);
    end;
    Dec(s);
    d := n1 div (tmp shr 1);

    for i := 1 to k do
    begin
        repeat
            a := RandomNumber(n1);
        until a > 0;

        All := True;
        tmp := d;
        for r := 0 to s - 1 do
        begin
            if not ((TDoubledInt.PowerMod(a, d, n) <> 1) and (TDoubledInt.PowerMod(a, tmp, n) <> n1)) then
            begin
                All := False;
                Break;
            end;
            tmp := tmp shl 1;
            if (AbordPointer <> nil) and AbordPointer^ then
                Exit(False);
        end;
        if All then
            Exit(False);
    end;

    Exit(True);
end;

class function TPrimeTests.RandomPrime(const LessThanHalf: boolean): TuInt;
var
    d: TDoubledInt;
    l: PLongWord;
    i: PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TuInt) shr 1 - 1 do
        l[i] := Random($FFFFFFFF);
    if LessThanHalf then
        d.SetBit(SizeOf(d) * 8 - 1, False);
    Exit(NextPrime((d mod TDoubledInt(TuInt(not TuInt(1))))));
end;

class function TPrimeTests.NextPrime(const Number: TuInt; const MultiThreadRec: PMultithreadedPrimeGeneratorRecord): TuInt;
const
    s = 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53;
    LowPrimes: array[0..14] of PtrUInt = (3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
var
    si, addiction: QWord;
    lp: PtrUInt;
    b: boolean;
    m: TuInt;
    AbordPointer : PBoolean;
begin
    Result := Number or 1;
    m := Result mod s;
    si := PQWord(@m)^;
    if MultiThreadRec <> nil then
    begin
       addiction := 2*MultiThreadRec^.Count;
       AbordPointer := @MultiThreadRec^.Done;
    end
    else
    begin
       addiction := 2;
       AbordPointer:=nil;
    end;
    repeat
        if ((AbordPointer <> nil) and (AbordPointer^)) then
           Break;
        repeat
            Inc(si, addiction);
            Result.AddQWord(addiction);
            b := False;
            for lp in LowPrimes do
                if si mod lp = 0 then
                begin
                    b := True;
                    break;
                end;
            if si > s then
                si -= s;
        until not b;
    until ((AbordPointer <> nil) and (AbordPointer^)) or (FermatRand(Result, 8, AbordPointer) and MillerRabin(Result, 24, AbordPointer));
end;

class function TPrimeTests.NextPrimeThread(P: Pointer): PtrInt;
var
    i, r : TuInt;
    ID : QWord;
begin
  i := PMultithreadedPrimeGeneratorRecord(P)^.Input;
  ID := InterlockedIncrement64(PMultithreadedPrimeGeneratorRecord(p)^.ID)-1;
  i.AddQWord(2*ID);
  if not PMultithreadedPrimeGeneratorRecord(P)^.Done then
    r := NextPrime(i, PMultithreadedPrimeGeneratorRecord(p));

  if (not PMultithreadedPrimeGeneratorRecord(P)^.Done) and (TryEnterCriticalSection(PMultithreadedPrimeGeneratorRecord(P)^.cs) <> 0) then
  begin
    if not PMultithreadedPrimeGeneratorRecord(P)^.Done then
      PMultithreadedPrimeGeneratorRecord(P)^.Result := r;
    PMultithreadedPrimeGeneratorRecord(P)^.Done := True;
    LeaveCriticalSection(PMultithreadedPrimeGeneratorRecord(P)^.cs); 
    RTLEventSetEvent(PMultithreadedPrimeGeneratorRecord(P)^.Event);
  end;

  if InterlockedDecrement64(PMultithreadedPrimeGeneratorRecord(p)^.ID) = 0 then
  begin
     RTLEventDestroy(PMultithreadedPrimeGeneratorRecord(P)^.Event);
     DoneCriticalSection(PMultithreadedPrimeGeneratorRecord(P)^.cs);
     FreeMem(P);
  end;

  Exit(0);
end;

class function TPrimeTests.NextMultithreadedPrime(const Number: TuInt; const ThreadCount: PtrUInt): TuInt;
var
   MultithreadedRecord : PMultithreadedPrimeGeneratorRecord;
   i : Integer;
begin
   MultithreadedRecord:=AllocMem(SizeOf(TMultithreadedPrimeGeneratorRecord));
   MultithreadedRecord^.Input:=Number;
   MultithreadedRecord^.Done:=False;
   MultithreadedRecord^.ID:=0;
   MultithreadedRecord^.Count:=ThreadCount;
   MultithreadedRecord^.Event := RTLEventCreate;
   InitCriticalSection(MultithreadedRecord^.cs);
   RTLEventResetEvent(MultithreadedRecord^.Event);

   for i := 0 to ThreadCount-1 do
       BeginThread(@NextPrimeThread, MultithreadedRecord);

   RTLEventWaitFor(MultithreadedRecord^.Event);
   Exit(MultithreadedRecord^.Result);
end;

class function TPrimeTests.ModPrimes(const p: TuInt): boolean;
begin
    Result := (PQWord(@p)^ and 1 <> 0) and (p mod 3 <> 0) and (p mod 5 <> 0) and (p mod 7 <> 0) and
        (p mod 11 <> 0) and (p mod 13 <> 0) and (p mod 17 <> 0) and (p mod 19 <> 0);
end;

class function TPrimeTests.NWDResult(const a, b, c: TDoubleSignedInt): TNWDResult;
begin
    Result[0] := a;
    Result[1] := b;
    Result[2] := c;
end;

class function TPrimeTests.SimpleEuclides(a, b: TuInt): TuInt;
var
    c: TuInt;
begin
    while b <> 0 do
    begin
        c := a mod b;
        a := b;
        b := c;
    end;
    Result := a;
end;

class function TPrimeTests.NWD(const j, k: TDoubleSignedInt; const Depth: PtrUInt): TNWDResult;
var
    p: ^TNWDResult;
    x, y: ^TDoubleSignedInt;
    q, r: ^TDoubleSignedInt;
begin
    try
        p := GetMem(SizeOf(TNWDResult));
        q := GetMem(SizeOf(TDoubleSignedInt));
        try
            r := GetMem(SizeOf(TDoubleSignedInt));

            if j = 0 then
                Exit(NWDResult(k, 1, 0));

            TDoubleSignedInt.DivMod(k, j, q^, r^);

            p^ := NWD(r^, j, Depth+1){%H-};

        finally
            FreeMem(r);
        end;

        try
            x := GetMem(SizeOf(TDoubleSignedInt));
            y := GetMem(SizeOf(TDoubleSignedInt));

            y^ := p^[1] - q^ * p^[2];
            x^ := p^[2];

            Exit(NWDResult(p^[0], x^, y^));
        finally
            FreeMem(x);
            FreeMem(y);
        end;
    finally
        FreeMem(p);
        FreeMem(q);
    end;
end;

class function TPrimeTests.InvertModulo(const a, m: TuInt): TuInt;
var
    m0, y, x, a0, q, t, m2 : TDoubleSignedInt;
begin
     if m = 1 then
        Exit(0);

     m0 := TDoubledInt(TuInt(m));
     a0 := TDoubledInt(TuInt(a));
     y := 0;
     x := 1;

     while a0 > 1 do
     begin
        t := m0;
        TDoubleSignedInt.DivMod(a0, m0, q, m2);
        m0 := m2;

        a0 := t;
        t := y;

        y := x-q*y;
        x := t;
     end;

     if x < 0 then
        x += TDoubledInt(TuInt(m));

     Exit(x.UnsignedValue);
end;

initialization

finalization
  while PrimeThreadCount > 0 do ;

end.
