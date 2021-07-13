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
    public

        class function RandomNumber(const m : TuInt) : TuInt;
    
        class function Fermat(const a, p : TuInt) : Boolean; overload;    
        class function Fermat(const p : TuInt) : Boolean; overload;
        class function FermatRand(const p : TuInt; const k : PtrUInt = 8) : Boolean; 

        class function ModPrimes(const p : TuInt) : Boolean;
        
        class function MillerRabin(const n : TuInt; const k : PtrUInt = 24) : Boolean;

        class function RandomPrime(const LessThanHalf : Boolean = False) : TuInt;
        class function NextPrime(const Number : TuInt) : TuInt;

        class function SimpleEuclides(a, b : TuInt) : TuInt; 
        class function NWDResult(const a, b, c : TDoubleSignedInt) : TNWDResult; inline;
        class function NWD(const j, k : TDoubleSignedInt) : TNWDResult;
        class function InvertModulo(const x, m : TuInt) : TuInt;
    end;   
    
implementation

class function TPrimeTests.RandomNumber(const m : TuInt) : TuInt;
var
    d : TDoubledInt;
    l : PLongWord;
    i : PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TuInt) shr 1 -1 do
        l[i] := Random($FFFFFFFF);
    Exit(d mod m);
end;

class function TPrimeTests.Fermat(const a, p : TuInt) : Boolean;
begin
    Result := TDoubledInt.PowerMod(a, p-1, p) = 1;
end;

class function TPrimeTests.Fermat(const p : TuInt) : Boolean; 
begin
    Result := Fermat(2, p) and Fermat(3, p) and Fermat(5, p) and Fermat(7, p) and Fermat(11, p) and Fermat(13, p) and Fermat(17, p);
end;

class function TPrimeTests.FermatRand(const p : TuInt; const k : PtrUInt) : Boolean; 
var
    i, a : PtrUInt;
begin
    for i := 1 to k do
    begin
        a := Random($FFFFFFF0)+2;
        if (SimpleEuclides(a, p) <> 1) or (not Fermat(a, p)) then
            Exit(False);
    end;
    Exit(True);
end;

class function TPrimeTests.MillerRabin(const n : TuInt; const k : PtrUInt) : Boolean;
var
    s, i, r : PtrUInt;
    tmp, a, d : TuInt;
    n1 : TuInt;
    all : Boolean;
begin
    if n and 1 = 0 then
        Exit(False);
    tmp := 1;
    s := 0;
    n1 := n-1;
    while n1 mod tmp = 0 do
    begin
        tmp := tmp shl 1;
        inc(s);
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
        for r := 0 to s-1 do
        begin    
            if not ((TDoubledInt.PowerMod(a, d, n) <> 1) and (TDoubledInt.PowerMod(a, tmp, n) <> n1)) then
            begin
                All := False;
                Break;
            end;
            tmp := tmp shl 1;                                                                
        end;
        if All then
            Exit(False);
    end;

    Exit(True); 
end;

class function TPrimeTests.RandomPrime(const LessThanHalf : Boolean) : TuInt;
var
    d : TDoubledInt;
    l : PLongWord;
    i : PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TuInt) shr 1 -1 do
        l[i] := Random($FFFFFFFF);
    if LessThanHalf then
        d.SetBit(SizeOf(d)*8-1, False);
    Exit(NextPrime((d mod TDoubledInt(TuInt(not TuInt(1))))));
end;

class function TPrimeTests.NextPrime(const Number : TuInt) : TuInt;
const
    s = 3*5*7*11*13*17*19*23*29*31*37*41*43*47*53;
    LowPrimes : array[0..14] of PtrUInt = (3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
var
    si, lp : PtrUInt;
    b : Boolean;
    m : TuInt;
begin
    Result := Number or 1;
    m := Result mod s;
    si := PQWord(@m)^;
    repeat         
        repeat
            inc(si, 2);
            Result.AddQWord(2);
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
    until FermatRand(Result) and MillerRabin(Result);
end;

class function TPrimeTests.ModPrimes(const p : TuInt) : Boolean;
begin
    Result := (PQWord(@p)^ and 1 <> 0) and (p mod 3 <> 0) and (p mod 5 <> 0) and (p mod 7 <> 0) and (p mod 11 <> 0) and (p mod 13 <> 0) and (p mod 17 <> 0) and (p mod 19 <> 0);
end;

class function TPrimeTests.NWDResult(const a, b, c: TDoubleSignedInt
  ): TNWDResult;
begin
    Result[0] := a;
    Result[1] := b;
    Result[2] := c;
end;

class function TPrimeTests.SimpleEuclides(a, b : TuInt) : TuInt;
var
    c : TuInt;
begin
    while b <> 0 do
    begin
        c := a mod b;
        a := b;
        b := c;
    end;
    Result := a;
end;

class function TPrimeTests.NWD(const j, k: TDoubleSignedInt): TNWDResult;
var
  p : TNWDResult;
  x, y : TDoubleSignedInt;
  q, r : TDoubleSignedInt;
begin
  if j = 0 then
  begin
     Result := NWDResult(k, 1, 0);
     exit;
  end;

  TDoubleSignedInt.DivMod(k, j, q, r);

  p := NWD(r, j);
  y := p[1]-q*p[2];
  x := p[2];
         
  Result := NWDResult(p[0], x, y);
end;

class function TPrimeTests.InvertModulo(const x, m : TuInt) : TuInt;
var
    tmp : TNWDResult;
begin
    tmp := NWD(TDoubledInt(TuInt(x)), TDoubledInt(TuInt(m)));
    while tmp[2].Negative do
        tmp[2] += TDoubledInt(TuInt(m));
    if tmp[2] <> TDoubledInt(TuInt(x)) then
        Exit(tmp[2].AbsoluteValue);
    while tmp[1].Negative do
        tmp[1] += TDoubledInt(TuInt(m));;
    Exit(tmp[1].AbsoluteValue);
end;

end.
