unit PrimeLib;

{$Mode ObjFpc}

interface

uses
    DoubleInt;

type
    generic TPrimeTests<TInt> = class
    type
        TDoubledInt = specialize TDoubleInt<TInt>;
        TNWDResult = array[0..2] of TInt;
    public

        class function RandomNumber(const m : TInt) : TInt;
    
        class function Fermat(const a, p : TInt) : Boolean; overload;    
        class function Fermat(const p : TInt) : Boolean; overload;
        class function FermatRand(const p : TInt; const k : PtrUInt = 8) : Boolean; 

        class function ModPrimes(const p : TInt) : Boolean;
        
        class function MillerRabin(const n : TInt; const k : PtrUInt = 24) : Boolean;

        class function RandomPrime : TInt;
        class function NextPrime(const Number : TInt) : TInt;

        class function NWDResult(const a, b, c : TInt) : TNWDResult; inline;
        class function NWD(const j, k : TInt) : TNWDResult;
    end;
    
implementation

class function TPrimeTests.RandomNumber(const m : TInt) : TInt;
var
    d : TDoubledInt;
    l : PLongWord;
    i : PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TInt) shr 1 -1 do
        l[i] := Random($FFFFFFFF);
    Exit(d mod m);
end;

class function TPrimeTests.Fermat(const a, p : TInt) : Boolean;
begin
    Result := TDoubledInt.PowerMod(a, p-1, p) = 1;
end;

class function TPrimeTests.Fermat(const p : TInt) : Boolean; 
begin
    Result := Fermat(2, p) and Fermat(3, p) and Fermat(5, p) and Fermat(7, p) and Fermat(11, p) and Fermat(13, p) and Fermat(17, p);
end;

class function TPrimeTests.FermatRand(const p : TInt; const k : PtrUInt) : Boolean; 
var
    i, a : PtrUInt;
begin
    for i := 1 to k do
    begin
        a := Random($FFFFFFF0)+2;
        if (NWD(a, p)[0] <> 1) or (not Fermat(a, p)) then
            Exit(False);
    end;
    Exit(True);
end;

class function TPrimeTests.MillerRabin(const n : TInt; const k : PtrUInt) : Boolean;
var
    s, i, r : PtrUInt;
    tmp, a, d : TInt;
    n1 : TInt;
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

class function TPrimeTests.RandomPrime : TInt;
var
    d : TDoubledInt;
    l : PLongWord;
    i : PtrUInt;
begin
    l := @d;
    for i := 0 to SizeOf(TInt) shr 1 -1 do
        l[i] := Random($FFFFFFFF);
    Exit(NextPrime((d mod TDoubledInt(TInt(not TInt(1))))));
end;

class function TPrimeTests.NextPrime(const Number : TInt) : TInt;
const
    s = 3*5*7*11*13*17*19*23*29*31*37*41*43*47*53;
    LowPrimes : array[0..14] of PtrUInt = (3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
var
    si, lp : PtrUInt;
    b : Boolean;
    m : TInt;
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

class function TPrimeTests.ModPrimes(const p : TInt) : Boolean;
begin
    Result := (PQWord(@p)^ and 1 <> 0) and (p mod 3 <> 0) and (p mod 5 <> 0) and (p mod 7 <> 0) and (p mod 11 <> 0) and (p mod 13 <> 0) and (p mod 17 <> 0) and (p mod 19 <> 0);
end;

class function TPrimeTests.NWDResult(const a, b, c : TInt) : TNWDResult;
begin
    Result[0] := a;
    Result[1] := b;
    Result[2] := c;
end;

class function TPrimeTests.NWD(const j, k : TInt) : TNWDResult;
var
  r : TInt;
  p : TNWDResult;
  x, y : TInt;
  q : TInt;
begin
  if j = 0 then
  begin
     Result := NWDResult(k, 1, 0);
     exit;
  end;

  TInt.DivMod(k, j, q, r);
  //r := k mod j;
  p := NWD(r, j);
  y := p[1]-{(k div j)}q*p[2];
  x := p[2];
         
  Result := NWDResult(p[0], x, y);
end;

end.
