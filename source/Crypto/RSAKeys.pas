unit RSAKeys;

{$Mode ObjFpc}

interface

uses
    DoubleInt, PrimeLib;


type
    generic TNWDResults<T> = record
        First, Second, Third : T;
    end;

//(n, e) - public; (n, d) - private;
generic procedure GenerateRSAKey<TuInt>(out d, e, n : TuInt; const MaxThreadCount : PtrUInt = 1);
generic function CodeRSA<TuInt>(const Data, e, n : TuInt) : TuInt;
generic function tee<T>(const Value : T; out OutValue : T) : T; inline;

generic function Max<Ordinal>(const a, b : Ordinal) : Ordinal; overload; inline;
generic function Min<Ordinal>(const a, b : Ordinal) : Ordinal; overload; inline;

function Pow3(const x : QWord) : QWord; inline;

implementation

uses
    SafeRandomGenerator;

generic function tee<T>(const Value : T; out OutValue : T) : T; inline;
begin
    OutValue := Value;
    Exit(Value);
end;

generic function CodeRSA<TuInt>(const Data, e, n : TuInt) : TuInt;
begin
    Result := specialize TDoubleInt<TuInt>.PowerMod(Data, e, n);
end;

generic function Max<Ordinal>(const a, b : Ordinal) : Ordinal; inline;
begin
    if a > b then
       Exit(a);
    Exit(b);
end;

generic function Min<Ordinal>(const a, b : Ordinal) : Ordinal; inline;
begin
    if a < b then
       Exit(a);
    Exit(b);
end;

function Pow3(const x : QWord) : QWord; inline;
begin
    Exit(x*x*x);
end;

generic procedure GenerateRSAKey<TuInt>(out d, e, n : TuInt; const MaxThreadCount : PtrUInt);
type
    TPrimeClass = specialize TPrimeTests<TuInt>;
    TDoubleUInt = specialize TDoubleInt<TuInt>;
var
    p, q : TuInt;
    phi : TuInt;
    k : Integer;

    Threads : QWord;
begin
     p := 0;
     q := 0;
    repeat
      RandomSafeBuf(@p, SizeOf(p) div 2);
      RandomSafeBuf(@q, SizeOf(q) div 2);
      p.SetBit(SizeOf(p)*4-1, True);
      q.SetBit(SizeOf(q)*4-1, True);
      k := 0;
      while p*q div q <> p do
      begin
        if k and 1 = 0 then
           p.ShrOne
        else
           q.ShrOne;
        Inc(k);
      end;
      if MaxThreadCount > 1 then
         Threads := specialize Max<QWord>(1, specialize Min<QWord>(SizeOf(TuInt) div 32, MaxThreadCount))
         else
         Threads:=1;
      if Threads > 1 then
      begin         
        p := TPrimeClass.NextMultithreadedPrime(p, Threads);
        q := TPrimeClass.NextMultithreadedPrime(q, Threads);
      end else begin
        p := TPrimeClass.NextPrime(p);
        q := TPrimeClass.NextPrime(q);
      end;
      n := p*q;
    until n div p = q;

    phi := (p-1)*(q-1);
    RandomSafeBuf(@e, SizeOf(e));
    e := e mod phi + 10;
    repeat
        e += 1;
        if e >= phi then
            e := 2;
    until (TPrimeClass.SimpleEuclides(e, phi) = 1) and (TDoubleUInt(specialize tee<TuInt>(TPrimeClass.InvertModulo(e, phi), d))*TDoubleUInt(e) mod TDoubleUInt(phi) = 1) and (e > 1) and (e < phi);
    phi.Zero;
end;

end.
