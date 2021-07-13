unit RSAKeys;

{$Mode ObjFpc}

interface

uses
    DoubleInt, PrimeLib;


type
    generic TNWDResults<T> = record
        First, Second, Third : T;
    end;

generic procedure GenerateRSAKey<TuInt>(out d, e, n : TuInt; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);
generic function CodeRSA<TuInt>(const Data, e, n : TuInt) : TuInt;
generic function tee<T>(const Value : T; out OutValue : T) : T; inline;

function RandomFromBuf(const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0) : LongWord;
procedure RandomBufFromBuf(const Buf : Pointer; const Size : PtrUInt; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);

implementation

uses
    math, sha1;

function RandomFromBuf(const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0) : LongWord;
var
    e : Extended;
    r : Integer;
    Context: TSHA1Context;
    code : TSHA1Digest;
begin
  if (RandomBuf = nil) or (RandomBufSize = 0) then
     Exit(Random($FFFFFFFF));
  e := Random;
  SHA1Init(Context);
  SHA1Update(Context, RandomBuf^, RandomBufSize);
  e := 1;
  for r := 0 to random(16)+15 do
    e *= (1+Random) + random(16);
  SHA1Update(Context, e, SizeOf(e));
  SHA1Final(Context, code);
  Result := 0;
  for r := 0 to 4 do
    Result := Result xor PLongWord(@code)[r];
end;

procedure RandomBufFromBuf(const Buf : Pointer; const Size : PtrUInt; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);
var
    i : Integer;
    b : PLongWord;
    x : LongWord;
begin
  b := Buf;
  for i := 0 to Size div 4 -1 do
    b[i] := RandomFromBuf(RandomBuf, RandomBufSize);
  if Size and 3 > 0 then
  begin
    x := RandomFromBuf(RandomBuf, RandomBufSize);
    Move(x, (Buf + Size and (not 3))^, Size and 3);
  end;
end;

generic function tee<T>(const Value : T; out OutValue : T) : T; inline;
begin
    OutValue := Value;
    Exit(Value);
end;

generic function CodeRSA<TuInt>(const Data, e, n : TuInt) : TuInt;
begin
    Result := specialize TDoubleInt<TuInt>.PowerMod(Data, e, n);
end;

generic procedure GenerateRSAKey<TuInt>(out d, e, n : TuInt; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);
type
    TPrimeClass = specialize TPrimeTests<TuInt>;
    TDoubleUInt = specialize TDoubleInt<TuInt>;
var
    p, q : TuInt;
    phi : TuInt;
    k : Integer;
begin
    repeat
      RandomBufFromBuf(@p, SizeOf(p) div 2, RandomBuf, RandomBufSize);
      RandomBufFromBuf(@q, SizeOf(q) div 2, RandomBuf, RandomBufSize);
      p.SetBit(SizeOf(p)*8-1, True);
      q.SetBit(SizeOf(p)*8-1, True);
      k := 0;
      while p*q div q <> p do
      begin
        if k and 1 = 0 then
           p.ShrOne
        else
           q.ShrOne;
        Inc(k);
      end;
      p := TPrimeClass.NextPrime(p);
      q := TPrimeClass.NextPrime(q);
      n := p*q;
    until n div p = q;

    phi := (p-1)*(q-1);
    RandomBufFromBuf(@e, SizeOf(e), RandomBuf, RandomBufSize);
    e := e mod phi + 10;
    repeat
        e += 1;
        if e >= phi then
            e := 2;
    until (TPrimeClass.SimpleEuclides(e, phi) = 1) and (TDoubleUInt(specialize tee<TuInt>(TPrimeClass.InvertModulo(e, phi), d))*TDoubleUInt(e) mod TDoubleUInt(phi) = 1) and (e > 1) and (e < phi);
    phi.Zero;
end;

end.
