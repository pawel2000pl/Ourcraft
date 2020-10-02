unit RandomGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, SimpleCache;

type

  TConstRandomCacheKey = record
    i : Integer;
    SeedOffset : QWord;
  end;

  TGeneratorCache = specialize TSimpleCache<TConstRandomCacheKey, Double>;

  { TRandomGenerator }

  TRandomGenerator = class
  private
    fSeed : QWord;
    Cache : TGeneratorCache;
  public
    property Seed : QWord read fSeed write fSeed;  
    function ConstRandom(i : Integer; const SeedOffset : QWord = 0) : Double;
    function RandomAngle(const x : Double) : Double;
    function LinearRandom(const x : Double) : Double; overload;
    function LinearRandom(const Dim : array of Double) : Double; overload;
    constructor Create(const InitSeed : QWord);
    destructor Destroy; override;
  end;

  function CreateConstRandomCacheKey(const i : Integer; const SeedOffset : QWord) : TConstRandomCacheKey; inline;

implementation

{$RangeChecks off}

function PowerMod(const x, n, m : QWord) : QWord;
var
  h : QWord;
begin
  if n = 0 then
  begin
    Result := 1;
    exit;
  end;

  h := PowerMod(x, n shr 1, m) mod m;
  if (n and 1) = 1 then
    Result := h * h * x mod m
  else
    Result := h * h;
end;

function CreateConstRandomCacheKey(const i: Integer; const SeedOffset: QWord
  ): TConstRandomCacheKey;
begin
  Result.i := i;
  Result.SeedOffset:=SeedOffset;
end;

{ TRandomGenerator }

function TRandomGenerator.ConstRandom(i: Integer; const SeedOffset: QWord): Double;
var
  p : Integer;
begin
  If Cache.GetItem(CreateConstRandomCacheKey(i, SeedOffset), Result{%H-}) then
     exit;
  if i<0 then
  begin
    i := -i;
    p := 613;
  end
  else
    p := 389;
  Result := PowerMod(Seed+SeedOffset+PowerMod(i+3, p, 4294967161), 163, 4294967291)/4294967291;
  Cache.AddItem(CreateConstRandomCacheKey(i, SeedOffset), Result);
end;

function TRandomGenerator.RandomAngle(const x : Double) : Double;
var
  fx : Double;
  x0 : Integer;
  a, b : Double;
begin
  x0 := floor(x);
  fx := x-x0;
  a := ConstRandom(x0, Seed);
  b := ConstRandom(x0-1, Seed);
  if (a-b)>0.5 then
     b+=1
     else if (b-a)>0.5 then
       a+=1;
  Result := 2*pi*(a*fx + b*(1-fx))+2*pi*floor(a*100+b*10);
end;

function TRandomGenerator.LinearRandom(const x : Double) : Double; overload;
var
  fx : Double;
  x0 : Integer;
begin
  x0 := floor(x);
  fx := x-x0;
  Result := (ConstRandom(x0)*fx+ConstRandom(x0-1)*(1-fx));
end;

function TRandomGenerator.LinearRandom(const Dim : array of Double) : Double; overload;
var
  fx : array of Double;
  x0 : array of Integer;
  i, c : Integer;
  l : LongWord;
  d, r : double;
begin
  c := length(Dim);
  setlength(x0, c);
  setlength(fx, c);
  for i := 0 to c-1 do
  begin
    x0[i] := floor(Dim[i]);
    fx[i] := Dim[i]-x0[i];
  end;

  Result := 0;
  for l := 0 to (1 shl c) -1 do
  begin
    d := 1;
    for i := 0 to c-1 do
        if l and (1 shl i) = 0 then
        d *= fx[i]
        else
        d *= (1-fx[i]);

    r := 1;
    for i := 0 to c-1 do
        r += ConstRandom(x0[i] - (l shr i and 1), 613+i);
    r := ConstRandom(floor64(r*4294967311));

    Result += r*d;
  end;
end;

constructor TRandomGenerator.Create(const InitSeed: QWord);
begin
  fSeed := InitSeed;
  Cache := TGeneratorCache.Create();
end;

destructor TRandomGenerator.Destroy;
begin
  Cache.Free;
  inherited Destroy;
end;

end.

