unit DeterminedRandomGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SimpleCache, Incrementations;

const
  ExampleSeedOffset : array[0..7] of QWord = (3392232331, 2697266237, 2106098177, 2139937253, 3402672613, 2000438761, 1622329061, 3220468841);

type

  TConstRandomCacheKey = record
    i : integer;
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
    function ConstRandom(const i : integer; const SeedOffset : QWord = 0) : double; overload;
    function ConstRandom(const Dim : array of Integer; const SeedOffset : QWord = 0) : double; overload;
    function RandomAngle(const x : double; const SeedOffset : QWord = 0) : double;
    function LinearRandom(const x : double; const SeedOffset : QWord = 0) : double; overload;
    function LinearRandom(const Dim : array of double; const SeedOffset : QWord = 0) : double; overload;
    function PerlinNoise(const Dim : array of double; const SeedOffset : QWord = 0) : double;
    procedure RandomVector(const Dim : array of Double; var Vector : array of Double; const SeedOffset : QWord = 0); overload;
    procedure RandomVector(const Dim : array of Integer; var Vector : array of Double; const SeedOffset : QWord = 0); overload;
    constructor Create(const InitSeed : QWord);
    destructor Destroy; override;
  end;

function CreateConstRandomCacheKey(const i : integer;
  const SeedOffset : QWord) : TConstRandomCacheKey; inline;

implementation

{$RangeChecks off}

function PowerMod(const x, n, m : QWord) : QWord;
begin
  if n = 0 then
     exit(1);
  if (n and 1) = 1 then
    exit(sqr(PowerMod(x, n shr 1, m)) mod m * x mod m)
  else
    exit(sqr(PowerMod(x, n shr 1, m)) mod m);
end;

function CreateConstRandomCacheKey(const i : integer;
  const SeedOffset : QWord) : TConstRandomCacheKey;
begin
  Result.i := i;
  Result.SeedOffset := SeedOffset;
end;

{ TRandomGenerator }

function TRandomGenerator.ConstRandom(const i : integer; const SeedOffset : QWord) : double;
var
  p, b : integer;
begin
  if Cache.GetItem(CreateConstRandomCacheKey(i, SeedOffset), Result{%H-}) then
    exit;
  if i < 0 then
  begin
    b := -i;
    p := 397;
  end
  else
  begin
    b := i;
    p := 383;
  end;
  Result := PowerMod(Seed + SeedOffset + PowerMod(b + 3, p, 4294967161),
    163, 4294967291) / 4294967291;
  Cache.AddItem(CreateConstRandomCacheKey(i, SeedOffset), Result);
end;

function TRandomGenerator.ConstRandom(const Dim: array of Integer; const SeedOffset: QWord): double;
var
  i, c : Integer;
  r : Double;
begin
  c := length(Dim);
  r := 1;
  for i := 0 to c - 1 do
    r += ConstRandom(Dim[i], SeedOffset+ExampleSeedOffset[0] + i);
  Result := ConstRandom(floor64(r * 4294967311), SeedOffset);
end;

function TRandomGenerator.RandomAngle(const x: double; const SeedOffset: QWord): double;
var
  fx : double;
  x0 : integer;
  a, b : double;
begin
  x0 := floor(x);
  fx := x - x0;
  a := ConstRandom(x0, SeedOffset);
  b := ConstRandom(x0 - 1, SeedOffset);
  if (a - b) > 0.5 then
    b += 1
  else if (b - a) > 0.5 then
    a += 1;
  Result := 2 * pi * (a * fx + b * (1 - fx)) + 2 * pi * floor(a * 100 + b * 10);
end;

function TRandomGenerator.LinearRandom(const x: double; const SeedOffset: QWord
  ): double;
var
  fx : double;
  x0 : integer;
begin
  x0 := floor(x);
  fx := x - x0;
  Result := ConstRandom(x0, SeedOffset) * fx + ConstRandom(x0 - 1, SeedOffset) * (1 - fx);
end;

function TRandomGenerator.LinearRandom(const Dim: array of double;
  const SeedOffset: QWord): double;
var
  fx : array of double;
  x0 : array of integer;
  i, c : integer;
  l : longword;
  d, r : double;
begin
  c := Length(Dim);
  SetLength(x0{%H-}, c);
  SetLength(fx{%H-}, c);
  for i := 0 to c - 1 do
  begin
    x0[i] := floor(Dim[i]);
    fx[i] := Dim[i] - x0[i];
  end;

  Result := 0;
  for l := 0 to (1 shl c) - 1 do
  begin
    d := 1;
    for i := 0 to c - 1 do
      if l and (1 shl i) = 0 then
        d *= fx[i]
      else
        d *= (1 - fx[i]);

    r := 1;
    for i := 0 to c - 1 do
      r += ConstRandom(x0[i] - (l shr i and 1), ExampleSeedOffset[0] + i);
    r := ConstRandom(floor64(r * 4294967311), SeedOffset);

    Result += r * d;
  end;

  SetLength(x0, 0);
  SetLength(fx, 0);
end;

function TRandomGenerator.PerlinNoise(const Dim: array of double;
  const SeedOffset: QWord): double;
var
  Coords : array of Integer;    
  fraqs : array of Double;
  c : Integer;

  function dotGridGradient(const Mask : QWord) : Double;
  type
    TQWordSet = set of 0..63;
    PQWordSet = ^TQWordSet;
  var
    pqs : PQWordSet;
    i : Integer;
    Gradient : array of Double;
    Coords2 : array of Integer;
  begin
    SetLength(Gradient{%H-}, c);
    SetLength(Coords2{%H-}, c);
    pqs := Pointer(@Mask);
    for i := 0 to c-1 do
      Coords2[i] := Coords[i] + ifthen(i in pqs^, 1, 0);
    RandomVector(Coords2, Gradient, SeedOffset);
    Result := 0;
    for i := 0 to c-1 do
      Result += (Dim[i] - Coords2[i]) * sin(pi*Gradient[i]);
    SetLength(Gradient, 0);
    SetLength(Coords2, 0);
  end;

var
  Mask : QWord;

function Merge(const Depth : Integer) : Double;
var
  a, b : Double;
begin
  if Depth = 0 then
  begin
     a := dotGridGradient(PostInc(Mask));
     b := dotGridGradient(PostInc(Mask))
  end
  else
  begin
    a := Merge(Depth-1);
    b := Merge(Depth-1);
  end;
  Result := (b - a) * (3.0 - fraqs[Depth] * 2.0) * sqr(fraqs[Depth]) + a;
end;

var
  i : Integer;
begin
  c := Length(Dim);
  SetLength(Coords{%H-}, c);
  SetLength(fraqs{%H-}, c);

  for i := 0 to c-1 do
  begin
    Coords[i] := floor(Dim[i]);
    fraqs[i] := Dim[i]-Coords[i];
  end;

  Mask := 0;
  Result := Merge(c-1);

  SetLength(Coords, 0);
  SetLength(fraqs, 0);
end;

procedure TRandomGenerator.RandomVector(const Dim: array of Double;
  var Vector: array of Double; const SeedOffset: QWord);
var
  i, c : Integer;
  d : Double;
  v : Integer;
begin
  c := Min(Length(Dim), Length(Vector));
  d := 0;
  v := Floor(LinearRandom(Dim, SeedOffset)*ExampleSeedOffset[0]);
  for i := 0 to c-1 do
  begin
    Vector[i] := ConstRandom(v, SeedOffset+i)*2-1;
    d += sqr(Vector[i]);
  end;
  d := sqrt(d);
  for i := 0 to c-1 do
    Vector[i] /= d;
end;

procedure TRandomGenerator.RandomVector(const Dim: array of Integer;
  var Vector: array of Double; const SeedOffset: QWord);
var
  i, c : Integer;
  d : Double;
  v : Integer;
begin
  c := Min(Length(Dim), Length(Vector));
  d := 0;
  v := Floor(ConstRandom(Dim, SeedOffset)*ExampleSeedOffset[0]);
  for i := 0 to c-1 do
  begin
    Vector[i] := ConstRandom(v, SeedOffset+i)*2-1;
    d += sqr(Vector[i]);
  end;
  d := sqrt(d);
  for i := 0 to c-1 do
    Vector[i] /= d;
end;

constructor TRandomGenerator.Create(const InitSeed : QWord);
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
