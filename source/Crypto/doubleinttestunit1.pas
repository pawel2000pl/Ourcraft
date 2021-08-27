unit DoubleIntTestUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DoubleInt, PrimeLib, RSAKeys, RSACodeStream, AnyBase2OtherBase, RSAMultiKeyGenerator;

type

  { TDoubleIntTestCase }

  TDoubleIntTestCase= class(TTestCase)
  published
    procedure TimeTest;

    procedure AdditionTest;
    procedure SubtractionTest;
    procedure ModuloTest;

    procedure MixedTest;
  end;

  { TPrimeLibTestCase }

  generic TPrimeLibTestCase<TuInt> = class(TTestCase)
  type
      TPrimeClass = specialize TPrimeTests<TuInt>;
  published
      procedure InvertModulo;
  end;

  { TRSATestCase }

  generic TRSATestCase<TUsedType> = class(TTestCase)
  private
      d, e, n : TUsedType;
      procedure AssertKeys; overload;
      procedure AssertKeys(const td, te, tn : TUsedType); overload;
  published
    procedure GeneratingKeys;
    procedure GeneratingKeysMultithread;
    procedure StreamTest;
    procedure MultikeyGenerator;
  end;

implementation

function PowerMod(const x, n, m : QWord) : QWord;
begin
  if n = 0 then
     exit(1);
  if (n and 1) = 1 then
    exit(sqr(PowerMod(x, n shr 1, m)) mod m * x mod m)
  else
    exit(sqr(PowerMod(x, n shr 1, m)) mod m);
end;


generic procedure TestAdditions<TIntType>(assert : TAssert; const Count : Integer = $FFFF);
var
  i : Integer;
  t1, t2 : QWord;
  i1, i2, i3 : TIntType;
begin
  for i := 0 to Count-1 do
  begin
    t1 := random($FFFFFFFF);
    t2 := random($FFFFFFFF);
    i1 := t1;
    i2 := t2;
    i3 := t1 + t2;
    assert.AssertTrue('AddingTest - size: ' + IntToStr(8*SizeOf(TIntType)), i3 = i1 + i2);
  end;
end;

generic procedure TestSubtraction<TIntType>(assert : TAssert; const Count : Integer = $FFFF);
var
  i : Integer;
  t1, t2 : int64;
  i1, i2, i3 : TIntType;
begin
  for i := 0 to Count-1 do
  begin
    t1 := random($FFFFFFFFFFFF);
    t2 := random($FFFFFFFFFFFF);
    i1 := t1;
    i2 := t2;
    i3 := t1 - t2;
    assert.AssertTrue('SubtractionTest - size: ' + IntToStr(8*SizeOf(TIntType)), i3 = i1 - i2);
  end;
end;

generic procedure RecurencyTest<TIntType>(assert : TAssert; const Count : Integer = $F; const MaxDepth : Integer = $F);
var
  HalfValue : TIntType;

        procedure Test(var Value : TIntType; const Depth : Integer);
        var
            ChangedValue : TIntType;
            OriginalValue : TIntType;
            ID : Byte;
        begin
          if Depth > MaxDepth then
             Exit;
          OriginalValue:=Value;
          ChangedValue := Random($FFFF)+1;
          ID := Random(3);
          case ID of
               0: Value := Value + ChangedValue;
               1: Value := Value - ChangedValue;
               2: Value := Value * ChangedValue;
          end;
          Test(Value, Depth+1);
          case ID of
               0: Value := Value - ChangedValue;
               1: Value := Value + ChangedValue;
               2: Value := Value div ChangedValue;
          end;
          if Value <> OriginalValue then
             assert.Fail('RecurencyTest, depth: ' + IntToStr(Depth) + ' size: ' + IntToStr(8*SizeOf(TIntType)));
        end;

var
    i : Integer;
    Value : TIntType;
begin
  FillByte(HalfValue{%H-}, SizeOf(HalfValue), 0);
  FillByte(HalfValue, SizeOf(HalfValue) div 2, $FF);
  for i := 0 to Count -1 do
  begin
    Value:=HalfValue;
    Test(Value, 0);
  end;
end;

generic procedure TestModulo<TIntType>(assert : TAssert; const Count : Integer = $FF);
var
    i, j : Integer;
    a, b, c : TIntType;
begin
  for i := 0 to Count-1 do
  begin
    a := 1;
    b := 1;
    c := 1;
    for j := 0 to SizeOf(TIntType) div 2 -1 do
    begin
         a := (a shl 8) or Random($FF);
         b := (b shl 8) or Random($FF);
         c := (c shl 8) or Random($FF);
    end;                                
    b := b shr (SizeOf(TIntType)*2 + 1) +1;
    c := c shr (SizeOf(TIntType)*2 + 8);
    if b <= c then
       c := c mod b;
    assert.AssertTrue('ModuloTest - size: ' + IntToStr(8*SizeOf(TIntType)), (a*b+c) mod b = c);
  end;
end;

{ TPrimeLibTestCase }

procedure TPrimeLibTestCase.InvertModulo;
begin
    if (TPrimeClass.InvertModulo(3, 11) <> 4) then
       Fail('Invert modulo does not work');
end;

{ TRSATestCase }

procedure TRSATestCase.AssertKeys;
begin
  AssertKeys(d, e, n);
end;

procedure TRSATestCase.AssertKeys(const td, te, tn: TUsedType);
var
    data : TUsedType;
    Data1, Data2, Data3 : AnsiString;
begin
  data := 10;
  Data1 := Base2Base(data.ToBinaryString, 2, 10);
  data := specialize CodeRSA<TUsedType>(data, te, tn);
  Data2 := Base2Base(data.ToBinaryString, 2, 10);
  data := specialize CodeRSA<TUsedType>(data, td, tn);
  Data3 := Base2Base(data.ToBinaryString, 2, 10);


  if Data1 <> Data3 then
      Fail('Corruptes keys');
  AssertTrue(Data1 <> Data2);
  AssertTrue(Data3 <> Data2);
end;

procedure TRSATestCase.GeneratingKeys;
var
    t : QWord;
begin
    t :=GetTickCount64;
    specialize GenerateRSAKey<TUsedType>(d, e, n);

    writeln('d = ', Base2Base(d.ToBinaryString, 2, 10));
    writeln('e = ', Base2Base(e.ToBinaryString, 2, 10));
    writeln('n = ', Base2Base(n.ToBinaryString, 2, 10));
                                                          
    writeln('Codes for size ' + IntToStr(SizeOf(TUsedType)*8) + ' generated in: ', GetTickCount64 - t);

    AssertKeys;
end;

procedure TRSATestCase.GeneratingKeysMultithread;
var
    t : QWord;
begin
    t :=GetTickCount64;
    specialize GenerateRSAKey<TUsedType>(d, e, n, 4);

    writeln('d = ', Base2Base(d.ToBinaryString, 2, 10));
    writeln('e = ', Base2Base(e.ToBinaryString, 2, 10));
    writeln('n = ', Base2Base(n.ToBinaryString, 2, 10));

    writeln('Multithreaded codes for size ' + IntToStr(SizeOf(TUsedType)*8) + ' generated in: ', GetTickCount64 - t);

    AssertKeys;
end;

procedure TRSATestCase.StreamTest;
const
  TestSize = 16*1024;
var
    t : QWord;
    i : Integer;
    MS1, MS2, MS3 : TMemoryStream;
begin
    specialize GenerateRSAKey<TUsedType>(d, e, n);
    AssertKeys;
    t := GetTickCount64;
    try
      MS1 := TMemoryStream.Create;
      MS2 := TMemoryStream.Create;
      MS3 := TMemoryStream.Create;

      for i := 0 to TestSize-1 do
          MS1.WriteByte(Random($100));

      MS1.Position := 0;

      Writeln('Saved ', specialize CodeStream<TUsedType>(MS1, MS2, MS1.Size, e, n), ' bytes');

      MS2.Position := 0;
      Writeln('Saved ', specialize DeCodeStream<TUsedType>(MS2, MS3, MS2.Size, d, n), ' bytes');

      t :=  GetTickCount64 - t;
      writeln('Code-stream: efficient-level: ', (MS1.Size+MS3.Size)/(t/1000)/1024:4:4, ' kB/s');

      MS1.Position:=0;
      MS3.Position:=0;
      if MS1.Size = MS3.Size then
          for i := 0 to TestSize-1 do
              AssertTrue(MS1.ReadByte = MS3.ReadByte)
          else
              Fail('Size not equal: ' + IntToStr(MS1.Size) + ' vs ' + IntToStr(MS3.Size));

    finally   
      MS1.Free;
      MS2.Free;
      MS3.Free;
    end;
end;

procedure TRSATestCase.MultikeyGenerator;
var
    i : Integer;
    codes : specialize TArrayOfRSAKeySearcher<TUsedType>;
begin
  specialize GenerateRSAKeys<TUsedType>(16, codes);
    for i := 0 to 15 do
    begin
      writeln(i);

      writeln('d = ', Base2Base(codes[i].d.ToBinaryString, 2, 10));
      writeln('e = ', Base2Base(codes[i].e.ToBinaryString, 2, 10));
      writeln('n = ', Base2Base(codes[i].n.ToBinaryString, 2, 10));

      AssertKeys(codes[i].d, codes[i].e, codes[i].n);

      codes[i].Free;
      writeln;
    end;
end;

procedure TDoubleIntTestCase.TimeTest;
var
    t : QWord;
    ba, bb, bc : uInt65536;
begin
    bb := 150;   
    AssertTrue((bb*17+3) mod 17 = 3);

    t := GetTickCount64;
    ba := 10;
    bb := 102;
    bc := 17;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;

    writeln('Time test: ', GetTickCount64-t, ' ms');

    AssertTrue((bb*17+3) mod 17 = 3);
end;

procedure TDoubleIntTestCase.AdditionTest;
begin
  specialize TestAdditions<uInt128>(self);
  specialize TestAdditions<uInt256>(self);
  specialize TestAdditions<uInt512>(self);
  specialize TestAdditions<uInt1024>(self);
  specialize TestAdditions<uInt2048>(self);
  specialize TestAdditions<uInt4096>(self);

  specialize TestAdditions<Int128>(self);
  specialize TestAdditions<Int256>(self);
  specialize TestAdditions<Int512>(self);
  specialize TestAdditions<Int1024>(self);
  specialize TestAdditions<Int2048>(self);
  specialize TestAdditions<Int4096>(self);
end;

procedure TDoubleIntTestCase.SubtractionTest;
begin
  specialize TestSubtraction<Int128>(self);
  specialize TestSubtraction<Int256>(self);
  specialize TestSubtraction<Int512>(self);
  specialize TestSubtraction<Int1024>(self);
  specialize TestSubtraction<Int2048>(self);
  specialize TestSubtraction<Int4096>(self);
end;

procedure TDoubleIntTestCase.ModuloTest;
begin
  specialize TestModulo<uInt128>(self);
  specialize TestModulo<uInt256>(self);
  specialize TestModulo<uInt512>(self);
  specialize TestModulo<uInt1024>(self);
  specialize TestModulo<uInt2048>(self);
  specialize TestModulo<uInt4096>(self);
end;

procedure TDoubleIntTestCase.MixedTest;
begin
  specialize RecurencyTest<Int16384>(self);
  specialize RecurencyTest<Int32768>(self);
  specialize RecurencyTest<Int65536>(self);
end;

initialization
  Randomize;
  RegisterTest(TDoubleIntTestCase);        
  RegisterTest(specialize TPrimeLibTestCase<uInt512>);
  RegisterTest(specialize TRSATestCase<uInt512>);
  RegisterTest(specialize TRSATestCase<uInt1024>);
  RegisterTest(specialize TRSATestCase<uInt2048>);
end.

