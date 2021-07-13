unit DoubleInt;

{$Mode ObjFpc}
{$ModeSwitch AdvancedRecords} 

interface

type

    TQWordAbstraction = record
    public
        Value : QWord;

        class operator := (const q : QWord) : TQWordAbstraction;
    
        function Lo : LongWord;
        function Hi : LongWord;

        procedure SetLo(const AValue : LongWord);
        procedure SetHi(const AValue : LongWord);

        function GetBit(const Index : PtrUInt) : Boolean;
        function Product(const a, b : TQWordAbstraction) : TQWordAbstraction;
        function Sum(const a, b : TQWordAbstraction) : QWord;
        function AddQWord(const x : QWord) : QWord;
        procedure Zero;
    end;
    
    {Warning: only for unsigned!}
    
    generic TDoubleInt<TInt> = record
        Integers : array[0..1] of TInt;

        procedure Zero;  
        function AddQWord(const AValue : QWord) : QWord;
        procedure ShrOne; inline;  
        function ReturnShrOne : TDoubleInt;
        procedure ShlOne; inline;  
        function ReturnShlOne : TDoubleInt;

        function Add(const x : TDoubleInt) : QWord;
        function Subtrack(const x : TDoubleInt) : QWord;

        class operator := (const AValue : QWord) : TDoubleInt;
        class operator := (const AValue : TDoubleInt) : TInt; 
        class operator := (const AValue : TInt) : TDoubleInt;
        class operator +(const a,b : TDoubleInt) : TDoubleInt; 
        class operator -(const a,b : TDoubleInt) : TDoubleInt; 
        class operator *(const a,b : TDoubleInt) : TDoubleInt; 
        class operator div (const a : TDoubleInt; const b : TDoubleInt) : TDoubleInt; 
        class operator mod (const a : TDoubleInt; const b : TDoubleInt) : TDoubleInt; 

        class operator not (const x : TDoubleInt) : TDoubleInt;
        class operator and (const a, b : TDoubleInt) : TDoubleInt;
        class operator or (const a, b : TDoubleInt) : TDoubleInt;
        class operator xor (const a, b : TDoubleInt) : TDoubleInt;
        class operator in (const a : TDoubleInt; const b : QWord) : Boolean;
        
        class operator >(const a,b : TDoubleInt) : Boolean; 
        class operator <(const a,b : TDoubleInt) : Boolean; 
        class operator >=(const a,b : TDoubleInt) : Boolean; 
        class operator <=(const a,b : TDoubleInt) : Boolean; 
        class operator =(const a,b : TDoubleInt) : Boolean; 
        class operator <>(const a,b : TDoubleInt) : Boolean; 

        class operator shl (const a : TDoubleInt; const b : PtrInt) : TDoubleInt;
        class operator shr (const a : TDoubleInt; const b : PtrInt) : TDoubleInt;

        ///Returns rest less than $FFFFFFFF
        function Sum(const a, b : TDoubleInt) : QWord;
        function SumHalf(const a : TDoubleInt; const b : TInt) : QWord;
        ///Returns rest (it is possible to check if it was overflowed)
        function Difference(const a, b : TDoubleInt) : QWord;
        function Product(const a, b : TDoubleInt; const NeedResult : Boolean = True) : TDoubleInt;

        class procedure DivMod(const a : TDoubleInt; const b : TDoubleInt; out Quotient, Modulo : TDoubleInt; const NeedQuotient : Boolean = true); static;
        class function PowerMod(const x, n, m : TInt) : TInt; static;

        function MoveBitsLeft(const Offset : PtrInt) : TDoubleInt;
        function MoveBitsRight(const Offset : PtrInt) : TDoubleInt;
        
        function CompareTo(const x : TDoubleInt) : Integer;

        procedure SetBit(const Index : Integer; const AValue : Boolean);
        function GetBit(const Index : Integer) : Boolean;

        function ToBinaryString : AnsiString;
        procedure FromBinaryString(const s : AnsiString);
        
        function Lo : TInt;
        function Hi : TInt;
        procedure SetLo(const AValue : TInt);
        procedure SetHi(const AValue : TInt);
    end;

    { TSignedInt }

    generic TSignedInt<TUnsignedType> = record
        UnsignedValue :  TUnsignedType;
        procedure Assign(const Buf; Size : PtrUInt; const Signed : Boolean);
        function Negative : Boolean;
        function Sign : Integer;
        function AbsoluteValue : TUnsignedType;
        function CompareTo(const x : TSignedInt) : Integer;
    
        class operator := (const AValue : Int64) : TSignedInt;
        class operator := (const AValue : QWord) : TSignedInt;
        class operator := (const AValue : TUnsignedType) : TSignedInt;
        class operator +(const a,b : TSignedInt) : TSignedInt; 
        class operator -(const a,b : TSignedInt) : TSignedInt; 
        class operator -(const a : TSignedInt) : TSignedInt; 
        class operator *(const a,b : TSignedInt) : TSignedInt; 
        class operator div (const a, b : TSignedInt) : TSignedInt; 
        class operator mod (const a, b : TSignedInt) : TSignedInt; 
        class procedure DivMod(const a : TSignedInt; const b : TSignedInt; out Quotient, Modulo : TSignedInt; const NeedQuotient : Boolean = true); static;
        
        class operator >(const a,b : TSignedInt) : Boolean; 
        class operator <(const a,b : TSignedInt) : Boolean; 
        class operator >=(const a,b : TSignedInt) : Boolean; 
        class operator <=(const a,b : TSignedInt) : Boolean; 
        class operator =(const a,b : TSignedInt) : Boolean; 
        class operator <>(const a,b : TSignedInt) : Boolean; 
    end;
    
    uInt128 = specialize TDoubleInt<TQWordAbstraction>; 
    uInt256 = specialize TDoubleInt<uInt128>; 
    uInt512 = specialize TDoubleInt<uInt256>; 
    uInt1024 = specialize TDoubleInt<uInt512>; 
    uInt2048 = specialize TDoubleInt<uInt1024>; 
    uInt4096 = specialize TDoubleInt<uInt2048>; 
    uInt8192 = specialize TDoubleInt<uInt4096>; 
    uInt16384 = specialize TDoubleInt<uInt8192>; 
    uInt32768 = specialize TDoubleInt<uInt16384>; 
    uInt65536 = specialize TDoubleInt<uInt32768>; 

    Int128 = specialize TSignedInt<uInt128>;
    Int256 = specialize TSignedInt<uInt256>;
    Int512 = specialize TSignedInt<uInt512>;
    Int1024 = specialize TSignedInt<uInt1024>;
    Int2048 = specialize TSignedInt<uInt2048>;
    Int4096 = specialize TSignedInt<uInt4096>;
    Int8192 = specialize TSignedInt<uInt8192>;
    Int16384 = specialize TSignedInt<uInt16384>;
    Int32768 = specialize TSignedInt<uInt32768>;
    Int65536 = specialize TSignedInt<uInt65536>;

implementation

{$RangeChecks Off}

class operator TQWordAbstraction.:= (const q : QWord) : TQWordAbstraction;
begin
    Result.Value := q;
end;

function TQWordAbstraction.Lo : LongWord;
begin
    Result := Value and $FFFFFFFF;
end;

function TQWordAbstraction.Hi : LongWord;
begin
    Result := Value shr 32;
end;

procedure TQWordAbstraction.SetLo(const AValue : LongWord);
begin
    Value := Value and $FFFFFFFF00000000 or AValue;
end;

procedure TQWordAbstraction.SetHi(const AValue : LongWord);
begin
    Value := Value and $00000000FFFFFFFF or (QWord(AValue) shl 32);
end;

function LongWordProduct(const a, b : LongWord; out res : LongWord) : LongWord;
var
    q : QWord;
begin
    q := QWord(a)*QWord(b);
    res := q and $FFFFFFFF;
    Result := q shr 32;
end;

function TQWordAbstraction.Product(const a, b : TQWordAbstraction) : TQWordAbstraction;
var
    r0, r1, r2 : LongWord;
    t1, t2 : LongWord;
    q1 : QWord;
    l : LongWord;
begin
    r0 := LongWordProduct(a.Lo, b.Lo, l);
    SetLo(l);

    r1 := LongWordProduct(a.Lo, b.Hi, t1);
    r2 := LongWordProduct(a.Hi, b.Lo, t2);
    q1 := QWord(t1) + QWord(t2) + r0;
    setHi(q1 and $FFFFFFFF);
    q1 := q1 shr 32 + r1 + r2;

    Result.Value := a.Hi*b.Hi + q1;
end;

function TQWordAbstraction.Sum(const a, b : TQWordAbstraction) : QWord;
begin
    Value := QWord(a.Value+b.Value);
    if Value < a.Value then
        Result := 1
    else
        Result := 0;
end;

function TQWordAbstraction.AddQWord(const x : QWord) : QWord;
var
    q : TQWordAbstraction;
begin
    q.Value := x;
    Result := Sum(Self, q);
end;

function TQWordAbstraction.GetBit(const Index : PtrUInt) : Boolean;
begin
    Result := (1 shl Index) and Value <> 0;
end;

procedure TQWordAbstraction.Zero;
begin
    Value := 0;
end;

procedure TDoubleInt.Zero;
var
    b : PByte;
    i : PtrUInt;
begin
    b := Pointer(@Self);
    for i := 0 to 2*SizeOf(TInt)-1 do
       b[i] := 0; 
end;

procedure TDoubleInt.ShrOne;
begin
    self := ReturnShrOne;
end;

function TDoubleInt.ReturnShrOne : TDoubleInt;
var
    Q, RP : PQWord;
    t, r : QWord;
    i : PtrUInt;
begin
    r := 0;
    Q := @Self;
    RP := @Result;
    for i := SizeOf(TInt) shr 2 -1 downto 0 do
    begin
      t := Q[i];
      RP[i] := (Q[i] shr 1) or r;  
      r := (t and 1) shl 63;
    end;
end;

procedure TDoubleInt.ShlOne;
begin
    self := ReturnShlOne;
end;

function TDoubleInt.ReturnShlOne : TDoubleInt;
var
    Q, RP : PQWord;
    t, r : QWord;
    i : PtrUInt;
begin
    r := 0;
    Q := @Self;
    RP := @Result;
    for i := 0 to SizeOf(TInt) shr 2 -1 do
    begin
      t := Q[i];
      RP[i] := (Q[i] shl 1) or r;  
      r := (t and (1 shl 63)) shr 63;
    end;
end;

function TDoubleInt.AddQWord(const AValue : QWord) : QWord;
var
    lc : PLongWord;
    i : PtrUInt;
begin
    Result := AValue;
    lc := @Self;
    for i := 0 to SizeOf(TInt) shr 1 -1 do
    begin
        Result := Result + lc[i];
        lc[i] := Result and LongWord(-1);
        Result := Result shr 32;
        if Result = 0 then
            Exit;
    end;
end;

function TDoubleInt.SumHalf(const a : TDoubleInt; const b : TInt) : QWord;
begin
    Result := Integers[1].AddQWord(Integers[0].Sum(a.Integers[0], b));
end;

function TDoubleInt.Sum(const a, b : TDoubleInt) : QWord;
var
    la, lb, lc : PLongWord;
    i : PtrUInt;
begin
    Result := 0;
    la := @a;
    lb := @b;
    lc := @Self;
    for i := 0 to SizeOf(TInt) shr 1 -1 do
    begin
        Result := la[i] + lb[i] + Result;
        lc[i] := Result and LongWord(-1);
        Result := Result shr 32;
    end;
end;

function TDoubleInt.Difference(const a, b : TDoubleInt) : QWord;
begin
    Result := Sum(a, not b);
    Result += AddQWord(1);
end;

function TDoubleInt.Add(const x : TDoubleInt) : QWord;
begin
    Exit(Sum(Self, x));
end;

function TDoubleInt.Subtrack(const x : TDoubleInt) : QWord;
begin
    Result := Add(not x);
    Result += AddQWord(1);
end;

function TDoubleInt.CompareTo(const x : TDoubleInt) : Integer;
var
    b, bx : PQWord;
    i : PtrUInt;
begin
    b := @Self;
    bx := @x;
    Result := 0;
    for i := SizeOf(TInt) shr 2 -1 downto 0 do
    begin
        if b[i] > bx[i] then
            Exit(1)
        else if b[i] < bx[i] then
            Exit(-1);
    end;
end;

function TDoubleInt.Product(const a, b : TDoubleInt; const NeedResult : Boolean) : TDoubleInt; 
var
    r0, r1, r2 : TInt;
    t1, t2 : TInt;
    q1 : QWord;
begin
    r0 := Integers[0].Product(a.Integers[0], b.Integers[0]);

    r1 := t1.Product(a.Integers[0], b.Integers[1]);
    r2 := t2.Product(a.Integers[1], b.Integers[0]);

    q1 := Integers[1].Sum(t1, t2);
    q1 += Integers[1].Sum(Integers[1], r0);

    if NeedResult then
    begin
        Result.Integers[1] := Result{%H-}.Integers[0].Product(a.Integers[1], b.Integers[1]);
        Result.AddQWord(q1);
        Result.SumHalf(Result, r1);
        Result.SumHalf(Result, r2);
    end;
end;

class procedure TDoubleInt.DivMod(const a : TDoubleInt; const b : TDoubleInt; out Quotient, Modulo : TDoubleInt; const NeedQuotient : Boolean); static;
var
    bb : TDoubleInt;
    i, c, size, offset : PtrUInt;
begin
    size := SizeOf(b) shl 3;
    c := size;
    while (c > 0) and (not b.GetBit(c-1)) do
        Dec(c);

    offset := size-c;
    if c = size then
        bb := b
    else
    begin
        bb.Zero;
        for i := 0 to c-1 do
            bb.SetBit(i+offset, b.GetBit(i));
    end;
    
    Modulo := a;
    if NeedQuotient then
        Quotient.Zero;
    for i := offset downto 0 do
    begin    
        if Modulo >= bb then
        begin 
            if NeedQuotient then
                Quotient.SetBit(i, True);
            Modulo.Subtrack(bb);
        end;
        bb.ShrOne;
    end;
end;

class function TDoubleInt.PowerMod(const x, n, m : TInt) : TInt;
var
    i, c : PtrUInt;
    w : TDoubleInt;
    m2, x2 : TDoubleInt;
    multipled : Boolean;
begin
    m2 := TDoubleInt(m);
    x2 := TDoubleInt(x) mod m2;
    c := SizeOf(TInt) shl 3;
    w := 1;
    multipled := False;
    for i := c-1 downto 0 do
        if n.GetBit(i) then
        begin    
            multipled := True;
            w := (((w * w) mod m2) * x2) mod m2;
        end else if multipled then
            w := (w * w) mod m2;
    Result := w.Integers[0];
end;

function TDoubleInt.MoveBitsLeft(const Offset : PtrInt) : TDoubleInt;
var
    i, b, e : PtrInt;
begin
    if Offset > 0 then
    begin
        b := 0;
        e := SizeOf(TInt) shl 4 - Offset;
    end else if Offset < 0 then
    begin
        b := -Offset;
        e := SizeOf(TInt) shl 4;
    end else
        Exit(Self);
    Result.Zero;
    for i := b to e-1 do
        Result.SetBit(i+Offset, self.GetBit(i));
end;

function TDoubleInt.MoveBitsRight(const Offset : PtrInt) : TDoubleInt;
begin
    Exit(MoveBitsLeft(-Offset));
end;

procedure TDoubleInt.SetBit(const Index : Integer; const AValue : Boolean);
var
    v : QWord;
begin
    if AValue then
        v := QWord(1) shl (Index and 63)
    else
        v := 0;
    PQWord(@Self)[Index shr 6] := PQWord(@Self)[Index shr 6] and (not (QWord(1) shl (Index and 63))) or v;
end;

function TDoubleInt.GetBit(const Index : Integer) : Boolean;
begin
    Result := (PQWord(@Self)[Index shr 6] and (QWord(1) shl (Index and 63))) <> 0;
end;

function TDoubleInt.Lo : TInt;
begin
    Result := Integers[0];
end;

function TDoubleInt.Hi : TInt;
begin
    Result := Integers[1];
end;

procedure TDoubleInt.SetLo(const AValue : TInt);
begin
    Integers[0] := AValue;
end;

procedure TDoubleInt.SetHi(const AValue : TInt);
begin
    Integers[1] := AValue;
end;

function TDoubleInt.ToBinaryString : AnsiString;
var
    i, c : PtrUInt;
begin
    c := SizeOf(TInt) shl 4;
    SetLength(Result{%H-}, c);
    for i := 0 to c-1 do
        if GetBit(i) then
            Result[c-i] := '1'
        else
            Result[c-i] := '0';
end;

procedure TDoubleInt.FromBinaryString(const s : AnsiString);
var
    i, c, cs : PtrUInt;
begin
    c := SizeOf(TInt) shl 4;
    cs := Length(s);
    for i := 0 to c-1 do
        SetBit(i, (cs-i > 0) and (s[cs-i] = '1'));
end;

class operator TDoubleInt.+(const a,b : TDoubleInt) : TDoubleInt; 
begin
    Result.Sum(a, b);
end;

class operator TDoubleInt.-(const a,b : TDoubleInt) : TDoubleInt; 
begin
    Result.Difference(a, b);
end;

class operator TDoubleInt.*(const a,b : TDoubleInt) : TDoubleInt; 
begin
   Result.Product(a, b, False);
end;

class operator TDoubleInt.div (const a : TDoubleInt; const b : TDoubleInt) : TDoubleInt; 
var 
    m : TDoubleInt;
begin
    DivMod(a, b, Result, m, True);
end;

class operator TDoubleInt.mod (const a : TDoubleInt; const b : TDoubleInt) : TDoubleInt; 
var 
    q : TDoubleInt;
begin
    if a >= b then
        DivMod(a, b, q, Result, False)
    else
        Exit(a);
end;

class operator TDoubleInt.:= (const AValue : TDoubleInt) : TInt;
begin
    Result := AValue.Lo;
end;

class operator TDoubleInt.:= (const AValue : QWord) : TDoubleInt;
begin
    Result.Zero;
    PQWord(@Result)^ := AValue;
end;

class operator TDoubleInt.:= (const AValue : TInt) : TDoubleInt;
begin
    Result.Integers[1] := 0;
    Result.Integers[0] := AValue;
end;

class operator TDoubleInt.>(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) > 0;
end;

class operator TDoubleInt.<(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) < 0;
end;

class operator TDoubleInt.>=(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) >= 0;
end;

class operator TDoubleInt.<=(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) <= 0;
end;

class operator TDoubleInt.=(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) = 0;
end;

class operator TDoubleInt.<>(const a,b : TDoubleInt) : Boolean; 
begin
    Result := a.CompareTo(b) <> 0;
end;

class operator TDoubleInt.not (const x : TDoubleInt) : TDoubleInt;
var
    Q1, Q2 : PQWord;
    i :PtrUInt;
begin
    Q1 := @x;
    Q2 := @Result;
    for i := 0 to SizeOf(TInt) shr 2-1 do
        Q2[i] := not Q1[i];
end;

class operator TDoubleInt.and (const a, b : TDoubleInt) : TDoubleInt;
var
    i : PtrUInt;
    qa, qb, qr : PQWord;
begin
    qa := @a;
    qb := @b;
    qr := @Result;
    for i := 0 to SizeOf(TInt) shr 2-1 do
        qr[i] := qa[i] and qb[i];
end;

class operator TDoubleInt.or (const a, b : TDoubleInt) : TDoubleInt;
var
    i : PtrUInt;
    qa, qb, qr : PQWord;
begin
    qa := @a;
    qb := @b;
    qr := @Result;
    for i := 0 to SizeOf(TInt) shr 2-1 do
        qr[i] := qa[i] or qb[i];
end;

class operator TDoubleInt.xor (const a, b : TDoubleInt) : TDoubleInt;
var
    i : PtrUInt;
    qa, qb, qr : PQWord;
begin
    qa := @a;
    qb := @b;
    qr := @Result;
    for i := 0 to SizeOf(TInt) shr 2-1 do
        qr[i] := qa[i] xor qb[i];
end;

class operator TDoubleInt.in (const a : TDoubleInt; const b : QWord) : Boolean;
begin
    Result := a.GetBit(b);
end;

class operator TDoubleInt.shl (const a : TDoubleInt; const b : PtrInt) : TDoubleInt;
begin
    if b = 1 then
        Exit(a.ReturnShlOne);
    Exit(a.MoveBitsLeft(b));
end;

class operator TDoubleInt.shr (const a : TDoubleInt; const b : PtrInt) : TDoubleInt;
begin
    if b = 1 then
        Exit(a.ReturnShrOne);
    Exit(a.MoveBitsRight(b));
end;

procedure TSignedInt.Assign(const Buf; Size : PtrUInt; const Signed : Boolean);
var
    b, bu : PByte;
    i : PtrUInt;
begin
    if Size > SizeOf(TUnsignedType) then
        Size := SizeOf(TUnsignedType);

    UnsignedValue.Zero;
    b := @buf;
    bu := @UnsignedValue;
    if (b[Size-1] and %10000000 <> 0) and (SizeOf(buf) < SizeOf(TUnsignedType)) then
        UnsignedValue := not UnsignedValue;
    for i := 0 to Size-1 do
        bu[i] := b[i];
end;

function TSignedInt.Negative : Boolean;
begin
    Result := UnsignedValue.GetBit(SizeOf(TUnsignedType) shl 3 -1);
end;

function TSignedInt.Sign: Integer;
begin
  if Negative then
     Exit(-1);
  if Self > 0 then
    Exit(1);
  Exit(0);
end;

function TSignedInt.AbsoluteValue : TUnsignedType;
begin
    if Negative then
        Result := (-self).UnsignedValue
    else
        Result := Self.UnsignedValue;
end;

function TSignedInt.CompareTo(const x : TSignedInt) : Integer;
var
    ss, sx : Boolean;
begin
    ss := Negative;
    sx := x.Negative;
    if sx <> ss then
    begin
        if ss then
            Exit(-1)
        else
            Exit(1);
    end;
    Result := UnsignedValue.CompareTo(x.UnsignedValue);
end;

class operator TSignedInt.:= (const AValue : Int64) : TSignedInt;
begin
    Result.Assign(AValue, SizeOf(AValue), True);
end;

class operator TSignedInt.:= (const AValue : QWord) : TSignedInt;
begin
    Result.Assign(AValue, SizeOf(AValue), False);
end;

class operator TSignedInt.:= (const AValue : TUnsignedType) : TSignedInt;
begin
    Result.Assign(AValue, SizeOf(AValue), False);
end;

class operator TSignedInt.+(const a,b : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := a.UnsignedValue+b.UnsignedValue;
end;

class operator TSignedInt.-(const a,b : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := a.UnsignedValue-b.UnsignedValue;
end;

class operator TSignedInt.-(const a : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := not a.UnsignedValue;
    Result.UnsignedValue.AddQWord(1);
end;

class operator TSignedInt.*(const a,b : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := a.AbsoluteValue * b.AbsoluteValue;
    if a.Negative xor b.Negative then
        Result := -Result;
end;

class operator TSignedInt.div (const a, b : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := a.AbsoluteValue div b.AbsoluteValue;
    if a.Negative xor b.Negative then
        Result := -Result;
end;

class operator TSignedInt.mod (const a, b : TSignedInt) : TSignedInt; 
begin
    Result.UnsignedValue := a.AbsoluteValue mod b.AbsoluteValue;
    if a.Negative then
        Result := -Result;
end;

class procedure TSignedInt.DivMod(const a : TSignedInt; const b : TSignedInt; out Quotient, Modulo : TSignedInt; const NeedQuotient : Boolean = true); static;
begin
    TUnsignedType.DivMod(a.AbsoluteValue, b.AbsoluteValue, Quotient.UnsignedValue, Modulo.UnsignedValue, NeedQuotient);
    if a.Negative then
        Modulo := -{%H-}Modulo;    
    if NeedQuotient then
        if a.Negative xor b.Negative then
            Quotient := -{%H-}Quotient;
end;

class operator TSignedInt.>(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) > 0;
end;

class operator TSignedInt.<(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) < 0;
end;

class operator TSignedInt.>=(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) >= 0;
end;

class operator TSignedInt.<=(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) <= 0;
end;

class operator TSignedInt.=(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) = 0;
end;

class operator TSignedInt.<>(const a,b : TSignedInt) : Boolean; 
begin
    Result := a.CompareTo(b) <> 0;
end;

end.
