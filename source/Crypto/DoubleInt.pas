unit DoubleInt;

{This module was created by Paweł Bielecki}

{$Mode ObjFpc}
{$ModeSwitch AdvancedRecords}
{$Optimization AUTOINLINE}
{$Optimization CONSTPROP}

{$Define MultithreadedProduct}
{$Define FastButConsumingMemoryDivision}

interface

type

    { TQWordAbstraction }

    TQWordOverlay = record
    {$IfDef MULTITHREADEDPRODUCT}
    private
        class function AsyncProduct(P : Pointer) : PtrInt; static; //same as Product()
    {$EndIf}
    public
        Value : QWord;

        class operator := (const q : QWord) : TQWordOverlay;
        class operator := (const q : TQWordOverlay) : QWord;
    
        function Lo : LongWord;
        function Hi : LongWord;

        procedure SetLo(const AValue : LongWord);
        procedure SetHi(const AValue : LongWord);

        function GetBit(const Index : PtrUInt) : Boolean;
        function Product(const a, b : TQWordOverlay) : TQWordOverlay;
        function Sum(const a, b : TQWordOverlay) : QWord;
        function AddQWord(const x : QWord) : QWord;
        procedure Zero;
    end;
    
    {Warning: only for unsigned!}
    
    { TDoubleInt }

    generic TDoubleInt<TInt> = record
    type
        PInt = ^TInt;
        PDoubleInt = ^TDoubleInt; 
        PPDoubleInt = ^PDoubleInt;
    private
        {$IfDef MULTITHREADEDPRODUCT}
        ///Pointer to PPDoubleInt - [0]^ := a; [1]^ := b; [2]^ := a*b; [3]^ := Result;
        class function AsyncProduct(P : Pointer) : PtrInt; static;
        //creates if thread pool is not overloaded - returns 0 if it is
        class function CreateNewProductThread(const a, b, prod, res : PInt) : TThreadID; static;
        {$EndIf}                                                   
        function GetQWords(const Index : PtrUInt): QWord; inline;
        function GetLongWords(const Index : PtrUInt): LongWord; inline;
        function GetWords(const Index : PtrUInt): Word; inline;
        function GetBytes(const Index : PtrUInt): Byte; inline;
        procedure SetQWords(const Index : PtrUInt; const AValue: QWord); inline;
        procedure SetLongWords(const Index : PtrUInt; const AValue: LongWord); inline;
        procedure SetWords(const Index : PtrUInt; const AValue: Word); inline;
        procedure SetBytes(const Index : PtrUInt; const AValue: Byte); inline;
    public
        Integers : array[0..1] of TInt;

        procedure Zero;
        function IsZero : Boolean;

        function AddQWord(const AValue : QWord) : QWord;
        procedure ShrOne(const Output : PDoubleInt);
        procedure ShrOne; overload; inline;
        function ReturnShrOne : TDoubleInt; inline;
        procedure ShlOne(const Output : PDoubleInt);
        procedure ShlOne; overload; inline;
        function ReturnShlOne : TDoubleInt; inline;

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

        ///Returns rest less than $FFFFFFFFFFFFFFFF
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
        
        function GetLo : TInt; inline;
        function GetHi : TInt; inline;
        procedure SetLo(const AValue : TInt); inline;
        procedure SetHi(const AValue : TInt); inline;

        property Lo : TInt read GetLo write SetLo;
        property Hi : TInt read GetHi write SetHi;

        property QWords[const Index : PtrUInt] : QWord read GetQWords write SetQWords;
        property LongWords[const Index : PtrUInt] : LongWord read GetLongWords write SetLongWords;
        property Words[const Index : PtrUInt] : Word read GetWords write SetWords;
        property Bytes[const Index : PtrUInt] : Byte read GetBytes write SetBytes;
    end;

    { TSignedInt }

    generic TSignedInt<TUnsignedType> = record
        UnsignedValue :  TUnsignedType;
        procedure Assign(const Buf; Size : PtrUInt; const Signed : Boolean);
        function Negative : Boolean; inline;
        function Sign : Integer; inline;
        function AbsoluteValue : TUnsignedType; inline;
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

    uInt128 = specialize TDoubleInt<TQWordOverlay>; 
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

{$if FPC_FULLVERSION >= 30200}
generic procedure DivMod<TuInt>(const Dividend, Divisor: TuInt; out Result, Remainder : TuInt); overload;
generic function CopyUnsigned<TFromType, TToType>(const Source : TFromType) : TToType;
{$endif}
                           
{$IfDef MULTITHREADEDPRODUCT}     

const
   IntSizeForMultithreading = 1024; //In Bytes

function CheckIfCanCreateThread : Boolean; //DO NOT USE - ONLY FOR GENERICS
procedure DoneThread(const ID : TThreadID); //DO NOT USE - ONLY FOR GENERICS

{$EndIf}

function Shr3(const x : Int64) : Int64; inline; overload; //ONLY FOR GENERICS
function Shr3(const x : Int32) : Int32; inline; overload; //ONLY FOR GENERICS

implementation

{$IfDef MULTITHREADEDPRODUCT}
var
    CounterLock : TRTLCriticalSection;
    ThreadCount : PtrUInt;

function CheckIfCanCreateThread : Boolean;
begin
    EnterCriticalSection(CounterLock);
    Result := ThreadCount < 15;
    if Result then
       Inc(ThreadCount);
    LeaveCriticalSection(CounterLock);
end;

procedure DoneThread(const ID: TThreadID);
begin
    if ID = 0 then
       Exit;
    WaitForThreadTerminate(ID, 0);
    EnterCriticalSection(CounterLock);
    if ThreadCount > 0 then
       Dec(ThreadCount);
    LeaveCriticalSection(CounterLock);
end;

{$EndIf}

{$RangeChecks Off}

{$if FPC_FULLVERSION >= 30200}

generic procedure DivMod<TuInt>(const Dividend, Divisor: TuInt; out Result, Remainder : TuInt); overload;
begin
     TuInt.DivMod(Dividend, Divisor, Result, Remainder);
end;

generic function CopyUnsigned<TFromType, TToType>(const Source : TFromType) : TToType;
var
    copySize : PtrUInt;
begin
    if SizeOf(TFromType) < SizeOf(TToType) then
       copySize:=SizeOf(TFromType)
    else
       copySize:=SizeOf(TToType);
    Move(Source, Result{%H-}, copySize);
end;

{$endif}

function Shr3(const x : Int64) : Int64; inline; overload;
begin
  if x >= 0 then
     Exit(x shr 3);
  exit((x shr 3) or $E000000000000000);
end;

function Shr3(const x : Int32) : Int32; inline; overload;
begin
  if x >= 0 then
     Exit(x shr 3);
  exit((x shr 3) or $E0000000);
end;

{ TQWordOverlay }

class operator TQWordOverlay.:= (const q : QWord) : TQWordOverlay;
begin
    Result.Value := q;
end;

class operator TQWordOverlay.:=(const q: TQWordOverlay): QWord;
begin
  Exit(q.Value);
end;

function TQWordOverlay.Lo : LongWord;
begin
    Result := Value and $FFFFFFFF;
end;

function TQWordOverlay.Hi : LongWord;
begin
    Result := Value shr 32;
end;

procedure TQWordOverlay.SetLo(const AValue : LongWord);
begin
    Value := Value and $FFFFFFFF00000000 or AValue;
end;

procedure TQWordOverlay.SetHi(const AValue : LongWord);
begin
    Value := Value and $00000000FFFFFFFF or (QWord(AValue) shl 32);
end;

{$IfDef MULTITHREADEDPRODUCT}
class function TQWordOverlay.AsyncProduct(P: Pointer): PtrInt;
type
    PQWordAbstraction = ^TQWordOverlay;
    PPQWordAbstraction = ^PQWordAbstraction;
begin
    PPQWordAbstraction(P)[3]^ := PPQWordAbstraction(P)[2]^.Product(PPQWordAbstraction(P)[0]^, PPQWordAbstraction(P)[1]^);
    Exit(0);
end;
{$EndIf}

function LongWordProduct(const a, b : LongWord; out res : LongWord) : LongWord;
var
    q : QWord;
begin
    q := QWord(a)*QWord(b);
    res := q and $FFFFFFFF;
    Result := q shr 32;
end;

function TQWordOverlay.Product(const a, b : TQWordOverlay) : TQWordOverlay;
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

function TQWordOverlay.Sum(const a, b : TQWordOverlay) : QWord;
begin
    Value := QWord(a.Value+b.Value);
      if Value < a.Value then
        Result := 1
    else
        Result := 0;
end;

function TQWordOverlay.AddQWord(const x : QWord) : QWord;
var
    q : TQWordOverlay;
begin
    q.Value := x;
    Result := Sum(Self, q);
end;

function TQWordOverlay.GetBit(const Index : PtrUInt) : Boolean;
begin
    Result := (1 shl Index) and Value <> 0;
end;

procedure TQWordOverlay.Zero;
begin
    Value := 0;
end;          

{ TDoubleInt }

procedure TDoubleInt.Zero;
var
    i : PtrUInt;
begin
    for i := 0 to SizeOf(TInt) shr 2 -1 do
       PQWord(Pointer(@Self))[i] := 0;
end;

function TDoubleInt.IsZero: Boolean;
var
    i : PtrUInt;
    q : PQWord;
begin
    q := @self;
    for i := 0 to SizeOf(TInt) shr 2 do
       if q[i] <> 0 then
           Exit(False);
    Exit(True);
end;

procedure TDoubleInt.ShrOne(const Output: PDoubleInt);
var
    Q, RP : PQWord;
    t, r : QWord;
    i : PtrUInt;
begin
    r := 0;
    Q := @Self;
    RP := Pointer(Output);
    for i := SizeOf(TInt) shr 2 -1 downto 0 do
    begin
      t := Q[i];
      RP[i] := (Q[i] shr 1) or r;
      r := (t and 1) shl 63;
    end;
end;

procedure TDoubleInt.ShrOne;
begin
  ShrOne(@Self);
end;

function TDoubleInt.ReturnShrOne : TDoubleInt;
begin
    ShrOne(@Result);
end;

procedure TDoubleInt.ShlOne(const Output: PDoubleInt);
var
    Q, RP : PQWord;
    t, r : QWord;
    i : PtrUInt;
begin
    r := 0;
    Q := @Self;
    RP := Pointer(Output);
    for i := 0 to SizeOf(TInt) shr 2 -1 do
    begin
      t := Q[i];
      RP[i] := (Q[i] shl 1) or r;
      r := (t and (1 shl 63)) shr 63;
    end;
end;

procedure TDoubleInt.ShlOne;
begin
  ShlOne(@Self);
end;

function TDoubleInt.ReturnShlOne : TDoubleInt;
begin
    ShlOne(@Result);
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
    {$ifdef MULTITHREADEDPRODUCT}
    ThreadsID : array[0..1] of TThreadID;
    {$EndIf}
begin
    {$ifdef MULTITHREADEDPRODUCT}
    ThreadsID[0] := CreateNewProductThread(@a.Integers[0], @b.Integers[0], @Integers[0], @r0);
    ThreadsID[1] := CreateNewProductThread(@a.Integers[0], @b.Integers[1], @t1, @r1);
                                                                 
    r2 := t2.Product(a.Integers[1], b.Integers[0]);
    DoneThread(ThreadsID[0]);
    DoneThread(ThreadsID[1]);
    {$Else}
    r0 := Integers[0].Product(a.Integers[0], b.Integers[0]);

    r1 := t1.Product(a.Integers[0], b.Integers[1]);
    r2 := t2.Product(a.Integers[1], b.Integers[0]);  
    {$EndIf}

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

{$IfDef MULTITHREADEDPRODUCT}

class function TDoubleInt.AsyncProduct(P: Pointer): PtrInt;
begin
    PPDoubleInt(P)[3]^ := PPDoubleInt(P)[2]^.Product(PPDoubleInt(P)[0]^, PPDoubleInt(P)[1]^);
    Freemem(P);
    Exit(0);
end;

class function TDoubleInt.CreateNewProductThread(const a, b, prod, res: PInt): TThreadID;
var
    Data : ^PInt;
begin
    if (SizeOf(TInt) > IntSizeForMultithreading) and CheckIfCanCreateThread then
    begin
      Data := GetMem(4*SizeOf(PInt));
      Data[0] := a;
      Data[1] := b;
      Data[2] := prod;
      Data[3] := res;
      Exit(BeginThread(@TInt.AsyncProduct, Data));
    end;
    res^ := prod^.Product(a^, b^);
    Exit(0);
end;    
{$EndIf}

function TDoubleInt.GetQWords(const Index : PtrUInt): QWord;
begin
  If Index < SizeOf(TInt) shr 2 then
     Exit(PQWord(@Self)[Index]);
end;              

function TDoubleInt.GetLongWords(const Index : PtrUInt): LongWord;
begin
  If Index < SizeOf(TInt) shr 1 then
     Exit(PLongWord(@Self)[Index]);
end;

function TDoubleInt.GetWords(const Index : PtrUInt): Word;
begin
  If Index < SizeOf(TInt) then
     Exit(PWord(@Self)[Index]);
end;               

function TDoubleInt.GetBytes(const Index : PtrUInt): Byte;
begin
  If Index < SizeOf(TInt) shl 1 then
     Exit(PByte(@Self)[Index]);
end;

procedure TDoubleInt.SetQWords(const Index: PtrUInt; const AValue: QWord);
begin
  If Index < SizeOf(TInt) shr 2 then
     PQWord(@Self)[Index] := AValue;
end;

procedure TDoubleInt.SetLongWords(const Index: PtrUInt; const AValue: LongWord);
begin
  If Index < SizeOf(TInt) shr 1 then
     PLongWord(@Self)[Index] := AValue;
end;

procedure TDoubleInt.SetWords(const Index: PtrUInt; const AValue: Word);
begin
  If Index < SizeOf(TInt) then
     PWord(@Self)[Index] := AValue;
end;  

procedure TDoubleInt.SetBytes(const Index: PtrUInt; const AValue: Byte);
begin
  If Index < SizeOf(TInt) shl 1 then
     PByte(@Self)[Index] := AValue;
end;

{$IfDef FASTBUTCONSUMINGMEMORYDIVISION}

class procedure TDoubleInt.DivMod(const a : TDoubleInt; const b : TDoubleInt; out Quotient, Modulo : TDoubleInt; const NeedQuotient : Boolean); static;
var
    bbTab : array[0..7, 0..2] of TDoubleInt;
    bb : PDoubleInt;
    i, c, size, shrCount, offset : PtrInt;
begin
    if a < b then
    begin
        if NeedQuotient then
            Quotient.Zero;
        Modulo := a;
        Exit;
    end;

    size := SizeOf(b) shl 3;
    c := size;
    while (c > 0) and (not b.GetBit(c-1)) do
        Dec(c);

    if c = 0 then
    begin
        if NeedQuotient then
           Quotient := a;
        Modulo := 0;
        Exit;
    end;

    while not a.GetBit(Size-1) do
        Dec(Size);
    offset := size-c;

    bbTab[0, 1] := b;
    for i := 1 to 7 do
         bbTab[i-1, 1].ShrOne(@bbTab[i, 1]);

    for i := 0 to 7 do
    begin
         bbTab[i, 0].Zero;
         bbTab[i, 2].Zero;
    end;

    PByte(Pointer(@bbTab[7, 1])-1)^ := PByte(@bbTab[0, 1])^ shl 1;
    for i := 6 downto 1 do
        PByte(Pointer(@bbTab[i, 1])-1)^ := PByte(Pointer(@bbTab[i+1, 1])-1)^ shl 1;

    shrCount := -Offset;

    Modulo := a;
    if NeedQuotient then
        Quotient.Zero;
    for i := offset downto 0 do
    begin
        bb := PDoubleInt(Pointer(@bbTab[shrCount and 7, 1]) + Shr3(shrCount));
        if Modulo >= bb^ then
        begin
            if NeedQuotient then
                Quotient.SetBit(i, True);
            Modulo.Subtrack(bb^);
        end;
        Inc(shrCount);
    end;

end;

{$else}

class procedure TDoubleInt.DivMod(const a : TDoubleInt; const b : TDoubleInt; out Quotient, Modulo : TDoubleInt; const NeedQuotient : Boolean); static;
var
    bb : TDoubleInt;
    i, c, size, offset : PtrUInt;
begin
    if a < b then
    begin
        if NeedQuotient then
            Quotient.Zero;
        Modulo := a;
        Exit;
    end;
    size := SizeOf(b) shl 3;
    c := size;
    while (c > 0) and (not b.GetBit(c-1)) do
        Dec(c);

    if c = 0 then
    begin
        if NeedQuotient then
           Quotient := a;
        Modulo := 0;
        Exit;
    end;

    offset := size-c;
    if c = size then
        bb := b
    else
        bb := b shl offset;

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

{$endIf}

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
begin
    if AValue then
       PQWord(@Self)[Index shr 6] := PQWord(@Self)[Index shr 6] or (QWord(1) shl (Index and 63))
       else
       PQWord(@Self)[Index shr 6] := PQWord(@Self)[Index shr 6] and (not (QWord(1) shl (Index and 63)));
end;

function TDoubleInt.GetBit(const Index : Integer) : Boolean;
begin
    Result := (PQWord(@Self)[Index shr 6] and (QWord(1) shl (Index and 63))) <> 0;
end;

function TDoubleInt.GetLo : TInt;
begin
    Result := Integers[0];
end;

function TDoubleInt.GetHi : TInt;
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
    Result := AValue.Integers[0];
end;

class operator TDoubleInt.:= (const AValue : QWord) : TDoubleInt;  
var
    i : PtrUInt;
begin
    PQWord(Pointer(@Result))[0] := AValue;
    for i := 1 to SizeOf(TInt) shr 2 -1 do
       PQWord(Pointer(@Result))[i] := 0;
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
    if Signed and (b[Size-1] and %10000000 <> 0) and (SizeOf(buf) < SizeOf(TUnsignedType)) then
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
        Exit((-self).UnsignedValue);
    Exit(Self.UnsignedValue);
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

{$IfDef MULTITHREADEDPRODUCT}
initialization
    InitCriticalSection(CounterLock);

finalization
    DoneCriticalSection(CounterLock);
{$EndIf}

end.
