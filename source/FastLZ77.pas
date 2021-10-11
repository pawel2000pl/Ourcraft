unit FastLZ77;

{$Mode ObjFpc}

interface 

uses Classes;

const
    DictionarySizeLog2 = 4;
    DictionaryLength = 1 shl DictionarySizeLog2;
    DictionaryMask = DictionaryLength-1;
    
type
    TFastLZ77Dictionary = array[0..DictionaryLength-1] of Byte;

    TFastLZ77Record = packed record
        Used : Byte;
        buf : array[0..7] of Byte;
    end;

    TFastLZ77 = class 
    private
        FDictionary : TFastLZ77Dictionary;
        FDictionaryPosition : PtrUInt;
    protected
        function FindInDictionary(const Buf : PByte; const BufSize : PtrUInt; out Position, SizeMinusOne : PtrUInt) : Boolean;
        function WriteToDictionary(const Buf : PByte; const BufSize : PtrUInt) : PtrUInt;
        procedure ReadFromDictionary(const Buf : PByte; const Position, BufSize : PtrUInt);
    public
        constructor Create;
    end;

    TFastLZ77Function = function(const Buf : PByte; const Size : PtrUInt; const DestBuf : PByte) : PtrUInt;
    TFastLZ77Direction = (fl77Compress, fl77Uncompress);
    
function FastLZ77CompressBuf(const Buf : PByte; const Size : PtrUInt; const DestBuf : PByte) : PtrUInt;
function FastLZ77UnCompressBuf(const Buf : PByte; const Size : PtrUInt; const DestBuf : PByte) : PtrUInt;

function FastLZ77Stream(Source, Dest : TStream; const Direction : TFastLZ77Direction) : PtrUInt; overload;
function FastLZ77Stream(Source, Dest : TStream; const Size : PtrUInt; const Direction : TFastLZ77Direction) : PtrUInt; overload;

implementation

uses
    math;

function Min(const a, b : PtrUInt) : PtrUInt; inline;
begin
    if a < b then
        Exit(a);
    Exit(b);
end;

function TFastLZ77.FindInDictionary(const Buf : PByte; const BufSize : PtrUInt; out Position, SizeMinusOne : PtrUInt) : Boolean;
var
    i, j : PtrUInt;
begin
    Position := 0;
    SizeMinusOne := 0;
    
    for i := Low(FDictionary) to High(FDictionary) do
        for j := 0 to Min(BufSize, DictionaryLength)-1 do
            if FDictionary[(i+j+FDictionaryPosition) and DictionaryMask] = Buf[j] then
            begin
                if j > SizeMinusOne then
                begin
                    SizeMinusOne := j;
                    Position := i;
                end;
            end
            else
                Break;
    
    Result := SizeMinusOne > 0;
end;

function TFastLZ77.WriteToDictionary(const Buf : PByte; const BufSize : PtrUInt) : PtrUInt;
var
    i : PtrUInt;
begin
    Result := FDictionaryPosition;
    for i := 0 to BufSize-1 do
        FDictionary[(i+FDictionaryPosition) and DictionaryMask] := Buf[i];
    FDictionaryPosition := (FDictionaryPosition + BufSize) and DictionaryMask;
end;

procedure TFastLZ77.ReadFromDictionary(const Buf : PByte; const Position, BufSize : PtrUInt);
var
    i : PtrUInt;
begin
    for i := 0 to BufSize-1 do
        Buf[i] := FDictionary[(i + FDictionaryPosition + Position) and DictionaryMask];
end;

constructor TFastLZ77.Create;
var
    i : Integer;
begin
    for i := low(FDictionary) to High(FDictionary) do
        FDictionary[i] := 0;
    FDictionaryPosition := 0;
end;

function FastLZ77CompressBuf(const Buf : PByte; const Size : PtrUInt; const DestBuf : PByte) : PtrUInt;
var
    Position, FoundedPosition, FoundedSize, i, RecIndex : PtrUInt;
    rec : TFastLZ77Record;
    FastLZ77 : TFastLZ77;
begin
    Position := 0;
    RecIndex := 0;
    Result := 0;
    rec.Used := 0;
    FastLZ77 := TFastLZ77.Create;
    
    while Position < Size do
    begin
        if FastLZ77.FindInDictionary(Buf + Position, Size-Position, FoundedPosition, FoundedSize) then
        begin               
            rec.Used := rec.Used or (1 shl RecIndex); 
            rec.buf[RecIndex] := (FoundedPosition and DictionaryMask) or ((FoundedSize and DictionaryMask) shl DictionarySizeLog2);
            Inc(FoundedSize);  
            FastLZ77.WriteToDictionary(Buf + Position, FoundedSize);
            Inc(Position, FoundedSize); 
        end
        else
        begin
            FastLZ77.WriteToDictionary(Buf + Position, 1);
            rec.buf[RecIndex] := buf[Position];
            Inc(Position);
        end;
        
        Inc(RecIndex);
        if (RecIndex >= SizeOf(TFastLZ77Record)-SizeOf(TFastLZ77Record.Used)) or (Size <= Position) then
        begin 
            for i := 0 to RecIndex do
               DestBuf[Result+i] := PByte(@rec)[i];
            Inc(Result, RecIndex+1); 
            RecIndex := 0;
            rec.Used := 0;
        end;
    end;

    FastLZ77.Free;
end;

function FastLZ77UnCompressBuf(const Buf : PByte; const Size : PtrUInt; const DestBuf : PByte) : PtrUInt;
var
    Position, FoundedPosition, FoundedSize, i, ReadedSize : PtrUInt;
    rec : TFastLZ77Record;
    FastLZ77 : TFastLZ77;
begin
    Result := 0;
    Position := 0;
    FastLZ77 := TFastLZ77.Create;

    while Position < Size do
    begin
        ReadedSize := Min(SizeOf(TFastLZ77Record), Size - Position);
        for i := 0 to ReadedSize -1 do
            PByte(@rec)[i] := Buf[Position + i];
        Inc(Position, ReadedSize); 

        for i := 0 to (ReadedSize -1 - SizeOf(TFastLZ77Record.Used)) and DictionaryMask do
        begin
            if (1 shl i) and rec.Used <> 0 then
            begin
                FoundedPosition := rec.buf[i] and DictionaryMask;
                FoundedSize := (rec.buf[i] shr DictionarySizeLog2) and DictionaryMask + 1;
                FastLZ77.ReadFromDictionary(DestBuf + Result, FoundedPosition, FoundedSize);
                FastLZ77.WriteToDictionary(DestBuf + Result, FoundedSize);
                Inc(Result, FoundedSize);                
            end
            else
            begin
                DestBuf[Result] := rec.buf[i];
                FastLZ77.WriteToDictionary(@rec.buf[i], 1);
                Inc(Result);
            end;
        end;
    end;

    FastLZ77.Free;
end;

function FastLZ77Stream(Source, Dest : TStream; const Direction : TFastLZ77Direction) : PtrUInt; overload;
begin
    Exit(FastLZ77Stream(Source, Dest, Source.Size - Source.Position, Direction));
end;

function FastLZ77Stream(Source, Dest : TStream; const Size : PtrUInt; const Direction : TFastLZ77Direction) : PtrUInt; overload;
const 
    BufOverload : array[TFastLZ77Direction] of Double = (9/8, 16);
var
    MS1, MS2 : TMemoryStream;
    fun : TFastLZ77Function;
begin
    if Source is TMemoryStream then
        MS1 := Source as TMemoryStream
    else
    begin
        MS1 := TMemoryStream.Create;
        MS1.CopyFrom(Source, Size);
        MS1.Position:=0;
    end;
    
    MS2 := TMemoryStream.Create;
    
    MS2.Size := ceil(BufOverload[Direction]) * Size;

    if Direction = fl77Compress then
        fun := @FastLZ77CompressBuf
    else
        fun := @FastLZ77UnCompressBuf;
    
    Result := fun(MS1.Memory + MS1.Position, Size, MS2.Memory);
    MS2.Size := Result;
    MS2.Position := 0;

    Dest.CopyFrom(MS2, MS2.Size);
    if MS1 = Source then
        Source.Seek(Size, soFromCurrent)
    else
        MS1.Free;
    MS2.Free;
end;

end.
