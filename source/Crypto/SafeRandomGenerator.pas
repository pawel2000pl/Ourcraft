unit SafeRandomGenerator;

{$Mode ObjFpc}

interface

uses
    Classes;

function SafeRandom(const Range : LongWord = 4294967291) : LongWord;
procedure RandomSafeBuf(const Ptr : Pointer; const Size : PtrUInt);
    
procedure PutFileInTheBox(const FileName : AnsiString);
procedure PutStreamInTheBox(const Stream : TStream);
procedure PutInTheBox(const P : Pointer; const Size : PtrUInt);

//Unix only
procedure MixDevRandom;

implementation

{$RangeChecks Off}

uses
    SysUtils, math, sha1 {$IfDef Unix}, BaseUnix{$EndIf};
    
const
    LowPrimes : array[0..14] of PtrUInt = (3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
    
var
    HashArray : array[Word] of QWord;
    HashArrayPosition : Word = 0;


function BinaryDoubleToQWord(const d : Double) : QWord; inline;
begin
    Exit(PQWord(@d)^);
end;

function SafeRandom(const Range : LongWord) : LongWord;
var
    i : Integer;
    tmp : QWord;
begin
    tmp := 1;
    for i := HashArrayPosition to HashArrayPosition + 16 do
        tmp := (tmp + (HashArray[i and $FFFF] mod Range) * LowPrimes[i mod 15]) + LowPrimes[(i+2) mod 15];
    tmp := tmp xor BinaryDoubleToQWord(Random);
    HashArray[HashArrayPosition] := HashArray[HashArrayPosition] xor tmp;
    HashArrayPosition := (HashArrayPosition + 1) and $FFFF;
    Exit(LongWord(tmp mod Range));
end;
    
function ModuloBuf(const Buf: Pointer; const Size: PtrUInt; const InitValue: PtrUInt = 0; const Base: LongWord = 4294967291): LongWord;
var
  i : PtrInt;
begin
  Result := InitValue;
  for i := (Size shr 2) - 1 downto 0 do
    Result := ((QWord(Result) shl 32) or PLongWord(Buf)[i]) mod Base;
  for i := (Size and 3) downto 1 do
    Result := ((QWord(Result) shl 32) or PByte(Buf)[Size-i]) mod Base;
end;

procedure RandomSafeBuf(const Ptr: Pointer; const Size: PtrUInt);
var
  i : PtrUInt;
begin
  for i := 0 to Size-1 do
    PByte(Ptr)[i] := SafeRandom() and $FF;
end;

procedure PutFileInTheBox(const FileName : AnsiString);
var
    FS : TFileStream;
begin
    FS := TFileStream.Create(FileName, fmOpenRead);
    PutStreamInTheBox(FS);
    FS.Free;
end;

procedure PutStreamInTheBox(const Stream : TStream);
var
    Size, rSize : Integer;
    buf : array[word] of Byte;
begin
    Stream.Position := 0;
    Size := Stream.Size;
    while Size > 0 do
    begin
        rSize := min(Size, SizeOf(buf));
        Stream.ReadBuffer(buf, rSize);
        Dec(Size, rSize);
        PutInTheBox(@buf[0], rSize);
    end;
end;

procedure PutInTheBox(const P : Pointer; const Size : PtrUInt);
var
    i : Integer;
    buf : PByte;
    Context: TSHA1Context;
    code : TSHA1Digest;
begin
    buf := P;
    
    SHA1Init(Context);
    SHA1Update(Context, P^, Size);    
    SHA1Final(Context, code);
    
    for i := 0 to Size-1 do
        HashArray[(HashArrayPosition+i) and $FFFF] := HashArray[(HashArrayPosition+i) and $FFFF] xor (buf[i] shl (i and $3F)) xor HashArray[buf[i] shl (i and 7)]
            xor PLongWord(@code)[0 or (i and 2)] xor (PLongWord(@code)[1 or (i and 2)] shl 32) xor HashArray[code[1 or (i and 18)] or (code[0 or (i and 18)] shl 8)];     

    HashArray[0] := HashArray[0] xor ModuloBuf(@HashArray, SizeOf(HashArray));
    HashArrayPosition := (HashArrayPosition+Size) and $FFFF; 
end;

procedure MixDevRandom;
{$IfDef Unix}
const 
    RandomDevice = '/dev/random';
var
    fd : cInt;
    buf : array[word] of QWord;
begin
    if not FileExists(RandomDevice) then
        Exit;
    fd := fpOpen(RandomDevice, O_RdOnly);
    fpRead(fd, buf, SizeOf(buf));    
    PutInTheBox(@buf, SizeOf(buf));
    fpClose(fd);
end;
{$else}
begin
end;
{$endif}
    
procedure InitHashArray;
var
    i : Integer;
begin
    Randomize;
    for i := low(HashArray) to High(HashArray) do
        HashArray[i] := BinaryDoubleToQWord(Random) xor BinaryDoubleToQWord(sqrt(i+pi)) xor (Random($FFFFFFFF) shl 16);
    PutFileInTheBox(ParamStr(0));
    MixDevRandom;
end;

initialization
    InitHashArray;

end.

