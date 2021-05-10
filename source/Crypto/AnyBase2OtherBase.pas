unit AnyBase2OtherBase;
{$mode objfpc}

interface

type
    TCharToByte = function(const c : Char) : Byte;
    TByteToChar = function(const b : Byte) : Char;

///Standard conversions for every system with base from 2 to 16
function StandardConversionFromCharToByteForHex(const c : Char) : Byte;
function StandardConversionFromByteToCharForHex(const b : Byte) : Char;
    
///converst String to Array of Byte using Conversion function. Warning: it need arr with same length as Str
procedure StrToArrayOfByte(const Str : AnsiString; var arr : array of Byte; const Conversion : TCharToByte);

///converst Array of Byte to String using Conversion function
procedure ArrayOfByteToStr(const arr : array of Byte; var Str : AnsiString; const Conversion : TByteToChar);
    
///predictes length (in deciminals) of integer converted from FromBase to ToBase
function PredictIntegerLength(const FromLength : LongWord; const FromBase, ToBase : Byte) : LongWord;

///conver x from BaseX to equal y in BaseY. Warning: y must have been length enough and filled with zeros
procedure Base2Base(const x : array of Byte; const BaseX : Byte; var y : array of Byte; const BaseY : Byte); overload;

///conver x from BaseX to equal y in BaseY
function Base2Base(const x : AnsiString; const FromBase, ToBase : Byte; const InputConversion : TCharToByte; const OutputConversion : TByteToChar) : AnsiString; overload;

///same as below but with using standard conversions
function Base2Base(const x : AnsiString; const FromBase, ToBase : Byte) : AnsiString; overload;

implementation

uses
    SysUtils, Math;

function StandardConversionFromCharToByteForHex(const c : Char) : Byte;
begin
    if c in ['0'..'9'] then
        Exit(Byte(c)-Byte('0'));    
    if c in ['A'..'F'] then
        Exit(Byte(c)-Byte('A')+10); 
    Exit(Byte(c)-Byte('a')+10);
end;

function StandardConversionFromByteToCharForHex(const b : Byte) : Char;
const
    conv : array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
    Result := conv[b];
end;
    
procedure Multiple(var x : array of Byte; const BaseX : Byte; const y : LongWord);
var
    i : Integer;
    r : QWord;
begin
    r := 0;
    for i := low(x) to High(x) do
    begin
        r := r + x[i]*y;
        x[i] := r mod BaseX;
        r := r div BaseX;
    end;    
end;

procedure Add(var x : array of Byte; const BaseX : Byte; const y : LongWord);
var
    i : Integer;
    r : QWord;
begin
    r := y;
    for i := low(x) to High(x) do
    begin
        r := r + x[i];
        x[i] := r mod BaseX;
        r := r div BaseX;
        if r = 0 then
            Exit;
    end;    
end;

function PredictIntegerLength(const FromLength : LongWord; const FromBase, ToBase : Byte) : LongWord;
begin
    Result := 1+ceil(FromLength*logn(ToBase, FromBase));
end;

procedure Base2Base(const x : array of Byte; const BaseX : Byte; var y : array of Byte; const BaseY : Byte); overload;
var
    i : Integer;
begin
    for i := High(x) downto Low(x) do
    begin
        Multiple(y, BaseY, BaseX);
        Add(y, BaseY, x[i]);
    end;
end;

procedure StrToArrayOfByte(const Str : AnsiString; var arr : array of Byte; const Conversion : TCharToByte);
var
    i, c, l : Integer;
begin
    c := Length(Str);
    l := low(arr);
    for i := 0 to c-1 do
      arr[i+l] := Conversion(Str[c-i]);    
end;    

procedure CutZeros(var Str : AnsiString; const Zero : Char);
var
    n, c : Integer;
begin
    n := 0;
    c := Length(Str);
    while (n < c-1) and (Str[n+1] = Zero) do 
        Inc(n);
    Delete(Str, 1, n);
end;

procedure ArrayOfByteToStr(const arr : array of Byte; var Str : AnsiString; const Conversion : TByteToChar);
var
    i, l, c : Integer;
begin
    l := low(arr);
    c := Length(arr);
    SetLength(Str, c);    
    i := 0;
    for i := 0 to c-1 do
        Str[c-i] := Conversion(arr[i+l]); 
    CutZeros(Str, Conversion(0));
end;    

function Base2Base(const x : AnsiString; const FromBase, ToBase : Byte; const InputConversion : TCharToByte; const OutputConversion : TByteToChar) : AnsiString;
var
    c, xl : Integer;
    tempX, tempY : array of Byte;
begin
    xl := Length(x);
    c := PredictIntegerLength(xl, FromBase, ToBase);
    SetLength(tempX, xl);
    SetLength(tempY, c);
    StrToArrayOfByte(x, tempX, InputConversion);
    Base2Base(tempX, FromBase, tempY, ToBase);
    ArrayOfByteToStr(tempY, Result, OutputConversion);
end;

function Base2Base(const x : AnsiString; const FromBase, ToBase : Byte) : AnsiString; overload;
begin
    Result := Base2Base(x, FromBase, ToBase, @StandardConversionFromCharToByteForHex, @StandardConversionFromByteToCharForHex);
end;

end.
