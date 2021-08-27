unit OurAccounts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RSACodeStream, RSAMultiKeyGenerator, RSAKeys, DoubleInt, AnyBase2OtherBase;

type

  { TUserAccount }

  TUserAccount = class
  private
    FPrivatePassword : uInt2048;
    FPublicPassword : uInt2048;
    FPasswordModulo : uInt2048;
  public
    procedure GeneratePassword;

  end;

function Alphabet62ConversionFromCharToByteForHex(const c : Char) : Byte;
function Alphabet62ConversionFromByteToCharForHex(const b : Byte) : Char;

implementation

const
  AlphabetArray : array[0..61] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z');

var
  AlphabetInversionArray : array[char] of Byte;

procedure CreateAlphabetInversionArray;
var
  i : Integer;
begin
  for i := low(AlphabetArray) to High(AlphabetArray) do
      AlphabetInversionArray[AlphabetArray[i]] := i;
end;

function Alphabet62ConversionFromCharToByteForHex(const c: Char): Byte;
begin
   Result := AlphabetInversionArray[c];
end;

function Alphabet62ConversionFromByteToCharForHex(const b: Byte): Char;
begin
   Result := AlphabetArray[b];
end;

{ TUserAccount }

procedure TUserAccount.GeneratePassword;
begin
  specialize GenerateRSAKey<uInt2048>(FPrivatePassword, FPublicPassword, FPasswordModulo);
end;

initialization
    CreateAlphabetInversionArray;

end.

