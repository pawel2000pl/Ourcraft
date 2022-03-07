unit JenkinsHash;

{$mode objfpc}{$H+}

interface

function jenkins_one_at_a_time_hash(const buf : Pointer; const Size : PtrUInt) : LongWord; inline; 
generic function jenkins_one_at_a_time_hash<T>(const buf : T) : LongWord; inline;

implementation

function jenkins_one_at_a_time_hash(const buf : Pointer; const Size : PtrUInt) : LongWord;
var
  i : PtrUInt;
begin
  Result := 0;
  for i := 0 to Size-1 do
  begin
    Inc(Result, PByte(buf)[i]);
    Inc(Result, Result shl 10);
    Result := Result xor (Result shr 6);
  end;
  Inc(Result, Result shl 3);
  Result := Result xor (Result shr 11);
  Inc(Result, Result shl 15);
end;

generic function jenkins_one_at_a_time_hash<T>(const buf : T) : LongWord;
var
  i : PtrUInt;
begin
  Result := 0;
  for i := 0 to SizeOf(T)-1 do
  begin
    Inc(Result, PByte(@buf)[i]);
    Inc(Result, Result shl 10);
    Result := Result xor (Result shr 6);
  end;
  Inc(Result, Result shl 3);
  Result := Result xor (Result shr 11);
  Inc(Result, Result shl 15);
end;

end.

