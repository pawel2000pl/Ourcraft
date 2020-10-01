{Use macro for Ordinal_Type and Ordinal_Type_2}

function PostInc(var x : Ordinal_Type; const Increment : Ordinal_Type_2) : Ordinal_Type; overload; inline;
begin
  Result := x;
  Inc(x, Increment);
end;

function PreInc(var x : Ordinal_Type; const Increment : Ordinal_Type_2) : Ordinal_Type; overload; inline;
begin
  Inc(x, Increment);
  Result := x;
end;

function PreDec(var x : Ordinal_Type; const Decrement : Ordinal_Type_2) : Ordinal_Type; overload; inline;
begin
  Dec(x, Decrement);
  Result := x;
end;

function PostDec(var x : Ordinal_Type; const Decrement : Ordinal_Type_2) : Ordinal_Type; overload; inline;
begin
  Result := x;
  Dec(x, Decrement);
end;     
