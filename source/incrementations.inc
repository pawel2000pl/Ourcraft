{Use macro for Ordinal_Type}

function PostInc(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
begin
  Result := x;
  Inc(x);
end;

function PreInc(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
begin
  Inc(x);
  Result := x;
end;

function PreDec(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
begin
  Dec(x);
  Result := x;
end;

function PostDec(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
begin
  Result := x;
  Dec(x);
end;          

procedure UpdateIfGreater(var VariableToUpdate : Ordinal_Type; const Test : Ordinal_Type); overload; inline;
begin
  if Test > VariableToUpdate then
    VariableToUpdate := Test;
end;

procedure UpdateIfLesser(var VariableToUpdate : Ordinal_Type; const Test : Ordinal_Type); overload; inline;
begin
  if Test < VariableToUpdate then
    VariableToUpdate := Test;
end;

{$define Ordinal_Type_2 := uint8}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := uint16}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := uint32}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := uint64}
{$include incrementations2.inc}

{$define Ordinal_Type_2 := int8}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := int16}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := int32}
{$include incrementations2.inc}
{$define Ordinal_Type_2 := int64}
{$include incrementations2.inc}

{$undef Ordinal_Type_2}
