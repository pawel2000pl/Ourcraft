{Use macro for Ordinal_Type}

function PostDec(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
function PreDec(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
function PreInc(var x : Ordinal_Type) : Ordinal_Type; overload; inline;
function PostInc(var x : Ordinal_Type) : Ordinal_Type; overload; inline; 

procedure UpdateIfGreater(var VariableToUpdate : Ordinal_Type; const Test : Ordinal_Type); overload; inline;
procedure UpdateIfLesser(var VariableToUpdate : Ordinal_Type; const Test : Ordinal_Type); overload; inline;

{$define Ordinal_Type_2 := uint8}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := uint16}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := uint32}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := uint64}
{$include incrementation_headers_2.inc}

{$define Ordinal_Type_2 := int8}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := int16}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := int32}
{$include incrementation_headers_2.inc}
{$define Ordinal_Type_2 := int64}
{$include incrementation_headers_2.inc}

{$undef Ordinal_Type_2}
