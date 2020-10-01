{Use macro for Ordinal_Type and Ordinal_Type_2}

function PostInc(var x : Ordinal_Type; const Increment : Ordinal_Type_2) : Ordinal_Type; overload; inline;
function PreInc(var x : Ordinal_Type; const Increment : Ordinal_Type_2) : Ordinal_Type; overload; inline;
function PreDec(var x : Ordinal_Type; const Decrement : Ordinal_Type_2) : Ordinal_Type; overload; inline;
function PostDec(var x : Ordinal_Type; const Decrement : Ordinal_Type_2) : Ordinal_Type; overload; inline;
