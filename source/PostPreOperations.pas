{
  Copyright (C) 2022 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

{$IfNDef INCLUDE_POST_PRE_OPERATION}
{$PUSH}

unit PostPreOperations;

{$Mode ObjFpc}
{$Macro ON}

interface

{$Define INCLUDE_POST_PRE_OPERATION}
{$Define INCLUDE_POST_PRE_OPERATION_HEADERS}

{$Define OrdinalType := uInt64}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt32}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt16}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt8}
{$Include PostPreOperations.pas}

{$Define OrdinalType := Int64}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int32}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int16}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int8}
{$Include PostPreOperations.pas}

{$UnDef INCLUDE_POST_PRE_OPERATION_HEADERS}
{$UnDef INCLUDE_POST_PRE_OPERATION}

implementation

{$Define INCLUDE_POST_PRE_OPERATION}
{$Define INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION}

{$Define OrdinalType := uInt64}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt32}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt16}
{$Include PostPreOperations.pas}
{$Define OrdinalType := uInt8}
{$Include PostPreOperations.pas}

{$Define OrdinalType := Int64}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int32}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int16}
{$Include PostPreOperations.pas}
{$Define OrdinalType := Int8}
{$Include PostPreOperations.pas}

{$UnDef INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION}
{$UnDef INCLUDE_POST_PRE_OPERATION}

end.

{$POP}
{$EndIf}



{$IfDef INCLUDE_POST_PRE_OPERATION_HEADERS}

    function PostDec(var x : OrdinalType) : OrdinalType; overload; inline;
    function PreDec(var x : OrdinalType) : OrdinalType; overload; inline;
    function PreInc(var x : OrdinalType) : OrdinalType; overload; inline;
    function PostInc(var x : OrdinalType) : OrdinalType; overload; inline; 

    procedure UpdateIfGreater(var VariableToUpdate : OrdinalType; const Test : OrdinalType); overload; inline;
    procedure UpdateIfLesser(var VariableToUpdate : OrdinalType; const Test : OrdinalType); overload; inline;

    {$UnDef INCLUDE_POST_PRE_OPERATION_HEADERS}
    {$Define INCLUDE_POST_PRE_OPERATION_HEADERS_2}

    {$Define OrdinalType2 := uInt8}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt16}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt32}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt64}
    {$Include PostPreOperations.pas}

    {$Define OrdinalType2 := Int8}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int16}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int32}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int64}
    {$Include PostPreOperations.pas}

    {$UnDef OrdinalType2}
    {$UnDef INCLUDE_POST_PRE_OPERATION_HEADERS_2}
    {$Define INCLUDE_POST_PRE_OPERATION_HEADERS}

{$EndIf}



{$IfDef INCLUDE_POST_PRE_OPERATION_HEADERS_2}

    function PostInc(var x : OrdinalType; const Increment : OrdinalType2) : OrdinalType; overload; inline;
    function PreInc(var x : OrdinalType; const Increment : OrdinalType2) : OrdinalType; overload; inline;
    function PreDec(var x : OrdinalType; const Decrement : OrdinalType2) : OrdinalType; overload; inline;
    function PostDec(var x : OrdinalType; const Decrement : OrdinalType2) : OrdinalType; overload; inline;

{$EndIf}


    
{$IfDef INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION}

    function PostInc(var x : OrdinalType) : OrdinalType; overload; inline;
    begin
        Result := x;
        Inc(x);
    end;

    function PreInc(var x : OrdinalType) : OrdinalType; overload; inline;
    begin
        Inc(x);
        Result := x;
    end;

    function PreDec(var x : OrdinalType) : OrdinalType; overload; inline;
    begin
        Dec(x);
        Result := x;
    end;

    function PostDec(var x : OrdinalType) : OrdinalType; overload; inline;
    begin
        Result := x;
        Dec(x);
    end;          

    procedure UpdateIfGreater(var VariableToUpdate : OrdinalType; const Test : OrdinalType); overload; inline;
    begin
        if Test > VariableToUpdate then
            VariableToUpdate := Test;
    end;

    procedure UpdateIfLesser(var VariableToUpdate : OrdinalType; const Test : OrdinalType); overload; inline;
    begin
        if Test < VariableToUpdate then
            VariableToUpdate := Test;
    end;

    {$UnDef INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION}
    {$Define INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION_2}
    
    {$Define OrdinalType2 := uInt8}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt16}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt32}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := uInt64}
    {$Include PostPreOperations.pas}

    {$Define OrdinalType2 := Int8}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int16}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int32}
    {$Include PostPreOperations.pas}
    {$Define OrdinalType2 := Int64}
    {$Include PostPreOperations.pas}

    {$UnDef OrdinalType2}
    {$UnDef INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION_2}
    {$Define INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION}

{$EndIf}


    
{$IfDef INCLUDE_POST_PRE_OPERATION_IMPLEMENTATION_2}

    function PostInc(var x : OrdinalType; const Increment : OrdinalType2) : OrdinalType; overload; inline;
    begin
        Result := x;
        Inc(x, Increment);
    end;

    function PreInc(var x : OrdinalType; const Increment : OrdinalType2) : OrdinalType; overload; inline;
    begin
        Inc(x, Increment);
        Result := x;
    end;

    function PreDec(var x : OrdinalType; const Decrement : OrdinalType2) : OrdinalType; overload; inline;
    begin
        Dec(x, Decrement);
        Result := x;
    end;

    function PostDec(var x : OrdinalType; const Decrement : OrdinalType2) : OrdinalType; overload; inline;
    begin
        Result := x;
        Dec(x, Decrement);
    end;     

{$EndIf}
    
