{
  Copyright (C) 2020 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

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

unit Sorts;

{$mode objfpc}{$H+}{$macro on}

interface

uses
  Classes, SysUtils, Math;

type

  {$define StaticSort}
  {$Include SortsHeader.inc}  
  {$undef StaticSort}
  {$Include SortsHeader.inc}  

  { TMemorySorter }

  generic TMemorySorter<TValue> = class(specialize TStaticSort<TValue>)
  public
    class function Compare(const a, b: TValue): integer; override;
  end;

  { TMemoryBSearch }

  generic TMemoryBSearch<TValue> = class(specialize TStaticBSearch<TValue, TValue>)
  public
    class function Compare(const a: TValue; const b: TKey): integer; override;
  end;

  { TIntegerSort }

  TIntegerSort = class(specialize TStaticSort<integer>)
  public
    //TValue is now Integer
    class function Compare(const a, b : TValue) : integer; override;
  end;

implementation

{ TMemoryBSearch }

class function TMemoryBSearch.Compare(const a: TValue; const b: TKey): integer;
begin
  Result := CompareMemRange(@a, @b, SizeOf(TValue));
end;

{ TMemorySorter }

class function TMemorySorter.Compare(const a, b: TValue): integer;
begin
  Result := CompareMemRange(@a, @b, SizeOf(TValue));
end;

{ TIntegerSort }

class function TIntegerSort.Compare(const a, b : TValue) : integer;
begin
  Result := a - b;
end;

{$define StaticSort}
{$Include SortsImplementation.inc}
{$undef StaticSort}      
{$Include SortsImplementation.inc}

end.
