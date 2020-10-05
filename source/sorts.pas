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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  { TSort }

  generic TSort<TValue> = class
  public
    //abstract method must be oevrrided
    class function Compare(const a, b : TValue) : integer; virtual; abstract;
    class procedure swap(var a, b : TValue); virtual;

    //best for almost sorted
    class procedure Insert(var tab : array of TValue); overload;
    class procedure Insert(var tab : array of TValue; const offset, n : integer); overload;
    //best for random less than 127
    class procedure InsertComb(var tab : array of TValue); overload;
    class procedure InsertComb(var tab : array of TValue;
      const offset, n : integer); overload;
    //better for completly random       
    class procedure Quick(var A : array of TValue); overload;
    class procedure Quick(var A : array of TValue; const l, r : integer;
      const MaxDepth : UIntPtr = 32); overload;
    //better for half-sorted       
    class procedure Quick2(var A : array of TValue); overload;
    class procedure Quick2(var AI : array of TValue; const ALo, AHi : integer);
      overload;
    //stable and always constant time
    class procedure Merge(var Tab : array of TValue; const a, b : integer); overload;
    class procedure Merge(var Tab : array of TValue); overload;
    //check if is sorted
    class function Sorted(const Tab : array of TValue) : boolean; overload;
    class function Sorted(const Tab : array of TValue;
      const offset, n : integer) : boolean; overload;
    //check sort level (0-1) / usually useless
    class function SortLevel(const Tab : array of TValue) : double; overload;
    class function SortLevel(const Tab : array of TValue;
      const offset, n : integer) : double; overload;
  end;

  { TBSearch }

  generic TBSearch<TValue, TKey> = class
  public
    //abstract method must be oevrrided
    class function Compare(const a : TValue; const b : TKey) : integer;
      virtual; abstract;

    //searching in sorted table
    class function BSearch(const Tab : array of TValue; const Key : TKey;
      const a, b : integer) : integer; overload;
    class function BSearch(const Tab : array of TValue; const Key : TKey) : integer;
      overload;
  end;

  { TMemorySorter }

  generic TMemorySorter<TValue> = class(specialize TSort<TValue>)
  public
    class function Compare(const a, b: TValue): integer; override;
  end;

  { TMemoryBSearch }

  generic TMemoryBSearch<TValue> = class(specialize TBSearch<TValue, TValue>)
  public
    class function Compare(const a: TValue; const b: TKey): integer; override;
  end;

  { TIntegerSort }

  TIntegerSort = class(specialize TSort<integer>)
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

{ TSort }

class procedure TSort.swap(var a, b : TValue);
var
  tmp : TValue;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

class procedure TSort.InsertComb(var tab : array of TValue; const offset, n : integer);
const
  d = 9 / 23;
var
  gap, i, j : integer;
  x : TValue;
begin
  gap := round(d ** round(logn(d, n)));
  while gap > 1 do
  begin
    gap := round(gap * d);
    for i := gap to n - 1 do
    begin
      x := tab[i + Offset];
      j := i;
      while ((j >= gap) and (Compare(x, tab[j - gap + Offset]) < 0)) do
      begin
        tab[j + Offset] := tab[j - gap + Offset];
        Dec(j, gap);
      end;
      tab[j + Offset] := x;
    end;
  end;
end;

class procedure TSort.Insert(var tab : array of TValue; const offset, n : integer);
var
  x : TValue;
  i, j : integer;
begin
  for i := 1 to n - 1 do
  begin
    x := tab[i + Offset];
    j := i;
    while Compare(x, tab[j - 1 + Offset]) < 0 do
    begin
      tab[j + Offset] := tab[j - 1 + Offset];
      Dec(j, 1);
    end;
    tab[j + Offset] := x;
  end;
end;

class procedure TSort.Quick2(var AI : array of TValue; const ALo, AHi : integer);
var
  Lo, Hi : integer;
  T, Pivot : TValue;
begin
  Lo := ALo;
  Hi := AHi;
  Pivot := AI[(Lo + Hi) div 2];
  repeat
    while Compare(AI[Lo], Pivot) < 0 do
      Inc(Lo);
    while Compare(AI[Hi], Pivot) > 0 do
      Dec(Hi);

    if Lo <= Hi then
    begin
      T := AI[Lo];
      AI[Lo] := AI[Hi];
      AI[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > ALo then
    Quick2(AI, ALo, Hi);
  if Lo < AHi then
    Quick2(AI, Lo, AHi);
end;

class procedure TSort.Quick(var A : array of TValue; const l, r : integer;
  const MaxDepth : UIntPtr);
var
  i, j : integer;
  x : TValue;
begin
  if (r - l <= 48) or (MaxDepth <= 0) then
  begin
    InsertComb(A, l, r - l + 1);
    exit;
  end;

  x := A[r];
  i := l - 1;
  for j := l to r - 1 do
  begin
    if Compare(A[j], x) <= 0 then
    begin
      Inc(i);
      swap(A[i], A[j]);
    end;
  end;
  swap(A[i + 1], A[r]);

  Quick(A, l, i, MaxDepth - 1);
  Quick(A, i + 2, MaxDepth - 1);
end;

class procedure TSort.Merge(var Tab : array of TValue; const a, b : integer);
var
  half : integer;
  i, ia, ib : integer;
  tmp : array of TValue;
begin
  if (b - a) <= 1 then
    exit;
  half := (a + b) div 2;
  Merge(Tab, a, half);
  Merge(Tab, half + 1, b);
  i := 0;
  ia := a;
  ib := half + 1;
  SetLength(tmp, b - a + 1);
  while (ia <= half) and (ib <= b) do
  begin
    if Compare(Tab[ia], Tab[ib]) < 0 then
    begin
      tmp[i] := Tab[ia];
      Inc(ia);
    end
    else
    begin
      tmp[i] := Tab[ib];
      Inc(ib);
    end;
    Inc(i);
  end;
  while (ia <= half) do
  begin
    tmp[i] := Tab[ia];
    Inc(ia);
    Inc(i);
  end;
  while (ib <= b) do
  begin
    tmp[i] := Tab[ib];
    Inc(ib);
    Inc(i);
  end;
  for i := 0 to b - a do
    Tab[i + a] := tmp[i];
  SetLength(tmp, 0);
end;

class procedure TSort.InsertComb(var tab : array of TValue);
begin
  InsertComb(tab, low(tab), length(tab));
end;

class procedure TSort.Insert(var tab : array of TValue);
begin
  Insert(tab, low(tab), Length(tab));
end;

class procedure TSort.Quick(var A : array of TValue);
begin
  Quick(A, low(A), high(A));
end;

class procedure TSort.Quick2(var A : array of TValue);
begin
  Quick2(A, Low(A), High(A));
end;

class procedure TSort.Merge(var Tab : array of TValue);
begin
  Merge(Tab, low(Tab), high(Tab));
end;

class function TSort.Sorted(const Tab : array of TValue) : boolean;
begin
  Result := Sorted(Tab, low(tab), length(Tab));
end;

class function TSort.Sorted(const Tab : array of TValue;
  const offset, n : integer) : boolean;
var
  i : integer;
begin
  Result := True;
  if n > 1 then
    for i := 0 to n - 2 do
      if Compare(Tab[i + offset], Tab[i + 1 + offset]) > 0 then
      begin
        Sorted := False;
        exit;
      end;
end;

class function TSort.SortLevel(const Tab : array of TValue) : double;
begin
  Result := SortLevel(Tab, low(tab), length(tab));
end;

class function TSort.SortLevel(const Tab : array of TValue;
  const offset, n : integer) : double;
var
  i, c : integer;
begin
  c := 0;
  if n <= 1 then
  begin
    Result := 1;
    exit;
  end;
  for i := 0 to n - 2 do
    if Compare(Tab[i + offset], Tab[i + 1 + offset]) > 0 then
      Inc(c);
  Result := 1 - c / (n - 1);
end;

{ TBSearch }

class function TBSearch.BSearch(const Tab : array of TValue; const Key : TKey;
  const a, b : integer) : integer;
var
  half : integer;
begin
  if (a > b) then
  begin
    Result := -1;
    exit;
  end;

  half := (a + b) div 2;
  case sign(Compare(Tab[half], Key)) of
    0: Result := half;
    1: Result := BSearch(Tab, Key, a, half - 1);
    -1: Result := BSearch(Tab, Key, half + 1, b);
  end;
end;

class function TBSearch.BSearch(const Tab : array of TValue; const Key : TKey) : integer;
begin
  Result := BSearch(tab, Key, low(Tab), high(tab));
end;

end.
