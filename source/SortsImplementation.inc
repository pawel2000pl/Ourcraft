
{$IfDef StaticSort}
{$define TClassSortName := TStaticSort }
{$define TClassBSearchName := TStaticBSearch }
{$Else}
{$define TClassSortName := TSort }
{$define TClassBSearchName := TBSearch }
{$EndIf}              

{ TSort }

{$ifdef StaticSort} class {$endif} procedure TClassSortName.swap(var a, b : TValue);
var
  tmp : TValue;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.InsertComb(var tab : array of TValue; const offset, n : integer);
const  {Warning: Wherever you put this algorithm, remember write the author: Paweł Bielecki}
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



{$ifdef StaticSort} class {$endif} procedure TClassSortName.InsertCombF(var tab : array of TValue; const offset, n : integer);
const  {Warning: Wherever you put this algorithm, remember write the author: Paweł Bielecki}
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
      if Compare(tab[i + Offset], tab[i - gap + Offset]) < 0 then
      begin
        x := tab[i + Offset];
        tab[i + Offset] := tab[i-gap + Offset];
        j := i-gap;
        while ((j >= gap) and (Compare(x, tab[j - gap + Offset]) < 0)) do
        begin
            tab[j + Offset] := tab[j - gap + Offset];
            dec(j, gap);
        end;
        tab[j + Offset] := x;
      end;
    end;
  end;
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Insert(var tab : array of TValue; const offset, n : integer);
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

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Quick2(var AI : array of TValue; const ALo, AHi : integer);
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

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Quick(var A : array of TValue; const l, r : integer;
  const MaxDepth : UIntPtr);
var
  i, j : integer;
  x : TValue;
begin
  if l>r then
     Exit;
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
  Quick(A, i + 2, r, MaxDepth - 1);
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Merge(var Tab : array of TValue; const a, b : integer);
var
  half : integer;
  i, ia, ib : integer;
  tmp : array of TValue;
begin
  if a >= b then
    exit;
  half := (a + b) div 2;
  Merge(Tab, a, half);
  Merge(Tab, half + 1, b);
  i := 0;
  ia := a;
  ib := half + 1;
  SetLength(tmp{%H-}, b - a + 1);
  while (ia <= half) and (ib <= b) do
    if Compare(Tab[ia], Tab[ib]) < 0 then
      tmp[PostInc(i)] := Tab[PostInc(ia)]
    else
      tmp[PostInc(i)] := Tab[PostInc(ib)];
  while (ia <= half) do
    tmp[PostInc(i)] := Tab[PostInc(ia)];
  while (ib <= b) do
    tmp[PostInc(i)] := Tab[PostInc(ib)];
  for i := 0 to b - a do
    Tab[a + i] := tmp[i];
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.InsertComb(var tab : array of TValue);
begin
  InsertComb(tab, low(tab), length(tab));
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.InsertCombF(var tab : array of TValue);
begin
  InsertCombF(tab, low(tab), length(tab));
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Insert(var tab : array of TValue);
begin
  Insert(tab, low(tab), Length(tab));
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Quick(var A : array of TValue);
begin
  Quick(A, low(A), high(A));
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Quick2(var A : array of TValue);
begin
  Quick2(A, Low(A), High(A));
end;

{$ifdef StaticSort} class {$endif} procedure TClassSortName.Merge(var Tab : array of TValue);
begin
  Merge(Tab, low(Tab), high(Tab));
end;

{$ifdef StaticSort} class {$endif} function TClassSortName.Sorted(const Tab : array of TValue) : boolean;
begin
  Result := Sorted(Tab, low(tab), length(Tab));
end;

{$ifdef StaticSort} class {$endif} function TClassSortName.Sorted(const Tab : array of TValue;
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

{$ifdef StaticSort} class {$endif} function TClassSortName.SortLevel(const Tab : array of TValue) : double;
begin
  Result := SortLevel(Tab, low(tab), length(tab));
end;

{$ifdef StaticSort} class {$endif} function TClassSortName.SortLevel(const Tab : array of TValue;
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

constructor TClassSortName.Create;
begin
  {$ifdef StaticSort} raise ENoConstructException.Create('Cannot create TStaticSort class - because it is static. Use non-static version (TSort) instead'); {$endif}
end;

{ TBSearch }

{$ifdef StaticSort} class {$endif} function TClassBSearchName.BSearch(const Tab : array of TValue; const Key : TKey;
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

{$ifdef StaticSort} class {$endif} function TClassBSearchName.BSearch(const Tab : array of TValue; const Key : TKey) : integer;
begin
  Result := BSearch(tab, Key, low(Tab), high(tab));
end;


{$undef TClassSortName}
{$undef TClassBSearchName}
