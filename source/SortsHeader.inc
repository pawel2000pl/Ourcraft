
{$IfDef StaticSort}
{$define TClassSortName := TStaticSort }
{$define TClassBSearchName := TStaticBSearch }
{$Else}
{$define TClassSortName := TSort }
{$define TClassBSearchName := TBSearch }
{$EndIf}

  { TSort }

  generic TClassSortName<TValue> = class
  public
    //abstract method must be oevrrided
    {$ifdef StaticSort} class {$endif} function Compare(const a, b : TValue) : integer; virtual; abstract;
    {$ifdef StaticSort} class {$endif} procedure swap(var a, b : TValue); virtual;

    //best for almost sorted
    {$ifdef StaticSort} class {$endif} procedure Insert(var tab : array of TValue); overload;
    {$ifdef StaticSort} class {$endif} procedure Insert(var tab : array of TValue; const offset, n : integer); overload;
    //best for random less than 127
    {$ifdef StaticSort} class {$endif} procedure InsertComb(var tab : array of TValue); overload;
    {$ifdef StaticSort} class {$endif} procedure InsertComb(var tab : array of TValue; const offset, n : integer); overload;
    //better for completly random       
    {$ifdef StaticSort} class {$endif} procedure Quick(var A : array of TValue); overload;
    {$ifdef StaticSort} class {$endif} procedure Quick(var A : array of TValue; const l, r : integer; const MaxDepth : UIntPtr = 32); overload;
    //better for half-sorted       
    {$ifdef StaticSort} class {$endif} procedure Quick2(var A : array of TValue); overload;
    {$ifdef StaticSort} class {$endif} procedure Quick2(var AI : array of TValue; const ALo, AHi : integer); overload;
    //stable and always constant time
    {$ifdef StaticSort} class {$endif} procedure Merge(var Tab : array of TValue; const a, b : integer); overload;
    {$ifdef StaticSort} class {$endif} procedure Merge(var Tab : array of TValue); overload;
    //check if is sorted
    {$ifdef StaticSort} class {$endif} function Sorted(const Tab : array of TValue) : boolean; overload;
    {$ifdef StaticSort} class {$endif} function Sorted(const Tab : array of TValue; const offset, n : integer) : boolean; overload;
    //check sort level (0-1) / usually useless
    {$ifdef StaticSort} class {$endif} function SortLevel(const Tab : array of TValue) : double; overload;
    {$ifdef StaticSort} class {$endif} function SortLevel(const Tab : array of TValue; const offset, n : integer) : double; overload;
  end;

  { TBSearch }

  generic TClassBSearchName<TValue, TKey> = class
  public
    //abstract method must be oevrrided
    {$ifdef StaticSort} class {$endif} function Compare(const a : TValue; const b : TKey) : integer;
      virtual; abstract;

    //searching in sorted table
    {$ifdef StaticSort} class {$endif} function BSearch(const Tab : array of TValue; const Key : TKey;
      const a, b : integer) : integer; overload;
    {$ifdef StaticSort} class {$endif} function BSearch(const Tab : array of TValue; const Key : TKey) : integer;
      overload;
  end;
  
{$undef TClassSortName}
{$undef StaticStatus}  
{$undef TClassBSearchName}
