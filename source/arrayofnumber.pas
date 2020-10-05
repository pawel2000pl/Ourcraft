unit ArrayOfNumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Collections;

type

  { TArrayOfNumber }

  generic TArrayOfNumber<TNumber> = class(specialize TCustomOrderArray<TNumber>) //allowed: Double, Single, Integer etc.
  public
    function GetAverage : Double;
    function GetStd(const Average : Double) : Double;
    procedure GetAverageAndStd(out Average, Std : Double);

    function GetMinIndex : Integer;
    function GetMaxIndex : Integer;
  end;

  TArrayOfDouble = specialize TArrayOfNumber<Double>;
  TArrayOfSingle = specialize TArrayOfNumber<Single>;   
  TArrayOfInteger = specialize TArrayOfNumber<Integer>;

implementation

{ TArrayOfNumber }

function TArrayOfNumber.GetAverage: Double;
var
  i, c : Integer;
begin
  Result := 0;
  c := GetCount;
  for i := 0 to c-1 do
    Result += Get(i);
  Result /= c;
end;

function TArrayOfNumber.GetStd(const Average: Double): Double;
var
  i, c : Integer;
begin
  Result := 0;
  c := GetCount;
  for i := 0 to c-1 do
    Result += sqr(Double(Average-Get(i)));
  Result := sqrt(Result)/c;
end;

procedure TArrayOfNumber.GetAverageAndStd(out Average, Std: Double);
begin
  Average := GetAverage;
  Std := GetStd(Average);
end;

function TArrayOfNumber.GetMinIndex: Integer;
var
  i, im : Integer;
  m : TNumber;
begin
  im := 0;
  m := Self[0];
  for i := 1 to Count-1 do
    if Self[i] < m then
    begin
      m := Self[i];
      im := i;
    end;
   Result := im;
end;

function TArrayOfNumber.GetMaxIndex: Integer;
var
  i, im : Integer;
  m : TNumber;
begin
  im := 0;
  m := Self[0];
  for i := 1 to Count-1 do
    if Self[i] > m then
    begin
      m := Self[i];
      im := i;
    end;
   Result := im;
end;

end.

