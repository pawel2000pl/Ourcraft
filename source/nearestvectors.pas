unit NearestVectors;

{$mode objfpc}{$H+}

interface

uses
  CalcUtils;

function GetCoordPriorityByDistanceCount: integer; inline;
function GetCoordPriorityByDistance(const i: integer): TIntVector3; inline;
function GetCoordPriorityByDistanceLength(const i: integer): double; inline;
function GetInverseCoordPriorityByDistance(const x, y, z: integer): integer; inline;

implementation

uses
  Sorts, PostPreOperations, Math;

type

  { TIntVector3Sort }

  TIntVector3Sort = class(specialize TStaticSort<TIntVector3>)
  {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
    type
    TValue = TIntVector3;
  {$ENDIF}
  public
    class function Compare(const a, b: TValue): integer; override;
  end;

const
  NearPrioritySide = 64;

var
  NearPriority: array[0..(2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) - 1] of TIntVector3;
  NearPriorityLengths: array[0..(2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) - 1] of double;
  NearPriorityInverse: array[-NearPrioritySide..NearPrioritySide, -NearPrioritySide..NearPrioritySide, -NearPrioritySide..NearPrioritySide] of uint32;

function GetCoordPriorityByDistance(const i: integer): TIntVector3;
begin
  Result := NearPriority[i];
end;

function GetCoordPriorityByDistanceLength(const i: integer): double;
begin
  Result := NearPriorityLengths[i];
end;

function GetInverseCoordPriorityByDistance(const x, y, z: integer): integer;
begin
  Result := NearPriorityInverse[x, y, z];
end;

function GetCoordPriorityByDistanceCount: integer;
begin
  Result := Length(NearPriority);
end;

{ TIntVector3Sort }

class function TIntVector3Sort.Compare(const a, b: TValue): integer;
begin
  Result := sign(Hypot3(a[axisX], a[axisY], a[axisZ]) - Hypot3(b[axisX], b[axisY], b[axisZ]));
end;

procedure InitNearPriority;
var
  x, y, z, i: integer;
begin
  i := 0;
  for x := -NearPrioritySide to NearPrioritySide do
    for y := -NearPrioritySide to NearPrioritySide do
      for z := -NearPrioritySide to NearPrioritySide do
        NearPriority[PostInc(i)] := IntVector3(x, y, z);
  TIntVector3Sort.InsertCombF(NearPriority);
  for i := 0 to GetCoordPriorityByDistanceCount - 1 do
  begin
    NearPriorityInverse[NearPriority[i][axisX], NearPriority[i][axisY], NearPriority[i][axisZ]] := i;
    NearPriorityLengths[i] := hypot3(NearPriority[i]);
  end;
end;

initialization
  InitNearPriority;

end.
