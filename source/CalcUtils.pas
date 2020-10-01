unit CalcUtils;

{$mode objfpc}
{$Macro on}

interface

uses
  Classes, Math, Sorts;

type
  TAxis = (axisX = 0, axisY = 1, axisZ = 2);
  TAxisSet = set of TAxis;
  TVector3 = array[TAxis] of double;
  TMatrix3x3 = array[TAxis, TAxis] of double;
  TIntVector3 = array[TAxis] of integer; //chunk in world
  TBlockCoord = packed array[TAxis] of byte; //in chunk

const
  IdentityMatrix : TMatrix3x3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1));

  {Rotation}
  rotateRoll = axisX;
  rotatePich = axisY;
  rotateYaw = axisZ;
  {Size}
  sizeWidth = axisX;
  sizeHeight = axisY;
  sizeDepth = axisZ;

type
  TRotationVector = TVector3;
  TSizeVector = TVector3;
  TIDType = (idBlock, idEntity, idItem);

  TIntRange = record
    Min, Max : integer;
  end;

operator +(const a, b : TIntVector3) : TIntVector3; inline;
operator -(const a, b : TIntVector3) : TIntVector3; inline;
operator * (const a : TIntVector3; const b : integer) : TIntVector3; inline;
operator * (const a : integer; const b : TIntVector3) : TIntVector3; inline;
operator := (const a : TBlockCoord) : TIntVector3; inline;
operator := (const a : TIntVector3) : TVector3; inline;

operator * (const a : TVector3; const b : double) : TVector3; inline;
operator * (const a : double; const b : TVector3) : TVector3; inline;
operator / (const a : TVector3; const b : double) : TVector3; inline;
operator +(const a : TVector3; const b : double) : TVector3; inline;
operator -(const a : TVector3; const b : double) : TVector3; inline;
operator +(const a, b : TVector3) : TVector3; inline;
operator -(const a, b : TVector3) : TVector3; inline;
operator = (const a, b : TBlockCoord) : boolean; inline;

operator = (const a, b : TVector3) : boolean; inline;
operator * (const a, b : TMatrix3x3) : TMatrix3x3;
operator +(const a, b : TMatrix3x3) : TMatrix3x3;
operator -(const a, b : TMatrix3x3) : TMatrix3x3;
operator * (const a : TMatrix3x3; const b : TVector3) : TVector3;
function Normalize(const v : TVector3) : TVector3; inline;
function Transposing(const m : TMatrix3x3) : TMatrix3x3;
function ReverseMatrix(const m : TMatrix3x3) : TMatrix3x3;
function CreateMatrix3x3(const a, b : TVector3) : TMatrix3x3; //iloczyn macierzowy
function VectorProduct(const a, b : TVector3) : TVector3; inline; //iloczyn wektorowy
function ScalarProduct(const a, b : TVector3) : double; inline; //iloczyn skalarny
function det(const a : TMatrix3x3) : double; inline; //wyznacznik macierzy

function CreateRotateMatrix(const Angle : double; const axis : TAxis) : TMatrix3x3;
  overload;
function CreateRotateMatrix(const Rotate : TRotationVector) : TMatrix3x3; overload; //zyx
function CreateRotateMatrixZXY(const Rotate : TRotationVector) : TMatrix3x3;
//zxy (Ourcraft default)
function CreateRotateMatrix(const Rotate : TRotationVector;
  const FirstAxis, SecondAxis, ThirdAxis : TAxis) : TMatrix3x3; overload;
function IntVector3(const x, y, z : integer) : TIntVector3; inline;
function Vector3(const x, y, z : double) : TVector3; inline;
function BlockCoord(const x, y, z : byte) : TBlockCoord; inline;

function Hypot3(const a, b, c : double) : double; overload; inline;
function Hypot3(const vector : TVector3) : double; overload; inline;

function FlatVector(v : TVector3; const axis : TAxisSet) : TVector3;

function GetCoordPriorityByDistanceCount : integer; inline;
function GetCoordPriorityByDistance(const i : integer) : TIntVector3; inline;      
function GetCoordPriorityByDistanceLength(const i : integer) : Double; inline;
function GetInverseCoordPriorityByDistance(const x, y, z : integer) : Integer; inline;

function Q_rsqrt(const number : single) : single; inline;

{$define Ordinal_Type := uint64}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint32}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint16}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint8}
{$include incrementation_headers.inc}

{$define Ordinal_Type := int64}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int32}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int16}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int8}
{$include incrementation_headers.inc}

{$UnDef Ordinal_Type}

implementation

type

  { TIntVector3Sort }

  TIntVector3Sort = class(specialize TSort<TIntVector3>)
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

const
  NearPrioritySide = 64;
  AToI : array[TAxis] of integer = (0, 1, 2);
  IToA : array[0..5] of TAxis = (axisX, axisY, axisZ, axisX, axisY, axisZ);

var
  NearPriority : array[0..(2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) *
    (2 * NearPrioritySide + 1) - 1] of TIntVector3;
  NearPriorityLengths : array[0..(2 * NearPrioritySide + 1) * (2 * NearPrioritySide + 1) *
    (2 * NearPrioritySide + 1) - 1] of Double;
  NearPriorityInverse : array[-NearPrioritySide..NearPrioritySide, -NearPrioritySide..NearPrioritySide, -NearPrioritySide..NearPrioritySide] of uint32;

  {$define Ordinal_Type := uint64}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint32}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint16}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint8}
  {$include incrementations.inc}

  {$define Ordinal_Type := int64}
  {$include incrementations.inc}
  {$define Ordinal_Type := int32}
  {$include incrementations.inc}
  {$define Ordinal_Type := int16}
  {$include incrementations.inc}
  {$define Ordinal_Type := int8}
  {$include incrementations.inc}
  {$UnDef Ordinal_Type}

operator +(const a, b : TIntVector3) : TIntVector3; inline;
begin
  Result[axisX] := a[axisX] + b[axisX];
  Result[axisY] := a[axisY] + b[axisY];
  Result[axisZ] := a[axisZ] + b[axisZ];
end;

operator-(const a, b: TIntVector3): TIntVector3;
begin
  Result[axisX] := a[axisX] - b[axisX];
  Result[axisY] := a[axisY] - b[axisY];
  Result[axisZ] := a[axisZ] - b[axisZ];
end;

operator * (const a : TIntVector3; const b : integer) : TIntVector3;
begin
  Result[axisX] := a[axisX] * b;
  Result[axisY] := a[axisY] * b;
  Result[axisZ] := a[axisZ] * b;
end;

operator * (const a : integer; const b : TIntVector3) : TIntVector3;
begin
  Result := b * a;
end;

operator := (const a : TBlockCoord) : TIntVector3; inline;
begin
  Result[axisX] := a[axisX];
  Result[axisY] := a[axisY];
  Result[axisZ] := a[axisZ];
end;

operator := (const a : TIntVector3) : TVector3; inline;
begin
  Result[axisX] := a[axisX];
  Result[axisY] := a[axisY];
  Result[axisZ] := a[axisZ];
end;

operator * (const a : TVector3; const b : double) : TVector3; inline;
begin
  Result[axisX] := a[axisX] * b;
  Result[axisY] := a[axisY] * b;
  Result[axisZ] := a[axisZ] * b;
end;

operator * (const a : double; const b : TVector3) : TVector3;
begin
  Result := b * a;
end;

operator / (const a : TVector3; const b : double) : TVector3; inline;
begin
  Result[axisX] := a[axisX] / b;
  Result[axisY] := a[axisY] / b;
  Result[axisZ] := a[axisZ] / b;
end;

operator +(const a : TVector3; const b : double) : TVector3; inline;
begin
  Result[axisX] := a[axisX] + b;
  Result[axisY] := a[axisY] + b;
  Result[axisZ] := a[axisZ] + b;
end;

operator -(const a : TVector3; const b : double) : TVector3; inline;
begin
  Result[axisX] := a[axisX] - b;
  Result[axisY] := a[axisY] - b;
  Result[axisZ] := a[axisZ] - b;
end;

operator +(const a, b : TVector3) : TVector3; inline;
begin
  Result[axisX] := a[axisX] + b[axisX];
  Result[axisY] := a[axisY] + b[axisY];
  Result[axisZ] := a[axisZ] + b[axisZ];
end;

operator -(const a, b : TVector3) : TVector3; inline;
begin
  Result[axisX] := a[axisX] - b[axisX];
  Result[axisY] := a[axisY] - b[axisY];
  Result[axisZ] := a[axisZ] - b[axisZ];
end;

operator = (const a, b : TBlockCoord) : boolean; inline;
begin
  Result := (a[axisX] = b[axisX]) and (a[axisY] = b[axisY]) and (a[axisZ] = b[axisZ]);
end;

function IntVector3(const x, y, z : integer) : TIntVector3; inline;
begin
  Result[axisX] := x;
  Result[axisY] := y;
  Result[axisZ] := z;
end;

function Vector3(const x, y, z : double) : TVector3; inline;
begin
  Result[axisX] := x;
  Result[axisY] := y;
  Result[axisZ] := z;
end;

function BlockCoord(const x, y, z : byte) : TBlockCoord; inline;
begin
  Result[axisX] := x;
  Result[axisY] := y;
  Result[axisZ] := z;
end;

operator = (const a, b : TVector3) : boolean;
begin
  Result := (a[axisX] = b[axisX]) and (a[axisY] = b[axisY]) and (a[axisZ] = b[axisZ]);
end;

operator * (const a, b : TMatrix3x3) : TMatrix3x3;
var
  i, j, k : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := low(TAxis) to High(TAxis) do
    begin
      Result[i, j] := 0;
      for k := axisX to axisZ do
        Result[i, j] += a[k, i] * b[j, k];
    end;
end;

operator +(const a, b : TMatrix3x3) : TMatrix3x3;
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := low(TAxis) to High(TAxis) do
      Result[i, j] := a[i, j] + b[i, j];
end;

operator -(const a, b : TMatrix3x3) : TMatrix3x3;
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := low(TAxis) to High(TAxis) do
      Result[i, j] := a[i, j] - b[i, j];
end;

operator * (const a : TMatrix3x3; const b : TVector3) : TVector3;
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
  begin
    Result[i] := 0;
    for j := axisX to axisZ do
      Result[i] += a[j, i] * b[j];
  end;
end;

function Normalize(const v : TVector3) : TVector3;
var
  len : double;
begin
  len := Hypot3(v);
  if len = 0 then
    exit(Vector3(1, 0, 0));
  Result := v / len;
end;

function Transposing(const m : TMatrix3x3) : TMatrix3x3;
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := low(TAxis) to High(TAxis) do
      Result[i, j] := m[j, i];
end;

function GetMinorDet(const m : TMatrix3x3; const col, row : TAxis) : double; inline;
begin
  Result :=
    m[IToA[AToI[col] + 1], IToA[AToI[row] + 1]] *
    m[IToA[AToI[col] + 2], IToA[AToI[row] + 2]] -
    m[IToA[AToI[col] + 1], IToA[AToI[row] + 2]] *
    m[IToA[AToI[col] + 2], IToA[AToI[row] + 1]];
end;

function ReverseMatrix(const m : TMatrix3x3) : TMatrix3x3;
var
  d : double;
begin
  d := det(m);
  if d = 0 then
    exit(m);

  Result[axisX, axisX] := GetMinorDet(m, axisX, axisX) / d;
  Result[axisY, axisX] := -GetMinorDet(m, axisX, axisY) / d;
  Result[axisZ, axisX] := GetMinorDet(m, axisX, axisZ) / d;

  Result[axisX, axisY] := -GetMinorDet(m, axisY, axisX) / d;
  Result[axisY, axisY] := GetMinorDet(m, axisY, axisY) / d;
  Result[axisZ, axisY] := -GetMinorDet(m, axisY, axisZ) / d;

  Result[axisX, axisZ] := GetMinorDet(m, axisZ, axisX) / d;
  Result[axisY, axisZ] := -GetMinorDet(m, axisZ, axisY) / d;
  Result[axisZ, axisZ] := GetMinorDet(m, axisZ, axisZ) / d;
end;

function CreateMatrix3x3(const a, b : TVector3) : TMatrix3x3;  //iloczyn macierzowy ab
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := low(TAxis) to High(TAxis) do
      Result[i, j] := a[i] * b[j];
end;

function VectorProduct(const a, b : TVector3) : TVector3;  //iloczyn wektorowy a x b
begin
  Result[axisX] := a[axisY] * b[axisZ] - a[axisZ] * b[axisY];
  Result[axisY] := a[axisZ] * b[axisX] - a[axisX] * b[axisZ];
  Result[axisZ] := a[axisX] * b[axisY] - a[axisY] * b[axisZ];
end;

function ScalarProduct(const a, b : TVector3) : double;  //iloczyn skalarny
begin
  Result := a[axisX] * b[axisX] + a[axisY] * b[axisY] + a[axisZ] * b[axisZ];
end;

function det(const a : TMatrix3x3) : double; inline; //wyznacznik
begin
  Result := (a[axisX, axisX] * a[axisY, axisY] * a[axisZ, axisZ]) +
    (a[axisY, axisX] * a[axisZ, axisY] * a[axisX, axisZ]) +
    (a[axisZ, axisX] * a[axisX, axisY] * a[axisY, axisZ]) -
    (a[axisZ, axisX] * a[axisY, axisY] * a[axisX, axisZ]) -
    (a[axisY, axisX] * a[axisX, axisY] * a[axisZ, axisZ]) -
    (a[axisX, axisX] * a[axisZ, axisY] * a[axisY, axisZ]);
end;

function CreateRotateMatrix(const Angle : double; const axis : TAxis) : TMatrix3x3;
  overload;
var
  c, s : double;
  NewX, NewY, NewZ : TAxis;
begin
  c := cos(Angle);
  s := sin(Angle);

  NewX := IToA[AToI[axisX] + AToI[axis]];
  NewY := IToA[AToI[axisY] + AToI[axis]];
  NewZ := IToA[AToI[axisZ] + AToI[axis]];

  Result[NewX, NewX] := 1;
  Result[NewY, NewX] := 0;
  Result[NewZ, NewX] := 0;
  Result[NewX, NewY] := 0;
  Result[NewY, NewY] := c;
  Result[NewZ, NewY] := -s;
  Result[NewX, NewZ] := 0;
  Result[NewY, NewZ] := s;
  Result[NewZ, NewZ] := c;
end;

function CreateRotateMatrix(const Rotate : TRotationVector;
  const FirstAxis, SecondAxis, ThirdAxis : TAxis) : TMatrix3x3; overload;
begin
  Result := CreateRotateMatrix(Rotate[FirstAxis], FirstAxis) *
    CreateRotateMatrix(Rotate[SecondAxis], SecondAxis) *
    CreateRotateMatrix(Rotate[ThirdAxis], ThirdAxis);
end;

function CreateRotateMatrixZXY(const Rotate : TRotationVector) : TMatrix3x3;
begin
  Result := CreateRotateMatrix(Rotate, axisZ, axisX, axisY);
end;

function CreateRotateMatrix(const Rotate : TRotationVector) : TMatrix3x3; overload;
begin
  Result := CreateRotateMatrix(Rotate, axisZ, axisY, axisX);
end;

function Hypot3(const a, b, c : double) : double; inline;
begin
  Result := sqrt(sqr(a) + sqr(b) + sqr(c));
end;

function Hypot3(const vector : TVector3) : double;
begin
  Result := Hypot3(vector[axisX], vector[axisY], vector[axisZ]);
end;

procedure InitNearPriority;
var
  x, y, z, i : integer;
begin
  i := 0;
  for x := -NearPrioritySide to NearPrioritySide do
    for y := -NearPrioritySide to NearPrioritySide do
      for z := -NearPrioritySide to NearPrioritySide do
        NearPriority[PostInc(i)] := IntVector3(x, y, z);
  TIntVector3Sort.InsertComb(NearPriority);
  for i := 0 to GetCoordPriorityByDistanceCount -1 do
  begin
    NearPriorityInverse[NearPriority[i][axisX], NearPriority[i][axisY], NearPriority[i][axisZ]] := i;
    NearPriorityLengths[i] := hypot3(NearPriority[i]);
  end;
  writeln(sizeof(NearPriorityInverse) + sizeof(NearPriorityLengths) + sizeof(NearPriority));
end;

function GetCoordPriorityByDistance(const i : integer) : TIntVector3; inline;
begin
  Result := NearPriority[i]; // mod length(NearPriority) ?
end;

function GetCoordPriorityByDistanceLength(const i : integer) : Double; inline;
begin
  Result := NearPriorityLengths[i]; // mod length(NearPriority) ?
end;

function GetInverseCoordPriorityByDistance(const x, y, z : integer) : Integer; inline;
begin
  Result := NearPriorityInverse[x, y, z];
end;

function FlatVector(v : TVector3; const axis : TAxisSet) : TVector3;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
    if not (a in axis) then
      v[a] := 0;
  Result := Normalize(v);
end;

function GetCoordPriorityByDistanceCount : integer; inline;
begin
  Result := Length(NearPriority);
end;

function Q_rsqrt(const number : single) : single; inline;
const
  threehalfs : single = 1.5;
var
  i : longint;
  x2, y : single;
begin
  x2 := number * 0.5;
  y := number;
  i := PLongInt(@number)^;
  i := $5f3759df - (i shr 1);
  y := PSingle(@i)^;
  y := y * (threehalfs - (x2 * y * y));
  y := y * (threehalfs - (x2 * y * y));
  Result := y;
end;

{ TIntVector3Sort }

class function TIntVector3Sort.Compare(const a, b : TValue) : integer;
begin
  Result := sign(Hypot3(a[axisX], a[axisY], a[axisZ]) -
    Hypot3(b[axisX], b[axisY], b[axisZ]));
end;

initialization
  InitNearPriority;

end.
