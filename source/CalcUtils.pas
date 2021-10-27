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

unit CalcUtils; 

{$mode objfpc}
{$ModeSwitch typehelpers}

interface

uses
  Classes, Math, Incrementations;

type
  TAxis = (AxisX = 0, AxisY = 1, AxisZ = 2);
  TAxisSet = set of TAxis;
  TVector3 = array[TAxis] of double;
  TMatrix3x3 = array[TAxis, TAxis] of double;
  TIntVector3 = array[TAxis] of integer; //chunk in world
  TBlockCoord = array[TAxis] of byte; //in chunk

  PVector3 = ^TVector3;
  PIntVector3 = ^TIntVector3;

const
  IdentityMatrix : TMatrix3x3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1));

  {Rotation}
  RotateRoll = axisX;
  RotatePich = axisY;
  RotateYaw = axisZ;
  {Size}
  SizeWidth = axisX;
  SizeHeight = axisY;
  SizeDepth = axisZ;

  NextAxis : array[TAxis] of TAxis = (AxisY, AxisZ, AxisX);

type
  TRotationVector = TVector3;
  TSizeVector = TVector3;
  TIDType = (idBlock, idEntity, idItem);

  TIntRange = record
    Min, Max : integer;
  end;

  { TVector3Helper }

  TVector3Helper = type helper for TVector3
  private
    function GetX: Double; inline;
    function GetY: Double; inline;
    function GetZ: Double; inline;
    procedure SetX(const AValue: Double); inline;
    procedure SetY(const AValue: Double); inline;
    procedure SetZ(const AValue: Double); inline;
  public
    property X : Double read GetX write SetX;
    property Y : Double read GetY write SetY;
    property Z : Double read GetZ write SetZ;
    property RotateRoll : Double read GetX write SetX;
    property RotatePich : Double read GetY write SetY;
    property RotateYaw : Double read GetZ write SetZ;
    property Width : Double read GetX write SetX;
    property Height : Double read GetY write SetY;
    property Depth : Double read GetZ write SetZ;
    function Min : Double; overload;
    function Max : Double; overload;
  end;

  { TIntVector3Helper }

  TIntVector3Helper = type helper for TIntVector3
  private
    function GetX: Integer; inline;
    function GetY: Integer; inline;
    function GetZ: Integer; inline;
    procedure SetX(const AValue: Integer); inline;
    procedure SetY(const AValue: Integer); inline;
    procedure SetZ(const AValue: Integer); inline;
  public
    property X : Integer read GetX write SetX;
    property Y : Integer read GetY write SetY;
    property Z : Integer read GetZ write SetZ;    
    property Width : Integer read GetX write SetX;
    property Height : Integer read GetY write SetY;
    property Depth : Integer read GetZ write SetZ;
    function Mask(const BitMask : PtrUInt) : TIntVector3; inline; 
    function Min : Double; overload;
    function Max : Double; overload;
  end;

  { TBlockCoordHelper }

  TBlockCoordHelper = type helper for TBlockCoord
  private
    function GetX: Byte; inline;
    function GetY: Byte; inline;
    function GetZ: Byte; inline;
    procedure SetX(const AValue: Byte); inline;
    procedure SetY(const AValue: Byte); inline;
    procedure SetZ(const AValue: Byte); inline;
  public
    property X : Byte read GetX write SetX;
    property Y : Byte read GetY write SetY;
    property Z : Byte read GetZ write SetZ;
    function Mask(const BitMask : Byte) : TBlockCoord; inline;
  end;

operator +(const a, b : TIntVector3) : TIntVector3; inline;  
operator +(const a : TIntVector3; const b : Integer) : TIntVector3; inline;
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
operator * (const a : TMatrix3x3; const b : Double) : TMatrix3x3;
operator * (const a : Double; const b : TMatrix3x3) : TMatrix3x3;

function floor(const v : TVector3) : TIntVector3; inline; overload;
function round(const v : TVector3) : TIntVector3; inline; overload;
function ceil(const v : TVector3) : TIntVector3; inline; overload;
function sign(const v : TVector3) : TIntVector3; inline; overload;

function Normalize(const v : TVector3) : TVector3; overload; inline;
function Normalize(const v : TVector3; const Default : TVector3) : TVector3; overload; inline;
function Transposing(const m : TMatrix3x3) : TMatrix3x3;
function ReverseMatrix(const m : TMatrix3x3) : TMatrix3x3;
function CreateMatrix3x3(const a, b : TVector3) : TMatrix3x3; //iloczyn macierzowy
function VectorProduct(const a, b : TVector3) : TVector3; inline; //iloczyn wektorowy
function ScalarProduct(const a, b : TVector3) : double; inline; //iloczyn skalarny
function det(const a : TMatrix3x3) : Extended; inline; //wyznacznik macierzy

function CreateRotateMatrix(const Angle : double; const axis : TAxis) : TMatrix3x3; overload;
function CreateRotateMatrix(const Rotate : TRotationVector) : TMatrix3x3; overload; //zyx
function CreateRotateMatrixZXY(const Rotate : TRotationVector) : TMatrix3x3; //zxy (Ourcraft default)
function CreateRotateMatrix(const Rotate : TRotationVector; const FirstAxis, SecondAxis, ThirdAxis : TAxis) : TMatrix3x3; overload;
function CreateRotateMatrixFromVector(const vector : TVector3) : TMatrix3x3;
function IntVector3(const x, y, z : integer) : TIntVector3; inline;
function Vector3(const x, y, z : double) : TVector3; inline;
function BlockCoord(const x, y, z : byte) : TBlockCoord; inline;

function Hypot3(const a, b, c : double) : double; overload; inline;
function Hypot3(const vector : TVector3) : double; overload; inline;  
function SquaredHypot3(const a, b, c : double) : double; overload; inline;
function SquaredHypot3(const vector : TVector3) : double; overload; inline;

function FlatVector(v : TVector3; const axis : TAxisSet) : TVector3;

function Q_rsqrt(const number : single) : single; inline;
function ModuloBuf(const Buf : Pointer; const Size : PtrUInt; const InitValue : PtrUInt = 0; const Base : LongWord = 4294967291) : LongWord;

procedure FromZeroTo2Pi(var d : Double);
procedure FromZeroTo2PiVector(var v : TVector3);

function DecreaseVector(const v : TVector3; const d : Double) : TVector3;
function BiggestDimension(const v : TVector3) : TVector3;
function NormalizeDimensionsIndepedently(const v : TVector3) : TVector3;

procedure UpdateIfLesser(var Vector : TIntVector3; const Test : TIntVector3); overload;
procedure UpdateIfGreater(var Vector : TIntVector3; const Test : TIntVector3); overload;

function CycledShr(const i : Integer; const value : PtrUInt) : Integer; inline;

generic procedure Swap<T>(var a, b : T);
generic procedure MinToLeft<T>(var a, b : T);
generic procedure MaxToLeft<T>(var a, b : T);
generic procedure MinToRight<T>(var a, b : T);
generic procedure MaxToRight<T>(var a, b : T);

implementation

const
  AToI : array[TAxis] of integer = (0, 1, 2);
  IToA : array[0..5] of TAxis = (axisX, axisY, axisZ, axisX, axisY, axisZ);

operator +(const a, b : TIntVector3) : TIntVector3; inline;
begin
  Result[axisX] := a[axisX] + b[axisX];
  Result[axisY] := a[axisY] + b[axisY];
  Result[axisZ] := a[axisZ] + b[axisZ];
end;

operator+(const a: TIntVector3; const b: Integer): TIntVector3;
begin
  Result[AxisX] := a[AxisX] + b;
  Result[AxisY] := a[AxisY] + b;
  Result[AxisZ] := a[AxisZ] + b;
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

function CreateRotateMatrixFromVector(const vector: TVector3): TMatrix3x3;
var
   u : TVector3;
   angle, c, s : Double;
begin
  angle := Hypot3(vector);
  c := cos(angle);
  s := sin(angle);
  u := vector/angle;

  Result[AxisX, AxisX] := c+sqr(u[AxisX])*(1-c);
  Result[AxisY, AxisX] := u[AxisX]*u[AxisY]*(1-c)-u[AxisZ]*s;
  Result[AxisZ, AxisX] := u[AxisX]*u[AxisZ]*(1-c)+u[AxisY]*s;

  Result[AxisX, AxisY] := u[AxisY]*u[AxisX]*(1-c)+u[AxisZ]*s;
  Result[AxisY, AxisY] := c+sqr(u[axisY])*(1-c);
  Result[AxisZ, AxisY] := u[AxisY]*u[AxisZ]*(1-c)-u[AxisX]*s;

  Result[AxisX, AxisZ] := u[AxisZ]*u[AxisX]*(1-c)-u[AxisY]*s;
  Result[AxisY, AxisZ] := u[AxisZ]*u[AxisY]*(1-c)+u[AxisX]*s;
  Result[AxisZ, AxisZ] := c+sqr(u[axisZ])*(1-c);
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

operator*(const a: TMatrix3x3; const b: Double): TMatrix3x3;
begin
  Exit(b*a);
end;

operator*(const a: Double; const b: TMatrix3x3): TMatrix3x3;
var
  i, j : TAxis;
begin
  for i := low(TAxis) to High(TAxis) do
    for j := axisX to axisZ do
      Result[i, j] := b[i, j] * a;
end;

function Hypot3(const a, b, c : double) : double; inline;
begin
  Exit(sqrt(SquaredHypot3(a, b, c)));
end;

function Hypot3(const vector : TVector3) : double;
begin
  Exit(Hypot3(vector[axisX], vector[axisY], vector[axisZ]));
end;

function SquaredHypot3(const a, b, c: double): double;
begin
   Exit(sqr(a) + sqr(b) + sqr(c));
end;

function SquaredHypot3(const vector: TVector3): double;
begin
  Exit(SquaredHypot3(vector[axisX], vector[axisY], vector[axisZ]));
end;

function floor(const v: TVector3): TIntVector3;
begin
  Result[axisX] := floor(v[axisX]);
  Result[axisY] := floor(v[axisY]);
  Result[axisZ] := floor(v[axisZ]);
end;

function round(const v: TVector3): TIntVector3;
begin
  Result[axisX] := round(v[axisX]);
  Result[axisY] := round(v[axisY]);
  Result[axisZ] := round(v[axisZ]);
end;

function ceil(const v: TVector3): TIntVector3;
begin
  Result[axisX] := ceil(v[axisX]);
  Result[axisY] := ceil(v[axisY]);
  Result[axisZ] := ceil(v[axisZ]);
end;

function sign(const v: TVector3): TIntVector3;
begin
  Exit(IntVector3(Math.Sign(v[AxisX]), Math.Sign(v[AxisY]), Math.Sign(v[AxisZ])));
end;

function Normalize(const v : TVector3) : TVector3;
begin
  Exit(Normalize(v, Vector3(1, 0, 0)));
end;

function Normalize(const v: TVector3; const Default: TVector3): TVector3;
var
  len : double;
begin
  len := Hypot3(v);
  if len = 0 then
    exit(Default);
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


function det(const a : TMatrix3x3) : Extended; inline; //wyznacznik
begin
  Result := (
            + Extended(a[axisX, axisX] * a[axisY, axisY] * a[axisZ, axisZ])
            + Extended(a[axisY, axisX] * a[axisZ, axisY] * a[axisX, axisZ])
            + Extended(a[axisZ, axisX] * a[axisX, axisY] * a[axisY, axisZ])
            ) - (
            + Extended(a[axisZ, axisX] * a[axisY, axisY] * a[axisX, axisZ])
            + Extended(a[axisY, axisX] * a[axisX, axisY] * a[axisZ, axisZ])
            + Extended(a[axisX, axisX] * a[axisZ, axisY] * a[axisY, axisZ])
            );
end;

function GetMinorDet(const m : TMatrix3x3; const col, row : TAxis) : Extended; inline;
begin
  Result :=
    Extended(m[IToA[AToI[col] + 1], IToA[AToI[row] + 1]] *
    m[IToA[AToI[col] + 2], IToA[AToI[row] + 2]]) -
    Extended(m[IToA[AToI[col] + 1], IToA[AToI[row] + 2]] *
    m[IToA[AToI[col] + 2], IToA[AToI[row] + 1]]);
end;

function ReverseMatrix(const m : TMatrix3x3) : TMatrix3x3;
var
  d : Extended;
begin
  d := det(m);
  if d = 0 then
    exit(m);

  Result[axisX, axisX] := GetMinorDet(m, axisX, axisX) / d;
  Result[axisY, axisX] := GetMinorDet(m, axisX, axisY) / d;
  Result[axisZ, axisX] := GetMinorDet(m, axisX, axisZ) / d;

  Result[axisX, axisY] := GetMinorDet(m, axisY, axisX) / d;
  Result[axisY, axisY] := GetMinorDet(m, axisY, axisY) / d;
  Result[axisZ, axisY] := GetMinorDet(m, axisY, axisZ) / d;

  Result[axisX, axisZ] := GetMinorDet(m, axisZ, axisX) / d;
  Result[axisY, axisZ] := GetMinorDet(m, axisZ, axisY) / d;
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
  Result[axisZ] := a[axisX] * b[axisY] - a[axisY] * b[axisX];
end;

function ScalarProduct(const a, b : TVector3) : double;  //iloczyn skalarny
begin
  Result := a[axisX] * b[axisX] + a[axisY] * b[axisY] + a[axisZ] * b[axisZ];
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

function FlatVector(v : TVector3; const axis : TAxisSet) : TVector3;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
    if not (a in axis) then
      v[a] := 0;
  Result := Normalize(v);
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

function ModuloBuf(const Buf: Pointer; const Size: PtrUInt;
  const InitValue: PtrUInt; const Base: LongWord): LongWord;
var
  i : PtrInt;
begin
  Result := InitValue;
  for i := (Size shr 2) - 1 downto 0 do
    Result := ((QWord(Result) shl 32) or PLongWord(Buf)[i]) mod Base;
  for i := (Size and 3) downto 1 do
    Result := ((QWord(Result) shl 32) or PByte(Buf)[Size-i]) mod Base;
end;

procedure FromZeroTo2Pi(var d: Double);
const
  border = 2*pi;
begin
  if (d>=0) and (d<border) then
     Exit;
  d -= border*floor(d/border);
end;

procedure FromZeroTo2PiVector(var v: TVector3);
var
  d, d2 : Double;
begin
  d := Hypot3(v);
  if d = 0 then
     Exit;
  d2 := d;
  FromZeroTo2Pi(d2);
  if d <> d2 then
    v := v*(d2/d);
end;

function DecreaseVector(const v: TVector3; const d: Double): TVector3;
var
  l : Double;
begin
  l := Hypot3(v);
  if l = 0 then
     Exit(v);

  if l < d then
     Exit(Vector3(0, 0, 0));

  Exit(v*((l-d)/l));
end;

{ TBlockCoordHelper }

function TBlockCoordHelper.GetX: Byte;
begin
  Result := Self[AxisX];
end;

function TBlockCoordHelper.GetY: Byte;
begin
  Result := Self[AxisY];
end;

function TBlockCoordHelper.GetZ: Byte;
begin
  Result := Self[AxisZ];
end;

procedure TBlockCoordHelper.SetX(const AValue: Byte);
begin
  Self[AxisX] := AValue;
end;

procedure TBlockCoordHelper.SetY(const AValue: Byte);
begin
  Self[AxisY] := AValue;
end;

procedure TBlockCoordHelper.SetZ(const AValue: Byte);
begin
  Self[AxisZ] := AValue;
end;

function TBlockCoordHelper.Mask(const BitMask: Byte): TBlockCoord;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
     Result[a] := Self[a] and BitMask;
end;

{ TIntVector3Helper }

function TIntVector3Helper.GetX: Integer;
begin
  Result := Self[AxisX];
end;

function TIntVector3Helper.GetY: Integer;
begin
  Result := Self[AxisY];
end;

function TIntVector3Helper.GetZ: Integer;
begin
  Result := Self[AxisZ];
end;

procedure TIntVector3Helper.SetX(const AValue: Integer);
begin
   Self[AxisX] := AValue;
end;

procedure TIntVector3Helper.SetY(const AValue: Integer);
begin
   Self[AxisY] := AValue;
end;

procedure TIntVector3Helper.SetZ(const AValue: Integer);
begin
   Self[AxisZ] := AValue;
end;

function TIntVector3Helper.Mask(const BitMask: PtrUInt): TIntVector3;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
     Result[a] := Self[a] and BitMask;
end;

function TIntVector3Helper.Min: Double;
begin
  Result := Math.Min(X, Math.Min(Y, Z));
end;

function TIntVector3Helper.Max: Double;
begin
  Result := Math.Max(X, Math.Max(Y, Z));
end;

{ TVector3Helper }

function TVector3Helper.GetX: Double;
begin
   Result := Self[AxisX];
end;

function TVector3Helper.GetY: Double;
begin
   Result := Self[AxisY];
end;

function TVector3Helper.GetZ: Double;
begin
   Result := Self[AxisZ];
end;

procedure TVector3Helper.SetX(const AValue: Double);
begin
  Self[AxisX] := AValue;
end;

procedure TVector3Helper.SetY(const AValue: Double);
begin
  Self[AxisY] := AValue;
end;

procedure TVector3Helper.SetZ(const AValue: Double);
begin
  Self[AxisZ] := AValue;
end;

function TVector3Helper.Min: Double;
begin
  Result := Math.Min(X, Math.Min(Y, Z))
end;

function TVector3Helper.Max: Double;
begin
  Result := Math.Max(X, Math.Max(Y, Z))
end;

function BiggestDimension(const v: TVector3): TVector3;
begin
  if abs(v[AxisX]) > abs(v[AxisY]) then
  begin
     if abs(v[AxisX]) > abs(v[axisZ]) then
        Exit(Vector3(v[AxisX], 0, 0))
     else
        Exit(Vector3(0, 0, v[AxisZ]));
  end
  else
  begin
     if abs(v[AxisY]) > abs(v[axisZ]) then
        Exit(Vector3(0, v[AxisY], 0))
     else
        Exit(Vector3(0, 0, v[AxisZ]));
  end;
end;

function NormalizeDimensionsIndepedently(const v: TVector3): TVector3;
var
  a : TAxis;
begin
  for a := Low(TAxis) to High(TAxis) do
    Result[a] := sign(v[a]);
end;

procedure UpdateIfLesser(var Vector: TIntVector3; const Test: TIntVector3);
begin
  UpdateIfLesser(Vector[AxisX], Test[AxisX]);
  UpdateIfLesser(Vector[AxisY], Test[AxisY]);
  UpdateIfLesser(Vector[AxisZ], Test[AxisZ]);
end;

procedure UpdateIfGreater(var Vector: TIntVector3; const Test: TIntVector3);
begin
  UpdateIfGreater(Vector[AxisX], Test[AxisX]);
  UpdateIfGreater(Vector[AxisY], Test[AxisY]);
  UpdateIfGreater(Vector[AxisZ], Test[AxisZ]);
end;

function CycledShr(const i : Integer; const value : PtrUInt) : Integer; inline;
begin
     if i < 0 then
        Exit(-1-((-1-i) shr value));
     Exit(i shr value);
end;

generic procedure Swap<T>(var a, b : T);
var
  x : T;
begin
  x := a;
  a := b;
  b := x;
end;

generic procedure MinToLeft<T>(var a, b : T);
begin
     if a > b then
        specialize Swap<T>(a, b);
end;

generic procedure MaxToLeft<T>(var a, b : T);
begin
     if a < b then
        specialize Swap<T>(a, b);
end;

generic procedure MinToRight<T>(var a, b : T);
begin
     specialize MaxToLeft<T>(a, b);
end;

generic procedure MaxToRight<T>(var a, b : T);
begin
     specialize MinToLeft<T>(a, b);
end;

end.
